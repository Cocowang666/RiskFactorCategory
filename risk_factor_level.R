library(tidyverse)
library(lubridate)
library(dplyr)
sample <- read.csv('sample.csv')
GP_diagnosis <- read.csv('GP_diagnosis.csv')
prescription <- read.csv('prescription.csv')
#BMI#
sample_bmi <- sample %>% 
  mutate(date = as.Date(date)) %>% 
  group_by(ppid) %>% 
  arrange(date) %>% 
  slice_tail(n = 1) %>% 
  mutate(
    bmi_risk = case_when(bmi < 18.5| bmi >= 30 ~ 'High Risk',
                         bmi > 25 & bmi < 30 ~ 'Medium Risk',
                         bmi >= 18.5 & bmi <= 25 ~ 'Low Risk',
                         TRUE ~ NA_character_
    )
  ) %>% 
  select(ppid,bmi_risk)

#alcohol#
sample_alcohol0 <- sample %>% 
  mutate(date = as.Date(date)) %>% 
  group_by(ppid) %>% 
  arrange(date) %>% 
  slice_tail(n=1) %>% 
  mutate(alcohol = case_when(
    str_detect(alcohol,'Heavy')|str_detect(alcohol,'Very Heavy')|str_detect(alcohol,'Above') ~ 'High Risk',
    str_detect(alcohol,'Rarely')|str_detect(alcohol,'Moderate')|str_detect(alcohol,'Non-drinker') ~ 'Low Risk',
    TRUE ~ as.character(alcohol)
  ))

diagosis_alcohol <- GP_diagnosis %>% 
  filter(diagnosis=='Alcoholic Related') %>% 
  mutate(datetime=as.Date(datetime)) %>% 
  group_by(ppid) %>%
  arrange(datetime) %>% 
  slice_tail(n=1) %>% 
  mutate(diagnosis = 'High Risk')

sample_alcohol <- sample_alcohol0 %>% 
  full_join(diagosis_alcohol,by='ppid')%>% 
  group_by(ppid) %>% 
  mutate(alcohol_risk = case_when(
    max(datetime,date,na.rm = TRUE) == datetime ~ diagnosis,
    max(datetime,date,na.rm = TRUE) == date ~ alcohol,
    TRUE ~ NA_character_
  )) %>% 
  select(ppid,alcohol_risk)

#blood pressure#
sample_systolic <- sample %>% 
  select(ppid,date,systolic) %>% 
  mutate(date = as.Date(date)) %>%
  group_by(ppid) %>% 
  arrange(date) %>% 
  slice_tail(n=2) %>% 
  summarise(
    avg_sbp = round(mean(systolic, na.rm=TRUE), 1),
    date_sbp = max(date, na.rm=TRUE)
  ) %>%
  mutate(systolic_risk = case_when(
    avg_sbp > 140 ~ 'High Risk',
    avg_sbp <= 140 ~ 'Low Risk',
    TRUE ~ NA_character_
  )) %>% 
  select(ppid,systolic_risk,date_sbp)

sample_diastolic <- sample %>% 
  select(ppid,date,diastolic) %>% 
  mutate(date = as.Date(date)) %>%
  group_by(ppid) %>% 
  arrange(date) %>% 
  slice_tail(n=2) %>% 
  summarise(
    avg_dbp = round(mean(diastolic, na.rm=TRUE), 1),
    date_dbp = max(date, na.rm=TRUE)
  ) %>%
  mutate(diastolic_risk = case_when(
    avg_dbp > 90 ~ 'High Risk',
    avg_dbp <= 90 ~ 'Low Risk',
    TRUE ~ NA_character_
  )) %>% 
  select(ppid,diastolic_risk,date_dbp)

sample_bp0 <- sample_systolic %>% 
  full_join(sample_diastolic,by = 'ppid')%>% 
  mutate(bp = case_when(
    systolic_risk == 'High Risk' | diastolic_risk == 'High Risk' ~ 'High Risk',
    TRUE ~ 'Low Risk'
  )) 
select(ppid,bp,date_sbp)

diagosis_bp <- GP_diagnosis %>% 
  filter(diagnosis == 'Hypertension') %>% 
  group_by(ppid) %>% 
  mutate(datetime=as.Date(datetime)) %>% 
  arrange(datetime) %>% 
  slice_tail(n = 1) %>% 
  mutate(diagnosis='High Risk')

sample_bp <- sample_bp0 %>% 
  full_join(diagosis_bp,by='ppid')%>% 
  group_by(ppid) %>% 
  mutate(bp_risk = case_when(max(datetime,date_sbp,na.rm = TRUE) == datetime ~ diagnosis,
                             max(datetime,date_sbp,na.rm = TRUE) == date_sbp ~ bp,
                             TRUE ~ NA_character_)) %>% 
  select(ppid,bp_risk)

#smoking#
sample_smoking0 <- sample %>% 
  select(ppid,date,smoking)

sample_smoking1 <- sample_smoking0 %>% 
  group_by(ppid) %>% 
  arrange(date) %>% 
  slice_tail(n=1) %>% 
  mutate(smoking1 = case_when(smoking == 'Current Smoker' ~ 'High Risk',
                             smoking == 'Non Smoker' ~ 'Low Risk',
                             TRUE ~ NA_character_))

sample_smoking2 <- sample_smoking0 %>% 
  group_by(ppid) %>% 
  arrange(date) %>% 
  mutate(prev_status = lag(smoking),prev_datetime=lag(date)) %>% 
  filter(smoking == 'Ex Smoker' & (prev_status == 'Current Smoker'|prev_status == 'Non Smoker')) %>% 
  mutate(max_datetime = max(prev_datetime)) %>% 
  filter(prev_datetime >= max_datetime) %>% 
  ungroup() %>% 
  select(ppid,prev_datetime,date) %>% 
  rename(trans_end_date = date, trans_start_date = prev_datetime)

sample_smoking3 <-sample_smoking2 %>% 
  right_join(sample_smoking0,by='ppid') %>% 
  filter(date >= trans_end_date) %>% 
  group_by(ppid) %>% 
  mutate(last_date = max(date),
         smoking3 = case_when(year(last_date)-year(trans_end_date) >= 5 ~ 'Low Risk',
                             year(last_date)-year(trans_start_date) < 5 ~ 'Medium Risk',
                             year(last_date)-year(trans_end_date) < 5 & year(last_date)-year(trans_start_date) >= 5 ~ 'Medium Risk',
                             TRUE ~ NA_character_)) %>% 
  select(ppid,smoking3)

sample_smoking4 <- sample_smoking3 %>% 
  right_join(sample_smoking1,by='ppid')%>% 
  mutate(smoking4 = case_when(is.na(smoking3)~smoking1,
                             is.na(smoking1)~smoking3,
                             TRUE ~ as.character(smoking1))) %>% 
  select(ppid,smoking4)

sample_smoking5 <- sample_smoking4 %>% 
  filter(!smoking4 %in% c('High Risk','Medium Risk','Low Risk')) %>% 
  select(ppid,smoking4) %>% 
  left_join(sample_smoking0,by='ppid') %>% 
  group_by(ppid) %>% 
  arrange(date) %>% 
  mutate(first_date = min(date),
         last_date = max(date),
         smoking5 = case_when(year(last_date)-year(first_date) < 5~'Medium Risk',
                              year(last_date)-year(first_date) >= 5~'Low Risk',
                              TRUE ~ NA_character_)) %>%
  select(ppid,smoking5)

smoking_id <- sample_smoking0 %>% 
  select(ppid) %>% 
  distinct()

sample_smoking6 <- sample_smoking4 %>% 
  left_join(sample_smoking5,by='ppid') %>% 
  mutate(smoking_risk = case_when(is.na(smoking4) ~ smoking5,
                             is.na(smoking5) ~ smoking4,
                             TRUE ~ as.character(smoking5))) %>% 
  select(ppid,smoking_risk) %>% 
  distinct()


sample_smoking <- smoking_id %>% 
  left_join(sample_smoking6,by='ppid') 


#lipids#
sample_ldl <- sample %>% 
  select(ppid,date,ldl) %>% 
  group_by(ppid) %>% 
  arrange(date) %>% 
  slice_tail(n=1) %>% 
  rename(date_ldl = date)%>%
  mutate(ldl_risk = case_when(ldl >= 3 ~ 'High Risk',
                         ldl < 3 ~ 'Low Risk',
                         TRUE ~ NA_character_)) 
 
sample_hdl <- sample %>% 
  select(ppid,sex,date,hdl) %>% 
  group_by(ppid) %>% 
  arrange(date) %>% 
  slice_tail(n=1) %>% 
  rename(date_hdl = date)%>%
  mutate(hdl_risk = case_when(hdl < 1.2 & sex == 'female' ~ 'High Risk',
                         hdl < 1 & sex == 'male' ~ 'High Risk',
                         hdl >= 1.2 & sex == 'female' ~ 'Low Risk',
                         hdl >= 1 & sex == 'male' ~ 'Low Risk',
                         TRUE ~ NA_character_)) 
 

sample_tri <- sample %>% 
  select(ppid,date,triglycerides) %>% 
  group_by(ppid) %>% 
  arrange(date) %>% 
  slice_tail(n=1) %>% 
  rename(date_tri = date)%>%
  mutate(tri_risk = case_when(triglycerides >= 2.3 ~ 'High Risk',
                                   triglycerides < 2.3 ~ 'Low Risk',
                                   TRUE ~ NA_character_)) 


pres<- prescription %>% 
  mutate(paiddate = as.Date(paiddate)) %>%
  group_by(ppid) %>%
  arrange(paiddate) %>%
  slice_tail(n=1) %>%
  select(ppid, paiddate, statin) %>% 
  mutate(statin_risk=if_else(statin == 1,'High Risk',NA)) 


sample_lipids <- sample_ldl %>%
  full_join(sample_hdl, by="ppid") %>%
  full_join(sample_tri, by="ppid") %>%
  full_join(pres, by="ppid") %>%
  mutate(
    lab_risk = case_when(
      ldl_risk == "High Risk" |
        tri_risk == "High Risk" |
        hdl_risk == "High Risk" ~ "High Risk",
      ldl_risk == "Low Risk" &
        tri_risk == "Low Risk" &
        hdl_risk == "Low Risk" ~ "Low Risk",
      TRUE ~ NA_character_
    ),
    
    days_since_rx = as.numeric(pmax(date_ldl, date_hdl, date_tri, na.rm=TRUE) - paiddate),
    
    lipid_risk = case_when(
      lab_risk == "High Risk" ~ "High Risk",
      lab_risk == "Low Risk" & (is.na(statin_risk) | days_since_rx >= 180) ~ "Low Risk",
      lab_risk == "Low Risk" & days_since_rx < 180 ~ "High Risk", 
      TRUE ~ NA_character_
    )
  ) %>%
  select(ppid, lipid_risk)
  
  
  

