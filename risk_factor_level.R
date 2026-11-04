library(dplyr)
library(lubridate)

sample       <- sample       %>% mutate(date     = as.Date(date))
GP_diagnosis <- GP_diagnosis %>% mutate(datetime = as.Date(datetime))
prescription <- prescription %>% mutate(paiddate = as.Date(paiddate))

ids <- distinct(sample, ppid)

# latest row per person 
latest_by <- function(df, date_col) {
  df %>% group_by(ppid) %>% slice_max({{ date_col }}, n = 1, with_ties = FALSE) %>% ungroup()
}

# latest diagnosis date per person for one diagnosis
latest_diag <- function(label, name) {
  GP_diagnosis %>% filter(diagnosis == label) %>%
    group_by(ppid) %>% summarise("{name}" := max(datetime), .groups = "drop")
}


# ---- BMI --------------------------------------------------------
bmi_risk <- latest_by(sample, date) %>%
  transmute(ppid, bmi = case_when(
    bmi < 18.5 | bmi >= 30  ~ "High Risk",
    bmi > 25               ~ "Medium Risk",
    TRUE                   ~ "Low Risk"))


# ---- Alcohol -------
alcohol_risk <- latest_by(sample, date) %>%
  select(ppid, alc_date = date, alcohol) %>%
  full_join(latest_diag("Alcoholic Related", "diag_date"), by = "ppid") %>%
  transmute(ppid, alcohol = case_when(
    !is.na(diag_date) & (is.na(alc_date) | diag_date >= alc_date) ~ "High Risk",
    grepl("Heavy|Above", alcohol)                                 ~ "High Risk",
    TRUE                                                          ~ "Low Risk"))


# ---- Blood pressure----------------------------------------------
bp_risk <- sample %>%
  group_by(ppid) %>% slice_max(date, n = 2, with_ties = FALSE) %>%
  summarise(avg_sbp = round(mean(systolic), 1),
            avg_dbp = round(mean(diastolic), 1),
            bp_date = max(date), .groups = "drop") %>%
  full_join(latest_diag("Hypertension", "htn_date"), by = "ppid") %>%
  transmute(ppid, bp = case_when(
    !is.na(htn_date) & (is.na(bp_date) | htn_date >= bp_date) ~ "High Risk",
    avg_sbp > 140 | avg_dbp > 90                              ~ "High Risk",
    TRUE                                                      ~ "Low Risk"))


# ---- Smoking ----------------------------------------------------
smoking_risk <- sample %>%
  arrange(ppid, date) %>%
  group_by(ppid) %>%
  summarise(
    latest     = last(smoking),
    last_date  = last(date),
    first_date = first(date),
    # last record where status becomes "Ex Smoker" from Current/Non
    quit_date  = suppressWarnings(max(date[smoking == "Ex Smoker" &
                                             lag(smoking) %in% c("Current Smoker", "Non Smoker")])),
    .groups = "drop") %>%
  mutate(quit_date = if_else(is.finite(quit_date), quit_date, as.Date(NA))) %>%
  transmute(ppid, smoking = case_when(
    latest == "Current Smoker"                                   ~ "High Risk",
    latest == "Non Smoker"                                       ~ "Low Risk",
    !is.na(quit_date) & year(last_date) - year(quit_date) >= 5   ~ "Low Risk",
    !is.na(quit_date)                                            ~ "Medium Risk",
    year(last_date) - year(first_date) >= 5                      ~ "Low Risk",    # fallback
    TRUE                                                         ~ "Medium Risk"))


# ---- Lipids -----------------------------------------------------
lipid_risk <- latest_by(sample, date) %>%
  select(ppid, lab_date = date, ldl, hdl, tri, sex) %>%
  left_join(prescription %>% group_by(ppid) %>%
              summarise(rx_date = max(paiddate), .groups = "drop"), by = "ppid") %>%
  mutate(
    lab_high = ldl >= 3 | tri >= 2.3 |
      (sex == "male" & hdl < 1) | (sex == "female" & hdl < 1.2),
    days_rx  = as.numeric(lab_date - rx_date)) %>%
  transmute(ppid, lipid = case_when(
    lab_high                         ~ "High Risk",
    !is.na(rx_date) & days_rx < 180  ~ "High Risk",
    TRUE                             ~ "Low Risk"))


# ---- Combine ----------------------------------------------------
risk_levels <- ids %>%
  left_join(bmi_risk,     by = "ppid") %>%
  left_join(alcohol_risk, by = "ppid") %>%
  left_join(bp_risk,      by = "ppid") %>%
  left_join(smoking_risk, by = "ppid") %>%
  left_join(lipid_risk,   by = "ppid")
