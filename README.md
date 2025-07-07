# README

## Overview

This R script performs data cleaning and risk stratification on a primary care dataset to classify patients by risk factors relevant for cardiovascular disease prevention. It uses linked datasets including biometrics, GP diagnoses, and prescriptions, and applies clinical rules to determine risk levels for:

* **BMI**
* **Alcohol consumption**
* **Blood pressure**
* **Smoking status**
* **Lipid profiles**

Each risk factor is categorized as *High Risk*, *Medium Risk*, or *Low Risk* based on clinical thresholds and time-based criteria.
The deailed catetorize rule is in the supplementary material(Table S1)

---

## Files Required

* `sample.csv`

  * includes patient-level repeated biometrics data (BMI, blood pressure, smoking, alcohol, lipids)
  * columns: `ppid`, `date`, `bmi`, `systolic`, `diastolic`, `smoking`, `alcohol`, `ldl`, `hdl`, `triglycerides`, etc.

* `GP_diagnosis.csv`

  * contains GP-coded diagnoses
  * columns: `ppid`, `diagnosis`, `datetime`

* `prescription.csv`

  * contains prescriptions
  * columns: `ppid`, `paiddate`, `statin`

---

## How to Run

1. Open RStudio or an R terminal.
2. Place all three CSV files (`sample.csv`, `GP_diagnosis.csv`, `prescription.csv`) in your working directory.
3. Load the script file.
4. Install required packages (if not already installed):

```r
install.packages(c("tidyverse","lubridate"))
```

5. Run the script.

It will read in the data and produce the following data frames in memory:

* `sample_bmi`: risk categories for BMI
* `sample_alcohol`: risk categories for alcohol
* `sample_bp`: risk categories for blood pressure
* `sample_smoking`: risk categories for smoking
* `sample_lipids`: risk categories for lipids

---

## Risk Rules Applied

See more details in supplementary metarial(Table S1)
---

## Output

This script currently just loads data, processes, and saves the risk-classified dataframes in memory. You can then export any of these results to CSV, for example:

```r
write.csv(sample_bmi, "sample_bmi.csv", row.names = FALSE)
```

---

## Dependencies

* `tidyverse`
* `lubridate`
* `dplyr`

---

## Notes

* Dates must be in `YYYY-MM-DD` format in the CSV files
* Code assumes there are no duplicate patients with identical `ppid` and `date` within each domain
* If you add new diagnoses or prescriptions, the logic may need to be expanded
* This simulated dataset differs from a real-world dataset, which may contain more missing values or irregular data.
* The main purpose of this file is to demonstrate the coding approach for risk factor classification. 

---
