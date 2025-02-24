#Regression modelling

library(readxl)
library(dplyr)
library(car)  # For Levene's test
library(openxlsx)  # For exporting results

# remove rows where VAS is na
patient_data <- read_xlsx('./covariates.xlsx')
patient_data <- patient_data[!is.na(patient_data$`Vas-12months`),]

#get biomarker levels at inclusion
biomarker_data <- read_xlsx('./biomarkers.xlsx')
#get markers at 0 weeks
biomarker_0weeks <- biomarker_data %>%
  filter(grepl("0weeks$", Biomarker)) %>%
  mutate(PatientID = as.numeric(sub("-.*", "", Biomarker))) %>%
  arrange(PatientID)

#filter biomarkers to only have the patient_ids contained in patient_data
biomarker_0weeks <- biomarker_0weeks[biomarker_0weeks$PatientID %in% patient_data$PatientID,]

#do the same for patient_data
patient_data <- patient_data[patient_data$PatientID %in% biomarker_0weeks$PatientID,]

#extract vas at 12 months
VAS_12Month <- patient_data %>% select(-Vas-12months)


