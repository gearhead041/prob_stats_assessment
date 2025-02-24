# Load required packages
library(readxl)
library(dplyr)
library(car)  # For Levene's test
library(openxlsx)  # For exporting results

patient_data <- read_xlsx('./covariates.xlsx')
biomarker_data <- read_xlsx('./biomarkers.xlsx')

#p-value to be used
alpha = 0.05 #0.05

#extract male & female
male_ids <- patient_data$PatientID[patient_data$`Sex (1=male, 2=female)` == 1]
# Extract Female IDs
female_ids <- patient_data$PatientID[patient_data$`Sex (1=male, 2=female)` == 2]

#get markers at 0 weeks
biomarker_0weeks <- biomarker_data %>%
  filter(grepl("0weeks$", Biomarker))

# Extract Male Data
biomarker_male <- biomarker_0weeks %>%
  filter(as.numeric(sub("-.*", "", Biomarker)) %in% male_ids)

# Extract Female Data
biomarker_female <- biomarker_0weeks %>%
  filter(as.numeric(sub("-.*", "", Biomarker)) %in% female_ids)


#list of biomarker
biomarkers <- setdiff(names(biomarker_0weeks), c("Biomarker", "PatientID"))
# Create an empty data frame to store results
results <- data.frame(
  Biomarker = character(),
  p_value = numeric(),
  Test = character(),
  stringsAsFactors = FALSE
)

for (biomarker in biomarkers) {
  male_values <- biomarker_male[[biomarker]]
  female_values <- biomarker_female[[biomarker]]
  
  # Check if both groups have more than one value (to perform t-test)
  if (length(male_values) > 1 & length(female_values) > 1) {
    # Levene's Test for Equality of Variance
    levene_p <- leveneTest(c(male_values, female_values), group = c(rep("Male", length(male_values)), rep("Female", length(female_values))))$"Pr(>F)"[1]
    
    # Choose appropriate t-test
    if (levene_p > 0.05) {
      # Variance is equal → Use Student's t-test
      t_test <- t.test(
        male_values,
        female_values,
        var.equal = TRUE,
        conf.level = 1 - alpha
      )
      test_used <- "Student's t-test"
    } else {
      # Variance is unequal → Use Welch's t-test
      t_test <- t.test(
        male_values,
        female_values,
        var.equal = FALSE,
        conf.level =  1 - alpha
      )
      test_used <- "Welch's t-test"
    }
    
    # Store results
    results <- rbind(
      results,
      data.frame(
        Biomarker = biomarker,
        p_value = t_test$p.value,
        Test = test_used,
        Significant = t_test$p.value < alpha,
        stringsAsFactors = FALSE
      )
    )
  }
}

# Save results to CSV
write.xlsx(results, "./t_test_results.xlsx", rowNames = FALSE)
