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



#remove non-numeric column from biomarker
biomarker_0weeks <- biomarker_0weeks %>% select(-Biomarker)

full_data <- merge(patient_data, biomarker_0weeks, by = "PatientID")
#remove patient id
full_data <- full_data %>% select(-PatientID)
#convert binary data
full_data$`Sex (1=male, 2=female)` <- ifelse(full_data$`Sex (1=male, 2=female)` == 1, 1,0)
full_data$`Smoker (1=yes, 2=no)` <- ifelse(full_data$`Smoker (1=yes, 2=no)` == 1, 1,0)

#split into training and testing data randomly
set.seed(41)
training_index <- sample(1:nrow(full_data), size = 0.8 * nrow(full_data))

training_set <- full_data[training_index,]
testing_set <- full_data[-training_index,]

#train model
model <- lm(`Vas-12months`~.,training_set)

#show residuals
plot(model$residuals,
     main="Plot of Residuals",
     xlab="Index",
     ylab = "Residuals")

#check for normality with histogram
hist(model$residuals, 
     main = "Histogram of Residuals", 
     xlab = "Residuals", 
     col = "lightblue", 
     border = "black", 
     breaks = 20)  

#make predictions on test set

predictions <- predict(model, newdata = testing_set)
residuals_model <- testing_set$`Vas-12months` - predictions

plot(predictions, residuals_model, 
     main = "Residuals vs. Fitted Values", 
     xlab = "Predicted VAS 12-Month", 
     ylab = "Residuals", 
     pch = 20, col = "blue")
abline(h = 0, col = "red", lwd = 2)
