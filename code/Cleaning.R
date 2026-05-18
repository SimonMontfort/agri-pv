# Replication Code for Data Cleaning "Agrivoltaics can reduce political polarization and local opposition to solar energy" #####################################################
# Authors: Lukas Fesenfeld, Leon Sistek, Simon Montfort, Dionis Anderegg, Jürg Rohrer, Tobias Schmidt
# Date: 06.05.2026

# R.version
# platform       aarch64-apple-darwin20      
# arch           aarch64                     
# os             darwin20                    
# system         aarch64, darwin20           
# status                                     
# major          4                           
# minor          5.2                         
# year           2025                        
# month          10                          
# day            31                          
# svn rev        88974                       
# language       R                           
# version.string R version 4.5.2 (2025-10-31)
# nickname       [Not] Part in a Rumble   


# setwd("[Specify working directory here]")

# Packages #######################################
library(openxlsx)
library(dplyr)
library(tidyverse)

# Data Import ##########################################

filename <- "data/data_complete.csv"
data_comp <- read.csv(filename, header = TRUE, sep = ",")

filename <- "data/data_incomplete.csv"
data_inc <- read.csv(filename, header = TRUE, sep = ",")

# Data Cleaning #####################################################

#Remove entries that answered outside the response period (03.05.2024 - 08.06.2024)
data_comp$StartDate <- as.POSIXct(data_comp$StartDate, format="%Y-%m-%d %H:%M:%S")
data_comp$EndDate <- as.POSIXct(data_comp$EndDate, format="%Y-%m-%d %H:%M:%S")
data_inc$StartDate <- as.POSIXct(data_inc$StartDate, format="%Y-%m-%d %H:%M:%S")
data_inc$EndDate <- as.POSIXct(data_inc$EndDate, format="%Y-%m-%d %H:%M:%S")

start_date <- as.POSIXct("2024-05-03 00:00:00", format = "%Y-%m-%d %H:%M:%S")
cutoff_date <- as.POSIXct("2024-06-09 00:00:00", format = "%Y-%m-%d %H:%M:%S")
data_comp <- data_comp[data_comp$StartDate >= start_date & data_comp$EndDate < cutoff_date, ]
data_inc <- data_inc[data_inc$StartDate >= start_date & data_inc$EndDate < cutoff_date, ]


# remove entries that decided not to participate
# 1 = want to participate, 2 = do not want to participate
data_comp <- data_comp[data_comp$Q1.2 == 1, ]
data_inc <- data_inc[data_inc$Q1.2 == 1, ]

# Remove all answers that did not answer question on ideology (last question to be used in the analysis)
data_inc <- data_inc[complete.cases(data_inc[, "left_right"]),]

#Remove all entries with progress != 100
data_comp <- data_comp[data_comp$Progress == 100, ]


## Remove speedy respondents ##########################################
#Using 40% of median response time
#rename the column duration
names(data_comp)[names(data_comp) == 'Duration (in seconds)'] <- 'Duration'
names(data_inc)[names(data_inc) == 'Duration (in seconds)'] <- 'Duration'

#Completes
print(nrow(data_comp[data_comp$Duration < 0.4 * median(data_comp$Duration), ]))

#remove those responses
data_comp <- data_comp[data_comp$Duration >= 0.4*median(data_comp$Duration), ]

#Speedy respondents from the incompletes are not being removed

## Merge completes and incompletes ################################
data <- rbind(data_comp, data_inc)

## Remove Straightliners #####################################

#detect Straightliners using Q136 (big matrix)
print(unique(data$Q136_1))

relevant_cols <- grep("^Q136_", names(data), value = TRUE)

if (!"straightliners" %in% names(data)) {
  data$straightliners <- NA
}

# Loop through each row
for (i in 1:nrow(data)) {
  # Extract the relevant row values based on the column names
  row_values <- unlist(data[i, relevant_cols, drop = FALSE])
  
  # Check if there are any non-NA values in the row
  if (!any(is.na(row_values))) {
    # Check if all values are the same as the first value
    if (all(row_values == row_values[1])) {
      data$straightliners[i] <- "1"
    } else {
      data$straightliners[i] <- "0"
    }
  } else {
    data$straightliners[i] <- NA
  }
}

# Remove straightliners
data <- data[data$straightliners == 0 | is.na(data$straightliners), ]



# Label and recode key variables and clean data ##########################################

## General Questions ##########################################

# age
names(data)[names(data) == 'Q2.1'] <- 'age'

unique(data$age)
current_year <- as.numeric(format(Sys.Date(), "%Y"))

# Replace birth years with age for entries above 1900
data$age <- ifelse(data$age > 1900, current_year - data$age, data$age)

data$age <- ceiling(data$age)

# Remove all entries with an age above 78
data <- subset(data, age >= 18 & age <= 78)

unique(data$age)

# gender
# 1 male, 2 female, 3 other, 99 prefer not to say
names(data)[names(data) == 'Q2.2'] <- 'gender'

# Keep only male and female
data <- data[data$gender %in% c(1, 2), ]

# Create factor
data$gender_f <- factor(data$gender, levels = c(1, 2), labels = c("Male", "Female"))

table(data$gender_f, useNA = "ifany")


# Q19.2_1 Environmentally friendly behavior is an important part of my being.
# 1 Completely disagree, 7 Completely agree
names(data)[names(data) == 'Q19.2_1'] <- 'environ_behaviour'

# Q19.2_2 The so-called “environmental crisis” facing humanity is greatly exaggerated. 
# 1 Completely disagree, 7 Completely agree
names(data)[names(data) == 'Q19.2_2'] <- 'crisis_exag'
# Reverse scaling
table(data$crisis_exag, useNA = "ifany")
data$crisis_exag <- 8 - data$crisis_exag
table(data$crisis_exag, useNA = "ifany")

# Q19.2_3 If things continue as they are, we will soon experience a major environmental disaster.
# 1 Completely disagree, 7 Completely agree
names(data)[names(data) == 'Q19.2_3'] <- 'environment_disaster'

# Create environmental score by averaging over the responses from Q19
data <- data %>%
  mutate(environment_score = rowMeans(dplyr::select(., environ_behaviour, crisis_exag, environment_disaster), na.rm = TRUE))

# Display the summary of the new variable to check its distribution
summary(data$environment_score)

## General Questions 2 ##########################################

# Q136 To what extent do you agree with the following statements:  To ensure the Swiss electricity supply in the future, ...
# Completely disagree (1) - Completely agree (7)

# Q136_4 …more solar power systems should be installed on buildings. 
names(data)[names(data) == 'Q136_4'] <- 'solar_roofs'
table(data$solar_roofs, useNA = "ifany")

# Q136_5 …more solar systems should be installed in open spaces (e.g. meadows, fields, etc.). 
names(data)[names(data) == 'Q136_5'] <- 'solar_open_space'
table(data$solar_open_space, useNA = "ifany")


## Block B Part 1 ##########################################

# Q7.10 How familiar are you, if at all, with agri-photovoltaics?
# Not familiar (1) - Very familiar (7)

names(data)[names(data) == 'Q7.10'] <- 'familiar_agri_pv'
table(data$familiar_agri_pv, useNA = "ifany")


## Block B Part 2 ##########################################


# Q8.2 The following questions continue to ask your opinion on agri-photovoltaics, a dual land use concept that combines the cultivation of agricultural crops with the installation of solar panels on land. 
# I find energy from agri-photovoltaics
# Very bad (1) - Very good (7)

names(data)[names(data) == 'Q8.2'] <- 'like_energy_agri_pv'
table(data$like_energy_agri_pv, useNA = "ifany")


## Comprehension & Manipulation Check ##########################################



# Q12.2 To your knowledge, how big or small is the potential for building agri-PV systems in the area around your apartment/house?
# Q12.2_1 0-500 meters around your residence
names(data)[names(data) == 'Q12.2_1'] <- 'potential_know_1'
data$potential_know_1[data$potential_know_1 == 1] <- 0 #small
data$potential_know_1[data$potential_know_1 == 3] <- 1 #large
table(data$potential_know_1)

# Q12.2_2 500-1500 meters around your residence
names(data)[names(data) == 'Q12.2_2'] <- 'potential_know_2'
data$potential_know_2[data$potential_know_2 == 1] <- 0 #small
data$potential_know_2[data$potential_know_2 == 3] <- 1 #large
table(data$potential_know_2)

# Q12.2_1 1500-4500 meters around your residence
names(data)[names(data) == 'Q12.2_3'] <- 'potential_know_3'
data$potential_know_3[data$potential_know_3 == 1] <- 0 #small
data$potential_know_3[data$potential_know_3 == 3] <- 1 #large
table(data$potential_know_3)


#Q12.4 For you personally: Do you see the construction of agri-photovoltaic projects in Switzerland as an advantage or a disadvantage?
# Very disadvantageous  (1)  - Very advantageous  (7) 
names(data)[names(data) == 'Q12.4'] <- 'personal_advantage'
table(data$personal_advantage)

#Q12.5 For Switzerland and its population as a whole: Do you consider the construction of agri-photovoltaic projects in Switzerland to be an advantage or a disadvantage?
# Very disadvantageous  (1)  - Very advantageous  (7) 
names(data)[names(data) == 'Q12.5'] <- 'ch_advantage'
table(data$ch_advantage)


## Dependent Variables ##########################################


#Q14.6 What is your personal attitude towards the expansion of agri-photovoltaic systems in Switzerland?
# I completely oppose  (1) - I completely support (7)

names(data)[names(data) == 'Q14.6'] <- 'attitude_expansion'
table(data$attitude_expansion, useNA = "ifany")

#Q14.7 What is your personal attitude towards the expansion of the following agri-photovoltaic systems?

#Q14.7_1 Small agri-PV system (up to 1 football field, approx. 1ha) 
names(data)[names(data) == 'Q14.7_1'] <- 'attitude_expansion_small'
table(data$attitude_expansion_small, useNA = "ifany")

#Q14.7_2 Medium-sized agri-PV plant (up to 5 football fields, approx. 5ha)  
names(data)[names(data) == 'Q14.7_2'] <- 'attitude_expansion_medium'
table(data$attitude_expansion_medium, useNA = "ifany")

#Q14.7_3 Large agri-PV plant (up to 10 football fields, approx. 10ha) 
names(data)[names(data) == 'Q14.7_3'] <- 'attitude_expansion_large'
table(data$attitude_expansion_large, useNA = "ifany")

#Q14.12 What is your personal attitude to the expansion of agri-photovoltaic systems in your immediate neighborhood?
# 	I completely reject  (1) -  I completely support  (7)  
names(data)[names(data) == 'Q14.12'] <- 'attitude_expansion_nearby'
table(data$attitude_expansion_nearby, useNA = "ifany")


#Q14.9 Imagine that the government is planning to change its renewable energy policy. 
#Part of this policy is to increase the number of agri-photovoltaic projects in the country. 
#You are asked to give your opinion on this policy.
# Completely disagree (1) - Completely agree (7)

#Q14.9_1 I intend to support policies aimed at increasing the number of agri-photovoltaic projects in the country. 
names(data)[names(data) == 'Q14.9_1'] <- 'support_policies'
table(data$support_policies, useNA = "ifany")

#Q14.11 What is your personal attitude to the following concrete political instruments for the expansion of agri-photovoltaic systems in Switzerland?
# I completely oppose (1) - I completely support (7)

#Q14.11_1 Simplified approval procedures 
names(data)[names(data) == 'Q14.11_1'] <- 'support_policy_1'
table(data$support_policy_1, useNA = "ifany")

#Q14.11_2 State financial support for agri-PV projects (support with a one-off payment of up to 60 percent of the eligible investment costs for large-scale systems) 
names(data)[names(data) == 'Q14.11_2'] <- 'support_policy_2'
table(data$support_policy_2, useNA = "ifany")

#Q14.11_4 Advisory services for farmers to promote the expansion of agri-PV systems on their fields 
names(data)[names(data) == 'Q14.11_4'] <- 'support_policy_4'
table(data$support_policy_4, useNA = "ifany")

#Q14.11_6 Increase in the one-off payment for the construction of agri-PV systems  
names(data)[names(data) == 'Q14.11_6'] <- 'support_policy_6'
table(data$support_policy_6, useNA = "ifany")


## Political Ideology ##########################################


# 18.6 "left_right" Where would you place your own political views on this scale?
# 1 Left, 10 Right numeric



## Demographics 2/Socioeconomics ##########################################


# Q20.3 "education" What is the highest level of education that you have completed with a certificate or diploma?
# No/Compulsory School  (1) 
# Apprenticeship/Vocational school  (2) 
# Maturity  (3) 
# Higher vocational education (including higher technical college HWV, HFG, HFS, engineering school HTL)  (4) 
# University/ETH/University of Applied Sciences  (5) 
# Don't know/no answer  (6) 

data$education_f <- factor(data$education, labels = c("No/Compulsory School", "Apprenticeship/Vocational school", "Maturity", "Higher vocational education", "University/ETH/University of Applied Sciences", "Don't know/no answer"))
table(data$education_f)
table(data$education, useNA = "ifany")


## General circle value ##########################################
# 1 if any circle value is 1, 0 otherwise

data <- data %>%
  mutate(circle = if_else(pmax(circle1, circle2, circle3, na.rm = TRUE) == 1, 1, 0))


# Remove all Variables not needed ##########################################

data <- data %>%
  dplyr::select(NIMBY, UserLanguage, age, gender, gender_f, environment_score, familiar_agri_pv, like_energy_agri_pv, urban_rural,
         potential_know_1, potential_know_2, potential_know_3, personal_advantage, ch_advantage,
         attitude_expansion_small, attitude_expansion_medium, attitude_expansion_large, attitude_expansion_nearby, attitude_expansion,
         support_policies, support_policy_1, support_policy_2, support_policy_4, support_policy_6,
         solar_roofs, solar_open_space,
         circle, circle1, circle2, circle3, left_right,
         QID1214859317_cjp1, QID1214859317_cjp2, QID246_cjp1, QID246_cjp2, QID251_cjp1, QID251_cjp2, QID255_cjp1, QID255_cjp2,
         Q15.8, Q15.9_1, Q15.9_2, Q15.14, Q15.15_1, Q15.15_2, Q15.20, Q15.21_1, Q15.21_2, Q15.26, Q15.27_1, Q15.27_2)



# Save cleaned dataset to csv ##########################################

write.csv(data, file = "data/data.csv")
saveRDS(data, "data/data.rds")





