setwd("D:/Studium/Master/Arbeit/Agri-PV/Auswertung/Welle 1")

# Data Import ----

#Import complete responses from Wave 1
filename <- "Welle1_240807.csv"
headers = read.csv(filename, header = FALSE, nrows = 1)
data_comp = read.csv(filename, skip = 3, header = FALSE)
colnames(data_comp)= headers

#Import incomplete responses
filename <- "Welle1_240807_Incomplete.csv"
headers = read.csv(filename, header = FALSE, nrows = 1)
data_inc = read.csv(filename, skip = 3, header = FALSE)
colnames(data_inc)= headers

#Import complete responses from Wave 2
filename <- "Welle2_240807.csv"
headers = read.csv(filename, header = FALSE, nrows = 1)
data2_comp = read.csv(filename, skip = 3, header = FALSE)
colnames(data2_comp)= headers

#Import incomplete responses from Wave 2
filename <- "Welle2_240807_Incomplete.csv"
headers = read.csv(filename, header = FALSE, nrows = 1)
data2_inc = read.csv(filename, skip = 3, header = FALSE)
colnames(data2_inc)= headers


# Data Cleaning ------------------------------------------------------------------------------------------------------------------------------------------

# Remove the first 28 lines (entries before 03rd of may) 
data_comp <- data_comp[-c(1:28), ]

# Remove the first line of the incompletes (entry before 03rd of may)
data_inc <- data_inc[-c(1:1), ]

# remove entries that decided not to participate
# 1 = want to participate, 2 = do not want to participate
data_comp <- data_comp[data_comp$Q1.2 == 1, ]
data_inc <- data_inc[data_inc$Q1.2 == 1, ]

# Remove all entries from incompletes that did not answer the questions in dependent variables
data_inc <- data_inc[complete.cases(data_inc[, c("Q14.11_1", "Q14.11_2", "Q14.11_3", "Q14.11_4", "Q14.11_5", "Q14.11_6")]), ]

#Remove all entries from incompletes in wave2 that did not answer the Agri_PV questions
data2_inc <- data2_inc[complete.cases(data2_inc[, c("Q14.9_1")]), ]

## Merge completes and incompletes of wave 1 and wave 2 ----

library(dplyr)

# Convert 'StartData' to POSIXct date-time format
data_comp$StartDate_trans <- as.POSIXct(data_comp$StartDate, format="%Y-%m-%d %H:%M:%S")
data_inc$StartDate_trans <- as.POSIXct(data_inc$StartDate, format="%Y-%m-%d %H:%M:%S")

data2_comp$StartDate_trans <- as.POSIXct(data2_comp$StartDate, format="%Y-%m-%d %H:%M:%S")
data2_inc$StartDate_trans <- as.POSIXct(data2_inc$StartDate, format="%Y-%m-%d %H:%M:%S")

#merge datasets
data <- rbind(data_comp, data_inc)
data2 <- rbind(data2_comp, data2_inc)

#Order after starting date
data <- data[order(data$StartDate_trans), ]
data2 <- data2[order(data2$StartDate_trans), ]

#Remove dataframes that are not needed
rm(data_comp, data_inc, data2_comp, data2_inc)


## Merge Datasets of Wave 1 and Wave 2 ----

#rename the Variable RecipientFirstName
names(data)[names(data) == 'RecipientFirstName'] <- 'participantId'
names(data2)[names(data2) == 'RecipientFirstName'] <- 'participantId'

# Remove rows with NA in participantId
data <- data[complete.cases(data$participantId), ]
data2 <- data2[complete.cases(data2$participantId), ]

#labels <- colnames(data2)
#print(labels)

# Find the index of column 'planted_delivered'
index_Qplanted<- which(names(data2) == "planted_delivered")
print(index_Qplanted)

# Deleting all columns that do not deal with AgriPV
data2 <- data2[, -((index_Qplanted):ncol(data2))]


# Add a suffix '_2' to all column names in data2_comp except 'participantId'
names(data2) <- ifelse(names(data2) == "participantId", 
                            "participantId", 
                            paste0(names(data2),"_2"))

#Join datasets from both waves together matching my participantId
data <- data %>%
  left_join(data2, by = "participantId")

#Create new column that is 0 when participant did not fill out the second survey and 1 if he did.
data <- data %>%
  mutate(wave2 = ifelse(!is.na(`StartDate_2`), 1, 0))

#Calculate percantage of participants that filled out the second survey
print(sum(data$wave2)/nrow(data))
print(sum(data$wave2))

## Remove questions on diet ----

start_col <- which(names(data) == "diet ")
end_col <- which(names(data) == "Q8_Click Count")


print(start_col)
print(end_col)

data <- data[, -c(start_col:end_col)]


## Remove speedy respondents ----

#rename the column duration
names(data)[names(data) == 'Duration (in seconds)'] <- 'Duration'

#calculate the median response time for all resonses
print(class(data$Duration))
median_time<- median(data$Duration, na.rm = TRUE)
median_time

#median time of completed responses
median_time_comp <- median(subset(data, Finished == 1)$Duration, na.rm = TRUE)
median_time_comp

#remove every entry with a duration less than 10min (speeders)
data <- data[data$Duration >= 600, ]

#remove entries from wave 1 after 08.06.2024
cutoff_date <- as.POSIXct("2024-06-09 00:00:00", format = "%Y-%m-%d %H:%M:%S")
data <- data[data$StartDate_trans < cutoff_date, ]

#variable_check <- class(data[200,231])
#print(variable_check)
#print(data[200,231])

## Detect Straightliners ----

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

#data <- data[data$straightliners == 0 | is.na(data$straightliners), ]

table(data$straightliners,  useNA = "ifany")
#NAs: Q136 hatte request response und nicht force response


# Label and recode key variables and clean data --------


library(tidyverse)

## Wave 1 ----

### General Questions -----

# age
names(data)[names(data) == 'Q2.1'] <- 'age'

unique(data$age)
current_year <- as.numeric(format(Sys.Date(), "%Y"))

# Replace birth years with age for entries above 1900
data$age <- ifelse(data$age > 1900, current_year - data$age, data$age)

data$age <- ceiling(data$age)

# Remove all entries with an age above 90 (loosing 87 participants through that)
data <- subset(data, age >= 18 & age <= 90)

unique(data$age)
hist(data$age, 
     main = "Distribution of Age in Wave 1",
     xlab = "Age (years)",
     cex.main = 1.7,
     cex.lab = 1.5,
     cex.axis = 1.3)

# gender
# 1 male, 2 female, 3 other, 99 prefer not to say
names(data)[names(data) == 'Q2.2'] <- 'gender'
# create dummy variable for gender
print(unique(data$gender))
table(data$gender)
data$gender_f <- factor(data$gender, labels = c("Male", "Female", "Other", "Prefer not to say"))

levels(data$gender_f)
table(data$gender_f)

data <- data %>%
  mutate(gender_binary = case_when(
    gender %in% c(1) ~ 0,
    gender %in% c(2) ~ 1,
    TRUE ~ NA_real_  #NA for all other values
  ))

table(data$gender_binary, useNA = "ifany")


# residence
# 1 main residence in Switzerland, 2 main residence not in Switzerland
names(data)[names(data) == 'Q2.3'] <- 'residence'
#recode variable
data$residence[data$residence == 2] <- 0 #Residence not in CH 
summary(data$residence)

# Swiss nationality
# 1 yes, 2 no
names(data)[names(data) == 'Q2.4'] <- 'swiss'
#recode variable
data$swiss[data$swiss == 2] <- 0 #no Swiss nationality
summary(data$swiss)

# How many people in household
# 1, 2, 3, 4, 5 = 5 or more
names(data)[names(data) == 'Q20.1'] <- 'household'


# Children under 18 in household
# 1 yes, 2 no
names(data)[names(data) == 'Q20.2'] <- 'kids'
#recode variable
data$kids[data$kids == 2] <- 0 #No children unter 18 in household 

# Important political topics in CH

  '______ Pension provision/AHV (1)
  ______ Unemployment (5)
  ______ Refugees (6)
  ______ Switzerland’s relationship with the European Union (7)
  ______ Healthcare / Health Insurance (8)
  ______ Power supply (9)
  ______ Traffic (10)
  ______ Globalization of the economy / free trade (11)
  ______ Environmental protection / climate change (12)
  ______ Crime (13)
  ______ Inequality in income and wealth (14)
  ______ Coexistence of people of different cultures and religions (15)
  ______ Foreign workers in Switzerland (16)
  ______ Increase in the Swiss resident population / urban sprawl / urbanization (17)
  ______ Food and Agriculture (18)'



# Q19.1_1 I feel very connected to the community in which I live
# 1 Completely disagree, 7 Completely agree
names(data)[names(data) == 'Q19.1_1'] <- 'community_connection'
summary(data$community_connection)
table(data$community_connection)

# Q19.1_2 In general, most people can be trusted
# 1 Completely disagree, 7 Completely agree
names(data)[names(data) == 'Q19.1_2'] <- 'people_trust'

# Q19.2_1 Environmentally friendly behavior is an important part of my being.
# 1 Completely disagree, 7 Completely agree
names(data)[names(data) == 'Q19.2_1'] <- 'environ_behaviour'

# Q19.2_2 The so-called “environmental crisis” facing humanity is greatly exaggerated. 
# 1 Completely disagree, 7 Completely agree
names(data)[names(data) == 'Q19.2_2'] <- 'crisis_exag'
# Reverse scaling
data$crisis_exag <- 8 - data$crisis_exag
table(data$crisis_exag, useNA = "ifany")

# Q19.2_3 If things continue as they are, we will soon experience a major environmental disaster.
# 1 Completely disagree, 7 Completely agree
names(data)[names(data) == 'Q19.2_3'] <- 'environment_disaster'

# Create environmental score by averaging over the responses from Q19
data <- data %>%
  mutate(environment_score = rowMeans(select(., environ_behaviour, crisis_exag, environment_disaster), na.rm = TRUE))

# Display the summary of the new variable to check its distribution
summary(data$environment_score)


### General Questions 2 -------

# Q136 To what extent do you agree with the following statements:  To ensure the Swiss electricity supply in the future, ...
# Completely disagree (1) - Completely agree (7)

# Q136_4 …more solar power systems should be installed on buildings. 
names(data)[names(data) == 'Q136_4'] <- 'solar_roofs'
table(data$solar_roofs, useNA = "ifany")

# Q135_5 …more solar systems should be installed in open spaces (e.g. meadows, fields, etc.). 
names(data)[names(data) == 'Q136_5'] <- 'solar_open_space'
table(data$solar_open_space, useNA = "ifany")

# Q3.3 Which of the following power generation plants, if any, are located near where you live?
# Yes (1), No (2), I don't know (3)
# Q3.3_2 Solar Park
names(data)[names(data) == 'Q3.3_2'] <- 'solar_park_nearby'
table(data$solar_park_nearby, useNA = "ifany")

### Block B Part 1 --------

#Q7.9 What feelings do you associate with your thoughts on agri-photovoltaics? Please use the scale below from very negative to very positive to rate your feelings.
# Very negative (1) - Very positive (7)

names(data)[names(data) == 'Q7.9'] <- 'feelings_agri_pv'
table(data$feelings_agri_pv, useNA = "ifany")

# Q7.10 How familiar are you, if at all, with agri-photovoltaics?
# Not familiar (1) - Very familiar (7)

names(data)[names(data) == 'Q7.10'] <- 'familiar_agri_pv'
table(data$familiar_agri_pv, useNA = "ifany")

# Q7.11 How much do you like or dislike the appearance of the agri-photovoltaic systems?
# Don't like it at all (1) - I like it very much (7)

names(data)[names(data) == 'Q7.11'] <- 'appearance_like_agri_pv'
table(data$appearance_agri_pv, useNA = "ifany")

# Q7.12 How important, if at all, is the appearance of the agri-photovoltaic system to you?
# Very unimportant (1) - Very important (7)

names(data)[names(data) == 'Q7.12'] <- 'appearance_important_agri_pv'
table(data$appearance_important_agri_pv, useNA = "ifany")


### Block B Part 2 -------


# Q8.2 The following questions continue to ask your opinion on agri-photovoltaics, a dual land use concept that combines the cultivation of agricultural crops with the installation of solar panels on land. 
# I find energy from agri-photovoltaics
# Very bad (1) - Very good (7)

names(data)[names(data) == 'Q8.2'] <- 'like_energy_agri_pv'
table(data$like_energy_agri_pv, useNA = "ifany")

# Q9.1 I am in favor of energy from agrivoltaics, even if it means an increase in my electricity bill.
# Completely disagree (1) - Completely agree (7)

names(data)[names(data) == 'Q9.1'] <- 'favour_agri_pv_energy_bill'
data$favour_agri_pv_energy_bill <- recode(data$favour_agri_pv_energy_bill,
                                          `2` = 1,
                                          `4` = 2,
                                          `5` = 3,
                                          `6` = 4,
                                          `7` = 5,
                                          `8` = 6,
                                          `9` = 7)

# Verify the recoding
table(data$favour_agri_pv_energy_bill, useNA = "ifany")

# Q9.2 Please indicate to what extent you agree or disagree with the following statements regarding agri-photovoltaics.
# Completely disagree (1) - Completely agree (7)

#Q9.2_1 Overall, I would describe agri-photovoltaics as an opportunity. 
names(data)[names(data) == 'Q9.2_1'] <- 'agri_pv_opportunity'
table(data$agri_pv_opportunity, useNA = "ifany")

#Q9.2_2 I believe the future for agri-photovoltaics looks promising.
names(data)[names(data) == 'Q9.2_2'] <- 'agri_pv_promising'
table(data$agri_pv_promising, useNA = "ifany")


### Comprehension & Manipulation Check ------


# Q12.1 The potential for the development of agri-photovoltaic projects within a radius of up to 4500 meters from my house/apartment is...
# Is not known to me  (1) 
# Known to me roughly  (2) 
# Known to me  (3) 

names(data)[names(data) == 'Q12.1'] <- 'potential_known'
table(data$potential_known, useNA = "ifany")

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

#Q12.3 How likely do you personally think it is that agri-PV systems will be built in the immediate vicinity of your house/apartment? 
# Very unlikely  (1) - Very likely  (5) 
names(data)[names(data) == 'Q12.3'] <- 'likelihood_vincinity'
table(data$likelihood_vincinity)

#Q12.4 For you personally: Do you see the construction of agri-photovoltaic projects in Switzerland as an advantage or a disadvantage?
# Very disadvantageous  (1)  - Very advantageous  (7) 
names(data)[names(data) == 'Q12.4'] <- 'personal_advantage'
table(data$personal_advantage)

#Q12.5 For Switzerland and its population as a whole: Do you consider the construction of agri-photovoltaic projects in Switzerland to be an advantage or a disadvantage?
# Very disadvantageous  (1)  - Very advantageous  (7) 
names(data)[names(data) == 'Q12.5'] <- 'ch_advantage'
table(data$ch_advantage)

#Q12.6 How concerned are you personally that agri-PV systems could be built in the immediate vicinity of your house/apartment? 
# Not concerned at all  (1)  - Extremely concerned  (5)
names(data)[names(data) == 'Q12.6'] <- 'personal_concerned'
table(data$personal_concerned, useNA ="ifany")


### Dependent Variables --------

#Q14.2 In such votes, more than half of those eligible to vote usually do not go to the polls. 
#What about you, how likely are you to take part in the vote on June 9, 2024?
# I will definitely participate.  (1) #
# I'm not quite sure yet, but I'll probably take part.  (2) 
#	I usually vote, but probably not this time.  (3) 
# I will not participate.  (4) 
# I have already voted by post.  (99) 
# I am not entitled to vote   (100) 


data <- data %>%
  # Create binary variable for entitled to vote
  mutate(entitled_to_vote = case_when(
    Q14.2 == 100 ~ 0,  # Not entitled to vote
    TRUE ~ 1           # Entitled to vote
  )) %>%
  # Rename and recode the take_part variable
  rename(take_part = Q14.2) %>%
  mutate(take_part_binary = case_when(
    take_part %in% c(1, 2) ~ 1,
    take_part %in% c(3, 4) ~ 0,
    TRUE ~ NA_real_  # NA if it doesn't match any condition
  ))
#table(data$entitled_to_vote, useNA = "ifany")
#table(data$take_part_binary, useNA = "ifany")
table(data$take_part, useNA = "ifany")


#Q14.3 Do you want to adopt the Federal Act on a Secure Electricity Supply from Renewable Energies, or Electricity Act for short, in the vote?
#Only displayed if they take part or plan to take part
# Yes  (1) 
# I have not decided yet.  (3) 
# No  (0) 
# don't know / no answer  (98) 

data <- data %>%
  rename(intended_vote = Q14.3) %>%
  mutate(intended_vote_scaled = case_when(
    
    intended_vote == 0 ~ 0,
    intended_vote == 3 ~ 1,
    intended_vote == 1 ~ 2,
    
    TRUE ~ NA_real_  # Use NA for any other values
  ))
table(data$intended_vote, useNA = "ifany")
table(data$intended_vote_scaled, useNA = "ifany")

#Q14.4 What do you think, would you accept the Federal Act on a Secure Electricity Supply using Renewable Energies, or Electricity Act for short, if you were to take part in the vote?
# Ony displayed when not planning to vote or not eligible
# Yes  (1) 
# I have not decided yet.  (3) 
# No  (0) 
# don't know / no answer  (98) 

data <- data %>%
  rename(intended_vote_if = Q14.4) %>%
  mutate(intended_vote_if_scaled = case_when(
    intended_vote_if == 0 ~ 0,
    intended_vote_if == 3 ~ 1,
    intended_vote_if == 1 ~ 2,
    TRUE ~ NA_real_  # Use NA for any other values
  ))
table(data$intended_vote_if_scaled, useNA = "ifany")
#table(data$intended_vote_if, useNA = "ifany")

# Create third variable that combines the intended vote of eligibles and non eligibles and a fourth one only with yes and no, dropping the "Have not decided yet" 
data <- data %>%
  # Step 1: Create the merged_vote variable if not already created
  mutate(merged_vote = coalesce(intended_vote_scaled, intended_vote_if_scaled)) %>%
  # Step 2: Create a new variable, merged_vote_modified, based on merged_vote
  mutate(merged_vote_modified = case_when(
    merged_vote == 2 ~ 1,  # Recode 2 to 1
    merged_vote == 1 ~ NA_real_,  # Assign NA to 1 
    TRUE ~ merged_vote  # Keep other values as is (e.g., 0)
  ))
# Display the table of the new merged column
table(data$merged_vote, useNA = "ifany")
table(data$merged_vote_modified, useNA = "ifany")


#Q14.5 How did you vote on the Federal Act on a Secure Electricity Supply from Renewable Energies, or Electricity Act for short?
#	Yes  (1) 
#	No  (3) 
# inserted empty  (0) 
#	don't know / no answer  (98) 

data <- data %>%
  rename(vote_mail = Q14.5) %>%
  mutate(vote_mail_binary = case_when(
    vote_mail == 1 ~ 1,
    vote_mail == 0 ~ 0,
    TRUE ~ NA_real_  # Use NA for any other values
  ))
#table(data$vote_mail_binary, useNA = "ifany")
#table(data$vote_mail, useNA = "ifany")


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

#Q14.8 How far should an agri-photovoltaic system be from your house/apartment for you to accept it? 
# 0-10000m
names(data)[names(data) == 'Q14.8_7'] <- 'distance_accept'
summary(data$distance_accept)

#Q236 In your opinion, which agricultural cultivation method should agri-PV systems be combined with most? Please select your preferred option from the list below?
# Cultivation of plant-based foods for direct human consumption  (1) 
# Cultivation of feed for animals  (2) 
# Cultivation of plant-based food for humans and feed for animals  (3) 
# I have no opinion on this  (4) 

names(data)[names(data) == 'Q236'] <- 'cultivation_preferred'
table(data$cultivation_preferred, useNA = "ifany")
data$cultivation_preferred_f <- factor(data$cultivation_preferred, labels = c("plant_human", "feed_animals", "Both", "No_opinion"))
table(data$cultivation_preferred_f, useNA = "ifany")

#Q17.2 To what extent do you agree or disagree with these statements about a planned agri-photovoltaic project near your place of residence?
# Completely disagree (1) - Completely agree (7)

#Q17.2_1 Most people I care about would expect me to give my approval to the project. 
names(data)[names(data) == 'Q17.2_1'] <- 'expect_approval'
table(data$expect_approval, useNA = "ifany")

#Q17.2_2 Most of my friends would advise me to support the project.
names(data)[names(data) == 'Q17.2_2'] <- 'advise_support'
table(data$advise_support, useNA = "ifany")

#Q17.2_3 My family would expect me to support the project. 
names(data)[names(data) == 'Q17.2_3'] <- 'expect_support_family'
table(data$expect_support_family, useNA = "ifany")

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

#Q14.11_3 Building permit for agri-PV systems, even if up to 30% losses in crop yield are to be expected 
names(data)[names(data) == 'Q14.11_3'] <- 'support_policy_3'
table(data$support_policy_3, useNA = "ifany")

#Q14.11_4 Advisory services for farmers to promote the expansion of agri-PV systems on their fields 
names(data)[names(data) == 'Q14.11_4'] <- 'support_policy_4'
table(data$support_policy_4, useNA = "ifany")

#Q14.11_5 Targeted state funding for Agri-PV systems for farmers who are switching from animal and feed production to the cultivation of plant-based food  
names(data)[names(data) == 'Q14.11_5'] <- 'support_policy_5'
table(data$support_policy_5, useNA = "ifany")

#Q14.11_6 Increase in the one-off payment for the construction of agri-PV systems  
names(data)[names(data) == 'Q14.11_6'] <- 'support_policy_6'
table(data$support_policy_6, useNA = "ifany")



### Political Ideology ----


#Q18.2 On a scale of 1-7, in your opinion should the government regulate the economy less (1) or more (7)?  
# 1 Less, 7 More
names(data)[names(data) == 'Q18.2'] <- 'regulate_economy'


# Q18.4 On a scale of 1-7, should the government regulate the lives of citizens less (1) or more (7)? 
# 1 Less, 7 More
names(data)[names(data) == 'Q18.4'] <- 'regulate_life'

# 18.6 "left_right" Where would you place your own political views on this scale?
# 1 Left, 10 Right numerisch


# Q18.8 pol_party_support Which party's goals and demands most closely correspond to your own views and wishes?
# SVP (Swiss People's Party)  (4) 
# SP (Social Democratic Party)  (3) 
# FDP.The Liberals (Free Democratic Party)  (1) 
# The Centre  (2) 
# GPS (The Greens)  (5) 
# GLP (Green Liberals)  (6) 
# EPP (Evangelical People’s Party of Switzerland)  (8) 
# League of Ticino  (9) 
# PdA (Swiss Labour Party)  (10) 
# CSP (Christian Social Party of Switzerland)  (12) 
# EDU (Swiss Federal Democratic Union)  (99) 
# Other  (20) 
# No  (0) 
# don't know / no answer  (98)


print(unique(data$pol_party_support))
table(data$pol_party_support, useNA = "ifany")
data$political_party_f <- factor(data$pol_party_support, labels = c("No", "FDP", "The Centre", "SP", "SVP", "GPS", "GLP", "EPP", "PdA", "CSP", "Other", "don't know/no answer", "EDU"))
levels(data$political_party_f)
table(data$political_party_f, useNA = "ifany")
                                 

### Demographics 2/Socioeconomics ----


# Q20.3 "education" What is the highest level of education that you have completed with a certificate or diploma?
# No/Compulsory School  (1) 
# Apprenticeship/Vocational school  (2) 
# Maturity  (3) 
# Higher vocational education (including higher technical college HWV, HFG, HFS, engineering school HTL)  (4) 
# University/ETH/University of Applied Sciences  (5) 
# Don't know/no answer  (6) 

table(data$education, useNA = "ifany")


# Q20.4 "income" How would you describe your household income?
# 1 difficult
# 2 modest
# 3 normal/average
# 4 above average

table(data$income, useNA = "ifany")
data$income_f <- factor(data$income, labels = c("difficult", "modest", "normal/avrage", "above average"))
levels(data$income_f)
table(data$income_f, useNA = "ifany")

# Q20.6 "urban_rural" In which area do you live?
# 1 Urban, 2 Suburban, 3 Rural
table(data$urban_rural, useNA = "ifany")
data$urban_rural_f <- factor(data$urban_rural, labels = c("Urban", "Suburban", "Rural"))

table(data$urban_rural_f, useNA = "ifany")

# Remove NA values from urban_rural_f
urban_rural_f_no_na <- na.omit(data$urban_rural_f)


# Q20.7 Is there a renewable energy system in your house?
# 1 yes, 2 no
names(data)[names(data) == 'Q20.7'] <- 'renewable_home'
#recode variable
data$renewable_home[data$renewable_home == 2] <- 0 #No renewable energy system in their house
table(data$renewable_home, useNA = "ifany")
summary(data$renewable_home)

# Q20.8 Do you or someone in your household work in one of these sectors?
# 1 Agricultural sector
# 2 Energy sector
# 3 None of these
names(data)[names(data) == 'Q20.8'] <- 'work'
table(data$work, useNA = "ifany")
data$work <- as.character(data$work)
table(data$work, useNA = "ifany")

# Create dummy variables for each sector
data$Agricultural_Sector <- as.factor(ifelse(is.na(data$work), NA, ifelse(grepl("1", data$work), 1, 0)))
data$Energy_Sector <- as.factor(ifelse(is.na(data$work), NA, ifelse(grepl("2", data$work), 1, 0)))
data$None_of_these <- as.factor(ifelse(is.na(data$work), NA, ifelse(grepl("3", data$work), 1, 0)))

head(data[, c("Agricultural_Sector", "Energy_Sector", "None_of_these")])

# Check if there are respondents that entered "None of these" while also ticking one of the other options
condition <- !is.na(data$None_of_these) & data$None_of_these == 1 & 
  (!is.na(data$Agricultural_Sector) & data$Agricultural_Sector == 1 | 
     !is.na(data$Energy_Sector) & data$Energy_Sector == 1)

# Count the number of rows that satisfy the condition
print(sum(condition, na.rm = TRUE))

table(data$Agricultural_Sector, useNA = "ifany")
table(data$Energy_Sector, useNA = "ifany")
table(data$None_of_these, useNA = "ifany")


##  Wave 2 ----

#Q14.2 Did you take part in the vote on the Electricity Act on 9 June 2024?
# I personally voted on June 9th.  (1)
# I voted by mail.  (99)
# I did not participate.  (2)
# I am not entitled to vote.  (100)

data <- data %>%
  # Create binary variable for entitled to vote
  mutate(entitled_to_vote_2 = case_when(
    Q14.2_2 == 100 ~ 0,  # Not entitled to vote
    TRUE ~ 1           # Entitled to vote
  )) %>%
  # Rename and recode the take_part_2 variable
  rename(take_part_2 = Q14.2_2) %>%
  mutate(take_part_2_binary = case_when(
    take_part_2 %in% c(1, 99) ~ 1,
    take_part_2 %in% c(2) ~ 0,
    TRUE ~ NA_real_  # NA for other values
  ))
table(data$entitled_to_vote_2, useNA = "ifany")
table(data$take_part_2, useNA = "ifany")
table(data$take_part_2_binary, useNA = "ifany")

#Q14.4 Do you think you would have accepted the Federal Act on a Secure Electricity Supply using Renewable Energies, or Electricity Act for short, if you had taken part in the vote?
# Only displayed if participant did not take part or is not entitled to vote
# Yes  (1)
# No  (0)
# don't know / no answer  (98)


data <- data %>%
  rename(vote_if_2 = Q14.4_2) %>%
  mutate(vote_if_binary_2 = case_when(
    vote_if_2 == 1 ~ 1,
    vote_if_2 == 0 ~ 0,
    TRUE ~ NA_real_  # Use NA for any other values
  ))
table(data$vote_if_binary_2, useNA = "ifany")
table(data$vote_if_2, useNA = "ifany")


#Q14.5 How did you vote on the Federal Act on a Secure Electricity Supply from Renewable Energies, or Electricity Act for short?
# Only shown to those who did vote
#	Yes  (1) 
#	No  (3) 
# inserted empty  (0) 
#	don't know / no answer  (98) 

data <- data %>%
  rename(vote_2 = Q14.5_2) %>%
  mutate(vote_binary_2 = case_when(
    vote_2 == 1 ~ 1,
    vote_2 == 3 ~ 0,
    TRUE ~ NA_real_  # Use NA for any other values
  ))
table(data$vote_binary_2, useNA = "ifany")
table(data$vote_2, useNA = "ifany")

# Create third variable that combines the vote of eligibles and the intended vote of non eligibles. 
data <- data %>%
  mutate(merged_vote_2 = coalesce(vote_binary_2, vote_if_binary_2))

# Display the table of the new merged column
table(data$merged_vote_2, useNA = "ifany")

#Q295 Can you briefly explain your decision in a few sentences?

#Q12.1 The potential for the development of agri-photovoltaic projects within a radius of up to 4500 meters from my house/apartment is...<br>
# Is not known to me  (1)
# Known to me roughly  (2)
# Known to me  (3)

table(data$Q12.1_2)
names(data)[names(data) == 'Q12.1_2'] <- 'potential_known_2'
data$potential_known_2_f <- factor(data$potential_known_2, labels = c("Not known", "Known roughly", "Known"))
levels(data$potential_known_2_f)
table(data$potential_known_2_f, useNA = "ifany")


#Q12.2 To your knowledge, how big or small is the potential for building agri-photovoltaic systems in the area around your apartment/house?
# Q12.2_1 0-500 meters around your residence
names(data)[names(data) == 'Q12.2_1_2'] <- 'potential_know_1_2'
data$potential_know_1_2[data$potential_know_1_2 == 1] <- 0 #small
data$potential_know_1_2[data$potential_know_1_2 == 3] <- 1 #large
table(data$potential_know_1_2, useNA = "ifany")

# Q12.2_2 500-1500 meters around your residence
names(data)[names(data) == 'Q12.2_2_2'] <- 'potential_know_2_2'
data$potential_know_2_2[data$potential_know_2_2 == 1] <- 0 #small
data$potential_know_2_2[data$potential_know_2_2 == 3] <- 1 #large
table(data$potential_know_2_2, useNA = "ifany")

# Q12.2_1 1500-4500 meters around your residence
names(data)[names(data) == 'Q12.2_3_2'] <- 'potential_know_3_2'
data$potential_know_3_2[data$potential_know_3_2 == 1] <- 0 #small
data$potential_know_3_2[data$potential_know_3_2 == 3] <- 1 #large
table(data$potential_know_3_2, useNA = "ifany")

#Q12.3 How likely do you personally think it is that agri-photovoltaic systems will be built in the immediate vicinity of your house/apartment?
# Very unlikely  (1) - Very likely  (5) 
names(data)[names(data) == 'Q12.3_2'] <- 'likelihood_vincinity_2'
table(data$likelihood_vincinity_2, useNA = "ifany")


#Q12.4 For you personally: Do you see the construction of agri-photovoltaic projects in Switzerland as an advantage or a disadvantage?
# Very disadvantageous  (1)  - Very advantageous  (7) 
names(data)[names(data) == 'Q12.4_2'] <- 'personal_advantage_2'
table(data$personal_advantage_2, useNA ="ifany")

#Q12.5 For Switzerland and its population as a whole: Do you consider the construction of agri-photovoltaic projects in Switzerland to be an advantage or a disadvantage?
# Very disadvantageous  (1)  - Very advantageous  (7) 
names(data)[names(data) == 'Q12.5_2'] <- 'ch_advantage_2'
table(data$ch_advantage_2, useNA ="ifany")

#Q12.6 How concerned are you personally that agri-PV systems could be built in the immediate vicinity of your house/apartment? 
# Not concerned at all  (1)  - Extremely concerned  (5)
names(data)[names(data) == 'Q12.6_2'] <- 'personal_concerned_2'
table(data$personal_concerned_2, useNA ="ifany")

#Q14.6 What is your personal attitude towards the expansion of agri-photovoltaic systems in Switzerland?
# I completely oppose  (1) - I completely support  (7)
names(data)[names(data) == 'Q14.6_2'] <- 'attitude_expansion_2'
table(data$attitude_expansion_2, useNA = "ifany")

#Q14.7 What is your personal attitude towards the expansion of the following agri-photovoltaic systems?

#Q14.7_1 Small agri-PV system (up to 1 football field, approx. 1ha) 
names(data)[names(data) == 'Q14.7_1_2'] <- 'attitude_expansion_small_2'
table(data$attitude_expansion_small_2, useNA = "ifany")

#Q14.7_2 Medium-sized agri-PV plant (up to 5 football fields, approx. 5ha)  
names(data)[names(data) == 'Q14.7_2_2'] <- 'attitude_expansion_medium_2'
table(data$attitude_expansion_medium_2, useNA = "ifany")

#Q14.7_3 Large agri-PV plant (up to 10 football fields, approx. 10ha) 
names(data)[names(data) == 'Q14.7_3_2'] <- 'attitude_expansion_large_2'
table(data$attitude_expansion_large_2, useNA = "ifany")

#Q14.12 Imagine that an agri-photovoltaic project is planned near where you live. 
#Together with other people from your neighborhood, you are asked to give your opinion on the project.
#What is your personal attitude to the expansion of agri-photovoltaic systems in your immediate neighborhood?
# I completely reject  (1) - I completely support  (7)
names(data)[names(data) == 'Q14.12_2'] <- 'attitude_expansion_nearby_2'
table(data$attitude_expansion_nearby_2, useNA = "ifany")

#Q14.8 How far should an agri-photovoltaic system be from your house/apartment for you to accept it? 
# 0-10000m
names(data)[names(data) == 'Q14.8_7_2'] <- 'distance_accept_2'
summary(data$distance_accept_2)

#Q236 In your opinion, which agricultural cultivation method should agri-PV systems be combined with most? Please select your preferred option from the list below?
# Cultivation of plant-based foods for direct human consumption  (1) 
# Cultivation of feed for animals  (2) 
# Cultivation of plant-based food for humans and feed for animals  (3) 
# I have no opinion on this  (4) 

names(data)[names(data) == 'Q236_2'] <- 'cultivation_preferred_2'
table(data$cultivation_preferred_2, useNA = "ifany")
data$cultivation_preferred_2_f <- factor(data$cultivation_preferred_2, labels = c("plant_human", "feed_animals", "Both", "No_opinion"))
table(data$cultivation_preferred_2_f, useNA = "ifany")

#Q17.2 To what extent do you agree or disagree with these statements about a planned agri-photovoltaic project near your place of residence?
# Completely disagree (1) - Completely agree (7)

#Q17.2_1 Most people I care about would expect me to give my approval to the project. 
names(data)[names(data) == 'Q17.2_1_2'] <- 'expect_approval_2'
table(data$expect_approval_2, useNA = "ifany")

#Q17.2_2 Most of my friends would advise me to support the project.
names(data)[names(data) == 'Q17.2_2_2'] <- 'advise_support_2'
table(data$advise_support_2, useNA = "ifany")

#Q17.2_3 My family would expect me to support the project. 
names(data)[names(data) == 'Q17.2_3_2'] <- 'expect_support_family_2'
table(data$expect_support_family_2, useNA = "ifany")

#Q14.9 Imagine that the government is planning to change its renewable energy policy. 
#Part of this policy is to increase the number of agri-photovoltaic projects in the country. 
#You are asked to give your opinion on this policy.
# Completely disagree (1) - Completely agree (7)

#Q14.9_1 I intend to support policies aimed at increasing the number of agri-photovoltaic projects in the country. 
names(data)[names(data) == 'Q14.9_1_2'] <- 'support_policies_2'
table(data$support_policies_2, useNA = "ifany")




## Check of NIMBY treatment and match by circle values -----

check <- read.csv("NIMBY_quant_new.csv", header = TRUE)

#rename circle names
names(check)[names(check) == 'Quant_Ertrag_0.500'] <- 'circle1_true'
names(check)[names(check) == 'Quant_Ertrag_500.1500'] <- 'circle2_true'
names(check)[names(check) == 'Quant_Ertrag_1500.4500'] <- 'circle3_true'


# Merge the datasets by RecipientID to include all respondents in 'data'
merged_df <- data %>%
  left_join(check, by = "participantId", suffix = c("_data", "_check"))

# Create the circle_match variable, defaulting to 0 if there are no matches
merged_df <- merged_df %>%
  mutate(circle_match = if_else(is.na(circle1_true), 0,
                                as.integer(circle1 == circle1_true &
                                           circle2 == circle2_true &
                                           circle3 == circle3_true)))

summary(merged_df$circle_match)

# Select only the necessary columns to add to 'data'
merged_df_red <- merged_df %>%
  select(participantId, circle_match)

# Create control dataframe
merged_df_control<- merged_df %>%
  select(participantId,circle1, circle1_true, circle2, circle2_true, circle3, circle3_true,  circle_match)

# Merge this with the original dataframe 'data to add the circle_match column
data <- data %>%
  left_join(merged_df_red, by = "participantId")

print(sum(data$circle_match))

print(sum(data$circle_match == 0))

print(sum(data$wave2[data$circle_match == 1]))

table(data$circle_match, useNA = "ifany")


## Remove straightliners ----

data <- data[data$straightliners == 0 | is.na(data$straightliners), ]


## Create extra variables for regression ----


### General circle value ----
# 1 if any circle value is 1, 0 otherwise

data <- data %>%
  mutate(circle = if_else(pmax(circle1, circle2, circle3, na.rm = TRUE) == 1, 1, 0))

### Split Variable (25.05.2024) ----
# 0 if before 24.05.2024 and 1 if after (mistake corrected then)

# Specify the split date
split_date <- as.POSIXct("2024-05-24 10:00:00", format="%Y-%m-%d %H:%M:%S")

data <- data %>%
  mutate(after = ifelse(StartDate_trans >= split_date, 1, 0))

count_after <- sum(data$after)

# Display the count
print(count_after)


### Create new variable for H4a ----
# 1 if in treatment group and circle = 1 and 0 if either in treatment group and circle1 = 0 or not in treatment group
data <- data %>%
  mutate(NIMBY_H4a = case_when(
    circle1 == 1 & NIMBY == 1 ~ 1,  # Both circle1 and NIMBY are 1
    (circle1 == 0 & NIMBY == 1) | NIMBY == 0 ~ 0  # Either circle1 = 0 and NIMBY = 1, or NIMBY = 0
  ))

table(data$NIMBY_H4a, useNA = "ifany")

### Variable for voting gap ----
# -1 when changed from yes (intended) to no (vote)
data <- data %>%
  mutate(vote_gap = merged_vote_2 - merged_vote_modified)

table(data$vote_gap)


# Subset the data ----

labels <- colnames(data)
print(labels)

#Treatment group
all_treat <- data[data$NIMBY == 1, ]

#Control group
all_control <- data[data$NIMBY == 0, ]

#Treatment group correctly treated in the whole dataset
all_treat_correct <- data[data$NIMBY == 1 & data$circle_match == 1 , ]
print(count(all_treat_correct))
#Treatment group correctly treated in the whole dataset that did not vote yet
all_treat_correct_vote <- data[data$NIMBY == 1 & data$circle_match == 1 & data$take_part != 99, ]

#Treatment group incorrectly treated in the whole dataset
all_treat_false <- data[data$NIMBY == 1 & data$circle_match == 0, ]

#All that are matched correctly (within control and treatment group)
all_match_correct <- data[data$circle_match == 1, ]

#All that are matched correctly and did not vote yet
all_match_correct_vote <- data[data$circle_match == 1 & data$take_part != 99, ]

# subset of control group that would be correctly treated
all_control_match_correct<- data[(data$NIMBY == 0 & data$circle_match == 1), ]
print(nrow(all_control_match_correct))

# subset of control group that would be correctly treated and did not vote yet
all_control_match_correct_vote<- data[(data$NIMBY == 0 & data$circle_match == 1) & data$take_part != 99, ]

#subset responses after 24.05.2024
data_after <- data[data$after ==1, ]
print(nrow(data_after))

#Treatment group after 24.05.2024
after_treat <- data[data$NIMBY == 1 & data$after == 1, ]

#Control group after 24.05.2024
after_control <- data[data$NIMBY == 0 & data$after == 1, ]

# create subset where respondent is either part of the control group or part of the treatment group but correctly treated
# all entries that are treated are correctly treated
all_control_treat_correct<- data[!(data$NIMBY == 1 & data$circle_match == 0), ]
print(sum(all_control_treat_correct$NIMBY == 0))
print(sum(all_control_treat_correct$NIMBY == 1))

# create subset where respondent is either part of the control group or part of the treatment group but incorrectly treated
# all entries that are treated are incorrectly treated
all_control_treat_false<- data[!(data$NIMBY == 1 & data$circle_match == 1), ]
print(sum(all_control_treat_false$NIMBY == 0))
print(sum(all_control_treat_false$NIMBY == 1))

# Save cleaned dataset to csv -----
write.csv(data, file = "data.csv")

# Check the percentage of correctly treated respondents
percentage_treat_correct = nrow(all_treat_correct)/nrow(all_treat)
print(percentage_treat_correct)
print(nrow(all_treat))
print(nrow(all_treat_correct))
print(nrow(data))
print(nrow(all_control))


# Check Treatment percentages ----

#How many respondents are correctly treated and had a 1 for circle1
print(sum(data$circle_match == 1 & data$circle1 == 1 & data$NIMBY == 1))

#How many respondents are correctly treated and had a 1 for circle2
print(sum(data$circle_match == 1 & data$circle2 == 1 & data$NIMBY == 1))

#How many respondents are correctly treated and had a 1 for circle3
print(sum(data$circle_match == 1 & data$circle3 == 1 & data$NIMBY == 1))

#Proportion of Circles =1 vs. Circle =0 in NIMBY = 1

table(all_match_correct$NIMBY,all_match_correct$circle)

#0.3182973 (501/1574) This implies that almost 31.83% of people in the NIMBY group received information about a high potential in at least one of the three circles

table(all_match_correct$NIMBY,all_match_correct$circle1)

#0.2496823 (393/1574) This implies that 24.97% of people in the NIMBY group received information about a high potential in circle 1

table(all_match_correct$NIMBY,all_match_correct$circle2)

#0.172003 (231/1574) This implies that 14.68% of people in the NIMBY group received information about a high potential in circle 2

table(all_match_correct$NIMBY,all_match_correct$circle3)

#0.139136 (219/1574) This implies that 13.91% of people in the NIMBY group received information about a high potential in circle 3

print(nrow(data[data$wave2 == 1 & data$circle_match == 1, ]))


# Balance Checks ----

library(sandwich)
library(lmtest)
library(estimatr)
library(modelsummary)
library(jtools)
library(broom.mixed)
stars<-c("." = 0.1,"*" =0.05,"**" =0.01, "***" =0.001)


## Function for automated regression with controls
model_control <- function(dependent_var, main_independent_var, control_variables, data) {
  lm_robust(as.formula(paste(dependent_var, "~", main_independent_var, "+", paste(control_variables, collapse = " + "))), data = data)
}

# define control variables
controls <- c("age", "gender", "environment_score", "solar_open_space", "feelings_agri_pv", "familiar_agri_pv", "like_energy_agri_pv")


## Randomization across the whole dataset ----

#age
b_age <- lm_robust(NIMBY ~ age, data = data)

#gender, reference: male
b_gender <- data %>%
  filter(gender_f != "Prefer not to say") %>%
  lm_robust(NIMBY ~ gender_f, data = .)


#income, reference: difficult
b_income <- lm_robust(NIMBY ~ income_f, data = data)

#region, reference: urban
b_region <- lm_robust(NIMBY ~ urban_rural_f, data = data)

#political party
b_party <- lm_robust(NIMBY ~ political_party_f, data = data)

#political spectrum (1-10)
b_left_right <- lm_robust(NIMBY ~ left_right, data = data)

#environmental score (1-7)
b_environment <- lm_robust(NIMBY ~ environment_score, data = data)


modelsummary(list("age" = b_age, "gender" = b_gender, "income" = b_income, "b_region" = b_region, "party" = b_party, "left_right" = b_left_right, "environment" = b_environment),
             stars = stars)


## Randomization across (potentially) correctly treated in control and treatment group ----

#age
b_age_correct <- lm_robust(NIMBY ~ age, data = all_match_correct)

#gender, reference: male
b_gender_correct <- all_match_correct %>%
  filter(gender_f != "Prefer not to say") %>%
  lm_robust(NIMBY ~ gender_f, data = .)


#income, reference: difficult
b_income_correct <- lm_robust(NIMBY ~ income_f, data = all_match_correct)

#region, reference: urban
b_region_correct <- lm_robust(NIMBY ~ urban_rural_f, data = all_match_correct)

#political party
b_party_correct <- lm_robust(NIMBY ~ political_party_f, data = all_match_correct)

#political spectrum (1-10)
b_left_right_correct <- lm_robust(NIMBY ~ left_right, data = all_match_correct)

#environmental score (1-7)
b_environment_correct <- lm_robust(NIMBY ~ environment_score, data = all_match_correct)

modelsummary(list("age" = b_age_correct, "gender" = b_gender_correct, "income" = b_income_correct, "b_region" = b_region_correct, "party" = b_party_correct,
                  "left_right" = b_left_right_correct, "environment" = b_environment_correct),
             stars = stars)



## Correctly vs. incorrectly treated ----

# age
b_age_treat <- lm_robust(circle_match ~ age, data = all_treat)

#gender, reference: male
b_gender_treat <- all_treat %>%
  filter(gender_f != "Prefer not to say") %>%
  lm_robust(circle_match ~ gender_f, data = .)

#income, reference: difficult
b_income_treat <- lm_robust(circle_match ~ income_f, data = all_treat)

#region, reference: urban
b_region_treat <- lm_robust(circle_match ~ urban_rural_f, data = all_treat)

#political party
b_party_treat <- lm_robust(circle_match ~ political_party_f, data = all_treat)

#political spectrum (1-10)
b_left_right_treat <- lm_robust(circle_match ~ left_right, data = all_treat)

modelsummary(list("age" = b_age_treat, "gender" = b_gender_treat, "income" = b_income_treat, "b_region" = b_region_treat, "party" = b_party_treat, "left_right" = b_left_right_treat),
             stars = stars)

## Check if there is a difference between control and correctly treated group ----

# use subset where respondent is either part of the control group or part of the treatment group but correctly treated

# age
b_age_treat_correct <- lm_robust(NIMBY ~ age, data = all_control_treat_correct)

#gender, reference: male
b_gender_treat_correct <- all_control_treat_correct %>%
  filter(gender_f != "Prefer not to say") %>%
  lm_robust(NIMBY ~ gender_f, data = .)

#income, reference: difficult
b_income_treat_correct <- lm_robust(NIMBY ~ income_f, data = all_control_treat_correct)

#region, reference: urban
b_region_treat_correct <- lm_robust(NIMBY ~ urban_rural_f, data = all_control_treat_correct)

#political party
b_party_treat_correct <- lm_robust(NIMBY ~ political_party_f, data = all_control_treat_correct)

#political spectrum (1-10)
b_left_right_treat_correct <- lm_robust(NIMBY ~ left_right, data = all_control_treat_correct)

modelsummary(list("age" = b_age_treat_correct, "gender" = b_gender_treat_correct, "income" = b_income_treat_correct, "b_region" = b_region_treat_correct, "party" = b_party_treat_correct, "left_right" = b_left_right_treat_correct),
             stars = stars)


## Check if there is a difference between control and incorrectly treated group ----

# use subset where respondent is either part of the control group or part of the treatment group but incorrectly treated

# age
b_age_treat_false <- lm_robust(NIMBY ~ age, data = all_control_treat_false)

#gender, reference: male
b_gender_treat_false <- all_control_treat_false %>%
  filter(gender_f != "Prefer not to say") %>%
  lm_robust(NIMBY ~ gender_f, data = .)

#income, reference: difficult
b_income_treat_false <- lm_robust(NIMBY ~ income_f, data = all_control_treat_false)

#income, reference: difficult
b_income_treat_correct <- lm_robust(circle_match ~ income_f, data = all_control_treat_false)

#region, reference: urban
b_region_treat_false <- lm_robust(NIMBY ~ urban_rural_f, data = all_control_treat_false)

#political party
b_party_treat_false <- lm_robust(NIMBY ~ political_party_f, data = all_control_treat_false)

#political spectrum (1-10)
b_left_right_treat_false <- lm_robust(NIMBY ~ left_right, data = all_control_treat_false)

modelsummary(list("age" = b_age_treat_false, "gender" = b_gender_treat_false, "income" = b_income_treat_false, "b_region" = b_region_treat_false, "party" = b_party_treat_false, "left_right" = b_left_right_treat_false),
             stars = stars)


## Balance check between control and treatment group for answers after 24.05.2024 ----

# age
after_age <- lm_robust(NIMBY ~ age, data = data_after)

#gender, reference: male
after_gender <- data_after %>%
  filter(gender_f != "Prefer not to say") %>%
  lm_robust(NIMBY ~ gender_f, data = .)

#income, reference: difficult
after_income <- lm_robust(NIMBY ~ income_f, data = data_after)

#income, reference: difficult
after_income <- lm_robust(circle_match ~ income_f, data = data_after)

#region, reference: urban
after_region <- lm_robust(NIMBY ~ urban_rural_f, data = data_after)

#political party
after_party <- lm_robust(NIMBY ~ political_party_f, data = data_after)

#political spectrum (1-10)
after_left_right <- lm_robust(NIMBY ~ left_right, data = data_after)

modelsummary(list("age" = after_age, "gender" = after_gender, "income" = after_income, "b_region" = after_region, "party" = after_party, "left_right" = after_left_right),
             stars = stars)

## Balance Check if there is a difference between the correctly treated before the 24.05. and after ----

# age
age_treat_before_after <- lm_robust(after ~ age, data = all_treat_correct)

#gender, reference: male
gender_treat_before_after <- all_treat_correct %>%
  filter(gender_f != "Prefer not to say") %>%
  lm_robust(after ~ gender_f, data = .)

#income, reference: difficult
income_treat_before_after <- lm_robust(after ~ income_f, data = all_treat_correct)

#region, reference: urban
region_treat_before_after <- lm_robust(after ~ urban_rural_f, data = all_treat_correct)

#political party
party_treat_before_after <- lm_robust(after ~ political_party_f, data = all_treat_correct)

#political spectrum (1-10)
left_right_treat_before_after <- lm_robust(after ~ left_right, data = all_treat_correct)

modelsummary(list("age" = age_treat_before_after, "gender" = gender_treat_before_after, "income" = income_treat_before_after, "b_region" = region_treat_before_after, "party" = party_treat_before_after, "left_right" = left_right_treat_before_after),
             stars = stars)


## Balanche Checks for circle values ----


### Correctly matched participants (control and treatment group) ----

#### Balane Checks Control/Treamtment with circle values = 1

# Circle1 = 1

# age
b_circle1_1_age_all_match_correct <- lm_robust(NIMBY ~ age, data = all_match_correct[all_match_correct$circle1 == 1, ])
summary(b_circle1_1_age_all_match_correct)

# gender
b_circle1_1_gender_all_match_correct <- lm_robust(NIMBY ~ gender_binary, data = all_match_correct[all_match_correct$circle1 == 1, ])
summary(b_circle1_1_gender_all_match_correct)

# income
b_circle1_1_inc_all_match_correct <- lm_robust(NIMBY ~ income_f, data = all_match_correct[all_match_correct$circle1 == 1, ])
summary(b_circle1_1_inc_all_match_correct)

#region, reference: urban
b_circle1_1_region_all_match_correct <- lm_robust(NIMBY ~ urban_rural_f, data = all_match_correct[all_match_correct$circle1 == 1, ])
summary(b_circle1_1_region_all_match_correct)

# education
b_circle1_1_edu_all_match_correct <- lm_robust(NIMBY ~ education, data = all_match_correct[all_match_correct$circle1 == 1, ])
summary(b_circle1_1_edu_all_match_correct)

#political spectrum (1-10)
b_circle1_1_left_right_all_match_correct <- lm_robust(NIMBY ~ left_right, data = all_match_correct[all_match_correct$circle1 == 1, ])
summary(b_circle1_1_left_right_all_match_correct)

# environment
b_circle1_1_env_all_match_correct <- lm_robust(NIMBY ~ environment_score, data = all_match_correct[all_match_correct$circle1 == 1, ])
summary(b_circle1_1_env_all_match_correct)



modelsummary(list("age" = b_circle1_1_age_all_match_correct, "gender" = b_circle1_1_gender_all_match_correct, "income" = b_circle1_1_inc_all_match_correct, "urban_rural" = b_circle1_1_region_all_match_correct,
                  "education" = b_circle1_1_edu_all_match_correct, "left_right" = b_circle1_1_left_right_all_match_correct, "environment_score" = b_circle1_1_env_all_match_correct),
             stars = stars)

# Circle2 = 1

all_match_correct_circle2_1 <- all_match_correct[all_match_correct$circle2 == 1, ]

# age
b_circle2_1_age_all_match_correct <- lm_robust(NIMBY ~ age, data = all_match_correct_circle2_1)
summary(b_circle1_1_age_all_match_correct)

# gender
b_circle2_1_gender_all_match_correct <- lm_robust(NIMBY ~ gender_binary, data = all_match_correct_circle2_1)
summary(b_circle1_1_gender_all_match_correct)

# income
b_circle2_1_inc_all_match_correct <- lm_robust(NIMBY ~ income_f, data = all_match_correct_circle2_1)
summary(b_circle1_1_inc_all_match_correct)

#region, reference: urban
b_circle2_1_region_all_match_correct <- lm_robust(NIMBY ~ urban_rural_f, data = all_match_correct_circle2_1)
summary(b_circle1_1_region_all_match_correct)

# education
b_circle2_1_edu_all_match_correct <- lm_robust(NIMBY ~ education, data = all_match_correct_circle2_1)
summary(b_circle1_1_edu_all_match_correct)

#political spectrum (1-10)
b_circle2_1_left_right_all_match_correct <- lm_robust(NIMBY ~ left_right, data = all_match_correct_circle2_1)
summary(b_circle1_1_left_right_all_match_correct)

# environment
b_circle2_1_env_all_match_correct <- lm_robust(NIMBY ~ environment_score, data = all_match_correct_circle2_1)
summary(b_circle1_1_env_all_match_correct)



modelsummary(list("age" = b_circle2_1_age_all_match_correct, "gender" = b_circle2_1_gender_all_match_correct, "income" = b_circle2_1_inc_all_match_correct, "urban_rural" = b_circle2_1_region_all_match_correct,
                  "education" = b_circle2_1_edu_all_match_correct, "left_right" = b_circle2_1_left_right_all_match_correct, "environment_score" = b_circle2_1_env_all_match_correct),
             stars = stars)



# Circle3 = 1

all_match_correct_circle3_1 <- all_match_correct[all_match_correct$circle3 == 1, ]

# age
b_circle3_1_age_all_match_correct <- lm_robust(NIMBY ~ age, data = all_match_correct_circle3_1)
summary(b_circle1_1_age_all_match_correct)

# gender
b_circle3_1_gender_all_match_correct <- lm_robust(NIMBY ~ gender_binary, data = all_match_correct_circle3_1)
summary(b_circle1_1_gender_all_match_correct)

# income
b_circle3_1_inc_all_match_correct <- lm_robust(NIMBY ~ income_f, data = all_match_correct_circle3_1)
summary(b_circle1_1_inc_all_match_correct)

#region, reference: urban
b_circle3_1_region_all_match_correct <- lm_robust(NIMBY ~ urban_rural_f, data = all_match_correct_circle3_1)
summary(b_circle1_1_region_all_match_correct)

# education
b_circle3_1_edu_all_match_correct <- lm_robust(NIMBY ~ education, data = all_match_correct_circle3_1)
summary(b_circle1_1_edu_all_match_correct)

#political spectrum (1-10)
b_circle3_1_left_right_all_match_correct <- lm_robust(NIMBY ~ left_right, data = all_match_correct_circle3_1)
summary(b_circle1_1_left_right_all_match_correct)

# environment
b_circle3_1_env_all_match_correct <- lm_robust(NIMBY ~ environment_score, data = all_match_correct_circle3_1)
summary(b_circle1_1_env_all_match_correct)



modelsummary(list("age" = b_circle3_1_age_all_match_correct, "gender" = b_circle3_1_gender_all_match_correct, "income" = b_circle3_1_inc_all_match_correct, "urban_rural" = b_circle3_1_region_all_match_correct,
                  "education" = b_circle3_1_edu_all_match_correct, "left_right" = b_circle3_1_left_right_all_match_correct, "environment_score" = b_circle3_1_env_all_match_correct),
             stars = stars)


# circle = 1

all_match_correct_circle_1 <- all_match_correct[all_match_correct$circle == 1, ]

# age
b_circle_1_age_all_match_correct <- lm_robust(NIMBY ~ age, data = all_match_correct_circle_1)
summary(b_circle1_1_age_all_match_correct)

# gender
b_circle_1_gender_all_match_correct <- lm_robust(NIMBY ~ gender_binary, data = all_match_correct_circle_1)
summary(b_circle1_1_gender_all_match_correct)

# income
b_circle_1_inc_all_match_correct <- lm_robust(NIMBY ~ income_f, data = all_match_correct_circle_1)
summary(b_circle1_1_inc_all_match_correct)

#region, reference: urban
b_circle_1_region_all_match_correct <- lm_robust(NIMBY ~ urban_rural_f, data = all_match_correct_circle_1)
summary(b_circle1_1_region_all_match_correct)

# education
b_circle_1_edu_all_match_correct <- lm_robust(NIMBY ~ education, data = all_match_correct_circle_1)
summary(b_circle1_1_edu_all_match_correct)

#political spectrum (1-10)
b_circle_1_left_right_all_match_correct <- lm_robust(NIMBY ~ left_right, data = all_match_correct_circle_1)
summary(b_circle1_1_left_right_all_match_correct)

# environment
b_circle_1_env_all_match_correct <- lm_robust(NIMBY ~ environment_score, data = all_match_correct_circle_1)
summary(b_circle1_1_env_all_match_correct)



modelsummary(list("age" = b_circle_1_age_all_match_correct, "gender" = b_circle_1_gender_all_match_correct, "income" = b_circle_1_inc_all_match_correct, "urban_rural" = b_circle_1_region_all_match_correct,
                  "education" = b_circle_1_edu_all_match_correct, "left_right" = b_circle_1_left_right_all_match_correct, "environment_score" = b_circle_1_env_all_match_correct),
             stars = stars)













#### Check if specific groups were more likely to get 1 one in the circles ----

##circle1

# age
b_circle1_age_all_match_correct <- lm_robust(circle1 ~ age, data = all_match_correct)
summary(b_circle1_age_all_match_correct)

# gender
b_circle1_gender_all_match_correct <- all_match_correct %>%
  filter(gender_f != "Prefer not to say") %>%
  lm_robust(circle1 ~ gender_f, data = .)
summary(b_circle1_gender_all_match_correct)

#region, reference: urban
b_circle1_region_all_match_correct <- lm_robust(circle1 ~ urban_rural_f, data = all_match_correct)
summary(b_circle1_region_all_match_correct)

# education
b_circle1_edu_all_match_correct <- lm_robust(circle1 ~ education, data = all_match_correct)
summary(b_circle1_edu_all_match_correct)

# income
b_circle1_inc_all_match_correct <- lm_robust(circle1 ~ income_f, data = all_match_correct)
summary(b_circle1_inc_all_match_correct)

#political spectrum (1-10)
b_circle1_left_right_all_match_correct <- lm_robust(circle1 ~ left_right, data = all_match_correct)
summary(b_circle1_left_right_all_match_correct)

# environment
b_circle1_env_all_match_correct <- lm_robust(circle1 ~ environment_score, data = all_match_correct)
summary(b_circle1_env_all_match_correct)

##circle2

# age
b_circle2_age_all_match_correct <- lm_robust(circle2 ~ age, data = all_match_correct)
summary(b_circle2_age_all_match_correct)

# gender
b_circle2_gender_all_match_correct <- all_match_correct %>%
  filter(gender_f != "Prefer not to say") %>%
  lm_robust(circle2 ~ gender_f, data = .)
summary(b_circle2_gender_all_match_correct)

#region, reference: urban
b_circle2_region_all_match_correct <- lm_robust(circle2 ~ urban_rural_f, data = all_match_correct)
summary(b_circle2_region_all_match_correct)

# education
b_circle2_edu_all_match_correct <- lm_robust(circle2 ~ education, data = all_match_correct)
summary(b_circle2_edu_all_match_correct)

# income
b_circle2_inc_all_match_correct <- lm_robust(circle2 ~ income_f, data = all_match_correct)
summary(b_circle2_inc_all_match_correct)

#political spectrum (1-10)
b_circle2_left_right_all_match_correct <- lm_robust(circle2 ~ left_right, data = all_match_correct)
summary(b_circle2_left_right_all_match_correct)

# environment
b_circle2_env_all_match_correct <- lm_robust(circle2 ~ environment_score, data = all_match_correct)
summary(b_circle2_env_all_match_correct)

##circle3 


# age
b_circle3_age_all_match_correct <- lm_robust(circle3 ~ age, data = all_match_correct)
summary(b_circle3_age_all_match_correct)

# gender
b_circle3_gender_all_match_correct <- all_match_correct %>%
  filter(gender_f != "Prefer not to say") %>%
  lm_robust(circle3 ~ gender_f, data = .)
summary(b_circle3_gender_all_match_correct)

#region, reference: urban
b_circle3_region_all_match_correct <- lm_robust(circle3 ~ urban_rural_f, data = all_match_correct)
summary(b_circle3_region_all_match_correct)

# education
b_circle3_edu_all_match_correct <- lm_robust(circle3 ~ education, data = all_match_correct)
summary(b_circle3_edu_all_match_correct)

# income
b_circle3_inc_all_match_correct <- lm_robust(circle3 ~ income_f, data = all_match_correct)
summary(b_circle3_inc_all_match_correct)

#political spectrum (1-10)
b_circle3_left_right_all_match_correct <- lm_robust(circle3 ~ left_right, data = all_match_correct)
summary(b_circle3_left_right_all_match_correct)

# environment
b_circle3_env_all_match_correct <- lm_robust(circle3 ~ environment_score, data = all_match_correct)
summary(b_circle3_env_all_match_correct)


### Control group, correctly matched ----

## circle1

# age
b_circle1_age_control_match_correct <- lm_robust(circle1 ~ age, data = all_control_match_correct)

#region, reference: urban
b_circle1_region_control_match_correct <- lm_robust(circle1 ~ urban_rural_f, data = all_control_match_correct)

#political spectrum (1-10)
b_circle1_left_right_control_match_correct <- lm_robust(circle1 ~ left_right, data = all_control_match_correct)

## circle2 

# age
b_circle2_age_control_match_correct <- lm_robust(circle2 ~ age, data = all_control_match_correct)

#region, reference: urban
b_circle2_region_control_match_correct <- lm_robust(circle2 ~ urban_rural_f, data = all_control_match_correct)

#political spectrum (1-10)
b_circle2_left_right_control_match_correct <- lm_robust(circle2 ~ left_right, data = all_control_match_correct)

## circle3 

# age
b_circle3_age_control_match_correct <- lm_robust(circle3 ~ age, data = all_control_match_correct)

#region, reference: urban
b_circle3_region_control_match_correct <- lm_robust(circle3 ~ urban_rural_f, data = all_control_match_correct)

#political spectrum (1-10)
b_circle2_left_right_control_match_correct <- lm_robust(circle2 ~ left_right, data = all_control_match_correct)


### Treatment group, correctly matched -----

## circle1

# age
b_circle1_age_treat_match_correct <- lm_robust(circle1 ~ age, data = all_treat_correct)

#region, reference: urban
b_circle1_region_treat_match_correct <- lm_robust(circle1 ~ urban_rural_f, data = all_treat_correct)

#political spectrum (1-10)
b_circle1_left_right_treat_match_correct <- lm_robust(circle1 ~ left_right, data = all_treat_correct)

## circle2

# age
b_circle2_age_treat_match_correct <- lm_robust(circle2 ~ age, data = all_treat_correct)

#region, reference: urban
b_circle2_region_treat_match_correct <- lm_robust(circle2 ~ urban_rural_f, data = all_treat_correct)

#political spectrum (1-10)
b_circle2_left_right_treat_match_correct <- lm_robust(circle2 ~ left_right, data = all_treat_correct)

## circle3

# age
b_circle2_age_treat_match_correct <- lm_robust(circle2 ~ age, data = all_treat_correct)

#region, reference: urban
b_circle3_region_treat_match_correct <- lm_robust(circle3 ~ urban_rural_f, data = all_treat_correct)

#political spectrum (1-10)
b_circle3_left_right_treat_match_correct <- lm_robust(circle3 ~ left_right, data = all_treat_correct)


## Balance check for the circles for responses after the 24.05.2024 ----

## circle1

# age
after_circle1_age_treat<- lm_robust(circle1 ~ age, data = after_treat)

#region, reference: urban
after_circle1_region_treat <- lm_robust(circle1 ~ urban_rural_f, data = after_treat)

#political spectrum (1-10)
after_circle1_left_right_treat <- lm_robust(circle1 ~ left_right, data = after_treat)

## circle2

# age
after_circle2_age_treat <- lm_robust(circle2 ~ age, data =after_treat)

#region, reference: urban
after_circle2_region_treat <- lm_robust(circle2 ~ urban_rural_f, data = after_treat)

#political spectrum (1-10)
after_circle2_left_right_treat <- lm_robust(circle2 ~ left_right, data = after_treat)

## circle3

# age
after_circle3_age_treat <- lm_robust(circle2 ~ age, data = after_treat)

#region, reference: urban
after_circle3_region_treat <- lm_robust(circle3 ~ urban_rural_f, data = after_treat)

#political spectrum (1-10)
after_circle3_left_right_treat <- lm_robust(circle3 ~ left_right, data = after_treat)

modelsummary(list("age1" = after_circle1_age_treat, "region1" = after_circle1_region_treat, "spectrum1" = after_circle1_left_right_treat,
                  "age2" = after_circle2_age_treat, "region2" = after_circle2_region_treat, "spectrum2" = after_circle2_left_right_treat,
                  "age3" = after_circle3_age_treat, "region3" = after_circle3_region_treat, "spectrum3" = after_circle3_left_right_treat),
             stars = stars)

 
# Comprehension/Manipulation Check ----

#potential_known, Reference category: Not known
comp_potential_known <- lm_robust(potential_known ~ NIMBY, data = all_match_correct)
summary(comp_potential_known)

#Overall, respondents tend to overestimate the potential further away from their home and underestimate the real potential closer to their home.

#0-500m
comp_potential_0_500 <- lm_robust(potential_know_1 ~ NIMBY, data = all_match_correct)

summary (comp_potential_0_500)

comp_potential_0_500_circle1 <- lm_robust(potential_know_1 ~ NIMBY*circle1, data = all_match_correct)

summary (comp_potential_0_500_circle1)

comp_potential_0_500_circle <- lm_robust(potential_know_1 ~ NIMBY*circle, data = all_match_correct)

summary (comp_potential_0_500_circle)

#People in circle 1 underestimated the real potential in this circles by around 18 percentage points if the real potential is high in circle 1 and slightly overestimated the potential in circle 1 by around 4.3 percentage points if the real potential was low

comp_potential_0_500_circle_split_0 <- lm_robust(potential_know_1 ~ NIMBY, data = all_match_correct [all_match_correct$circle1 ==0,])

summary (comp_potential_0_500_circle_split_0)

comp_potential_0_500_circle_split_1 <- lm_robust(potential_know_1 ~ NIMBY, data = all_match_correct [all_match_correct$circle1 ==1,])

summary (comp_potential_0_500_circle_split_1)

# Create the plot without a title
plot <- plot_summs(
  comp_potential_0_500_circle_split_0, 
  comp_potential_0_500_circle_split_1, 
  model.names = c("Low real Potential", "High real Potential"),
  coefs = c("NIMBY")
)

# Add a title using ggplot2
plot <- plot + ggtitle("Knowledge of Potential in Circle 1 (0m-500m)")

plot <- plot + 
  scale_y_discrete(expand = c(0, 0.5)) +
  theme(legend.text = element_text(size = 12))

# Display the plot
print(plot)


#500-1500m
comp_potential_500_1500 <- lm_robust(potential_know_2 ~ NIMBY, data = all_match_correct)

summary(comp_potential_500_1500 )

comp_potential_500_1500_circle2 <- lm_robust(potential_know_2 ~ NIMBY*circle2, data = all_match_correct)

summary (comp_potential_500_1500_circle2)

comp_potential_500_1500_circle <- lm_robust(potential_know_2 ~ NIMBY*circle, data = all_match_correct)

summary (comp_potential_500_1500_circle)

#People in circle 2 overestimated the real potential in this circles by around 11 percentage points if the potential is low in circle 2, however we do not find any overestimation effect if the real potential is high in circle 2

comp_potential_500_1500_circle_split_0 <- lm_robust(potential_know_2 ~ NIMBY, data = all_match_correct [all_match_correct$circle2 ==0,])

summary (comp_potential_500_1500_circle_split_0)

comp_potential_500_1500_circle_split_1 <- lm_robust(potential_know_2 ~ NIMBY, data = all_match_correct [all_match_correct$circle2 ==1,])

summary (comp_potential_500_1500_circle_split_1)

# Create the plot without a title
plot <- plot_summs(
  comp_potential_500_1500_circle_split_0, 
  comp_potential_500_1500_circle_split_1, 
  model.names = c("Low real Potential", "High real Potential"),
  coefs = c("NIMBY")
)

# Add a title using ggplot2
plot <- plot + ggtitle("Knowledge of Potential in Circle 2 (500m-1500m)")

plot <- plot + 
  scale_y_discrete(expand = c(0, 0.5)) +
  theme(legend.text = element_text(size = 12))

# Display the plot
print(plot)


#1500-4500m
comp_potential_1500_4500 <- lm_robust(potential_know_3 ~ NIMBY, data = all_match_correct)

summary(comp_potential_1500_4500)

comp_potential_1500_4500_circle3 <- lm_robust(potential_know_3 ~ NIMBY*circle3, data = all_match_correct)

summary (comp_potential_1500_4500_circle3)

comp_potential_1500_4500_circle <- lm_robust(potential_know_3 ~ NIMBY*circle, data = all_match_correct)

summary (comp_potential_1500_4500_circle)

#People in circle 3 overestimated the real potential in this circles by around 25 percentage points if the potential is low in circle 3, however we do not find any overestimation effect if the real potential is high in circle 3

comp_potential_1500_4500_circle_split_0 <- lm_robust(potential_know_3 ~ NIMBY, data = all_match_correct [all_match_correct$circle3 ==0,])

summary (comp_potential_1500_4500_circle_split_0)

comp_potential_1500_4500_circle_split_1 <- lm_robust(potential_know_3 ~ NIMBY, data = all_match_correct [all_match_correct$circle3 ==1,])

summary (comp_potential_1500_4500_circle_split_1)

# Create the plot without a title
plot <- plot_summs(
  comp_potential_1500_4500_circle_split_0, 
  comp_potential_1500_4500_circle_split_1, 
  model.names = c("Low real Potential", "High real Potential"),
  coefs = c("NIMBY")
)

# Add a title using ggplot2
plot <- plot + ggtitle("Knowledge of Potential in Circle 3 (1500m-4500m)")

plot <- plot + 
  scale_y_discrete(expand = c(0, 0.5)) +
  theme(legend.text = element_text(size = 12))

# Display the plot
print(plot)


#likelihood_vincinity 
comp_likelihood_vincinity <- lm_robust(likelihood_vincinity ~ NIMBY, data = all_match_correct)

summary (comp_likelihood_vincinity)


comp_likelihood_vincinity_circle1 <- lm_robust(likelihood_vincinity ~ NIMBY*circle1, data = all_match_correct)
summary (comp_likelihood_vincinity_circle1)


comp_likelihood_vincinity_circle2 <- lm_robust(likelihood_vincinity ~ NIMBY*circle2, data = all_match_correct)
summary (comp_likelihood_vincinity_circle2)


comp_likelihood_vincinity_circle3 <- lm_robust(likelihood_vincinity ~ NIMBY*circle3, data = all_match_correct)
summary (comp_likelihood_vincinity_circle3)


#personal_advantage 
comp_personal_advantage <- lm_robust(personal_advantage ~ NIMBY, data = all_match_correct)

summary(comp_personal_advantage)

comp_personal_advantage_circle1 <- lm_robust(personal_advantage ~ NIMBY*circle1, data = all_match_correct)
summary (comp_personal_advantage_circle1)


comp_personal_advantage_circle2 <- lm_robust(personal_advantage ~ NIMBY*circle2, data = all_match_correct)
summary (comp_personal_advantage_circle2)


comp_personal_advantage_circle3 <- lm_robust(personal_advantage ~ NIMBY*circle3, data = all_match_correct)
summary (comp_personal_advantage_circle3)

#ch_advantage
comp_ch_advantage <- lm_robust(ch_advantage ~ NIMBY, data = all_match_correct)

#personal_concerned
comp_personal_concerned <- lm_robust(personal_concerned ~ NIMBY, data = all_match_correct)
summary (comp_personal_concerned)

modelsummary(list( "0-500m" = comp_potential_0_500, "500-1500m" = comp_potential_500_1500,
                  "1500-4500m" = comp_potential_1500_4500, "likelihood_vincinity" = comp_likelihood_vincinity, "personal_advantage" = comp_personal_advantage,
                  "ch_advantage" = comp_ch_advantage, "personal_concerned" = comp_personal_concerned),
             stars = stars)

modelsummary(list("Likelihood of Agri-PV in Vincinity" = comp_likelihood_vincinity, "Personal Advantage from Agri-PV" = comp_personal_advantage,
                  "Advantage for CH" = comp_ch_advantage, "Personally Concerned of Agri-PV in Vincinity" = comp_personal_concerned),
             stars = stars)

# Create the plot without a title
plot_manip <- plot_summs(
  comp_likelihood_vincinity, 
  comp_personal_advantage, 
  comp_ch_advantage,
  comp_personal_concerned,
  model.names = c("Likelihood of Agri-PV in Vincinity", "Personal Advantage from Agri-PV", "Advantage for CH", "Personally Concerned of Agri-PV in \n Vincinity"),
  coefs = c("NIMBY")
)

# Add a title using ggplot2
plot_manip <- plot_manip + ggtitle("Manipulation Checks")

plot_manip <- plot_manip + 
  scale_y_discrete(expand = c(0, 0.3)) +
  theme(legend.text = element_text(size = 12))

# Display the plot
print(plot_manip)





# Dependent Variables (data with incompletes, including all potentially correctly treated respondents) ---------------------------------------------

## No controls, no interactions -----

#take_part
dep_take_part <- lm_robust(take_part_binary ~ NIMBY, data = all_match_correct_vote)

summary(dep_take_part)

#intended_vote_binary
#includes all who are eligible to vote
dep_intended_vote_scaled <- lm_robust(intended_vote_scaled ~ NIMBY, data = all_match_correct_vote)

#intended_vote_if_binary
#includes only those who are not eligible to vote
dep_intended_vote_if_scaled <- lm_robust(intended_vote_if_scaled ~ NIMBY, data = all_match_correct_vote)

#votes_merged,
#includes all who would have the intention to vote (also if not eligible)
dep_merged_vote <- lm_robust(merged_vote ~ NIMBY, data = all_match_correct_vote)


#Summary
modelsummary(list("take_part" = dep_take_part, "intended_vote" = dep_intended_vote_scaled, "intended_vote_if" = dep_intended_vote_if_scaled, "merged_vote" = dep_merged_vote),
             stars = stars)

#attitude_expansion
dep_attitue_expansion <- lm_robust(attitude_expansion ~ NIMBY, data = all_match_correct)

#attitude_expansion_small
dep_attitue_expansion_small <- lm_robust(attitude_expansion_small ~ NIMBY, data = all_match_correct)

#attitude_expansion_medium
dep_attitue_expansion_medium <- lm_robust(attitude_expansion_medium ~ NIMBY, data = all_match_correct)

#attitude_expansion_large
dep_attitue_expansion_large <- lm_robust(attitude_expansion_large ~ NIMBY, data = all_match_correct)

#attitude_expansion_nearby
dep_attitue_expansion_nearby <- lm_robust(attitude_expansion_nearby ~ NIMBY, data = all_match_correct)

# Create the plot
plot <- plot_summs(
  dep_attitue_expansion, 
  dep_attitue_expansion_small, 
  dep_attitue_expansion_medium, 
  dep_attitue_expansion_large,
  dep_attitue_expansion_nearby,
  model.names = c("Agri-PV Expansion in CH", "Expansion <1ha", "Expansion <5ha", "Expansion <10ha", "Expansion in Neighborhood"),
  coefs = c("NIMBY"),
  title = "Regression Outputs"
)

# Add a title using ggplot2
plot <- plot + ggtitle("Attitudes towards the Expansion of Agri-PV")

plot<- plot + 
  scale_y_discrete(expand = c(0, 0.3)) +
  theme(legend.text = element_text(size = 12))

# Display the plot
print(plot)

#Summary
modelsummary(list("Agri-PV Expansion in CH" = dep_attitue_expansion, "Expansion <1ha" = dep_attitue_expansion_small, "Expansion <5ha" = dep_attitue_expansion_medium,
                  "Expansion <10ha" = dep_attitue_expansion_large, "Expansion in Neighborhood" = dep_attitue_expansion_nearby),
             stars = stars)

#distance_accept
table(data$distance_accept, useNA = "ifany" )
dep_distance_accept <- lm_robust(distance_accept ~ NIMBY, data = all_match_correct)
summary(dep_distance_accept)


dep_distance_accept_circle <- lm_robust(distance_accept ~ NIMBY, data = all_match_correct[all_match_correct$circle == 1, ])
summary(dep_distance_accept_circle)

dep_distance_accept_circle1 <- lm_robust(distance_accept ~ NIMBY, data = all_match_correct[all_match_correct$circle1 == 1, ])
summary(dep_distance_accept_circle)

dep_distance_accept_circle2 <- lm_robust(distance_accept ~ NIMBY, data = all_match_correct[all_match_correct$circle2 == 1, ])
summary(dep_distance_accept_circle)

dep_distance_accept_circle3 <- lm_robust(distance_accept ~ NIMBY, data = all_match_correct[all_match_correct$circle3 == 1, ])
summary(dep_distance_accept_circle)

#Summary
modelsummary(list("Distance Preference of all observations" = dep_distance_accept, "High potential in at least one circle" = dep_distance_accept_circle, "High potential 0m-500m" = dep_distance_accept_circle1,
                  "High potential 500m-1500m" = dep_distance_accept_circle2, "High potential 1500m-4500m" = dep_distance_accept_circle3),
             stars = stars)



#Summary
modelsummary(list("attitude_expansion" = dep_attitue_expansion, "expansion_small" = dep_attitue_expansion_small, "expansion_medium" = dep_attitue_expansion_medium,
                  "expansion_large" = dep_attitue_expansion_large, "expansion_nearby" = dep_attitue_expansion_nearby, "distance_accept" = dep_distance_accept),
             stars = stars)

#cultivation_preferred, reference category: Plant based for direct human consumption
#dep_cultivation_preferred <- lm_robust(NIMBY ~ cultivation_preferred_f, data = all_match_correct)

#expect_approval
dep_expect_approval <- lm_robust(expect_approval ~ NIMBY, data = all_match_correct)

#advise_support
dep_advise_support <- lm_robust(advise_support ~ NIMBY, data = all_match_correct)

#expect_support_family
dep_expect_support_family <- lm_robust(expect_support_family ~ NIMBY, data = all_match_correct)

#Summary
modelsummary(list("expect_approval" = dep_expect_approval, "advise_support" = dep_advise_support,
                  "support_family" = dep_expect_support_family),
             stars = stars)


#support_policies
dep_support_policies <- lm_robust(support_policies ~ NIMBY, data = all_match_correct)

#support_policy individually
dep_support_policy_1 <- lm_robust(support_policy_1 ~ NIMBY, data = all_match_correct)

dep_support_policy_2 <- lm_robust(support_policy_2 ~ NIMBY, data = all_match_correct)

dep_support_policy_3 <- lm_robust(support_policy_3 ~ NIMBY, data = all_match_correct)

dep_support_policy_4 <- lm_robust(support_policy_4 ~ NIMBY, data = all_match_correct)

dep_support_policy_5 <- lm_robust(support_policy_5 ~ NIMBY, data = all_match_correct)

dep_support_policy_6 <- lm_robust(support_policy_6 ~ NIMBY, data = all_match_correct)

#Summary
modelsummary(list("General policy support for Agri-PV" = dep_support_policies, "Simplified approval procedures" = dep_support_policy_1, "Financial support for large projects" = dep_support_policy_2,
                  "Approvals despite 30% loss of harvest" = dep_support_policy_3, "Advisory services for farmers" = dep_support_policy_4, "State funding when switching to plant based food production" = dep_support_policy_5, "Increased one-off payments" = dep_support_policy_6),
             stars = stars)

# Create the plot
plot <- plot_summs(
  dep_support_policies, 
  dep_support_policy_1, 
  dep_support_policy_2, 
  dep_support_policy_3,
  dep_support_policy_4,
  dep_support_policy_5,
  dep_support_policy_6,
  model.names = c("General policy support for Agri-PV", "Simplified approval procedures", "Financial support for large projects", "Approvals despite 30% loss of harvest", "Advisory services for farmers", "State funding when switching to plant based food production", "Increased one-off payments"),
  coefs = c("NIMBY"),
  title = "Regression Outputs"
)

# Add a title using ggplot2
plot <- plot + ggtitle("Attitudes towards political instruments for the expansion of Agri-PV in CH")

plot<- plot + 
  scale_y_discrete(expand = c(0, 0.3)) +
  theme(legend.text = element_text(size = 12)) 


# Display the plot
print(plot)


## Interaction with one circle value (data with incompletes, including all potentially correctly treated respondents) ----

#take_part
dep_take_part_int_1 <- lm_robust(take_part_binary ~ NIMBY * circle1, data = all_match_correct_vote)
dep_take_part_int_2 <- lm_robust(take_part_binary ~ NIMBY * circle2, data = all_match_correct_vote)
dep_take_part_int_3 <- lm_robust(take_part_binary ~ NIMBY * circle3, data = all_match_correct_vote)
dep_take_part_int <- lm_robust(take_part_binary ~ NIMBY * circle, data = all_match_correct_vote)

summary(dep_take_part_int)

#intended_vote_binary
#includes all who are eligible to vote
dep_intended_vote_scaled_int_1 <- lm_robust(intended_vote_scaled ~ NIMBY * circle1, data = all_match_correct_vote)
dep_intended_vote_scaled_int_2 <- lm_robust(intended_vote_scaled ~ NIMBY * circle2, data = all_match_correct_vote)
dep_intended_vote_scaled_int_3 <- lm_robust(intended_vote_scaled ~ NIMBY * circle3, data = all_match_correct_vote)
dep_intended_vote_scaled_int <- lm_robust(intended_vote_scaled ~ NIMBY * circle, data = all_match_correct_vote)



# Create the plot
plot_summs(
  dep_intended_vote_scaled_int_1, 
  dep_intended_vote_scaled_int_2, 
  dep_intended_vote_scaled_int_3, 
  dep_intended_vote_scaled_int,
  model.names = c("Model 1: NIMBY * circle1", "Model 2: NIMBY * circle2", "Model 3: NIMBY * circle3", "Model 4: NIMBY * circle"),
  coefs = c("NIMBY", "circle1", "circle2", "circle3", "circle"),  # Adjust based on your actual coefficients
  title = "Regression Outputs"
)

summary(dep_intended_vote_scaled_int)

#intended_vote_if_binary
#includes only those who are not eligible to vote
dep_intended_vote_if_scaled_int_1 <- lm_robust(intended_vote_if_scaled ~ NIMBY * circle1, data = all_match_correct_vote)
dep_intended_vote_if_scaled_int_2 <- lm_robust(intended_vote_if_scaled ~ NIMBY * circle2, data = all_match_correct_vote)
dep_intended_vote_if_scaled_int_3 <- lm_robust(intended_vote_if_scaled ~ NIMBY * circle3, data = all_match_correct_vote)
dep_intended_vote_if_scaled_int <- lm_robust(intended_vote_if_scaled ~ NIMBY * circle, data = all_match_correct_vote)

#votes_merged,
#includes all who would have the intention to vote (also if not eligible)
dep_merged_vote_int_1 <- lm_robust(merged_vote ~ NIMBY * circle1, data = all_match_correct_vote)
dep_merged_vote_int_2 <- lm_robust(merged_vote ~ NIMBY * circle2, data = all_match_correct_vote)
dep_merged_vote_int_3 <- lm_robust(merged_vote ~ NIMBY * circle3, data = all_match_correct_vote)
dep_merged_vote_int <- lm_robust(merged_vote ~ NIMBY * circle, data = all_match_correct_vote)

summary(dep_merged_vote_int_3)

dep_merged_vote_int_split <- lm_robust(merged_vote ~ NIMBY, data = all_match_correct[all_match_correct$circle3==1,])
summary(dep_merged_vote_int_split)

#Summary
modelsummary(list("take_part" = dep_take_part_int_1, "intended_vote" = dep_intended_vote_scaled_int_1, "intended_vote_if" = dep_intended_vote_if_scaled_int_1, "merged_vote" = dep_merged_vote_int_1),
             stars = stars)

#Create dataset to check the results of the regression
data_vote_test <- all_match_correct %>%
  select(take_part_binary, intended_vote_binary, intended_vote_if_binary, circle1, circle2, circle3, NIMBY, circle_match, after) %>%
  filter(!is.na(intended_vote_if_binary))

#attitude_expansion
dep_attitue_expansion_int_1 <- lm_robust(attitude_expansion ~ NIMBY * circle1, data = all_match_correct)
dep_attitue_expansion_int_2 <- lm_robust(attitude_expansion ~ NIMBY * circle2, data = all_match_correct)
dep_attitue_expansion_int_3 <- lm_robust(attitude_expansion ~ NIMBY * circle3, data = all_match_correct)
dep_attitue_expansion_int <- lm_robust(attitude_expansion ~ NIMBY * circle, data = all_match_correct)

summary(dep_attitue_expansion_int_3)

#attitude_expansion_small
dep_attitue_expansion_small_int_1 <- lm_robust(attitude_expansion_small ~ NIMBY * circle1, data = all_match_correct)
dep_attitue_expansion_small_int_2 <- lm_robust(attitude_expansion_small ~ NIMBY * circle2, data = all_match_correct)
dep_attitue_expansion_small_int_3 <- lm_robust(attitude_expansion_small ~ NIMBY * circle3, data = all_match_correct)
dep_attitue_expansion_small_int <- lm_robust(attitude_expansion_small ~ NIMBY * circle, data = all_match_correct)

summary(dep_attitue_expansion_small_int_2)


#attitude_expansion_medium
dep_attitue_expansion_medium_int_1 <- lm_robust(attitude_expansion_medium ~ NIMBY * circle1, data = all_match_correct)
dep_attitue_expansion_medium_int_2 <- lm_robust(attitude_expansion_medium ~ NIMBY * circle2, data = all_match_correct)
dep_attitue_expansion_medium_int_3 <- lm_robust(attitude_expansion_medium ~ NIMBY * circle3, data = all_match_correct)
dep_attitue_expansion_medium_int <- lm_robust(attitude_expansion_medium ~ NIMBY * circle, data = all_match_correct)

#attitude_expansion_large
dep_attitue_expansion_large_int_1 <- lm_robust(attitude_expansion_large ~ NIMBY * circle1, data = all_match_correct)
dep_attitue_expansion_large_int_2 <- lm_robust(attitude_expansion_large ~ NIMBY * circle2, data = all_match_correct)
dep_attitue_expansion_large_int_3 <- lm_robust(attitude_expansion_large ~ NIMBY * circle3, data = all_match_correct)
dep_attitue_expansion_large_int <- lm_robust(attitude_expansion_large ~ NIMBY * circle, data = all_match_correct)

summary(dep_attitue_expansion_large_int)

#attitude_expansion_nearby
dep_attitue_expansion_nearby_int_1 <- lm_robust(attitude_expansion_nearby ~ NIMBY * circle1, data = all_match_correct)
dep_attitue_expansion_nearby_int_2 <- lm_robust(attitude_expansion_nearby ~ NIMBY * circle2, data = all_match_correct)
dep_attitue_expansion_nearby_int_3 <- lm_robust(attitude_expansion_nearby ~ NIMBY * circle3, data = all_match_correct)
dep_attitue_expansion_nearby_int <- lm_robust(attitude_expansion_nearby ~ NIMBY * circle, data = all_match_correct)

summary(dep_attitue_expansion_nearby_int)

#distance_accept
dep_distance_accept_int_1 <- lm_robust(distance_accept ~ NIMBY * circle1, data = all_match_correct)
dep_distance_accept_int_2 <- lm_robust(distance_accept ~ NIMBY * circle2, data = all_match_correct)
dep_distance_accept_int_3 <- lm_robust(distance_accept ~ NIMBY * circle3, data = all_match_correct)
dep_distance_accept_int <- lm_robust(distance_accept ~ NIMBY * circle, data = all_match_correct)

summary(dep_distance_accept_int_1)

#Summary
modelsummary(list("attitude_expansion" = dep_attitue_expansion_int_1, "expansion_small" = dep_attitue_expansion_small_int_1, "expansion_medium" = dep_attitue_expansion_medium_int_1,
                  "expansion_large" = dep_attitue_expansion_large_int_1, "expansion_nearby" = dep_attitue_expansion_nearby_int_1, "distance_accept" = dep_distance_accept_int_1),
             stars = stars)

# Create the plot without a title
plot_manip <- plot_summs(
  comp_likelihood_vincinity, 
  comp_personal_advantage, 
  comp_ch_advantage,
  comp_personal_concerned,
  model.names = c("Likelihood of Agri-PV in Vincinity", "Personal Advantage from Agri-PV", "Advantage for CH", "Personally Concerned of Agri-PV in Vincinity"),
  coefs = c("NIMBY")
)

# Add a title using ggplot2
plot_manip <- plot_manip + ggtitle("Manipulation Checks")

# Display the plot
print(plot_manip)

#cultivation_preferred, reference category: Plant based for direct human consumption
#dep_cultivation_preferred_int <- lm_robust(cultivation_preferred_f ~ NIMBY * circle1 * circle2 * circle3, data = all_match_correct)

#expect_approval
dep_expect_approval_int_1 <- lm_robust(expect_approval ~ NIMBY * circle1, data = all_match_correct)
dep_expect_approval_int_2 <- lm_robust(expect_approval ~ NIMBY * circle2, data = all_match_correct)
dep_expect_approval_int_3 <- lm_robust(expect_approval ~ NIMBY * circle3, data = all_match_correct)
dep_expect_approval_int <- lm_robust(expect_approval ~ NIMBY * circle, data = all_match_correct)

summary(dep_expect_approval_int_1)

#advise_support
dep_advise_support_int_1 <- lm_robust(advise_support ~ NIMBY * circle1, data = all_match_correct)
dep_advise_support_int_2 <- lm_robust(advise_support ~ NIMBY * circle2, data = all_match_correct)
dep_advise_support_int_3 <- lm_robust(advise_support ~ NIMBY * circle3, data = all_match_correct)
dep_advise_support_int <- lm_robust(advise_support ~ NIMBY * circle, data = all_match_correct)

#expect_support_family
dep_expect_support_family_int_1 <- lm_robust(expect_support_family ~ NIMBY * circle1, data = all_match_correct)
dep_expect_support_family_int_2 <- lm_robust(expect_support_family ~ NIMBY * circle2, data = all_match_correct)
dep_expect_support_family_int_3 <- lm_robust(expect_support_family ~ NIMBY * circle3, data = all_match_correct)
dep_expect_support_family_int <- lm_robust(expect_support_family ~ NIMBY * circle, data = all_match_correct)

#Summary
modelsummary(list("expect_approval" = dep_expect_approval_int, "advise_support" = dep_advise_support_int,
                  "support_family" = dep_expect_support_family_int),
             stars = stars)

#support_policies
dep_support_policies_int_1 <- lm_robust(support_policies ~ NIMBY * circle1, data = all_match_correct)
dep_support_policies_int_2 <- lm_robust(support_policies ~ NIMBY * circle2, data = all_match_correct)
dep_support_policies_int_3 <- lm_robust(support_policies ~ NIMBY * circle3, data = all_match_correct)
dep_support_policies_int <- lm_robust(support_policies ~ NIMBY * circle, data = all_match_correct)

summary(dep_support_policies_int)

#support_policy individually
dep_support_policy_1_int_1 <- lm_robust(support_policy_1 ~ NIMBY * circle1, data = all_match_correct)
dep_support_policy_1_int_2 <- lm_robust(support_policy_1 ~ NIMBY * circle2, data = all_match_correct)
dep_support_policy_1_int_3 <- lm_robust(support_policy_1 ~ NIMBY * circle3, data = all_match_correct)
dep_support_policy_1_int <- lm_robust(support_policy_1 ~ NIMBY * circle, data = all_match_correct)

dep_support_policy_2_int_1 <- lm_robust(support_policy_2 ~ NIMBY * circle1, data = all_match_correct)
dep_support_policy_2_int_2 <- lm_robust(support_policy_2 ~ NIMBY * circle2, data = all_match_correct)
dep_support_policy_2_int_3 <- lm_robust(support_policy_2 ~ NIMBY * circle3, data = all_match_correct)
dep_support_policy_2_int <- lm_robust(support_policy_2 ~ NIMBY * circle, data = all_match_correct)

dep_support_policy_3_int_1 <- lm_robust(support_policy_3 ~ NIMBY * circle1, data = all_match_correct)
dep_support_policy_3_int_2 <- lm_robust(support_policy_3 ~ NIMBY * circle2, data = all_match_correct)
dep_support_policy_3_int_3 <- lm_robust(support_policy_3 ~ NIMBY * circle3, data = all_match_correct)
dep_support_policy_3_int <- lm_robust(support_policy_3 ~ NIMBY * circle, data = all_match_correct)

dep_support_policy_4_int_1 <- lm_robust(support_policy_4 ~ NIMBY * circle1, data = all_match_correct)
dep_support_policy_4_int_2 <- lm_robust(support_policy_4 ~ NIMBY * circle2, data = all_match_correct)
dep_support_policy_4_int_3 <- lm_robust(support_policy_4 ~ NIMBY * circle3, data = all_match_correct)
dep_support_policy_4_int <- lm_robust(support_policy_4 ~ NIMBY * circle, data = all_match_correct)

dep_support_policy_5_int_1 <- lm_robust(support_policy_5 ~ NIMBY * circle1, data = all_match_correct)
dep_support_policy_5_int_2 <- lm_robust(support_policy_5 ~ NIMBY * circle2, data = all_match_correct)
dep_support_policy_5_int_3 <- lm_robust(support_policy_5 ~ NIMBY * circle3, data = all_match_correct)
dep_support_policy_5_int <- lm_robust(support_policy_5 ~ NIMBY * circle, data = all_match_correct)

dep_support_policy_6_int_1 <- lm_robust(support_policy_6 ~ NIMBY * circle1, data = all_match_correct)
dep_support_policy_6_int_2 <- lm_robust(support_policy_6 ~ NIMBY * circle2, data = all_match_correct)
dep_support_policy_6_int_3 <- lm_robust(support_policy_6 ~ NIMBY * circle3, data = all_match_correct)
dep_support_policy_6_int <- lm_robust(support_policy_6 ~ NIMBY * circle, data = all_match_correct)

dep_support_policy_int <- lm_robust(support_policies ~ NIMBY, data = all_match_correct[all_match_correct$circle3==1,])
summary(dep_support_policy_int)
#Summary
modelsummary(list("support_policies" = dep_support_policies_int, "support_policy_1" = dep_support_policy_1_int, "support_policy_2" = dep_support_policy_2_int,
                  "support_policy_3" = dep_support_policy_3_int, "support_policy_4" = dep_support_policy_4_int, "support_policy_5" = dep_support_policy_5_int, "support_policy_6" = dep_support_policy_6_int),
             stars = stars)



## Interaction with circle values, controlling for the other interactions ----

# data with incompletes, including all potentially correctly treated respondents)

#take_part
dep_take_part_int_all <- lm_robust(take_part_binary ~ NIMBY * circle1 + NIMBY * circle2 + NIMBY * circle3, data = all_match_correct_vote_vote)

#intended_vote_binary
#includes all who are eligible to vote
dep_intended_vote_scaled_int_all <- lm_robust(intended_vote_scaled ~ NIMBY * circle1 + NIMBY * circle2 + NIMBY * circle3, data = all_match_correct_vote_vote)

#intended_vote_if_binary
#includes only those who are not eligible to vote
dep_intended_vote_if_scaled_int_all <- lm_robust(intended_vote_if_scaled ~ NIMBY * circle1 + NIMBY * circle2 + NIMBY * circle3, data = all_match_correct_vote_vote)

#votes_merged,
#includes all who would have the intention to vote (also if not eligible)
dep_merged_vote_int_all <- lm_robust(merged_vote ~ NIMBY * circle1 + NIMBY * circle2 + NIMBY * circle3, data = all_match_correct_vote_vote)


#Summary
modelsummary(list("take_part" = dep_take_part_int_all, "intended_vote" = dep_intended_vote_scaled_int_all, "intended_vote_if" = dep_intended_vote_if_scaled_int_all, "merged_vote" = dep_merged_vote_int_all),
             stars = stars)

#attitude_expansion
dep_attitue_expansion_int_all <- lm_robust(attitude_expansion ~ NIMBY * circle1 + NIMBY * circle2 + NIMBY * circle3, data = all_match_correct)

#attitude_expansion_small
dep_attitue_expansion_small_int_all <- lm_robust(attitude_expansion_small ~ NIMBY * circle1 + NIMBY * circle2 + NIMBY * circle3, data = all_match_correct)

#attitude_expansion_medium
dep_attitue_expansion_medium_int_all <- lm_robust(attitude_expansion_medium ~ NIMBY * circle1 + NIMBY * circle2 + NIMBY * circle3, data = all_match_correct)

#attitude_expansion_large
dep_attitue_expansion_large_int_all <- lm_robust(attitude_expansion_large ~ NIMBY * circle1 + NIMBY * circle2 + NIMBY * circle3, data = all_match_correct)

#attitude_expansion_nearby
dep_attitue_expansion_nearby_int_all <- lm_robust(attitude_expansion_nearby ~ NIMBY * circle1 + NIMBY * circle2 + NIMBY * circle3, data = all_match_correct)

#distance_accept
dep_distance_accept_int <- lm_robust(distance_accept ~ NIMBY * circle1 + NIMBY * circle2 + NIMBY * circle3, data = all_match_correct)

summary(dep_distance_accept_int)

#Summary
modelsummary(list("attitude_expansion" = dep_attitue_expansion_int_all, "expansion_small" = dep_attitue_expansion_small_int_all, "expansion_medium" = dep_attitue_expansion_medium_int_all,
                  "expansion_large" = dep_attitue_expansion_large_int_all, "expansion_nearby" = dep_attitue_expansion_nearby_int_all, "distance_accept" = dep_distance_accept_int_all),
             stars = stars)

#cultivation_preferred, reference category: Plant based for direct human consumption
#dep_cultivation_preferred_int <- lm_robust(cultivation_preferred_f ~ NIMBY * circle1 * circle2 * circle3, data = all_match_correct)

#expect_approval
dep_expect_approval_int_all <- lm_robust(expect_approval ~ NIMBY * circle1 + NIMBY * circle2 + NIMBY * circle3, data = all_match_correct)

#advise_support
dep_advise_support_int_all <- lm_robust(advise_support ~ NIMBY * circle1 + NIMBY * circle2 + NIMBY * circle3, data = all_match_correct)

#expect_support_family
dep_expect_support_family_int_all <- lm_robust(expect_support_family ~ NIMBY * circle1 + NIMBY * circle2 + NIMBY * circle3, data = all_match_correct)

#Summary
modelsummary(list("expect_approval" = dep_expect_approval_int_all, "advise_support" = dep_advise_support_int_all, "support_family" = dep_expect_support_family_int_all),
             stars = stars)


#support_policies
dep_support_policies_int_all <- lm_robust(support_policies ~ NIMBY * circle1 + NIMBY * circle2 + NIMBY * circle3, data = all_match_correct)

#support_policy individually
dep_support_policy_1_int_all <- lm_robust(support_policy_1 ~ NIMBY * circle1 + NIMBY * circle2 + NIMBY * circle3, data = all_match_correct)

dep_support_policy_2_int_all <- lm_robust(support_policy_2 ~ NIMBY * circle1 + NIMBY * circle2 + NIMBY * circle3, data = all_match_correct)

dep_support_policy_3_int_all <- lm_robust(support_policy_3 ~ NIMBY * circle1 + NIMBY * circle2 + NIMBY * circle3, data = all_match_correct)

dep_support_policy_4_int_all <- lm_robust(support_policy_4 ~ NIMBY * circle1 + NIMBY * circle2 + NIMBY * circle3, data = all_match_correct)

dep_support_policy_5_int_all <- lm_robust(support_policy_5 ~ NIMBY * circle1 + NIMBY * circle2 + NIMBY * circle3, data = all_match_correct)

dep_support_policy_6_int_all <- lm_robust(support_policy_6 ~ NIMBY * circle1 + NIMBY * circle2 + NIMBY * circle3, data = all_match_correct)

#Summary
modelsummary(list("support_policies" = dep_support_policies_int_all, "support_policy_1" = dep_support_policy_1_int_all, "support_policy_2" = dep_support_policy_2_int_all,
                  "support_policy_3" = dep_support_policy_3_int_all, "support_policy_4" = dep_support_policy_4_int_all, "support_policy_5" = dep_support_policy_5_int_all, "support_policy_6" = dep_support_policy_6_int_all),
             stars = stars)


## With controls (data with incompletes, including all potentially correctly treated respondents) ----

#take_part
dep_take_part_control <- model_control("take_part_binary", "NIMBY *familiar_agri_pv", controls, all_match_correct_vote)

summary(dep_take_part_control)

#intended_vote_binary
#includes all who are eligible to vote
dep_intended_vote_scaled_control <- model_control("intended_vote_scaled", "NIMBY *familiar_agri_pv", controls, all_match_correct_vote)

#intended_vote_if_binary
#includes only those who are not eligible to vote
dep_intended_vote_if_scaled_control <- model_control("intended_vote_if_scaled", "NIMBY *familiar_agri_pv", controls, all_match_correct_vote)

#votes_merged,
#includes all who would have the intention to vote (also if not eligible)
dep_merged_vote_control <- model_control("merged_vote_2", "NIMBY*familiar_agri_pv", controls, all_match_correct_vote)
summary(dep_merged_vote_control)

#Summary
modelsummary(list("take_part" = dep_take_part_control, "intended_vote" = dep_intended_vote_scaled_control, "intended_vote_if" = dep_intended_vote_if_scaled_control, "merged_vote" = dep_merged_vote_control),
             stars = stars)

#attitude_expansion
dep_attitue_expansion_control <- model_control("attitude_expansion", "NIMBY*familiar_agri_pv", controls, all_match_correct)

#attitude_expansion_small
dep_attitue_expansion_small_control <- model_control("attitude_expansion_small", "NIMBY*familiar_agri_pv", controls, all_match_correct)

#attitude_expansion_medium
dep_attitue_expansion_medium_control <- model_control("attitude_expansion_medium", "NIMBY*familiar_agri_pv", controls, all_match_correct)

#attitude_expansion_large
dep_attitue_expansion_large_control <- model_control("attitude_expansion_large", "NIMBY*familiar_agri_pv", controls, all_match_correct)

#attitude_expansion_nearby
dep_attitue_expansion_nearby_control <- model_control("attitude_expansion_nearby", "NIMBY*familiar_agri_pv ", controls, all_match_correct)


#distance_accept
dep_distance_accept_control <- model_control("distance_accept", "NIMBY*familiar_agri_pv", controls, all_match_correct)


#Summary
modelsummary(list("attitude_expansion" = dep_attitue_expansion_control, "expansion_small" = dep_attitue_expansion_small_control, "expansion_medium" = dep_attitue_expansion_medium_control,
                  "expansion_large" = dep_attitue_expansion_large_control, "expansion_nearby" = dep_attitue_expansion_nearby_control, "distance_accept" = dep_distance_accept_control),
             stars = stars)

#cultivation_preferred, reference category: Plant based for direct human consumption
#dep_cultivation_preferred <- lm_robust(NIMBY ~ cultivation_preferred_f, data = all_match_correct)

#expect_approval
dep_expect_approval_control <- model_control("expect_approval", "NIMBY * familiar_agri_pv", controls, all_match_correct)

#advise_support
dep_advise_support_control <- model_control("advise_support", "NIMBY * familiar_agri_pv", controls, all_match_correct)


#expect_support_family
dep_expect_support_family_control <- model_control("expect_support_family", "NIMBY * familiar_agri_pv", controls, all_match_correct)


#Summary
modelsummary(list("expect_approval" = dep_expect_approval_control, "advise_support" = dep_advise_support_control,
                  "support_family" = dep_expect_support_family_control),
             stars = stars)


#support_policies
dep_support_policies_control <- model_control("support_policies", "NIMBY * solar_open_space", controls, all_match_correct)


#support_policy individually
dep_support_policy_1_control <- model_control("support_policy_1", "NIMBY * solar_open_space", controls, all_match_correct)

dep_support_policy_2_control <- model_control("support_policy_2", "NIMBY * solar_open_space", controls, all_match_correct)

dep_support_policy_3_control <- model_control("support_policy_3", "NIMBY * solar_open_space", controls, all_match_correct)

dep_support_policy_4_control <- model_control("support_policy_4", "NIMBY * solar_open_space", controls, all_match_correct)

dep_support_policy_5_control <- model_control("support_policy_5", "NIMBY * solar_open_space", controls, all_match_correct)

dep_support_policy_6_control <- model_control("support_policy_6", "NIMBY * solar_open_space", controls, all_match_correct)

#Summary
modelsummary(list("support_policies" = dep_support_policies_control, "support_policy_1" = dep_support_policy_1_control, "support_policy_2" = dep_support_policy_2_control,
                  "support_policy_3" = dep_support_policy_3_control, "support_policy_4" = dep_support_policy_4_control, "support_policy_5" = dep_support_policy_5_control, "support_policy_6" = dep_support_policy_6_control),
             stars = stars)



## Regression Voting Results ----


all_match_correct_circle1 <- data[data$circle_match == 1 & data$circle1 == 1 & data$take_part != 99, ]


#votes_merged
#H4
#includes all who voted, the intention if eligible to vote and excludes those who voted already before they filled out wave 2

dep_merged_vote_H4 <- lm_robust(merged_vote ~ NIMBY, data = all_match_correct_vote)
summary(dep_merged_vote_H4)

#regression revealed vote
dep_merged_vote_H4_revealed <- lm_robust(merged_vote_2 ~ NIMBY , data = all_match_correct_vote )
summary(dep_merged_vote_H4_revealed)

#with controls
dep_merged_vote_control_2 <- model_control("merged_vote_2", "NIMBY", controls, all_match_correct_vote)
summary(dep_merged_vote_control_2)

#H4a

#regression intended vote
dep_merged_vote_H4a_intended <- lm_robust(merged_vote ~ NIMBY_H4a, data = all_match_correct_vote)
summary(dep_merged_vote_H4a_intended)

#regression intended vote
dep_merged_vote_revealed_1 <- lm_robust(merged_vote_2 ~ NIMBY, data = all_match_correct_vote[all_match_correct_vote$circle1 == 1, ])
summary(dep_merged_vote_revealed_1)

#regression revealed vote
dep_merged_vote_H4a_revealed <- lm_robust(merged_vote_2 ~ NIMBY_H4a, data = all_match_correct_vote)
summary(dep_merged_vote_H4a_revealed)

#H4b
dep_vote_gap <- lm_robust(vote_gap ~ NIMBY, data = all_match_correct_vote)
summary(dep_vote_gap)

table(all_match_correct_vote$merged_vote, useNA = "ifany")
table(all_match_correct_vote$merged_vote_2, useNA = "ifany")

#regression revealed vote
dep_merged_vote_revealed <- lm_robust(merged_vote_2 ~ NIMBY , data = all_match_correct_vote )
summary(dep_merged_vote_revealed)



#Summary
modelsummary(list("Influence of treatment on intended vote" = dep_merged_vote_H4, "Influence of treatment on revealed vote" = dep_merged_vote_H4_revealed, "Influence of high pot. in close proximity on revealed vote" = dep_merged_vote_revealed_1, "Influence of treatment on difference between intended and revealed vote" = dep_vote_gap),
             stars = stars)


# Check external validity ----
# Are the population characteristics equal to the sample characteristics














# Visualize some results -----


## Descriptive Stats ----

#age
all_match_correct_age <- all_match_correct %>%
  mutate(age_bin = cut(age, breaks = seq(18, 90, by = 5), right = FALSE, include.lowest = TRUE)) %>%
  group_by(age_bin) %>%
  summarise(count = n()) %>%
  mutate(percent = count / sum(count) * 100)

#summary(data$age)

#Import population statistics for age
filename <- "Age_CH.csv"
age_ch <- read.csv(filename, header = TRUE, sep = ";")

# Convert Percent to numeric
age_ch$Percent <- as.numeric(gsub(",", ".", age_ch$Percent))

# Create age bins for Swiss data
age_ch_bins <- age_ch %>%
  mutate(age_bin = cut(ALTER, breaks = seq(18, 90, by = 5), right = FALSE, include.lowest = TRUE)) %>%
  group_by(age_bin) %>%
  summarise(percent = sum(Percent))

# Display Swiss data
print(age_ch_bins)

# Add a source column to distinguish between datasets
all_match_correct_age <- all_match_correct_age %>% mutate(source = "Sample")
age_ch_bins <- age_ch_bins %>% mutate(source = "Population")

# Combine datasets
combined_data <- bind_rows(all_match_correct_age, age_ch_bins)

# Plot the data
ggplot(combined_data, aes(x = age_bin, y = percent, fill = source)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Age", y = "Percent", title = "Age Distribution Comparison") +
  scale_fill_manual(values = c("Sample" = "#1f77b4", "Population" = "#ff7f0e")) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 12),
    plot.title = element_text(size = 16, face = "bold"),
    legend.title = element_blank(),
    legend.text = element_text(size = 12)
  )


#Gender
#Import Gender Population Statistics
filename <- "gender_ch.csv"
gender_ch <- read.csv(filename, header = TRUE, sep = ";")

table(all_match_correct$gender)
#prepare sample data
all_match_correct_gender<- all_match_correct %>%
  filter(!is.na(gender)) %>%  # Remove NA values
  count(gender) %>%  # Count occurrences, creates 'n'
  filter(!gender %in% c("3", "99")) %>%
  mutate(Type = recode(gender, `1` = "male", `2` = "female")) %>%
  select(Type, n) %>%  # 'n' is the default name from count()
  rename(Count = n) %>%
  mutate(Percent = Count / sum(Count) * 100)  # Calculate percentages


all_match_correct_gender <- all_match_correct_gender %>% mutate(source = "Sample")
gender_ch <- gender_ch %>% mutate(source = "Population")

# Ensure Percent is numeric in both data frames
all_match_correct_gender <- all_match_correct_gender %>%
  mutate(Percent = as.numeric(Percent))

gender_ch <- gender_ch %>%
  mutate(Percent = gsub(",", ".", Percent)) %>%  # Replace commas with dots
  mutate(Percent = as.numeric(Percent))  # Convert to numeric


gender_ch <- gender_ch %>%
  rename(
    Type = GESCHLECHT,
    Count = Frequency
  )

combined_data_gender <- bind_rows(all_match_correct_gender, gender_ch)

#create plot
ggplot(combined_data_gender, aes(x = Type, y = Percent, fill = source)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Gender", y = "Percentage (%)", title = "Gender Distribution Comparison") +
  scale_fill_manual(values = c("Sample" = "#1f77b4", "Population" = "#ff7f0e")) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    plot.title = element_text(size = 16, face = "bold"),
    legend.title = element_blank(),
    legend.text = element_text(size = 12)
  ) +
  coord_cartesian(ylim = c(45, 52))  # Set the y-axis limits from 40% to 100%

## Urban Rural

#Import population statistics for age
filename <- "urban_rural_ch.csv"
urban_rural_ch <- read.csv(filename, header = TRUE, sep = ";")


all_match_correct_urban_rural <- data %>%
  filter(!is.na(urban_rural)) %>%  # Remove NA values
  count(urban_rural) %>%  # Count occurrences, creates 'n'
  mutate(Type = recode(urban_rural, `1` = "urban", `2` = "suburban", `3` = "rural")) %>%
  select(Type, n) %>%  # 'n' is the default name from count()
  rename(Count = n) %>%
  mutate(Percent = Count / sum(Count) * 100)  # Calculate percentages

all_match_correct_urban_rural <- all_match_correct_urban_rural %>% mutate(source = "Sample")
urban_rural_ch <- urban_rural_ch %>% mutate(source = "Population")

# Ensure Percent is numeric in both data frames
all_match_correct_urban_rural <- all_match_correct_urban_rural %>%
  mutate(Percent = as.numeric(Percent))

urban_rural_ch <- urban_rural_ch %>%
  mutate(Percent = gsub(",", ".", Percent)) %>%  # Replace commas with dots
  mutate(Percent = as.numeric(Percent))  # Convert to numeric

urban_rural_ch <- urban_rural_ch %>%
  rename(
    Type = urbrur,
    Count = Frequency
  ) %>%
  select(Type, Count, Percent, source)  # Keep only desired columns

combined_data_urban_rural <- bind_rows(all_match_correct_urban_rural, urban_rural_ch)

ggplot(combined_data_urban_rural, aes(x = Type, y = Percent, fill = source)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Category", y = "Percentage (%)", title = "Degree of Urbanization") +
  scale_fill_manual(values = c("Sample" = "#1f77b4", "Population" = "#ff7f0e")) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    plot.title = element_text(size = 16, face = "bold"),
    legend.title = element_blank(),
    legend.text = element_text(size = 12)
  )



## Differences Wave 1 / Wave 2 ----

#age
wave1_age <- all_match_correct %>%
  mutate(age_bin = cut(age, breaks = seq(18, 90, by = 5), right = FALSE, include.lowest = TRUE)) %>%
  group_by(age_bin) %>%
  summarise(count = n()) %>%
  mutate(percent = count / sum(count) * 100)

wave2_age<- all_match_correct[all_match_correct$wave2 == 1, ] %>%
  mutate(age_bin = cut(age, breaks = seq(18, 90, by = 5), right = FALSE, include.lowest = TRUE)) %>%
  group_by(age_bin) %>%
  summarise(count = n()) %>%
  mutate(percent = count / sum(count) * 100)

#summary(data$age)

# Add a source column to distinguish between datasets
wave1_age <- wave1_age %>% mutate(source = "Wave1")
wave2_age <- wave2_age %>% mutate(source = "Wave2")

# Combine datasets
combined_data <- bind_rows(wave1_age, wave2_age)

# Plot the data
ggplot(combined_data, aes(x = age_bin, y = percent, fill = source)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Age", y = "Percent", title = "Age Distribution Comparison") +
  scale_fill_manual(values = c("Wave1" = "#1f77b4", "Wave2" = "#ff7f0e")) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 12),
    plot.title = element_text(size = 16, face = "bold"),
    legend.title = element_blank(),
    legend.text = element_text(size = 12)
  )


#Gender

#prepare sample data wave 1
wave1_gender<- all_match_correct %>%
  filter(!is.na(gender)) %>%  # Remove NA values
  count(gender) %>%  # Count occurrences, creates 'n'
  filter(!gender %in% c("3", "99")) %>%
  mutate(Type = recode(gender, `1` = "male", `2` = "female")) %>%
  select(Type, n) %>%  # 'n' is the default name from count()
  rename(Count = n) %>%
  mutate(Percent = Count / sum(Count) * 100)  # Calculate percentages

# wave 2
wave2_gender<- all_match_correct[all_match_correct$wave2 == 1, ] %>%
  filter(!is.na(gender)) %>%  # Remove NA values
  count(gender) %>%  # Count occurrences, creates 'n'
  filter(!gender %in% c("3", "99")) %>%
  mutate(Type = recode(gender, `1` = "male", `2` = "female")) %>%
  select(Type, n) %>%  # 'n' is the default name from count()
  rename(Count = n) %>%
  mutate(Percent = Count / sum(Count) * 100)  # Calculate percentages



wave1_gender <- wave1_gender %>% mutate(source = "Wave1")
wave2_gender <- wave2_gender %>% mutate(source = "Wave2")

# Ensure Percent is numeric in both data frames
wave1_gender <- wave1_gender %>%
  mutate(Percent = as.numeric(Percent))

wave2_gender <- wave2_gender %>%
  mutate(Percent = as.numeric(Percent))

combined_data_gender <- bind_rows(wave1_gender, wave2_gender)

#create plot
ggplot(combined_data_gender, aes(x = Type, y = Percent, fill = source)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Gender", y = "Percentage (%)", title = "Gender Distribution Comparison") +
  scale_fill_manual(values = c("Wave1" = "#1f77b4", "Wave2" = "#ff7f0e")) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    plot.title = element_text(size = 16, face = "bold"),
    legend.title = element_blank(),
    legend.text = element_text(size = 12)
  ) +
  coord_cartesian(ylim = c(45, 52))  # Set the y-axis limits from 40% to 100%

## Urban Rural

wave1_urban_rural <- all_match_correct %>%
  filter(!is.na(urban_rural)) %>%  # Remove NA values
  count(urban_rural) %>%  # Count occurrences, creates 'n'
  mutate(Type = recode(urban_rural, `1` = "urban", `2` = "suburban", `3` = "rural")) %>%
  select(Type, n) %>%  # 'n' is the default name from count()
  rename(Count = n) %>%
  mutate(Percent = Count / sum(Count) * 100)  # Calculate percentages

wave2_urban_rural <- all_match_correct[all_match_correct$wave2 == 1, ] %>%
  filter(!is.na(urban_rural)) %>%  # Remove NA values
  count(urban_rural) %>%  # Count occurrences, creates 'n'
  mutate(Type = recode(urban_rural, `1` = "urban", `2` = "suburban", `3` = "rural")) %>%
  select(Type, n) %>%  # 'n' is the default name from count()
  rename(Count = n) %>%
  mutate(Percent = Count / sum(Count) * 100)  # Calculate percentages

wave1_urban_rural <- wave1_urban_rural %>% mutate(source = "Wave1")
wave2_urban_rural <- wave2_urban_rural %>% mutate(source = "Wave2")

# Ensure Percent is numeric in both data frames
wave1_urban_rural <- wave1_urban_rural %>%
  mutate(Percent = as.numeric(Percent))

wave2_urban_rural <- wave2_urban_rural %>%
  mutate(Percent = as.numeric(Percent))


combined_data_urban_rural <- bind_rows(wave1_urban_rural, wave2_urban_rural)

ggplot(combined_data_urban_rural, aes(x = Type, y = Percent, fill = source)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Category", y = "Percentage (%)", title = "Degree of Urbanization") +
  scale_fill_manual(values = c("Wave1" = "#1f77b4", "Wave2" = "#ff7f0e")) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    plot.title = element_text(size = 16, face = "bold"),
    legend.title = element_blank(),
    legend.text = element_text(size = 12)
  )


## Differences Wave 1 / Wave 2 and Comparison to Population ------

#age
wave1_age <- all_match_correct %>%
  mutate(age_bin = cut(age, breaks = seq(18, 90, by = 5), right = FALSE, include.lowest = TRUE)) %>%
  group_by(age_bin) %>%
  summarise(count = n()) %>%
  mutate(percent = count / sum(count) * 100)

wave2_age<- all_match_correct[all_match_correct$wave2 == 1, ] %>%
  mutate(age_bin = cut(age, breaks = seq(18, 90, by = 5), right = FALSE, include.lowest = TRUE)) %>%
  group_by(age_bin) %>%
  summarise(count = n()) %>%
  mutate(percent = count / sum(count) * 100)

#Import population statistics for age
filename <- "Age_CH.csv"
age_ch <- read.csv(filename, header = TRUE, sep = ";")

# Convert Percent to numeric
age_ch$Percent <- as.numeric(gsub(",", ".", age_ch$Percent))

# Create age bins for Swiss data
age_ch_bins <- age_ch %>%
  mutate(age_bin = cut(ALTER, breaks = seq(18, 90, by = 5), right = FALSE, include.lowest = TRUE)) %>%
  group_by(age_bin) %>%
  summarise(percent = sum(Percent))

#summary(data$age)

# Add a source column to distinguish between datasets
wave1_age <- wave1_age %>% mutate(source = "Wave1")
wave2_age <- wave2_age %>% mutate(source = "Wave2")
age_ch_bins <- age_ch_bins %>% mutate(source = "Population")

# Combine datasets
combined_data <- bind_rows(wave1_age, wave2_age, age_ch_bins)

# Plot the data
ggplot(combined_data, aes(x = age_bin, y = percent, fill = source)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Age", y = "Percent", title = "Age Distribution Comparison") +
  scale_fill_manual(values = c("Wave1" = "#1f77b4", "Wave2" = "#ff7f0e", "Population" = "#999999")) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 12),
    plot.title = element_text(size = 16, face = "bold"),
    legend.title = element_blank(),
    legend.text = element_text(size = 12)
  )


#Gender

#prepare sample data wave 1
wave1_gender<- all_match_correct %>%
  filter(!is.na(gender)) %>%  # Remove NA values
  count(gender) %>%  # Count occurrences, creates 'n'
  filter(!gender %in% c("3", "99")) %>%
  mutate(Type = recode(gender, `1` = "male", `2` = "female")) %>%
  select(Type, n) %>%  # 'n' is the default name from count()
  rename(Count = n) %>%
  mutate(Percent = Count / sum(Count) * 100)  # Calculate percentages

# wave 2
wave2_gender<- all_match_correct[all_match_correct$wave2 == 1, ] %>%
  filter(!is.na(gender)) %>%  # Remove NA values
  count(gender) %>%  # Count occurrences, creates 'n'
  filter(!gender %in% c("3", "99")) %>%
  mutate(Type = recode(gender, `1` = "male", `2` = "female")) %>%
  select(Type, n) %>%  # 'n' is the default name from count()
  rename(Count = n) %>%
  mutate(Percent = Count / sum(Count) * 100)  # Calculate percentages

#Import Gender Population Statistics
filename <- "gender_ch.csv"
gender_ch <- read.csv(filename, header = TRUE, sep = ";")


gender_ch <- gender_ch %>%
  mutate(Percent = gsub(",", ".", Percent)) %>%  # Replace commas with dots
  mutate(Percent = as.numeric(Percent))  # Convert to numeric


gender_ch <- gender_ch %>%
  rename(
    Type = GESCHLECHT,
    Count = Frequency
  )



wave1_gender <- wave1_gender %>% mutate(source = "Wave1")
wave2_gender <- wave2_gender %>% mutate(source = "Wave2")
gender_ch <- gender_ch %>% mutate(source = "Population")

# Ensure Percent is numeric in both data frames
wave1_gender <- wave1_gender %>%
  mutate(Percent = as.numeric(Percent))

wave2_gender <- wave2_gender %>%
  mutate(Percent = as.numeric(Percent))

combined_data_gender <- bind_rows(wave1_gender, wave2_gender, gender_ch)

#create plot
ggplot(combined_data_gender, aes(x = Type, y = Percent, fill = source)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Gender", y = "Percentage (%)", title = "Gender Distribution Comparison") +
  scale_fill_manual(values = c("Wave1" = "#1f77b4", "Wave2" = "#ff7f0e", "Population" = "#999999")) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    plot.title = element_text(size = 16, face = "bold"),
    legend.title = element_blank(),
    legend.text = element_text(size = 12)
  ) +
  coord_cartesian(ylim = c(45, 52))  # Set the y-axis limits from 40% to 100%

## Urban Rural

wave1_urban_rural <- all_match_correct %>%
  filter(!is.na(urban_rural)) %>%  # Remove NA values
  count(urban_rural) %>%  # Count occurrences, creates 'n'
  mutate(Type = recode(urban_rural, `1` = "urban", `2` = "suburban", `3` = "rural")) %>%
  select(Type, n) %>%  # 'n' is the default name from count()
  rename(Count = n) %>%
  mutate(Percent = Count / sum(Count) * 100)  # Calculate percentages

wave2_urban_rural <- all_match_correct[all_match_correct$wave2 == 1, ] %>%
  filter(!is.na(urban_rural)) %>%  # Remove NA values
  count(urban_rural) %>%  # Count occurrences, creates 'n'
  mutate(Type = recode(urban_rural, `1` = "urban", `2` = "suburban", `3` = "rural")) %>%
  select(Type, n) %>%  # 'n' is the default name from count()
  rename(Count = n) %>%
  mutate(Percent = Count / sum(Count) * 100)  # Calculate percentages

#Import population statistics for age
filename <- "urban_rural_ch.csv"
urban_rural_ch <- read.csv(filename, header = TRUE, sep = ";")

urban_rural_ch <- urban_rural_ch %>% mutate(source = "Population")

urban_rural_ch <- urban_rural_ch %>%
  mutate(Percent = gsub(",", ".", Percent)) %>%  # Replace commas with dots
  mutate(Percent = as.numeric(Percent))  # Convert to numeric

urban_rural_ch <- urban_rural_ch %>%
  rename(
    Type = urbrur,
    Count = Frequency
  ) %>%
  select(Type, Count, Percent, source)  # Keep only desired columns


wave1_urban_rural <- wave1_urban_rural %>% mutate(source = "Wave1")
wave2_urban_rural <- wave2_urban_rural %>% mutate(source = "Wave2")

# Ensure Percent is numeric in both data frames
wave1_urban_rural <- wave1_urban_rural %>%
  mutate(Percent = as.numeric(Percent))

wave2_urban_rural <- wave2_urban_rural %>%
  mutate(Percent = as.numeric(Percent))


combined_data_urban_rural <- bind_rows(wave1_urban_rural, wave2_urban_rural, urban_rural_ch)

ggplot(combined_data_urban_rural, aes(x = Type, y = Percent, fill = source)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Category", y = "Percentage (%)", title = "Degree of Urbanization") +
  scale_fill_manual(values = c("Wave1" = "#1f77b4", "Wave2" = "#ff7f0e", "Population" = "#999999")) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    plot.title = element_text(size = 16, face = "bold"),
    legend.title = element_blank(),
    legend.text = element_text(size = 12)
  )


#Voting Behaviour Wave 1 Wave 2

# Prepare Wave 1 data
wave1_vote <- all_match_correct_vote %>%
  filter(!is.na(merged_vote_modified)) %>%  # Remove NA values
  count(merged_vote_modified) %>%  # Count occurrences, creates 'n'
  rename(Type = merged_vote_modified, Count = n) %>%  # Rename columns
  mutate(Percent = Count / sum(Count) * 100)  # Calculate percentages

# Prepare Wave 2 data
wave2_vote <- all_match_correct_vote %>%
  filter(wave2 == 1, !is.na(merged_vote_2)) %>%  # Remove NA values
  count(merged_vote_2) %>%  # Count occurrences, creates 'n'
  rename(Type = merged_vote_2, Count = n) %>%  # Rename columns
  mutate(Percent = Count / sum(Count) * 100)  # Calculate percentages

# Manually create population data
population_vote <- data.frame(
  Type = c(0, 1),  # 0 for No, 1 for Yes
  Count = c(31.28, 68.72),  # Population percentages
  Percent = c(31.28, 68.72),
  source = "Population"
)

# Add source labels to Wave 1 and Wave 2 data
wave1_vote <- wave1_vote %>% mutate(source = "Wave1")
wave2_vote <- wave2_vote %>% mutate(source = "Wave2")

# Ensure Percent is numeric in all data frames
wave1_vote <- wave1_vote %>%
  mutate(Percent = as.numeric(Percent))
wave2_vote <- wave2_vote %>%
  mutate(Percent = as.numeric(Percent))

# Combine all datasets
combined_data_vote <- bind_rows(wave1_vote, wave2_vote, population_vote)

# Convert Type to a factor with labels for better readability
combined_data_vote$Type <- factor(combined_data_vote$Type, levels = c(0, 1), labels = c("No", "Yes"))

# Create the plot
ggplot(combined_data_vote, aes(x = Type, y = Percent, fill = source)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Vote", y = "Percentage (%)", title = "Voting Behavior Comparison") +
  scale_fill_manual(values = c("Wave1" = "#1f77b4", "Wave2" = "#ff7f0e", "Population" = "#999999")) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    plot.title = element_text(size = 16, face = "bold"),
    legend.title = element_blank(),
    legend.text = element_text(size = 12)
  )


## Other Descriptives ----

#left-right
# Prepare wave1 data
wave1_left_right <- all_match_correct %>%
  group_by(left_right) %>%
  summarise(count = n()) %>%
  mutate(percent = count / sum(count) * 100)

# Prepare wave2 data
wave2_left_right <- all_match_correct %>%
  filter(wave2 == 1) %>%
  group_by(left_right) %>%
  summarise(count = n()) %>%
  mutate(percent = count / sum(count) * 100)

wave1_left_right <- wave1_left_right %>% mutate(source = "Wave1")
wave2_left_right <- wave2_left_right %>% mutate(source = "Wave2")

# Combine datasets
combined_data <- bind_rows(wave1_left_right, wave2_left_right)

# Plot the data
ggplot(combined_data, aes(x = as.factor(left_right), y = percent, fill = source)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Left-Right Scale", y = "Percent", title = "Left-Right Scale Distribution Comparison") +
  scale_fill_manual(values = c("Wave1" = "#1f77b4", "Wave2" = "#ff7f0e")) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    plot.title = element_text(size = 16, face = "bold"),
    legend.title = element_blank(),
    legend.text = element_text(size = 12)
  )

#environment score
# Prepare wave1 data
wave1_environment <- all_match_correct %>%
  group_by(environment_score) %>%
  summarise(count = n()) %>%
  mutate(percent = count / sum(count) * 100)

# Prepare wave2 data
wave2_environment <- all_match_correct %>%
  filter(wave2 == 1) %>%
  group_by(environment_score) %>%
  summarise(count = n()) %>%
  mutate(percent = count / sum(count) * 100)

wave1_environment <- wave1_environment %>% mutate(source = "Wave1")
wave2_environment <- wave2_environment %>% mutate(source = "Wave2")

# Combine datasets
combined_data <- bind_rows(wave1_environment, wave2_environment)

# Plot the data
ggplot(combined_data, aes(x = as.factor(environment_score), y = percent, fill = source)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Environment Score", y = "Percent", title = "Environmental Score Distribution Comparison") +
  scale_fill_manual(values = c("Wave1" = "#1f77b4", "Wave2" = "#ff7f0e")) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    plot.title = element_text(size = 16, face = "bold"),
    legend.title = element_blank(),
    legend.text = element_text(size = 12)
  )

# Prepare wave1 data
wave1_environment <- all_match_correct %>%
  group_by(environment_score) %>%
  summarise(count = n()) %>%
  mutate(percent = count / sum(count) * 100)

# Prepare wave2 data
wave2_environment <- all_match_correct %>%
  filter(wave2 == 1) %>%
  group_by(environment_score) %>%
  summarise(count = n()) %>%
  mutate(percent = count / sum(count) * 100)

# Add source column
wave1_environment <- wave1_environment %>% mutate(source = "Wave1")
wave2_environment <- wave2_environment %>% mutate(source = "Wave2")

# Combine datasets
combined_data <- bind_rows(wave1_environment, wave2_environment)

# Create a complete dataset including all scores from 1 to 7
all_scores <- data.frame(environment_score = 1:7)
combined_data_full <- combined_data %>%
  complete(source, environment_score = 1:7, fill = list(count = 0, percent = 0))

# Plot the data
ggplot(combined_data_full, aes(x = as.factor(environment_score), y = percent, fill = source)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Environment Score", y = "Percent", title = "Environmental Score Distribution Comparison") +
  scale_fill_manual(values = c("Wave1" = "#1f77b4", "Wave2" = "#ff7f0e")) +
  scale_x_discrete(limits = as.character(1:7)) + # Ensure all scores from 1 to 7 are shown
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    plot.title = element_text(size = 16, face = "bold"),
    legend.title = element_blank(),
    legend.text = element_text(size = 12)
  )

# Create a combined dataset
combined_data <- all_match_correct %>%
  mutate(source = if_else(wave2 == 1, "Wave2", "Wave1"))

# Plot the boxplot
ggplot(combined_data, aes(x = source, y = environment_score, fill = source)) +
  geom_boxplot(alpha = 0.6) +
  labs(x = "Wave", y = "Environment Score", title = "Boxplot of Environmental Scores by Wave") +
  scale_fill_manual(values = c("Wave1" = "#1f77b4", "Wave2" = "#ff7f0e")) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    plot.title = element_text(size = 16, face = "bold"),
    legend.title = element_blank(),
    legend.text = element_text(size = 12)
  )


# Combine datasets for density plot
combined_data <- all_match_correct %>%
  mutate(source = if_else(wave2 == 1, "Wave2", "Wave1"))

# Plot density
ggplot(combined_data, aes(x = environment_score, fill = source)) +
  geom_density(alpha = 0.6) +
  labs(x = "Environment Score", y = "Density", title = "Density Plot of Environmental Scores") +
  scale_fill_manual(values = c("Wave1" = "#1f77b4", "Wave2" = "#ff7f0e")) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    plot.title = element_text(size = 16, face = "bold"),
    legend.title = element_blank(),
    legend.text = element_text(size = 12)
  )


#like energy from agri pv
# Prepare wave1 data
wave1_energy_agri_pv <- all_match_correct %>%
  group_by(like_energy_agri_pv) %>%
  summarise(count = n()) %>%
  mutate(percent = count / sum(count) * 100)

# Prepare wave2 data
wave2_energy_agri_pv <- all_match_correct %>%
  filter(wave2 == 1) %>%
  group_by(like_energy_agri_pv) %>%
  summarise(count = n()) %>%
  mutate(percent = count / sum(count) * 100)

wave1_energy_agri_pv <- wave1_energy_agri_pv %>% mutate(source = "Wave1")
wave2_energy_agri_pv <- wave2_energy_agri_pv %>% mutate(source = "Wave2")

# Combine datasets
combined_data <- bind_rows(wave1_energy_agri_pv, wave2_energy_agri_pv)

# Plot the data
ggplot(combined_data, aes(x = as.factor(like_energy_agri_pv), y = percent, fill = source)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Attitude (1 - Very Bad, 7 - Very Good)", y = "Percent", title = "Comparison Attitude towards Energy from Agri-PV") +
  scale_fill_manual(values = c("Wave1" = "#1f77b4", "Wave2" = "#ff7f0e")) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    plot.title = element_text(size = 16, face = "bold"),
    legend.title = element_blank(),
    legend.text = element_text(size = 12)
  )

# solar open space
# Prepare wave1 data
wave1_solar_open_space <- all_match_correct %>%
  filter(!is.na(solar_open_space)) %>%
  group_by(solar_open_space) %>%
  summarise(count = n()) %>%
  mutate(percent = count / sum(count) * 100)

# Prepare wave2 data
wave2_solar_open_space <- all_match_correct %>%
  filter(wave2 == 1, !is.na(solar_open_space)) %>%
  group_by(solar_open_space) %>%
  summarise(count = n()) %>%
  mutate(percent = count / sum(count) * 100)

wave1_solar_open_space <- wave1_solar_open_space %>% mutate(source = "Wave1")
wave2_solar_open_space <- wave2_solar_open_space %>% mutate(source = "Wave2")

# Combine datasets
combined_data <- bind_rows(wave1_solar_open_space, wave2_solar_open_space)

# Plot the data
ggplot(combined_data, aes(x = as.factor(solar_open_space), y = percent, fill = source)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Attitude (1 - Disagree, 7 - Agree)", y = "Percent", title = "Comparison Attitude towards Expansion of Open Space Solar") +
  scale_fill_manual(values = c("Wave1" = "#1f77b4", "Wave2" = "#ff7f0e")) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    plot.title = element_text(size = 16, face = "bold"),
    legend.title = element_blank(),
    legend.text = element_text(size = 12)
  )


#Participation Wave 1
# I will definitely participate.  (1)
# I'm not quite sure yet, but I'll probably take part.  (2) 
#	I usually vote, but probably not this time.  (3) 
# I will not participate.  (4) 
# I have already voted by post.  (99) 
# I am not entitled to vote   (100) 

# Prepare Wave 1 data with three categories: No, Not decided yet, Yes
wave1_participation <- all_match_correct %>%
  filter(!is.na(take_part)) %>%  # Remove NA values
  count(take_part) %>%  # Count occurrences, creates 'n'
  rename(Type = take_part, Count = n) %>%  # Rename columns
  mutate(Percent = Count / sum(Count) * 100)  # Calculate percentages

# Convert 'Type' to a factor with labels for readability
wave1_participation$Type <- factor(wave1_participation$Type, levels = c(1, 2, 3, 4, 99, 100), labels = c("I will definitely participate.", "I'm not quite sure yet, but I'll probably take part", "I usually vote, but probably not this time", "I will not participate", "I have already voted by post", "I am not entitled to vote"))

# Create the plot for Wave 1 participation
ggplot(wave1_participation, aes(x = Type, y = Percent, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = NULL, y = "Percentage (%)", title = "Wave 1 Participation Intention") +
  scale_fill_manual(values = c("#1f77b4", "#ff7f0e", "#999999", "#ff9896", "#bcbd22", "#c5b0d5")) +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),  # Remove x-axis title
    axis.title.y = element_text(size = 14),
    axis.text.x = element_blank(),  # Remove x-axis labels
    axis.text.y = element_text(size = 12),
    plot.title = element_text(size = 16, face = "bold"),
    legend.position = "right",  # Show the legend on the right side
    legend.title = element_blank(),  # Remove legend title
    legend.text = element_text(size = 12)
  )


#Voting Behaviour Wave 1 Wave 2

# Prepare Wave 1 data with three categories: No, Not decided yet, Yes
wave1_vote <- all_match_correct_vote %>%
  filter(!is.na(merged_vote)) %>%  # Remove NA values
  count(merged_vote) %>%  # Count occurrences, creates 'n'
  rename(Type = merged_vote, Count = n) %>%  # Rename columns
  mutate(Percent = Count / sum(Count) * 100)  # Calculate percentages

# Convert 'Type' to a factor with labels for readability
wave1_vote$Type <- factor(wave1_vote$Type, levels = c(0, 1, 2), labels = c("No", "Not decided yet", "Yes"))

# Create the plot for Wave 1
ggplot(wave1_vote, aes(x = Type, y = Percent, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Vote Intention", y = "Percentage (%)", title = "Wave 1 Voting Intention") +
  scale_fill_manual(values = c("No" = "#1f77b4", "Not decided yet" = "#999999", "Yes" = "#ff7f0e")) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    plot.title = element_text(size = 16, face = "bold"),
    legend.position = "none"
  )



## Plots with all participants (circle_macht = 0 and 1) -----

#age
wave1_age_all <- data %>%
  mutate(age_bin = cut(age, breaks = seq(18, 90, by = 5), right = FALSE, include.lowest = TRUE)) %>%
  group_by(age_bin) %>%
  summarise(count = n()) %>%
  mutate(percent = count / sum(count) * 100)

wave2_age_all<- data[data$wave2 == 1, ] %>%
  mutate(age_bin = cut(age, breaks = seq(18, 90, by = 5), right = FALSE, include.lowest = TRUE)) %>%
  group_by(age_bin) %>%
  summarise(count = n()) %>%
  mutate(percent = count / sum(count) * 100)

#Import population statistics for age
filename <- "Age_CH.csv"
age_ch <- read.csv(filename, header = TRUE, sep = ";")

# Convert Percent to numeric
age_ch$Percent <- as.numeric(gsub(",", ".", age_ch$Percent))

# Create age bins for Swiss data
age_ch_bins <- age_ch %>%
  mutate(age_bin = cut(ALTER, breaks = seq(18, 90, by = 5), right = FALSE, include.lowest = TRUE)) %>%
  group_by(age_bin) %>%
  summarise(percent = sum(Percent))

#summary(data$age)

# Add a source column to distinguish between datasets
wave1_age_all <- wave1_age_all %>% mutate(source = "Wave1")
wave2_age_all <- wave2_age_all %>% mutate(source = "Wave2")
age_ch_bins <- age_ch_bins %>% mutate(source = "Population")

# Combine datasets
combined_data <- bind_rows(wave1_age_all, wave2_age_all, age_ch_bins)

# Plot the data
ggplot(combined_data, aes(x = age_bin, y = percent, fill = source)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Age", y = "Percent", title = "Age Distribution Comparison (All)") +
  scale_fill_manual(values = c("Wave1" = "#1f77b4", "Wave2" = "#ff7f0e", "Population" = "#999999")) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 12),
    plot.title = element_text(size = 16, face = "bold"),
    legend.title = element_blank(),
    legend.text = element_text(size = 12)
  )


#Gender

#prepare sample data wave 1
wave1_gender_all<- data %>%
  filter(!is.na(gender)) %>%  # Remove NA values
  count(gender) %>%  # Count occurrences, creates 'n'
  filter(!gender %in% c("3", "99")) %>%
  mutate(Type = recode(gender, `1` = "male", `2` = "female")) %>%
  select(Type, n) %>%  # 'n' is the default name from count()
  rename(Count = n) %>%
  mutate(Percent = Count / sum(Count) * 100)  # Calculate percentages

# wave 2
wave2_gender_all<- data[data$wave2 == 1, ] %>%
  filter(!is.na(gender)) %>%  # Remove NA values
  count(gender) %>%  # Count occurrences, creates 'n'
  filter(!gender %in% c("3", "99")) %>%
  mutate(Type = recode(gender, `1` = "male", `2` = "female")) %>%
  select(Type, n) %>%  # 'n' is the default name from count()
  rename(Count = n) %>%
  mutate(Percent = Count / sum(Count) * 100)  # Calculate percentages

#Import Gender Population Statistics
filename <- "gender_ch.csv"
gender_ch <- read.csv(filename, header = TRUE, sep = ";")


gender_ch <- gender_ch %>%
  mutate(Percent = gsub(",", ".", Percent)) %>%  # Replace commas with dots
  mutate(Percent = as.numeric(Percent))  # Convert to numeric


gender_ch <- gender_ch %>%
  rename(
    Type = GESCHLECHT,
    Count = Frequency
  )



wave1_gender_all <- wave1_gender_all %>% mutate(source = "Wave1")
wave2_gender_all <- wave2_gender_all %>% mutate(source = "Wave2")
gender_ch <- gender_ch %>% mutate(source = "Population")

# Ensure Percent is numeric in both data frames
wave1_gender_all <- wave1_gender_all %>%
  mutate(Percent = as.numeric(Percent))

wave2_gender_all <- wave2_gender_all %>%
  mutate(Percent = as.numeric(Percent))

combined_data_gender <- bind_rows(wave1_gender_all, wave2_gender_all, gender_ch)

#create plot
ggplot(combined_data_gender, aes(x = Type, y = Percent, fill = source)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Gender", y = "Percentage (%)", title = "Gender Distribution Comparison (ALL)") +
  scale_fill_manual(values = c("Wave1" = "#1f77b4", "Wave2" = "#ff7f0e", "Population" = "#999999")) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    plot.title = element_text(size = 16, face = "bold"),
    legend.title = element_blank(),
    legend.text = element_text(size = 12)
  ) +
  coord_cartesian(ylim = c(45, 52))  # Set the y-axis limits from 40% to 100%


## Urban Rural

wave1_urban_rural_all <- data %>%
  filter(!is.na(urban_rural)) %>%  # Remove NA values
  count(urban_rural) %>%  # Count occurrences, creates 'n'
  mutate(Type = recode(urban_rural, `1` = "urban", `2` = "suburban", `3` = "rural")) %>%
  select(Type, n) %>%  # 'n' is the default name from count()
  rename(Count = n) %>%
  mutate(Percent = Count / sum(Count) * 100)  # Calculate percentages

wave2_urban_rural_all <- data[data$wave2 == 1, ] %>%
  filter(!is.na(urban_rural)) %>%  # Remove NA values
  count(urban_rural) %>%  # Count occurrences, creates 'n'
  mutate(Type = recode(urban_rural, `1` = "urban", `2` = "suburban", `3` = "rural")) %>%
  select(Type, n) %>%  # 'n' is the default name from count()
  rename(Count = n) %>%
  mutate(Percent = Count / sum(Count) * 100)  # Calculate percentages

#Import population statistics for urban rural
filename <- "urban_rural_ch.csv"
urban_rural_ch <- read.csv(filename, header = TRUE, sep = ";")

urban_rural_ch <- urban_rural_ch %>% mutate(source = "Population")

urban_rural_ch <- urban_rural_ch %>%
  mutate(Percent = gsub(",", ".", Percent)) %>%  # Replace commas with dots
  mutate(Percent = as.numeric(Percent))  # Convert to numeric

urban_rural_ch <- urban_rural_ch %>%
  rename(
    Type = urbrur,
    Count = Frequency
  ) %>%
  select(Type, Count, Percent, source)  # Keep only desired columns


wave1_urban_rural_all <- wave1_urban_rural_all %>% mutate(source = "Wave1")
wave2_urban_rural_all <- wave2_urban_rural_all %>% mutate(source = "Wave2")

# Ensure Percent is numeric in both data frames
wave1_urban_rural_all <- wave1_urban_rural_all %>%
  mutate(Percent = as.numeric(Percent))

wave2_urban_rural_all <- wave2_urban_rural_all %>%
  mutate(Percent = as.numeric(Percent))


combined_data_urban_rural <- bind_rows(wave1_urban_rural_all, wave2_urban_rural_all, urban_rural_ch)

ggplot(combined_data_urban_rural, aes(x = Type, y = Percent, fill = source)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Category", y = "Percentage (%)", title = "Degree of Urbanization (ALL)") +
  scale_fill_manual(values = c("Wave1" = "#1f77b4", "Wave2" = "#ff7f0e", "Population" = "#999999")) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    plot.title = element_text(size = 16, face = "bold"),
    legend.title = element_blank(),
    legend.text = element_text(size = 12)
  )

### Comparison with and without mistake ----


#age
wave1_age_all <- data %>%
  mutate(age_bin = cut(age, breaks = seq(18, 90, by = 5), right = FALSE, include.lowest = TRUE)) %>%
  group_by(age_bin) %>%
  summarise(count = n()) %>%
  mutate(percent = count / sum(count) * 100)

wave2_age_all<- data[data$wave2 == 1, ] %>%
  mutate(age_bin = cut(age, breaks = seq(18, 90, by = 5), right = FALSE, include.lowest = TRUE)) %>%
  group_by(age_bin) %>%
  summarise(count = n()) %>%
  mutate(percent = count / sum(count) * 100)

wave1_age <- all_match_correct %>%
  mutate(age_bin = cut(age, breaks = seq(18, 90, by = 5), right = FALSE, include.lowest = TRUE)) %>%
  group_by(age_bin) %>%
  summarise(count = n()) %>%
  mutate(percent = count / sum(count) * 100)

wave2_age<- all_match_correct[all_match_correct$wave2 == 1, ] %>%
  mutate(age_bin = cut(age, breaks = seq(18, 90, by = 5), right = FALSE, include.lowest = TRUE)) %>%
  group_by(age_bin) %>%
  summarise(count = n()) %>%
  mutate(percent = count / sum(count) * 100)


#summary(data$age)

# Add a source column to distinguish between datasets
wave1_age_all <- wave1_age_all %>% mutate(source = "Wave1 all")
wave2_age_all <- wave2_age_all %>% mutate(source = "Wave2 all")

wave1_age <- wave1_age %>% mutate(source = "Wave1")
wave2_age <- wave2_age %>% mutate(source = "Wave2")

# Combine datasets
combined_data <- bind_rows(wave1_age_all, wave2_age_all, wave1_age, wave2_age)

combined_data$source <- factor(combined_data$source, levels = c("Wave1", "Wave2", "Wave1 all", "Wave2 all"))

# Plot the data
ggplot(combined_data, aes(x = age_bin, y = percent, fill = source)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Age", y = "Percent", title = "Age Distribution Comparison (All)") +
  scale_fill_manual(values = c("Wave1" = "#1f77b4", "Wave2" = "#6baed6", "Wave1 all" = "#ff7f6e", "Wave2 all" = "#d95f0e")) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 12),
    plot.title = element_text(size = 16, face = "bold"),
    legend.title = element_blank(),
    legend.text = element_text(size = 12)
  )

#Gender

#prepare sample data wave 1
wave1_gender_all<- data %>%
  filter(!is.na(gender)) %>%  # Remove NA values
  count(gender) %>%  # Count occurrences, creates 'n'
  filter(!gender %in% c("3", "99")) %>%
  mutate(Type = recode(gender, `1` = "male", `2` = "female")) %>%
  select(Type, n) %>%  # 'n' is the default name from count()
  rename(Count = n) %>%
  mutate(Percent = Count / sum(Count) * 100)  # Calculate percentages

# wave 2
wave2_gender_all<- data[data$wave2 == 1, ] %>%
  filter(!is.na(gender)) %>%  # Remove NA values
  count(gender) %>%  # Count occurrences, creates 'n'
  filter(!gender %in% c("3", "99")) %>%
  mutate(Type = recode(gender, `1` = "male", `2` = "female")) %>%
  select(Type, n) %>%  # 'n' is the default name from count()
  rename(Count = n) %>%
  mutate(Percent = Count / sum(Count) * 100)  # Calculate percentages

#prepare sample data wave 1
wave1_gender<- all_match_correct %>%
  filter(!is.na(gender)) %>%  # Remove NA values
  count(gender) %>%  # Count occurrences, creates 'n'
  filter(!gender %in% c("3", "99")) %>%
  mutate(Type = recode(gender, `1` = "male", `2` = "female")) %>%
  select(Type, n) %>%  # 'n' is the default name from count()
  rename(Count = n) %>%
  mutate(Percent = Count / sum(Count) * 100)  # Calculate percentages

# wave 2
wave2_gender<- all_match_correct[all_match_correct$wave2 == 1, ] %>%
  filter(!is.na(gender)) %>%  # Remove NA values
  count(gender) %>%  # Count occurrences, creates 'n'
  filter(!gender %in% c("3", "99")) %>%
  mutate(Type = recode(gender, `1` = "male", `2` = "female")) %>%
  select(Type, n) %>%  # 'n' is the default name from count()
  rename(Count = n) %>%
  mutate(Percent = Count / sum(Count) * 100)  # Calculate percentages



wave1_gender <- wave1_gender %>% mutate(source = "Wave1")
wave2_gender <- wave2_gender %>% mutate(source = "Wave2")
wave1_gender_all <- wave1_gender_all %>% mutate(source = "Wave1 all")
wave2_gender_all <- wave2_gender_all %>% mutate(source = "Wave2 all")

# Ensure Percent is numeric in both data frames
wave1_gender <- wave1_gender %>%
  mutate(Percent = as.numeric(Percent))

wave2_gender <- wave2_gender %>%
  mutate(Percent = as.numeric(Percent))

wave1_gender_all <- wave1_gender_all %>%
  mutate(Percent = as.numeric(Percent))

wave2_gender_all <- wave2_gender_all %>%
  mutate(Percent = as.numeric(Percent))

combined_data_gender <- bind_rows(wave1_gender, wave2_gender, wave1_gender_all, wave2_gender_all)

combined_data_gender$source <- factor(combined_data_gender$source, levels = c("Wave1", "Wave2", "Wave1 all", "Wave2 all"))

#create plot
ggplot(combined_data_gender, aes(x = Type, y = Percent, fill = source)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Gender", y = "Percentage (%)", title = "Gender Distribution Comparison All") +
  scale_fill_manual(values = c("Wave1" = "#1f77b4", "Wave2" = "#6baed6", "Wave1 all" = "#ff7f6e", "Wave2 all" = "#d95f0e")) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    plot.title = element_text(size = 16, face = "bold"),
    legend.title = element_blank(),
    legend.text = element_text(size = 12)
  ) +
  coord_cartesian(ylim = c(45, 52))  # Set the y-axis limits from 40% to 100%


## Urban Rural

wave1_urban_rural_all <- data %>%
  filter(!is.na(urban_rural)) %>%  # Remove NA values
  count(urban_rural) %>%  # Count occurrences, creates 'n'
  mutate(Type = recode(urban_rural, `1` = "urban", `2` = "suburban", `3` = "rural")) %>%
  select(Type, n) %>%  # 'n' is the default name from count()
  rename(Count = n) %>%
  mutate(Percent = Count / sum(Count) * 100)  # Calculate percentages

wave2_urban_rural_all <- data[data$wave2 == 1, ] %>%
  filter(!is.na(urban_rural)) %>%  # Remove NA values
  count(urban_rural) %>%  # Count occurrences, creates 'n'
  mutate(Type = recode(urban_rural, `1` = "urban", `2` = "suburban", `3` = "rural")) %>%
  select(Type, n) %>%  # 'n' is the default name from count()
  rename(Count = n) %>%
  mutate(Percent = Count / sum(Count) * 100)  # Calculate percentages

wave1_urban_rural <- all_match_correct %>%
  filter(!is.na(urban_rural)) %>%  # Remove NA values
  count(urban_rural) %>%  # Count occurrences, creates 'n'
  mutate(Type = recode(urban_rural, `1` = "urban", `2` = "suburban", `3` = "rural")) %>%
  select(Type, n) %>%  # 'n' is the default name from count()
  rename(Count = n) %>%
  mutate(Percent = Count / sum(Count) * 100)  # Calculate percentages

wave2_urban_rural <- all_match_correct[all_match_correct$wave2 == 1, ] %>%
  filter(!is.na(urban_rural)) %>%  # Remove NA values
  count(urban_rural) %>%  # Count occurrences, creates 'n'
  mutate(Type = recode(urban_rural, `1` = "urban", `2` = "suburban", `3` = "rural")) %>%
  select(Type, n) %>%  # 'n' is the default name from count()
  rename(Count = n) %>%
  mutate(Percent = Count / sum(Count) * 100)  # Calculate percentages


wave1_urban_rural_all <- wave1_urban_rural_all %>% mutate(source = "Wave1 all")
wave2_urban_rural_all <- wave2_urban_rural_all %>% mutate(source = "Wave2 all")
wave1_urban_rural <- wave1_urban_rural %>% mutate(source = "Wave1")
wave2_urban_rural <- wave2_urban_rural %>% mutate(source = "Wave2")

# Ensure Percent is numeric in both data frames
wave1_urban_rural_all <- wave1_urban_rural_all %>%
  mutate(Percent = as.numeric(Percent))

wave2_urban_rural_all <- wave2_urban_rural_all %>%
  mutate(Percent = as.numeric(Percent))


combined_data_urban_rural <- bind_rows(wave1_urban_rural_all, wave2_urban_rural_all, wave1_urban_rural, wave2_urban_rural)

combined_data_urban_rural$source <- factor(combined_data_urban_rural$source, levels = c("Wave1", "Wave2", "Wave1 all", "Wave2 all"))

ggplot(combined_data_urban_rural, aes(x = Type, y = Percent, fill = source)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Category", y = "Percentage (%)", title = "Degree of Urbanization (All)") +
  scale_fill_manual(values = c("Wave1" = "#1f77b4", "Wave2" = "#6baed6", "Wave1 all" = "#ff7f6e", "Wave2 all" = "#d95f0e")) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    plot.title = element_text(size = 16, face = "bold"),
    legend.title = element_blank(),
    legend.text = element_text(size = 12)
  )




## Distribution of Distance Preference -----



# Calculate cumulative percentage for all correctly matched respondents in control and treatment group
data_cumulative_all <- all_match_correct %>%
  group_by(distance_accept) %>%
  summarise(count = n()) %>%
  filter(!is.infinite(distance_accept) & !is.na(distance_accept)) %>%
  mutate(cum_percent = cumsum(count) / sum(count) * 100)

ggplot(data_cumulative_all, aes(x = distance_accept, y = cum_percent)) +
  geom_step() +
  labs(x = "Distance (m)", y = "Cumulative Percent (%)", title = "Cumulative Distribution of Distance Preference") +
  scale_x_continuous(breaks = seq(0, max(data_cumulative$distance_accept), by = 1000)) + 
  theme_classic()

# Calculate cumulative percentage for all correctly matched respondents in treatment group
data_cumulative_treat <- all_treat_correct_vote %>%
  group_by(distance_accept) %>%
  summarise(count = n()) %>%
  filter(!is.infinite(distance_accept) & !is.na(distance_accept)) %>%
  mutate(cum_percent = cumsum(count) / sum(count) * 100)

ggplot(data_cumulative_treat, aes(x = distance_accept, y = cum_percent)) +
  geom_step() +
  labs(x = "Distance (m)", y = "Cumulative Percent (%)", title = "Cumulative Distribution of Distance Preference") +
  scale_x_continuous(breaks = seq(0, max(data_cumulative$distance_accept), by = 1000)) + 
  theme_classic()

# Calculate cumulative percentage for all correctly matched respondents in control group
data_cumulative_control <- all_control_match_correct_vote %>%
  group_by(distance_accept) %>%
  summarise(count = n()) %>%
  filter(!is.infinite(distance_accept) & !is.na(distance_accept)) %>%
  mutate(cum_percent = cumsum(count) / sum(count) * 100)

ggplot(data_cumulative_control, aes(x = distance_accept, y = cum_percent)) +
  geom_step() +
  labs(x = "Distance (m)", y = "Cumulative Percent (%)", title = "Cumulative Distribution of Distance Preference") +
  scale_x_continuous(breaks = seq(0, max(data_cumulative$distance_accept), by = 1000)) + 
  theme_classic()



## One graph displaying both after 

# Calculate cumulative percentage for the treatment group
data_cumulative_treat <- all_treat_correct %>%
  group_by(distance_accept) %>%
  summarise(count = n()) %>%
  filter(!is.infinite(distance_accept) & !is.na(distance_accept)) %>%
  mutate(cum_percent = cumsum(count) / sum(count) * 100,
         group = "Treatment")

# Calculate cumulative percentage for the control group
data_cumulative_control <- all_control_match_correct%>%
  group_by(distance_accept) %>%
  summarise(count = n()) %>%
  filter(!is.infinite(distance_accept) & !is.na(distance_accept)) %>%
  mutate(cum_percent = cumsum(count) / sum(count) * 100,
         group = "Control")

# Combine the two datasets
data_cumulative <- bind_rows(data_cumulative_treat, data_cumulative_control)

# Plot the combined data
ggplot(data_cumulative, aes(x = distance_accept, y = cum_percent, colour = group, linetype = group)) +
  geom_step() +
  geom_hline(yintercept = seq(0, 100, by = 25), linetype = "dotted", color = "grey") +  # Horizontal lines at every 10% increment
  geom_vline(xintercept = seq(0, max(data_cumulative$distance_accept), by = 1000), linetype = "dotted", color = "grey") +  # Vertical lines at every 1000 meters
  labs(x = "Distance (m)", y = "Cumulative Percent (%)", title = "Distance Preference") +
  scale_x_continuous(breaks = seq(0, max(data_cumulative$distance_accept), by = 1000)) + 
  scale_color_manual(values = c("Treatment" = "blue", "Control" = "red")) +  # Set the colors for each group
  scale_linetype_manual(values = c("Treatment" = "solid", "Control" = "dashed")) +  # Set the linetypes for each group
  theme_classic() +
  theme(
    legend.title = element_blank(),
    legend.text = element_text(size = 14),          # Increase size of legend text
    axis.title.x = element_text(size = 16),         # Increase size of x-axis title
    axis.title.y = element_text(size = 16),         # Increase size of y-axis title
    axis.text.x = element_text(size = 14),          # Increase size of x-axis text
    axis.text.y = element_text(size = 14),          # Increase size of y-axis text
    plot.title = element_text(size = 16, face = "bold")
  )

wave1_urban_rural_all <- data %>%
  filter(!is.na(urban_rural)) %>%  # Remove NA values
  count(urban_rural) %>%  # Count occurrences, creates 'n'
  mutate(Type = recode(urban_rural, `1` = "urban", `2` = "suburban", `3` = "rural")) %>%
  select(Type, n) %>%  # 'n' is the default name from count()
  rename(Count = n) %>%
  mutate(Percent = Count / sum(Count) * 100)  # Calculate percentages

wave2_urban_rural_all <- data[data$wave2 == 1, ] %>%
  filter(!is.na(urban_rural)) %>%  # Remove NA values
  count(urban_rural) %>%  # Count occurrences, creates 'n'
  mutate(Type = recode(urban_rural, `1` = "urban", `2` = "suburban", `3` = "rural")) %>%
  select(Type, n) %>%  # 'n' is the default name from count()
  rename(Count = n) %>%
  mutate(Percent = Count / sum(Count) * 100)  # Calculate percentages

#Import population statistics for urban rural
filename <- "urban_rural_ch.csv"
urban_rural_ch <- read.csv(filename, header = TRUE, sep = ";")

urban_rural_ch <- urban_rural_ch %>% mutate(source = "Population")

urban_rural_ch <- urban_rural_ch %>%
  mutate(Percent = gsub(",", ".", Percent)) %>%  # Replace commas with dots
  mutate(Percent = as.numeric(Percent))  # Convert to numeric

urban_rural_ch <- urban_rural_ch %>%
  rename(
    Type = urbrur,
    Count = Frequency
  ) %>%
  select(Type, Count, Percent, source)  # Keep only desired columns


wave1_urban_rural_all <- wave1_urban_rural_all %>% mutate(source = "Wave1")
wave2_urban_rural_all <- wave2_urban_rural_all %>% mutate(source = "Wave2")

# Ensure Percent is numeric in both data frames
wave1_urban_rural_all <- wave1_urban_rural_all %>%
  mutate(Percent = as.numeric(Percent))

wave2_urban_rural_all <- wave2_urban_rural_all %>%
  mutate(Percent = as.numeric(Percent))


combined_data_urban_rural <- bind_rows(wave1_urban_rural_all, wave2_urban_rural_all, urban_rural_ch)

ggplot(combined_data_urban_rural, aes(x = Type, y = Percent, fill = source)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Category", y = "Percentage (%)", title = "Degree of Urbanization (ALL)") +
  scale_fill_manual(values = c("Wave1" = "#1f77b4", "Wave2" = "#ff7f0e", "Population" = "#999999")) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    plot.title = element_text(size = 16, face = "bold"),
    legend.title = element_blank(),
    legend.text = element_text(size = 12)
  )




### Create Graph for each circle ----

 
# Calculate cumulative percentage for the treatment group
data_cumulative_treat <- all_treat_correct %>%
  filter(circle == 1) %>%
  group_by(distance_accept) %>%
  summarise(count = n()) %>%
  filter(!is.infinite(distance_accept) & !is.na(distance_accept)) %>%
  mutate(cum_percent = cumsum(count) / sum(count) * 100,
         group = "Treatment")

# Calculate cumulative percentage for the control group
data_cumulative_control <- all_control_match_correct%>%
  filter(circle == 1) %>%
  group_by(distance_accept) %>%
  summarise(count = n()) %>%
  filter(!is.infinite(distance_accept) & !is.na(distance_accept)) %>%
  mutate(cum_percent = cumsum(count) / sum(count) * 100,
         group = "Control")

# Combine the two datasets
data_cumulative <- bind_rows(data_cumulative_treat, data_cumulative_control)


# Plot the combined data
ggplot(data_cumulative, aes(x = distance_accept, y = cum_percent, colour = group, linetype = group)) +
  geom_step() +
  geom_hline(yintercept = seq(0, 100, by = 25), linetype = "dotted", color = "grey") +  # Horizontal lines at every 10% increment
  geom_vline(xintercept = seq(0, max(data_cumulative$distance_accept), by = 1000), linetype = "dotted", color = "grey") +  # Vertical lines at every 1000 meters
  labs(x = "Distance (m)", y = "Cumulative Percent (%)", title = "Distance Preference (high Potential in at least one circle)") +
  scale_x_continuous(breaks = seq(0, max(data_cumulative$distance_accept), by = 1000)) + 
  scale_color_manual(values = c("Treatment" = "blue", "Control" = "red")) +  # Set the colors for each group
  scale_linetype_manual(values = c("Treatment" = "solid", "Control" = "dashed")) +  # Set the linetypes for each group
  theme_classic() +
  theme(
    legend.title = element_blank(),
    legend.text = element_text(size = 14),          # Increase size of legend text
    axis.title.x = element_text(size = 16),         # Increase size of x-axis title
    axis.title.y = element_text(size = 16),         # Increase size of y-axis title
    axis.text.x = element_text(size = 14),          # Increase size of x-axis text
    axis.text.y = element_text(size = 14),          # Increase size of y-axis text
    plot.title = element_text(size = 16, face = "bold")
  )


