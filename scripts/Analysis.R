# Replication Code for Descriptive and Regression Analysis "Agrivoltaics can reduce political polarization and local opposition to solar energy" #####################################################
# Authors: Lukas Fesenfeld, Leon Sistek, Simon Montfort, Dionis Anderegg, Jürg Rohrer, Tobias Schmidt
# Date: 30.06.2025

# R.version
# platform       aarch64-apple-darwin20     
# arch           aarch64                    
# os             darwin20                   
# system         aarch64, darwin20          
# status                                    
# major          4                          
# minor          3.2                        
# year           2023                       
# month          10                         
# day            31                         
# svn rev        85441                      
# language       R                          
# version.string R version 4.3.2 (2023-10-31)
# nickname       Eye Holes   


setwd("D:/Studium/Master/Arbeit/Agri-PV/Auswertung/Replication Material")

filename <- "data/data_cleaned.csv"
data <- read.csv(filename, header = TRUE, sep = ",")

# Packages #####################################################
library(dplyr)
library(tidyverse)
library(knitr)
library(sandwich)
library(lmtest)
library(estimatr)
library(modelsummary)
library(jtools)
library(broom)
library(broom.mixed)
library(kableExtra)
library(patchwork)
library(ggpubr)
library(writexl)
library(texreg)
library(stargazer)
library(emmeans)
library(scales)
library(vtable)
stars<-c("." = 0.1,"*" =0.05,"**" =0.01, "***" =0.001)

# Subset data #############################################

#Left leaning respondents
data_left <- data[!is.na(data$left_right) &  data$left_right < 6, ]
#Right leaning respondents
data_right <- data[!is.na(data$left_right) & data$left_right > 5, ]


# Controls ##############################################
## Function for automated regression with controls
model_control <- function(dependent_var, main_independent_var, control_variables, data) {
  lm_robust(as.formula(paste(dependent_var, "~", main_independent_var, "+", paste(control_variables, collapse = " + "))), data = data)
}

controls <- c("circle1", "circle2", "circle3", "age", "gender_f", "environment_score", "feelings_agri_pv", "familiar_agri_pv", "like_energy_agri_pv", "urban_rural_true")
controls_environment <- c("age", "gender_f", "feelings_agri_pv", "familiar_agri_pv", "like_energy_agri_pv", "urban_rural_true")
controls_urban_rural <- c("age", "gender_f", "environment_score", "feelings_agri_pv", "familiar_agri_pv", "like_energy_agri_pv")
controls_familiar <- c("age", "gender_f", "environment_score", "feelings_agri_pv", "like_energy_agri_pv", "urban_rural_true")
controls_like_energy <- c("age", "gender_f", "environment_score", "feelings_agri_pv", "familiar_agri_pv", "urban_rural_true")
controls_gender <- c("age", "environment_score", "feelings_agri_pv", "familiar_agri_pv", "like_energy_agri_pv", "urban_rural_true")
controls_gender_binary <- c("age", "gender_binary_f", "environment_score", "feelings_agri_pv", "familiar_agri_pv", "like_energy_agri_pv", "urban_rural_true")

# Balance Checks ##################################

#age
b_age <- lm_robust(age ~ NIMBY, data = data)

#gender, reference: male
b_gender <- data %>%
  filter(gender_f != "Prefer not to say") %>%
  lm_robust(NIMBY ~ gender_f, data = .)

#environmental score (1-7)
b_environment <- lm_robust(environment_score ~ NIMBY, data = data)

#feelings_agri_pv
b_feelings <- lm_robust(feelings_agri_pv ~ NIMBY, data = data)

#familiar_agri_pv
b_familiar <- lm_robust(familiar_agri_pv ~ NIMBY, data = data)

#like_energy_agri_pv
b_like_energy <- lm_robust(like_energy_agri_pv ~ NIMBY, data = data)

#region, reference: urban
b_urbanization <- lm_robust(urban_rural_true ~ NIMBY, data = data)

#political spectrum (1-10)
b_left_right <- lm_robust(left_right ~ NIMBY, data = data)

#circle 1
b_circle1 <- lm_robust(circle1 ~ NIMBY, data = data)

#circle 2
b_circle2 <- lm_robust(circle2 ~ NIMBY, data = data)

#circle 3
b_circle3 <- lm_robust(circle3 ~ NIMBY, data = data)

## Export as Table ###############################################

### As LateX
models_balance_check <- list(
  b_age,
  b_environment,
  b_feelings,
  b_familiar,
  b_like_energy,
  b_urbanization,
  b_left_right,
  b_circle1,
  b_circle2,
  b_circle3
)
texreg::texreg(lapply(models_balance_check, function(model) texreg::extract(model, include.ci = FALSE)),
               digits = 3, stars = c(0.001, 0.01, 0.05, 0.1),
               custom.model.names = c("B1", "B2", "B3", "B4", "B5", "B6", "B7", "B8", "B9", "B10"),
               fontsize = "small", longtable = T, no.margin = T,
               custom.coef.names = c("Intercept",
                                     "Treatment Group"),
               custom.gof.names =c(NA, NA, "Observations", NA),
               reorder.gof =c(3, 1, 2, 4),
               file = "tables/SI_Balance.tex",
               use.packages = F,
               caption = "B1: Age, B2: Environmental score, B3: Feelings towards Agri-PV,
               B4: Familiarity with Agri-PV, B5: Prior Agri-PV Preference, 
               B6: Degree of urbanization, B7: Political orientation, B8: Potential in circle 1 (0-500m),
               B9: Potential in circle 2 (500-1500m), B10: Potential in circle 3 (1500-4500m)"
)

### As HTML
texreg::htmlreg(lapply(models_balance_check, function(model) texreg::extract(model, include.ci = FALSE)), 
                digits = 3, stars = c(0.001, 0.01, 0.05, 0.1),
                custom.model.names = c("B1", "B2", "B3", "B4", "B5", "B6", "B7", "B8", "B9", "B10"),
                fontsize = "tiny", longtable = T, no.margin = T,
                custom.coef.names = c("Intercept",
                                      "Treatment Group"),
                custom.gof.names =c(NA, NA, "Observations", NA),
                reorder.gof =c(3, 1, 2, 4),
                file = "tables/SI_Balance.html",
                use.packages = F,
                caption = "B1: Age, B2: Environmental score, B3: Feelings towards Agri-PV,
               B4: Familiarity with Agri-PV, B5: Prior Agri-PV Preference, 
               B6: Degree of urbanization, B7: Political orientation, B8: Potential in circle 1 (0-500m),
               B9: Potential in circle 2 (500-1500m), B10: Potential in circle 3 (1500-4500m)"
)

# Regressions #####################################################################

## Comprehension Check ######################################################

### No Interactions ################################
# Knowledge Circle 1
comp_potential_0_500_control_left <- model_control("potential_know_1", "NIMBY", controls, data_left)
comp_potential_0_500_control_right <- model_control("potential_know_1", "NIMBY", controls, data_right)

comp_potential_0_500_circle_split_0_control_left <- model_control("potential_know_1", "NIMBY", controls_gender_binary, data_left [data_left$circle1 ==0,])
comp_potential_0_500_circle_split_0_control_right <- model_control("potential_know_1", "NIMBY", controls_gender_binary, data_right [data_right$circle1 ==0,])

comp_potential_0_500_circle_split_1_control_left <- model_control("potential_know_1", "NIMBY", controls_gender_binary, data_left [data_left$circle1 ==1,])
comp_potential_0_500_circle_split_1_control_right <- model_control("potential_know_1", "NIMBY", controls_gender_binary, data_right [data_right$circle1 ==1,])

#Knowledge Circle 2
comp_potential_500_1500_control_left <- model_control("potential_know_2", "NIMBY", controls, data_left)
comp_potential_500_1500_control_right <- model_control("potential_know_2", "NIMBY", controls, data_right)

comp_potential_500_1500_circle_split_0_control_left <- model_control("potential_know_2", "NIMBY", controls_gender_binary, data_left [data_left$circle2 ==0,])
comp_potential_500_1500_circle_split_0_control_right <- model_control("potential_know_2", "NIMBY", controls_gender_binary, data_right [data_right$circle2 ==0,])

comp_potential_500_1500_circle_split_1_control_left <- model_control("potential_know_2", "NIMBY", controls_gender_binary, data_left [data_left$circle2 ==1,])
comp_potential_500_1500_circle_split_1_control_right <- model_control("potential_know_2", "NIMBY", controls_gender_binary, data_right [data_right$circle2 ==1,])

#Knowledge Circle 3
comp_potential_1500_4500_control_left <- model_control("potential_know_3", "NIMBY", controls, data_left)
comp_potential_1500_4500_control_right <- model_control("potential_know_3", "NIMBY", controls, data_right)

comp_potential_1500_4500_circle_split_0_control_left <- model_control("potential_know_3", "NIMBY", controls_gender_binary, data_left [data_left$circle3 ==0,])
comp_potential_1500_4500_circle_split_0_control_right <- model_control("potential_know_3", "NIMBY", controls_gender_binary, data_right [data_right$circle3 ==0,])

comp_potential_1500_4500_circle_split_1_control_left <- model_control("potential_know_3", "NIMBY", controls_gender_binary, data_left [data_left$circle3 ==1,])
comp_potential_1500_4500_circle_split_1_control_right <- model_control("potential_know_3", "NIMBY", controls_gender_binary, data_right [data_right$circle3 ==1,])




#### SI Table 5 ###################################################

#### Comprehension Check
models_comp <- list(
  comp_potential_0_500_control_left,
  comp_potential_0_500_control_right,
  comp_potential_500_1500_control_left,
  comp_potential_500_1500_control_right,
  comp_potential_1500_4500_control_left,
  comp_potential_1500_4500_control_right
)

#LaTeX
texreg::texreg(lapply(models_comp, function(model) texreg::extract(model, include.ci = FALSE)), 
                digits = 3, stars = c(0.001, 0.01, 0.05, 0.1),
                custom.model.names = c("CC1, Left", "CC1, Right", "CC2, Left", "CC2, Right", "CC3, Left", "CC3, Right"),
                fontsize = "small", longtable = T, no.margin = T,
                custom.coef.names = c("Intercept",
                                      "NIMBY",
                                      #Realisitc AgriPV Potential
                                      "Potential in Circle 1",
                                      "Potential in Circle 2",
                                      "Potential in Circle 3",
                                      #Controls
                                      "Age",
                                      "Gender: Male",
                                      "Gender: Other",
                                      "Gender: Prefer not to say",
                                      "Environmental Score",
                                      "Feelings towards Agri-PV",
                                      "Familiarity with Agri-PV",
                                      "Prior Agri-PV Preference",
                                      "Degree of Urbanizaton"),
                groups = list("Realistic Agri-PV Potential" = 3:5, "Controls" = 6:14),
                custom.gof.names =c(NA, NA, "Observations", NA),
                reorder.gof =c(3, 1, 2, 4),
                file = "tables/SI_Table_5.tex",
                use.packages = F,
                caption = "CC1: Knowledge of potential in circle 1 (0m-500m, binary), CC2: Knowledge of potential in circle 2 (500m-1500m, binary), CC3: Knowledge of potential in circle 3 (1500m-4500m, binary)"
)
#html
texreg::htmlreg(lapply(models_comp, function(model) texreg::extract(model, include.ci = FALSE)), 
               digits = 3, stars = c(0.001, 0.01, 0.05, 0.1),
               custom.model.names = c("CC1, Left", "CC1, Right", "CC2, Left", "CC2, Right", "CC3, Left", "CC3, Right"),
               fontsize = "tiny", longtable = T, no.margin = T,
               custom.coef.names = c("Intercept",
                                     "NIMBY",
                                     #Realisitc AgriPV Potential
                                     "Potential in Circle 1",
                                     "Potential in Circle 2",
                                     "Potential in Circle 3",
                                     #Controls
                                     "Age",
                                     "Gender: Male",
                                     "Gender: Other",
                                     "Gender: Prefer not to say",
                                     "Environmental Score",
                                     "Feelings towards Agri-PV",
                                     "Familiarity with Agri-PV",
                                     "Prior Agri-PV Preference",
                                     "Degree of Urbanizaton"),
               groups = list("Realistic Agri-PV Potential" = 3:5, "Controls" = 6:14),
               custom.gof.names =c(NA, NA, "Observations", NA),
               reorder.gof =c(3, 1, 2, 4),
               file = "tables/SI_Table_5.html",
               use.packages = F,
               caption = "CC1: Knowledge of potential in circle 1 (0m-500m, binary), CC2: Knowledge of potential in circle 2 (500m-1500m, binary), CC3: Knowledge of potential in circle 3 (1500m-4500m, binary)"
)

#### SI Table 6 ########################################
#Circle 1

models_comp_1 <- list(
  comp_potential_0_500_circle_split_0_control_left,
  comp_potential_0_500_circle_split_0_control_right,
  comp_potential_0_500_circle_split_1_control_left,
  comp_potential_0_500_circle_split_1_control_right
)
#Latex
texreg::texreg(lapply(models_comp_1, function(model) texreg::extract(model, include.ci = FALSE)), 
                digits = 3, stars = c(0.001, 0.01, 0.05, 0.1),
                custom.model.names = c("CC1, Left, Low", "CC1, Right, Low", "CC1, Left, High", "CC1, Right, High"),
                fontsize = "small", longtable = T, no.margin = T,
                custom.coef.names = c("Intercept",
                                      "NIMBY",
                                      #Controls
                                      "Age",
                                      "Gender: Male",
                                      "Environmental Score",
                                      "Feelings towards Agri-PV",
                                      "Familiarity with Agri-PV",
                                      "Prior Agri-PV Preference",
                                      "Degree of Urbanizaton"),
                groups = list("Controls" = 3:9),
                custom.gof.names =c(NA, NA, "Observations", NA),
                reorder.gof =c(3, 1, 2, 4),
                file = "tables/SI_Table_6.tex",
                use.packages = F,
                caption = "CC1: Knowledge of potential in circle 1 (0m-500m, binary)"
)
#hmtl
texreg::htmlreg(lapply(models_comp_1, function(model) texreg::extract(model, include.ci = FALSE)), 
                digits = 3, stars = c(0.001, 0.01, 0.05, 0.1),
                custom.model.names = c("CC1, Left, Low", "CC1, Right, Low", "CC1, Left, High", "CC1, Right, High"),
                fontsize = "tiny", longtable = T, no.margin = T,
                custom.coef.names = c("Intercept",
                                      "NIMBY",
                                      #Controls
                                      "Age",
                                      "Gender: Male",
                                      "Environmental Score",
                                      "Feelings towards Agri-PV",
                                      "Familiarity with Agri-PV",
                                      "Prior Agri-PV Preference",
                                      "Degree of Urbanizaton"),
                groups = list("Controls" = 3:9),
                custom.gof.names =c(NA, NA, "Observations", NA),
                reorder.gof =c(3, 1, 2, 4),
                file = "tables/SI_Table_6.html",
                use.packages = F,
                caption = "CC1: Knowledge of potential in circle 1 (0m-500m, binary)"
)

#### SI Table 7##################################
#Circle 2
models_comp_2 <- list(
  comp_potential_500_1500_circle_split_0_control_left,
  comp_potential_500_1500_circle_split_0_control_right,
  comp_potential_500_1500_circle_split_1_control_left,
  comp_potential_500_1500_circle_split_1_control_right
)
#Latex
texreg::texreg(lapply(models_comp_2, function(model) texreg::extract(model, include.ci = FALSE)), 
                digits = 3, stars = c(0.001, 0.01, 0.05, 0.1),
                custom.model.names = c("CC2, Left, Low", "CC2, Right, Low", "CC2, Left, High", "CC2, Right, High"),
                fontsize = "small", longtable = T, no.margin = T,
                custom.coef.names = c("Intercept",
                                       "NIMBY",
                                       #Controls
                                       "Age",
                                       "Gender: Male",
                                       "Environmental Score",
                                       "Feelings towards Agri-PV",
                                       "Familiarity with Agri-PV",
                                       "Prior Agri-PV Preference",
                                       "Degree of Urbanizaton"),
                groups = list("Controls" = 3:9),
                custom.gof.names =c(NA, NA, "Observations", NA),
                reorder.gof =c(3, 1, 2, 4),
                file = "tables/SI_Table_7.tex",
                use.packages = F,
                caption = "CC1: Knowledge of potential in circle 2 (500m-1500m, binary)"
)
#Html
texreg::htmlreg(lapply(models_comp_2, function(model) texreg::extract(model, include.ci = FALSE)), 
                digits = 3, stars = c(0.001, 0.01, 0.05, 0.1),
                custom.model.names = c("CC2, Left, Low", "CC2, Right, Low", "CC2, Left, High", "CC2, Right, High"),
                fontsize = "tiny", longtable = T, no.margin = T,
                custom.coef.names = c("Intercept",
                                      "NIMBY",
                                      #Controls
                                      "Age",
                                      "Gender: Male",
                                      "Environmental Score",
                                      "Feelings towards Agri-PV",
                                      "Familiarity with Agri-PV",
                                      "Prior Agri-PV Preference",
                                      "Degree of Urbanizaton"),
                groups = list("Controls" = 3:9),
                custom.gof.names =c(NA, NA, "Observations", NA),
                reorder.gof =c(3, 1, 2, 4),
                file = "tables/SI_Table_7.html",
                use.packages = F,
                caption = "CC1: Knowledge of potential in circle 2 (500m-1500m, binary)"
)

#### SI Table 8 #####################################
#Circle 3
models_comp_3 <- list(
  comp_potential_1500_4500_circle_split_0_control_left,
  comp_potential_1500_4500_circle_split_0_control_right,
  comp_potential_1500_4500_circle_split_1_control_left,
  comp_potential_1500_4500_circle_split_1_control_right
)
#Latex
texreg::texreg(lapply(models_comp_3, function(model) texreg::extract(model, include.ci = FALSE)), 
                digits = 3, stars = c(0.001, 0.01, 0.05, 0.1),
                custom.model.names = c("CC3, Left, Low", "CC3, Right, Low", "CC3, Left, High", "CC3, Right, High"),
                fontsize = "small", longtable = T, no.margin = T,
                custom.coef.names = c("Intercept",
                                      "NIMBY",
                                      #Controls
                                      "Age",
                                      "Gender: Male",
                                      "Environmental Score",
                                      "Feelings towards Agri-PV",
                                      "Familiarity with Agri-PV",
                                      "Prior Agri-PV Preference",
                                      "Degree of Urbanizaton"),
                groups = list("Controls" = 3:9),
                custom.gof.names =c(NA, NA, "Observations", NA),
                reorder.gof =c(3, 1, 2, 4),
                file = "tables/SI_Table_8.tex",
                use.packages = F,
                caption = "CC1: Knowledge of potential in circle 3 (1500m-4500m, binary)"
)

#Html
texreg::htmlreg(lapply(models_comp_3, function(model) texreg::extract(model, include.ci = FALSE)), 
                digits = 3, stars = c(0.001, 0.01, 0.05, 0.1),
                custom.model.names = c("CC3, Left, Low", "CC3, Right, Low", "CC3, Left, High", "CC3, Right, High"),
                fontsize = "tiny", longtable = T, no.margin = T,
                custom.coef.names = c("Intercept",
                                      "NIMBY",
                                      #Controls
                                      "Age",
                                      "Gender: Male",
                                      "Environmental Score",
                                      "Feelings towards Agri-PV",
                                      "Familiarity with Agri-PV",
                                      "Prior Agri-PV Preference",
                                      "Degree of Urbanizaton"),
                groups = list("Controls" = 3:9),
                custom.gof.names =c(NA, NA, "Observations", NA),
                reorder.gof =c(3, 1, 2, 4),
                file = "tables/SI_Table_8.html",
                use.packages = F,
                caption = "CC1: Knowledge of potential in circle 3 (1500m-4500m, binary)"
)

### Interaction with Circles ################################
dep_potential_know_1_control_int_circles_left <- model_control("potential_know_1", "NIMBY * circle1 + NIMBY * circle2 + NIMBY * circle3", controls, data_left)
dep_potential_know_1_control_int_circles_right <- model_control("potential_know_1", "NIMBY * circle1 + NIMBY * circle2 + NIMBY * circle3", controls, data_right)

dep_potential_know_2_control_int_circles_left <- model_control("potential_know_2", "NIMBY * circle1 + NIMBY * circle2 + NIMBY * circle3", controls, data_left)
dep_potential_know_2_control_int_circles_right <- model_control("potential_know_2", "NIMBY * circle1 + NIMBY * circle2 + NIMBY * circle3", controls, data_right)

dep_potential_know_3_control_int_circles_left <- model_control("potential_know_3", "NIMBY * circle1 + NIMBY * circle2 + NIMBY * circle3", controls, data_left)
dep_potential_know_3_control_int_circles_right <- model_control("potential_know_3", "NIMBY * circle1 + NIMBY * circle2 + NIMBY * circle3", controls, data_right)


#### SI Table 14 ######################################
models_comp_int <- list(
  dep_potential_know_1_control_int_circles_left ,
  dep_potential_know_1_control_int_circles_right ,
  dep_potential_know_2_control_int_circles_left ,
  dep_potential_know_2_control_int_circles_right ,
  dep_potential_know_3_control_int_circles_left ,
  dep_potential_know_3_control_int_circles_right 
)

#Latex
texreg::texreg(lapply(models_comp_int, function(model) texreg::extract(model, include.ci = FALSE)), 
                digits = 3, stars = c(0.001, 0.01, 0.05, 0.1),
                custom.model.names = c("CC1, Left", "CC1, Right", "CC2, Left", "CC2, Right", "CC3, Left", "CC3, Right"),
                fontsize = "small", longtable = T, no.margin = T,
                custom.coef.names = c("Intercept",
                                      "NIMBY",
                                      #Realistic Agri-PV Potential
                                      "Potential in Circle 1",
                                      "Potential in Circle 2",
                                      "Potential in Circle 3",
                                      #Controls
                                      "Age",
                                      "Gender: Male",
                                      "Gender: Other",
                                      "Gender: Prefer not to say",
                                      "Environmental Score",
                                      "Feelings towards Agri-PV",
                                      "Familiarity with Agri-PV",
                                      "Prior Agri-PV Preference",
                                      "Degree of Urbanizaton",
                                      #Interactions
                                      "NIMBY × Pot. in Circle1",
                                      "NIMBY × Pot. in Circle2",
                                      "NIMBY × Pot. in Circle3"),
                groups = list("Realistic Agri-PV Potential" = 3:5, "Controls" = 6:14, "Interactions" = 15:17),
                custom.gof.names =c(NA, NA, "Observations", NA),
                reorder.gof =c(3, 1, 2, 4),
                file = "tables/SI_Table_14.tex",
                use.packages = F,
                caption = "CC1: Knowledge of potential in circle 1 (0m-500m, binary), CC2: Knowledge of potential in circle 2 (500m-1500m, binary), CC3: Knowledge of potential in circle 3 (1500m-4500m, binary)"
)

#Html
texreg::htmlreg(lapply(models_comp_int, function(model) texreg::extract(model, include.ci = FALSE)), 
                digits = 3, stars = c(0.001, 0.01, 0.05, 0.1),
                custom.model.names = c("CC1, Left", "CC1, Right", "CC2, Left", "CC2, Right", "CC3, Left", "CC3, Right"),
                fontsize = "tiny", longtable = T, no.margin = T,
                custom.coef.names = c("Intercept",
                                      "NIMBY",
                                      #Realistic Agri-PV Potential
                                      "Potential in Circle 1",
                                      "Potential in Circle 2",
                                      "Potential in Circle 3",
                                      #Controls
                                      "Age",
                                      "Gender: Male",
                                      "Gender: Other",
                                      "Gender: Prefer not to say",
                                      "Environmental Score",
                                      "Feelings towards Agri-PV",
                                      "Familiarity with Agri-PV",
                                      "Prior Agri-PV Preference",
                                      "Degree of Urbanizaton",
                                      #Interactions
                                      "NIMBY × Pot. in Circle1",
                                      "NIMBY × Pot. in Circle2",
                                      "NIMBY × Pot. in Circle3"),
                groups = list("Realistic Agri-PV Potential" = 3:5, "Controls" = 6:14, "Interactions" = 15:17),
                custom.gof.names =c(NA, NA, "Observations", NA),
                reorder.gof =c(3, 1, 2, 4),
                file = "tables/SI_Table_14.html",
                use.packages = F,
                caption = "CC1: Knowledge of potential in circle 1 (0m-500m, binary), CC2: Knowledge of potential in circle 2 (500m-1500m, binary), CC3: Knowledge of potential in circle 3 (1500m-4500m, binary)"
)

## Manipulation Check #######################################
### No Interactions ######################################

comp_personal_advantage_control_left <- model_control("personal_advantage", "NIMBY", controls, data_left)
comp_personal_advantage_control_right <- model_control("personal_advantage", "NIMBY", controls, data_right)

comp_ch_advantage_control_left <- model_control("ch_advantage", "NIMBY", controls, data_left)
comp_ch_advantage_control_right <- model_control("ch_advantage", "NIMBY", controls, data_right)


#### SI Table 9 ##################################################

models_manip <- list(
  comp_personal_advantage_control_left,
  comp_personal_advantage_control_right,
  comp_ch_advantage_control_left,
  comp_ch_advantage_control_right
)
#Latex
texreg::texreg(lapply(models_manip, function(model) texreg::extract(model, include.ci = FALSE)),
                digits = 3, stars = c(0.001, 0.01, 0.05, 0.1),
                custom.model.names = c("DV1, Left", "DV1, Right", "DV2, Left", "DV2, Right"),
                fontsize = "small", longtable = T, no.margin = T,
                custom.coef.names = c("Intercept",
                                      "NIMBY",
                                      #Realisitc AgriPV Potential
                                      "Potential in Circle 1",
                                      "Potential in Circle 2",
                                      "Potential in Circle 3",
                                      #Controls
                                      "Age",
                                      "Gender: Male",
                                      "Gender: Other",
                                      "Gender: Prefer not to say",
                                      "Environmental Score",
                                      "Feelings towards Agri-PV",
                                      "Familiarity with Agri-PV",
                                      "Prior Agri-PV Preference",
                                      "Degree of Urbanizaton"),
                groups = list("Realistic Agri-PV Potential" = 3:5, "Controls" = 6:14),
                custom.gof.names =c(NA, NA, "Observations", NA),
                reorder.gof =c(3, 1, 2, 4),
                file = "tables/SI_Table_9.tex",
                use.packages = F,
                caption = "DV1: Personal advantage from Agri-PV (1-7 Likert scale), DV2: Advantage for Switzerland (1-7 Likert scale)"
)

# Html
texreg::htmlreg(lapply(models_manip, function(model) texreg::extract(model, include.ci = FALSE)),
                digits = 3, stars = c(0.001, 0.01, 0.05, 0.1),
                custom.model.names = c("DV1, Left", "DV1, Right", "DV2, Left", "DV2, Right"),
                fontsize = "tiny", longtable = T, no.margin = T,
                custom.coef.names = c("Intercept",
                                      "NIMBY",
                                      #Realisitc AgriPV Potential
                                      "Potential in Circle 1",
                                      "Potential in Circle 2",
                                      "Potential in Circle 3",
                                      #Controls
                                      "Age",
                                      "Gender: Male",
                                      "Gender: Other",
                                      "Gender: Prefer not to say",
                                      "Environmental Score",
                                      "Feelings towards Agri-PV",
                                      "Familiarity with Agri-PV",
                                      "Prior Agri-PV Preference",
                                      "Degree of Urbanizaton"),
                groups = list("Realistic Agri-PV Potential" = 3:5, "Controls" = 6:14),
                custom.gof.names =c(NA, NA, "Observations", NA),
                reorder.gof =c(3, 1, 2, 4),
                file = "tables/SI_Table_9.html",
                use.packages = F,
                caption = "DV1: Personal advantage from Agri-PV (1-7 Likert scale), DV2: Advantage for Switzerland (1-7 Likert scale)"
)
### Interaction with Circles #########################

dep_personal_advantage_control_int_circles_left <- model_control("personal_advantage", "NIMBY * circle1 + NIMBY * circle2 + NIMBY * circle3", controls, data_left)
dep_personal_advantage_control_int_circles_right <- model_control("personal_advantage", "NIMBY * circle1 + NIMBY * circle2 + NIMBY * circle3", controls, data_right)

dep_ch_advantage_control_int_circles_left <- model_control("ch_advantage", "NIMBY * circle1 + NIMBY * circle2 + NIMBY * circle3", controls, data_left)
dep_ch_advantage_control_int_circles_right <- model_control("ch_advantage", "NIMBY * circle1 + NIMBY * circle2 + NIMBY * circle3", controls, data_right)

#### SI Table 15 ##################

models_manip_int <- list(
  dep_personal_advantage_control_int_circles_left,
  dep_personal_advantage_control_int_circles_right,
  dep_ch_advantage_control_int_circles_left,
  dep_ch_advantage_control_int_circles_right
)

#Latex
texreg::texreg(lapply(models_manip_int, function(model) texreg::extract(model, include.ci = FALSE)),
                digits = 3, stars = c(0.001, 0.01, 0.05, 0.1),
                custom.model.names = c("DV1, Left", "DV1, Right", "DV2, Left", "DV2, Right"),
                fontsize = "small", longtable = T, no.margin = T,
                custom.coef.names = c("Intercept",
                                      "NIMBY",
                                      #Realistic Agri-PV Potential
                                      "Potential in Circle 1",
                                      "Potential in Circle 2",
                                      "Potential in Circle 3",
                                      #Controls
                                      "Age",
                                      "Gender: Male",
                                      "Gender: Other",
                                      "Gender: Prefer not to say",
                                      "Environmental Score",
                                      "Feelings towards Agri-PV",
                                      "Familiarity with Agri-PV",
                                      "Prior Agri-PV Preference",
                                      "Degree of Urbanizaton",
                                      #Interactions
                                      "NIMBY × Pot. in Circle1",
                                      "NIMBY × Pot. in Circle2",
                                      "NIMBY × Pot. in Circle3"),
                groups = list("Realistic Agri-PV Potential" = 3:5, "Controls" = 6:14, "Interactions" = 15:17),
                custom.gof.names =c(NA, NA, "Observations", NA),
                reorder.gof =c(3, 1, 2, 4),
                file = "tables/SI_Table_15.tex",
                use.packages = F,
                caption = "DV1: Personal advantage from Agri-PV (1-7 Likert scale), DV2: Advantage for Switzerland (1-7 Likert scale)"
)


#Html
texreg::htmlreg(lapply(models_manip_int, function(model) texreg::extract(model, include.ci = FALSE)),
                digits = 3, stars = c(0.001, 0.01, 0.05, 0.1),
                custom.model.names = c("DV1, Left", "DV1, Right", "DV2, Left", "DV2, Right"),
                fontsize = "tiny", longtable = T, no.margin = T,
                custom.coef.names = c("Intercept",
                                      "NIMBY",
                                      #Realistic Agri-PV Potential
                                      "Potential in Circle 1",
                                      "Potential in Circle 2",
                                      "Potential in Circle 3",
                                      #Controls
                                      "Age",
                                      "Gender: Male",
                                      "Gender: Other",
                                      "Gender: Prefer not to say",
                                      "Environmental Score",
                                      "Feelings towards Agri-PV",
                                      "Familiarity with Agri-PV",
                                      "Prior Agri-PV Preference",
                                      "Degree of Urbanizaton",
                                      #Interactions
                                      "NIMBY × Pot. in Circle1",
                                      "NIMBY × Pot. in Circle2",
                                      "NIMBY × Pot. in Circle3"),
                groups = list("Realistic Agri-PV Potential" = 3:5, "Controls" = 6:14, "Interactions" = 15:17),
                custom.gof.names =c(NA, NA, "Observations", NA),
                reorder.gof =c(3, 1, 2, 4),
                file = "tables/SI_Table_15.html",
                use.packages = F,
                caption = "DV1: Personal advantage from Agri-PV (1-7 Likert scale), DV2: Advantage for Switzerland (1-7 Likert scale)"
)


## Attitude Expansion ############################################

### No Interactions #############################################
# attitude_expansion
comp_attitude_expansion_control_left <- model_control("attitude_expansion", "NIMBY", controls, data_left)
comp_attitude_expansion_control_right <- model_control("attitude_expansion", "NIMBY", controls, data_right)

# attitude_expansion_small
comp_attitude_expansion_small_control_left <- model_control("attitude_expansion_small", "NIMBY", controls, data_left)
comp_attitude_expansion_small_control_right <- model_control("attitude_expansion_small", "NIMBY", controls, data_right)

# attitude_expansion_medium
comp_attitude_expansion_medium_control_left <- model_control("attitude_expansion_medium", "NIMBY", controls, data_left)
comp_attitude_expansion_medium_control_right <- model_control("attitude_expansion_medium", "NIMBY", controls, data_right)

# attitude_expansion_large
comp_attitude_expansion_large_control_left <- model_control("attitude_expansion_large", "NIMBY", controls, data_left)
comp_attitude_expansion_large_control_right <- model_control("attitude_expansion_large", "NIMBY", controls, data_right)

# attitude_expansion_nearby
comp_attitude_expansion_nearby_control_left <- model_control("attitude_expansion_nearby", "NIMBY", controls, data_left)
comp_attitude_expansion_nearby_control_right <- model_control("attitude_expansion_nearby", "NIMBY", controls, data_right)


#### SI Table 10 #################################################
models_expansion <- list(
  comp_attitude_expansion_small_control_left,
  comp_attitude_expansion_small_control_right,
  comp_attitude_expansion_medium_control_left,
  comp_attitude_expansion_medium_control_right,
  comp_attitude_expansion_large_control_left,
  comp_attitude_expansion_large_control_right,
  comp_attitude_expansion_control_left,
  comp_attitude_expansion_control_right,
  comp_attitude_expansion_nearby_control_left,
  comp_attitude_expansion_nearby_control_right
  
)

#LaTeX
texreg::texreg(lapply(models_expansion, function(model) texreg::extract(model, include.ci = FALSE)), 
                digits = 3, stars = c(0.001, 0.01, 0.05, 0.1),
                custom.model.names = c("DV3, Left", "DV3, Right", "DV4, Left", "DV4, Right", "DV5, Left", "DV5, Right", "DV6, Left", "DV6, Right", "DV7, Left", "DV7, Right"),
                fontsize = "small", longtable = T, no.margin = T,
                custom.coef.names = c("Intercept",
                                      "NIMBY",
                                      #Realisitc AgriPV Potential
                                      "Potential in Circle 1",
                                      "Potential in Circle 2",
                                      "Potential in Circle 3",
                                      #Controls
                                      "Age",
                                      "Gender: Male",
                                      "Gender: Other",
                                      "Gender: Prefer not to say",
                                      "Environmental Score",
                                      "Feelings towards Agri-PV",
                                      "Familiarity with Agri-PV",
                                      "Prior Agri-PV Preference",
                                      "Degree of Urbanizaton"),
                groups = list("Realistic Agri-PV Potential" = 3:5, "Controls" = 6:14),
                custom.gof.names =c(NA, NA, "Observations", NA),
                reorder.gof =c(3, 1, 2, 4),
                file = "tables/SI_Table_10.tex",
                use.packages = F,
                caption = "DV3: Expansion \\textless{}1ha, DV4: Expansion \\textless{}5ha, DV5: Expansion \\textless{}10ha, DV6: Expansion in Switzerland, DV7: Expanion in neighbourhood"
)

#html
texreg::htmlreg(lapply(models_expansion, function(model) texreg::extract(model, include.ci = FALSE)), 
                digits = 3, stars = c(0.001, 0.01, 0.05, 0.1),
                custom.model.names = c("DV3, Left", "DV3, Right", "DV4, Left", "DV4, Right", "DV5, Left", "DV5, Right", "DV6, Left", "DV6, Right", "DV7, Left", "DV7, Right"),
                fontsize = "tiny", longtable = T, no.margin = T,
                custom.coef.names = c("Intercept",
                                      "NIMBY",
                                      #Realisitc AgriPV Potential
                                      "Potential in Circle 1",
                                      "Potential in Circle 2",
                                      "Potential in Circle 3",
                                      #Controls
                                      "Age",
                                      "Gender: Male",
                                      "Gender: Other",
                                      "Gender: Prefer not to say",
                                      "Environmental Score",
                                      "Feelings towards Agri-PV",
                                      "Familiarity with Agri-PV",
                                      "Prior Agri-PV Preference",
                                      "Degree of Urbanizaton"),
                groups = list("Realistic Agri-PV Potential" = 3:5, "Controls" = 6:14),
                custom.gof.names =c(NA, NA, "Observations", NA),
                reorder.gof =c(3, 1, 2, 4),
                file = "tables/SI_Table_10.html",
                use.packages = F,
                caption = "DV3: Expansion <1ha, DV4: Expansion <5ha, DV5: Expansion <10ha, DV6: Expansion in Switzerland, DV7: Expanion in neighbourhood"
)

### Interaction with Circles ####################################################
dep_attitude_expansion_control_int_circles_left <- model_control("attitude_expansion", "NIMBY * circle1 + NIMBY * circle2 + NIMBY * circle3", controls, data_left)
dep_attitude_expansion_control_int_circles_right <- model_control("attitude_expansion", "NIMBY * circle1 + NIMBY * circle2 + NIMBY * circle3", controls, data_right)

dep_attitude_expansion_small_control_int_circles_left <- model_control("attitude_expansion_small", "NIMBY * circle1 + NIMBY * circle2 + NIMBY * circle3", controls, data_left)
dep_attitude_expansion_small_control_int_circles_right <- model_control("attitude_expansion_small", "NIMBY * circle1 + NIMBY * circle2 + NIMBY * circle3", controls, data_right)

dep_attitude_expansion_medium_control_int_circles_left <- model_control("attitude_expansion_medium", "NIMBY * circle1 + NIMBY * circle2 + NIMBY * circle3", controls, data_left)
dep_attitude_expansion_medium_control_int_circles_right <- model_control("attitude_expansion_medium", "NIMBY * circle1 + NIMBY * circle2 + NIMBY * circle3", controls, data_right)

dep_attitude_expansion_large_control_int_circles_left <- model_control("attitude_expansion_large", "NIMBY * circle1 + NIMBY * circle2 + NIMBY * circle3", controls, data_left)
dep_attitude_expansion_large_control_int_circles_right <- model_control("attitude_expansion_large", "NIMBY * circle1 + NIMBY * circle2 + NIMBY * circle3", controls, data_right)

dep_attitude_expansion_nearby_control_int_circles_left <- model_control("attitude_expansion_nearby", "NIMBY * circle1 + NIMBY * circle2 + NIMBY * circle3", controls, data_left)
dep_attitude_expansion_nearby_control_int_circles_right <- model_control("attitude_expansion_nearby", "NIMBY * circle1 + NIMBY * circle2 + NIMBY * circle3", controls, data_right)

#### SI Table 16 ##########################################

models_expansion_int <- list(
  dep_attitude_expansion_small_control_int_circles_left,
  dep_attitude_expansion_small_control_int_circles_right,
  dep_attitude_expansion_medium_control_int_circles_left,
  dep_attitude_expansion_medium_control_int_circles_right,
  dep_attitude_expansion_large_control_int_circles_left,
  dep_attitude_expansion_large_control_int_circles_right,
  dep_attitude_expansion_control_int_circles_left,
  dep_attitude_expansion_control_int_circles_right,
  dep_attitude_expansion_nearby_control_int_circles_left,
  dep_attitude_expansion_nearby_control_int_circles_right
)

#Latex

texreg::texreg(lapply(models_expansion_int, function(model) texreg::extract(model, include.ci = FALSE)), 
                digits = 3, stars = c(0.001, 0.01, 0.05, 0.1),
                custom.model.names = c("DV3, Left", "DV3, Right", "DV4, Left", "DV4, Right", "DV5, Left", "DV5, Right", "DV6, Left", "DV6, Right", "DV7, Left", "DV7, Right"),
                fontsize = "small", longtable = T, no.margin = T,
                custom.coef.names = c("Intercept",
                                      "NIMBY",
                                      #Realistic Agri-PV Potential
                                      "Potential in Circle 1",
                                      "Potential in Circle 2",
                                      "Potential in Circle 3",
                                      #Controls
                                      "Age",
                                      "Gender: Male",
                                      "Gender: Other",
                                      "Gender: Prefer not to say",
                                      "Environmental Score",
                                      "Feelings towards Agri-PV",
                                      "Familiarity with Agri-PV",
                                      "Prior Agri-PV Preference",
                                      "Degree of Urbanizaton",
                                      #Interactions
                                      "NIMBY × Pot. in Circle1",
                                      "NIMBY × Pot. in Circle2",
                                      "NIMBY × Pot. in Circle3"),
                groups = list("Realistic Agri-PV Potential" = 3:5, "Controls" = 6:14, "Interactions" = 15:17),
                custom.gof.names =c(NA, NA, "Observations", NA),
                reorder.gof =c(3, 1, 2, 4),
                file = "tables/SI_Table_16.tex",
                use.packages = F,
                caption = "DV3: Expansion \\textless{}1ha, DV4: Expansion \\textless{}5ha, DV5: Expansion \\textless{}10ha, DV6: Expansion in Switzerland, DV7: Expanion in neighbourhood"
)

#html
texreg::htmlreg(lapply(models_expansion_int, function(model) texreg::extract(model, include.ci = FALSE)), 
                digits = 3, stars = c(0.001, 0.01, 0.05, 0.1),
                custom.model.names = c("DV3, Left", "DV3, Right", "DV4, Left", "DV4, Right", "DV5, Left", "DV5, Right", "DV6, Left", "DV6, Right", "DV7, Left", "DV7, Right"),
                fontsize = "tiny", longtable = T, no.margin = T,
                custom.coef.names = c("Intercept",
                                      "NIMBY",
                                      #Realistic Agri-PV Potential
                                      "Potential in Circle 1",
                                      "Potential in Circle 2",
                                      "Potential in Circle 3",
                                      #Controls
                                      "Age",
                                      "Gender: Male",
                                      "Gender: Other",
                                      "Gender: Prefer not to say",
                                      "Environmental Score",
                                      "Feelings towards Agri-PV",
                                      "Familiarity with Agri-PV",
                                      "Prior Agri-PV Preference",
                                      "Degree of Urbanizaton",
                                      #Interactions
                                      "NIMBY × Pot. in Circle1",
                                      "NIMBY × Pot. in Circle2",
                                      "NIMBY × Pot. in Circle3"),
                groups = list("Realistic Agri-PV Potential" = 3:5, "Controls" = 6:14, "Interactions" = 15:17),
                custom.gof.names =c(NA, NA, "Observations", NA),
                reorder.gof =c(3, 1, 2, 4),
                file = "tables/SI_Table_16.html",
                use.packages = F,
                caption = "DV3: Expansion <1ha, DV4: Expansion <5ha, DV5: Expansion <10ha, DV6: Expansion in Switzerland, DV7: Expanion in neighbourhood"
)




## Policy Support ##################################################

### No Interactions ###################################
dep_support_policies_control_left <- model_control("support_policies", "NIMBY", controls, data_left)
dep_support_policies_control_right <- model_control("support_policies", "NIMBY", controls, data_right)

# support_policy individually
dep_support_policy_1_control_left <- model_control("support_policy_1", "NIMBY", controls, data_left)
dep_support_policy_1_control_right <- model_control("support_policy_1", "NIMBY", controls, data_right)

dep_support_policy_2_control_left <- model_control("support_policy_2", "NIMBY", controls, data_left)
dep_support_policy_2_control_right <- model_control("support_policy_2", "NIMBY", controls, data_right)

dep_support_policy_4_control_left <- model_control("support_policy_4", "NIMBY", controls, data_left)
dep_support_policy_4_control_right <- model_control("support_policy_4", "NIMBY", controls, data_right)

dep_support_policy_6_control_left <- model_control("support_policy_6", "NIMBY", controls, data_left)
dep_support_policy_6_control_right <- model_control("support_policy_6", "NIMBY", controls, data_right)

#### SI Table 13 #####################################

models_policies <- list(
  dep_support_policies_control_left,
  dep_support_policies_control_right,
  dep_support_policy_1_control_left,
  dep_support_policy_1_control_right,
  dep_support_policy_4_control_left,
  dep_support_policy_4_control_right,
  dep_support_policy_6_control_left,
  dep_support_policy_6_control_right,
  dep_support_policy_2_control_left,
  dep_support_policy_2_control_right
)
#LaTeX
texreg::texreg(lapply(models_policies, function(model) texreg::extract(model, include.ci = FALSE)), 
               digits = 3, stars = c(0.001, 0.01, 0.05, 0.1),
               custom.model.names = c("DV8, Left", "DV8, Right", "DV9, Left", "DV9, Right", "DV10, Left", "DV10, Right", "DV11, Left", "DV11, Right", "DV12, Left", "DV12, Right"),
               fontsize = "small", longtable = T, no.margin = T,
               custom.coef.names = c("Intercept",
                                     "NIMBY",
                                     #Realisitc AgriPV Potential
                                     "Potential in Circle 1",
                                     "Potential in Circle 2",
                                     "Potential in Circle 3",
                                     #Controls
                                     "Age",
                                     "Gender: Male",
                                     "Gender: Other",
                                     "Gender: Prefer not to say",
                                     "Environmental Score",
                                     "Feelings towards Agri-PV",
                                     "Familiarity with Agri-PV",
                                     "Prior Agri-PV Preference",
                                     "Degree of Urbanizaton"),
               groups = list("Realistic Agri-PV Potential" = 3:5, "Controls" = 6:14),
               custom.gof.names =c(NA, NA, "Observations", NA),
               reorder.gof =c(3, 1, 2, 4),
               file = "tables/SI_Table_13.tex",
               use.packages = F,
               caption = "DV8: General policy support , DV9: Simplified approval procedures, DV10: Advisory services for farmers, DV11: Increased one-off payments, DV12: Financial support for large projects"
)

#html
texreg::htmlreg(lapply(models_policies, function(model) texreg::extract(model, include.ci = FALSE)), 
                digits = 3, stars = c(0.001, 0.01, 0.05, 0.1),
                custom.model.names = c("DV8, Left", "DV8, Right", "DV9, Left", "DV9, Right", "DV10, Left", "DV10, Right", "DV11, Left", "DV11, Right", "DV12, Left", "DV12, Right"),
                fontsize = "tiny", longtable = T, no.margin = T,
                custom.coef.names = c("Intercept",
                                      "NIMBY",
                                      #Realisitc AgriPV Potential
                                      "Potential in Circle 1",
                                      "Potential in Circle 2",
                                      "Potential in Circle 3",
                                      #Controls
                                      "Age",
                                      "Gender: Male",
                                      "Gender: Other",
                                      "Gender: Prefer not to say",
                                      "Environmental Score",
                                      "Feelings towards Agri-PV",
                                      "Familiarity with Agri-PV",
                                      "Prior Agri-PV Preference",
                                      "Degree of Urbanizaton"),
                groups = list("Realistic Agri-PV Potential" = 3:5, "Controls" = 6:14),
                custom.gof.names =c(NA, NA, "Observations", NA),
                reorder.gof =c(3, 1, 2, 4),
                file = "tables/SI_Table_13.html",
                use.packages = F,
                caption = "DV8: General policy support , DV9: Simplified approval procedures, DV10: Advisory services for farmers, DV11: Increased one-off payments, DV12: Financial support for large projects"
)

### Interaction with Circles ###################################################
dep_support_policies_control_int_circles_left <- model_control("support_policies", "NIMBY * circle1 + NIMBY * circle2 + NIMBY * circle3", controls, data_left)
dep_support_policies_control_int_circles_right <- model_control("support_policies", "NIMBY * circle1 + NIMBY * circle2 + NIMBY * circle3", controls, data_right)

dep_support_policy_1_control_int_circles_left <- model_control("support_policy_1", "NIMBY * circle1 + NIMBY * circle2 + NIMBY * circle3", controls, data_left)
dep_support_policy_1_control_int_circles_right <- model_control("support_policy_1", "NIMBY * circle1 + NIMBY * circle2 + NIMBY * circle3", controls, data_right)

dep_support_policy_2_control_int_circles_left <- model_control("support_policy_2", "NIMBY * circle1 + NIMBY * circle2 + NIMBY * circle3", controls, data_left)
dep_support_policy_2_control_int_circles_right <- model_control("support_policy_2", "NIMBY * circle1 + NIMBY * circle2 + NIMBY * circle3", controls, data_right)

dep_support_policy_4_control_int_circles_left <- model_control("support_policy_4", "NIMBY * circle1 + NIMBY * circle2 + NIMBY * circle3", controls, data_left)
dep_support_policy_4_control_int_circles_right <- model_control("support_policy_4", "NIMBY * circle1 + NIMBY * circle2 + NIMBY * circle3", controls, data_right)

dep_support_policy_6_control_int_circles_left <- model_control("support_policy_6", "NIMBY * circle1 + NIMBY * circle2 + NIMBY * circle3", controls, data_left)
dep_support_policy_6_control_int_circles_right <- model_control("support_policy_6", "NIMBY * circle1 + NIMBY * circle2 + NIMBY * circle3", controls, data_right)


#### SI Table 17 #################################################

models_policies_int <- list(
  dep_support_policies_control_int_circles_left,
  dep_support_policies_control_int_circles_right,
  dep_support_policy_1_control_int_circles_left,
  dep_support_policy_1_control_int_circles_right,
  dep_support_policy_4_control_int_circles_left,
  dep_support_policy_4_control_int_circles_right,
  dep_support_policy_6_control_int_circles_left,
  dep_support_policy_6_control_int_circles_right,
  dep_support_policy_2_control_int_circles_left,
  dep_support_policy_2_control_int_circles_right
)

#LaTeX
texreg::texreg(lapply(models_policies_int, function(model) texreg::extract(model, include.ci = FALSE)), 
                digits = 3, stars = c(0.001, 0.01, 0.05, 0.1),
                custom.model.names = c("DV8, Left", "DV8, Right", "DV9, Left", "DV9, Right", "DV10, Left", "DV10, Right", "DV11, Left", "DV11, Right", "DV12, Left", "DV12, Right"),
                fontsize = "small", longtable = T, no.margin = T,
                custom.coef.names = c("Intercept",
                                      "NIMBY",
                                      #Realistic Agri-PV Potential
                                      "Potential in Circle 1",
                                      "Potential in Circle 2",
                                      "Potential in Circle 3",
                                      #Controls
                                      "Age",
                                      "Gender: Male",
                                      "Gender: Other",
                                      "Gender: Prefer not to say",
                                      "Environmental Score",
                                      "Feelings towards Agri-PV",
                                      "Familiarity with Agri-PV",
                                      "Prior Agri-PV Preference",
                                      "Degree of Urbanizaton",
                                      #Interactions
                                      "NIMBY × Pot. in Circle1",
                                      "NIMBY × Pot. in Circle2",
                                      "NIMBY × Pot. in Circle3"),
                groups = list("Realistic Agri-PV Potential" = 3:5, "Controls" = 6:14, "Interactions" = 15:17),
               custom.gof.names =c(NA, NA, "Observations", NA),
               reorder.gof =c(3, 1, 2, 4),
                file = "tables/SI_Table_17.tex",
                use.packages = F,
                caption = "DV8: General policy support , DV9: Simplified approval procedures, DV10: Advisory services for farmers, DV11: Increased one-off payments, DV12: Financial support for large projects"
)

#html
texreg::htmlreg(lapply(models_policies_int, function(model) texreg::extract(model, include.ci = FALSE)), 
                digits = 3, stars = c(0.001, 0.01, 0.05, 0.1),
                custom.model.names = c("DV8, Left", "DV8, Right", "DV9, Left", "DV9, Right", "DV10, Left", "DV10, Right", "DV11, Left", "DV11, Right", "DV12, Left", "DV12, Right"),
                fontsize = "tiny", longtable = T, no.margin = T,
                custom.coef.names = c("Intercept",
                                      "NIMBY",
                                      #Realistic Agri-PV Potential
                                      "Potential in Circle 1",
                                      "Potential in Circle 2",
                                      "Potential in Circle 3",
                                      #Controls
                                      "Age",
                                      "Gender: Male",
                                      "Gender: Other",
                                      "Gender: Prefer not to say",
                                      "Environmental Score",
                                      "Feelings towards Agri-PV",
                                      "Familiarity with Agri-PV",
                                      "Prior Agri-PV Preference",
                                      "Degree of Urbanizaton",
                                      #Interactions
                                      "NIMBY × Pot. in Circle1",
                                      "NIMBY × Pot. in Circle2",
                                      "NIMBY × Pot. in Circle3"),
                groups = list("Realistic Agri-PV Potential" = 3:5, "Controls" = 6:14, "Interactions" = 15:17),
                custom.gof.names =c(NA, NA, "Observations", NA),
                reorder.gof =c(3, 1, 2, 4),
                file = "tables/SI_Table_17.html",
                use.packages = F,
                caption = "DV8: General policy support , DV9: Simplified approval procedures, DV10: Advisory services for farmers, DV11: Increased one-off payments, DV12: Financial support for large projects"
)


# Regression Plots #####################################################################

create_error_plot <- function(variable, y_label, y_min = NULL, y_max = NULL, show_legend = FALSE, show_y_label = TRUE) {
  # Keep all data without filtering circle1 == 0
  data_left_plot <- data_left
  data_right_plot <- data_right
  
  # Add column to distinguish the groups
  data_left_plot$Side <- "Left"
  data_right_plot$Side <- "Right"
  
  # Combine both datasets
  combined_data <- rbind(data_left_plot, data_right_plot)
  print(nrow(combined_data))
  
  # Ensure NIMBY is a factor
  combined_data$NIMBY <- as.factor(combined_data$NIMBY)
  
  # Create plot
  p <- ggerrorplot(
    combined_data, 
    x = "NIMBY", 
    y = variable, 
    color = "Side",
    add = "mean",
    desc_stat = "mean_ci",
    error.plot = "pointrange",
    ylab = if (show_y_label) y_label else NULL
  ) + 
    # labs(tag = "a)") +
    guides(color = guide_legend(title = NULL)) +
    theme(
      legend.position = if (show_legend) "right" else "none",
      axis.title.x = element_blank(),
      axis.title.y = if (show_y_label) element_text() else element_blank()
    ) +
    scale_shape_manual(values = c(21, 21)) +
    scale_color_manual(values = c("Left" = "#4059AD", "Right" = "#CCA43B")) +
    scale_x_discrete(labels = c("0" = "Control", "1" = "NIMBY"))
  
  # Apply custom y-scale if both limits are given
  if (!is.null(y_min) && !is.null(y_max)) {
    p <- p + coord_cartesian(ylim = c(y_min, y_max))
  }
  
  return(p)
}




create_emm_plot <- function(data, title, y_label = NULL, y_limits, y_breaks, show_legend = FALSE) {
  p <- data %>%
    mutate(NIMBY = factor(NIMBY, levels = c(0,1), labels = c("Control", "Treatment")),
           left_right = factor(left_right, levels = c("Left", "Right"))) %>%
    ggplot(aes(x = NIMBY, y = emmean, group = left_right, col = left_right)) +
    geom_point(size = 3, position = position_dodge(0.3)) +
    geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.0, linewidth = 0.8, position = position_dodge(0.3)) +
    scale_y_continuous(limits = y_limits, breaks = y_breaks) +
    scale_color_manual(name = "Group", values = vals_legend) +
    scale_fill_manual(name = "Group", values = vals_legend) +
    labs(title = title, x = NULL, y = y_label) +
    theme_plot
  
  if (!show_legend) {
    p <- p + theme(legend.position = "none")
  }
  return(p)
}



# Function to calculate the p-values using a Z-test to check the similarity of proportions
z_test_p_values <- function(data_sum, n_options){
  
  p_values <- numeric(0) #Initialize empty vector
  
  for(i in seq(1, n_options, by = 2)){
    print(i)
    res <- prop.test(x = c(data_sum$count[i], data_sum$count[i+1]),
                     n = c(sum(data_sum$count[data_sum$NIMBY == 0]),
                           sum(data_sum$count[data_sum$NIMBY == 1])), correct = FALSE) 
    print(c(data_sum$count[i], data_sum$count[i+1]))
    print(c(sum(data_sum$count[data_sum$NIMBY == 0]),
            sum(data_sum$count[data_sum$NIMBY == 1])))
    p_values <- c(p_values, res$p.value)
  }
  
  return(p_values)
  
}

#Function to retrieve stars for significance brackets
significance_stars <- function(p) {
  if (p < 0.001) {
    return("***")
  } else if (p < 0.01) {
    return("**")
  } else if (p < 0.05) {
    return("*")
  } else {
    return("")  # Not significant
  }
}

theme_plot <- theme_light() +
  theme(legend.position = "right",
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        axis.text = element_text(size = 12, color = "black"),
        axis.title = element_text(size = 12),
        strip.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        axis.text.x = element_text(angle = 0, hjust = 0.5),
        axis.title.x = element_text(margin = margin(t=6)))

vals_legend <- c("#4059AD", "#CCA43B")

## Manipulation Check #####################################################
### Plot Fig. 3 in Main Manuscript ######################################################
# Fig. 3a displays respondents’ perceived personal advantage of building Agri-PV installations in 
# Switzerland, Fig. 3b shows respondents’ perceived advantage of Agri-PV installations for Switzerland as a whole. 

mean_1 <- create_error_plot("personal_advantage", "Ave. Rating", 4.6, 5.5, FALSE, TRUE) +
  ggtitle("Personal Advantage") +
  theme(
    plot.title = element_text(hjust = 0.5)
  )
mean_2 <- create_error_plot("ch_advantage", "Ave. Rating", 4.6, 5.5, TRUE, FALSE) +
  ggtitle("Advantage for CH") +
  theme(
    plot.title = element_text(hjust = 0.5)
  )

mean_1 <- mean_1 + labs(tag = "a)") + theme(plot.tag = element_text(face = "bold", size = 14))
mean_2 <- mean_2 + labs(tag = "b)") + theme(plot.tag = element_text(face = "bold", size = 14))

final_plot <- (mean_1 + mean_2) + 
  plot_annotation(
    title = "Manipulation Checks by Political Orientation", 
    tag_levels = "a", 
    tag_suffix = ")",
    theme = theme(
      plot.title = element_text(face = "bold", size = 16)
    )
  )
ggsave(filename = "plots/Fig_3.png", plot = final_plot, width = 7, height =3)

### EMM Plot #################################################
# Personal advantage
means_personal_left <- emmeans(comp_personal_advantage_control_left, "NIMBY")
means_personal_right <- emmeans(comp_personal_advantage_control_right, "NIMBY")

# CH Advantage
means_ch_left <- emmeans(comp_ch_advantage_control_left, "NIMBY")
means_ch_right <- emmeans(comp_ch_advantage_control_right, "NIMBY")


means_personal <- bind_rows(
  as.data.frame(means_personal_left) %>% mutate(left_right = "Left"),
  as.data.frame(means_personal_right) %>% mutate(left_right = "Right")
)

means_ch <- bind_rows(
  as.data.frame(means_ch_left) %>% mutate(left_right = "Left"),
  as.data.frame(means_ch_right) %>% mutate(left_right = "Right")
)

plot_personal_advantage <- create_emm_plot(
  data = means_personal,
  title = "Personal Advantage",
  y_label = "Estimated Mean",
  y_limits = c(4, 6),
  y_breaks = seq(4, 6, 0.4)
)

plot_ch_advantage <- create_emm_plot(
  data = means_ch,
  title = "Advantage for CH",
  y_limits = c(4, 6),
  y_breaks = seq(4, 6, 0.4),
  show_legend = TRUE
)

plot_personal_advantage <- plot_personal_advantage + labs(tag = "a)") + theme(plot.tag = element_text(face = "bold", size = 14))
plot_ch_advantage <- plot_ch_advantage + labs(tag = "b)") + theme(plot.tag = element_text(face = "bold", size = 14))


final_plot <- (plot_personal_advantage + plot_ch_advantage) + 
  plot_annotation(
    title = "Partisan and NIMBY Information effects on Agri-PV attitudes", 
    theme = theme(
      plot.title = element_text(face = "bold", size = 16),
      plot.caption = element_text(face = "plain", size = 12, hjust = 0.42, margin = margin(t = 6))
    )
  )
ggsave(filename = "plots/Manipulation_Check_EMM.png", plot = final_plot, width = 8.5, height =4)

## Attitude Expansion ################################################
### Plot Fig. 4 in Main Manuscript ######################################################
# Fig.4a to 4c present respondents’ support for the expansion of Agri-PV installations of different sizes, 
# measured on a 7-point Likert scale. Fig.4d and 4e illustrate respondents’ 
# general support for the expansion of Agri-PV in Switzerland (3d) and in their direct residential vicinity (3e), again 
# measured on a 7-point Likert scale (1 = “strong opposition”, 7 = “strong support”)

mean_1 <- create_error_plot("attitude_expansion", "Ave. Rating", 3.6, 5.6, FALSE, TRUE)  +
  ggtitle("Expansion in CH") +
  theme(
    plot.title = element_text(hjust = 0.5)
  )
mean_2 <- create_error_plot("attitude_expansion_small", "Ave. Rating", 3.6, 5.6, FALSE, TRUE) +
  ggtitle("Expansion <1ha") +
  theme(
    plot.title = element_text(hjust = 0.5)
  )
mean_3 <- create_error_plot("attitude_expansion_medium", "Ave. Rating", 3.6, 5.6, FALSE, FALSE) +
  ggtitle("Expansion <5ha") +
  theme(
    plot.title = element_text(hjust = 0.5)
  )
mean_4 <- create_error_plot("attitude_expansion_large", "Ave. Rating", 3.6, 5.6, FALSE, FALSE) +
  ggtitle("Expansion <10ha") +
  theme(
    plot.title = element_text(hjust = 0.5)
  )
mean_5 <- create_error_plot("attitude_expansion_nearby", "Ave. Rating", 3.6, 5.6, TRUE, FALSE) +
  ggtitle("Expansion in Neighbourhood") +
  theme(
    plot.title = element_text(hjust = 0.5)
  )
empty_plot <- ggplot() + theme_void()

mean_2 <- mean_2 + labs(tag = "a)") + theme(plot.tag = element_text(face = "bold", size = 14))
mean_3 <- mean_3 + labs(tag = "b)") + theme(plot.tag = element_text(face = "bold", size = 14))
mean_4 <- mean_4 + labs(tag = "c)") + theme(plot.tag = element_text(face = "bold", size = 14))
mean_1 <- mean_1 + labs(tag = "d)") + theme(plot.tag = element_text(face = "bold", size = 14))
mean_5 <- mean_5 + labs(tag = "e)") + theme(plot.tag = element_text(face = "bold", size = 14))

design <- "abc \n def"
final_plot <- mean_2 + mean_3 + mean_4 + mean_1 + mean_5 + guide_area() + 
  
  plot_layout(design = design, 
              guides = "collect" ) +
  plot_annotation(
    title = "Attitudes towards Agri-PV Expansion by Political Orientation",
    theme = theme(
      plot.title = element_text(size = 16, face = "bold")
    )
  ) &
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    legend.text = element_text(size=12)
  )


ggsave(filename = "plots/Fig_4.png", plot = final_plot, width = 10, height =6)

### EMM Plot #######################################
# Expansion <1ha
means_expansion_small_left <- emmeans(comp_attitude_expansion_small_control_left, "NIMBY")
means_expansion_small_right <- emmeans(comp_attitude_expansion_small_control_right, "NIMBY")

# Expansion <5ha
means_expansion_medium_left <- emmeans(comp_attitude_expansion_medium_control_left, "NIMBY")
means_expansion_medium_right <- emmeans(comp_attitude_expansion_medium_control_right, "NIMBY")

# Expansion <10ha
means_expansion_large_left <- emmeans(comp_attitude_expansion_large_control_left, "NIMBY")
means_expansion_large_right <- emmeans(comp_attitude_expansion_large_control_right, "NIMBY")

# Expansion in CH
means_expansion_ch_left <- emmeans(comp_attitude_expansion_control_left, "NIMBY")
means_expansion_ch_right <- emmeans(comp_attitude_expansion_control_right, "NIMBY")

#Expansion in Neighbourhood
means_expansion_nearby_left <- emmeans(comp_attitude_expansion_nearby_control_left, "NIMBY")
means_expansion_nearby_right <- emmeans(comp_attitude_expansion_nearby_control_right, "NIMBY")

means_expansion_small <- bind_rows(
  as.data.frame(means_expansion_small_left) %>% mutate(left_right = "Left"),
  as.data.frame(means_expansion_small_right) %>% mutate(left_right = "Right")
)

means_expansion_medium <- bind_rows(
  as.data.frame(means_expansion_medium_left) %>% mutate(left_right = "Left"),
  as.data.frame(means_expansion_medium_right) %>% mutate(left_right = "Right")
)

means_expansion_large <- bind_rows(
  as.data.frame(means_expansion_large_left) %>% mutate(left_right = "Left"),
  as.data.frame(means_expansion_large_right) %>% mutate(left_right = "Right")
)

means_expansion_ch <- bind_rows(
  as.data.frame(means_expansion_ch_left) %>% mutate(left_right = "Left"),
  as.data.frame(means_expansion_ch_right) %>% mutate(left_right = "Right")
)

means_expansion_nearby <- bind_rows(
  as.data.frame(means_expansion_nearby_left) %>% mutate(left_right = "Left"),
  as.data.frame(means_expansion_nearby_right) %>% mutate(left_right = "Right")
)

# Small expansion (<1ha)
attitude_small <- create_emm_plot(
  data = means_expansion_small,
  title = "Expansion <1ha",
  y_label = "Estimated Mean",
  y_limits = c(2.5, 6),
  y_breaks = seq(2.5, 6, 1)
)

# Medium expansion (<5ha)
attitude_medium <- create_emm_plot(
  data = means_expansion_medium,
  title = "Expansion <5ha",
  y_label = NULL,
  y_limits = c(2.5, 6),
  y_breaks = seq(2.5, 6, 1)
)

# Large expansion (<10ha)
attitude_large <- create_emm_plot(
  data = means_expansion_large,
  title = "Expansion <10ha",
  y_label = NULL,
  y_limits = c(2.5, 6),
  y_breaks = seq(2.5, 6, 1)
)

# Expansion in CH
attitude_CH <- create_emm_plot(
  data = means_expansion_ch,
  title = "Expansion in CH",
  y_label = "Estimated Mean",
  y_limits = c(3.5, 6),
  y_breaks = seq(3.5, 6, 0.5)
)

# Expansion in Neighbourhood
attitude_nearby <- create_emm_plot(
  data = means_expansion_nearby,
  title = "Expansion in Neighbourhood",
  y_label = NULL,
  y_limits = c(3.5, 6),
  y_breaks = seq(3.5, 6, 0.5),
  show_legend = TRUE
)

empty_plot <- ggplot() + theme_void()

attitude_small <- attitude_small + labs(tag = "a)") + theme(plot.tag = element_text(face = "bold", size = 14))
attitude_medium <- attitude_medium + labs(tag = "b)") + theme(plot.tag = element_text(face = "bold", size = 14))
attitude_large <- attitude_large + labs(tag = "c)") + theme(plot.tag = element_text(face = "bold", size = 14))
attitude_CH <- attitude_CH + labs(tag = "d)") + theme(plot.tag = element_text(face = "bold", size = 14))
attitude_nearby <- attitude_nearby + labs(tag = "e)") + theme(plot.tag = element_text(face = "bold", size = 14))

design <- "abc \n def"
final_plot <- attitude_small + attitude_medium + attitude_large + attitude_CH + attitude_nearby + guide_area() + 
  
  plot_layout(design = design, 
              guides = "collect" ) +
  plot_annotation(
    title = "",
    theme = theme(
      plot.title = element_text(size = 16, face = "bold")
    )
  ) &
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    legend.text = element_text(size=12)
  )


ggsave(filename = "plots/Attitudes_EMM.png", plot = final_plot, width = 10, height =6)


## Policies ################################
#Plot Fig. 6 in Main Manuscript ######################################################
# Fig. 6a–Fig. 6e show respondents’ support for various policy measures aimed at promoting the expansion 
#of Agri-PV in Switzerland, measured on a 7-point Likert scale ranging from 1 (“strong opposition”) to 7 (“strong support”)

mean_1 <- create_error_plot("support_policies", "Ave. Rating", 4.1, 5.65, FALSE, TRUE)  +
  ggtitle("General Policy Support") +
  theme(
    plot.title = element_text(hjust = 0.5)
  )
mean_2 <- create_error_plot("support_policy_1", "Ave. Rating", 4.1, 5.65, FALSE, FALSE) +
  ggtitle("Simplified Approval Procedures") +
  theme(
    plot.title = element_text(hjust = 0.5)
  )
mean_3 <- create_error_plot("support_policy_4", "Ave. Rating", 4.1, 5.65, FALSE, FALSE) +
  ggtitle("Advisory Services for Farmers") +
  theme(
    plot.title = element_text(hjust = 0.5)
  )
mean_4 <- create_error_plot("support_policy_6", "Ave. Rating", 3.9, 5.2, FALSE, TRUE) +
  ggtitle("Inreased One-Off Payments") +
  theme(
    plot.title = element_text(hjust = 0.5)
  )
mean_5 <- create_error_plot("support_policy_2", "Ave. Rating", 3.9, 5.2, TRUE, FALSE) +
  ggtitle("Financial Support for Large Projects") +
  theme(
    plot.title = element_text(hjust = 0.5)
  )
empty_plot <- ggplot() + theme_void()

mean_1 <- mean_1 + labs(tag = "a)") + theme(plot.tag = element_text(face = "bold", size = 14))
mean_2 <- mean_2 + labs(tag = "b)") + theme(plot.tag = element_text(face = "bold", size = 14))
mean_3 <- mean_3 + labs(tag = "c)") + theme(plot.tag = element_text(face = "bold", size = 14))
mean_4 <- mean_4 + labs(tag = "d)") + theme(plot.tag = element_text(face = "bold", size = 14))
mean_5 <- mean_5 + labs(tag = "e)") + theme(plot.tag = element_text(face = "bold", size = 14))

design <- "abc \n def"
final_plot <- mean_1 + mean_2 + mean_3 + mean_4 + mean_5 + guide_area() + 
  
  plot_layout(design = design, 
              guides = "collect" ) +
  plot_annotation(
    title = "Policy Support for Agri-PV by Political Orientation",
    theme = theme(
      plot.title = element_text(size = 16, face = "bold")
    )
  ) &
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    legend.text = element_text(size=12)
  )


ggsave(filename = "plots/Fig_6.png", plot = final_plot, width = 10, height =6)

### EMM Plot ###############################################
# Get estimated marginal means
means_support_policies_left <- emmeans(dep_support_policies_control_left, "NIMBY")
means_support_policies_right <- emmeans(dep_support_policies_control_right, "NIMBY")

means_support_policy_1_left <- emmeans(dep_support_policy_1_control_left, "NIMBY")
means_support_policy_1_right <- emmeans(dep_support_policy_1_control_right, "NIMBY")

means_support_policy_4_left <- emmeans(dep_support_policy_4_control_left, "NIMBY")
means_support_policy_4_right <- emmeans(dep_support_policy_4_control_right, "NIMBY")

means_support_policy_6_left <- emmeans(dep_support_policy_6_control_left, "NIMBY")
means_support_policy_6_right <- emmeans(dep_support_policy_6_control_right, "NIMBY")

means_support_policy_2_left <- emmeans(dep_support_policy_2_control_left, "NIMBY")
means_support_policy_2_right <- emmeans(dep_support_policy_2_control_right, "NIMBY")

# Combine them with left/right labels
means_support_policies <- bind_rows(
  as.data.frame(means_support_policies_left) %>% mutate(left_right = "Left"),
  as.data.frame(means_support_policies_right) %>% mutate(left_right = "Right")
)

means_support_policy_1 <- bind_rows(
  as.data.frame(means_support_policy_1_left) %>% mutate(left_right = "Left"),
  as.data.frame(means_support_policy_1_right) %>% mutate(left_right = "Right")
)

means_support_policy_4 <- bind_rows(
  as.data.frame(means_support_policy_4_left) %>% mutate(left_right = "Left"),
  as.data.frame(means_support_policy_4_right) %>% mutate(left_right = "Right")
)

means_support_policy_6 <- bind_rows(
  as.data.frame(means_support_policy_6_left) %>% mutate(left_right = "Left"),
  as.data.frame(means_support_policy_6_right) %>% mutate(left_right = "Right")
)

means_support_policy_2 <- bind_rows(
  as.data.frame(means_support_policy_2_left) %>% mutate(left_right = "Left"),
  as.data.frame(means_support_policy_2_right) %>% mutate(left_right = "Right")
)
  
plot_policy <- create_emm_plot(
  data = means_support_policies,
  title = "General Policy Support",
  y_label = "Estimated Mean",
  y_limits = c(3.5, 6),
  y_breaks = seq(3.5, 6, 0.5)
)

plot_policy_1 <- create_emm_plot(
  data = means_support_policy_1,
  title = "Simplified Approval Procedures",
  y_limits = c(3.5, 6),
  y_breaks = seq(3.5, 6, 0.5)
)

plot_policy_4 <- create_emm_plot(
  data = means_support_policy_4,
  title = "Advisory Services for Farmers",
  y_limits = c(3.5, 6),
  y_breaks = seq(3.5, 6, 0.5)
)

plot_policy_6 <- create_emm_plot(
  data = means_support_policy_6,
  title = "Increased One-Off Payments",
  y_label = "Estimated Mean",
  y_limits = c(3.2, 6),
  y_breaks = seq(3.2, 6, 0.6)
)

plot_policy_2 <- create_emm_plot(
  data = means_support_policy_2,
  title = "Financial Support for Large Projects",
  y_limits = c(3.2, 6),
  y_breaks = seq(3.2, 6, 0.6),
  show_legend = TRUE
)
empty_plot <- ggplot() + theme_void()

plot_policy <- plot_policy + labs(tag = "a)") + theme(plot.tag = element_text(face = "bold", size = 14))
plot_policy_1 <- plot_policy_1 + labs(tag = "b)") + theme(plot.tag = element_text(face = "bold", size = 14))
plot_policy_4 <- plot_policy_4 + labs(tag = "c)") + theme(plot.tag = element_text(face = "bold", size = 14))
plot_policy_6 <- plot_policy_6 + labs(tag = "d)") + theme(plot.tag = element_text(face = "bold", size = 14))
plot_policy_2 <- plot_policy_2 + labs(tag = "e)") + theme(plot.tag = element_text(face = "bold", size = 14))

design <- "abc \n def"
final_plot <- plot_policy + plot_policy_1 + plot_policy_4 + plot_policy_6 + plot_policy_2 + guide_area() + 
  
  plot_layout(design = design, 
              guides = "collect" ) +
  plot_annotation(
    title = "",
    theme = theme(
      plot.title = element_text(size = 16, face = "bold")
    )
  ) &
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    legend.text = element_text(size=12)
  )


ggsave(filename = "plots/Policies_EMM.png", plot = final_plot, width = 10, height =6)


# Descriptive Plots ##############################################

## General Discriptives ######################################
### SI Figure 1 ################################
#age
data_age <- data %>%
  mutate(age_bin = cut(age, breaks = seq(18, 90, by = 5), right = FALSE, include.lowest = TRUE)) %>%
  group_by(age_bin) %>%
  dplyr::summarise(count = n()) %>%
  mutate(percent = count / sum(count) * 100)

#summary(data$age)

#Import population statistics for age
filename <- "data/Age_CH.csv"
age_ch <- read.csv(filename, header = TRUE, sep = ";")


# Convert Percent to numeric
age_ch$Percent <- as.numeric(gsub(",", ".", age_ch$Percent))

# Create age bins for Swiss data
age_ch_bins <- age_ch %>%
  mutate(age_bin = cut(ALTER, breaks = seq(18, 90, by = 5), right = FALSE, include.lowest = TRUE)) %>%
  group_by(age_bin) %>%
  dplyr::summarise(percent = sum(Percent))

# Display Swiss data
print(age_ch_bins)

# Add a source column to distinguish between datasets
data_age <- data_age %>% mutate(source = "Sample")
age_ch_bins <- age_ch_bins %>% mutate(source = "Population")

# Combine datasets
combined_data <- bind_rows(data_age, age_ch_bins)

# Plot the data
ggplot(combined_data, aes(x = age_bin, y = percent, fill = source)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Age", y = "Percent", title = "Age Distribution Comparison") +
  scale_fill_manual(values = c("Sample" = "#4059AD", "Population" = "#CCA43B")) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1, color = "black"),
    axis.text.y = element_text(size = 12, color = "black"),
    plot.title = element_text(size = 16, face = "bold"),
    legend.title = element_blank(),
    legend.text = element_text(size = 12),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )

ggsave("plots/SI_Figure_1.png", width = 6, height = 4)

### SI Figure 2 ######################
#Gender

#Import Gender Population Statistics
filename <- "data/gender_ch.csv"
gender_ch <- read.csv(filename, header = TRUE, sep = ";")

table(data$gender)
#prepare sample data
data_gender<- data %>%
  filter(!is.na(gender)) %>%  # Remove NA values
  count(gender) %>% 
  filter(!gender %in% c("3", "99")) %>%
  mutate(Type = recode(gender, `1` = "male", `2` = "female")) %>%
  dplyr::select(Type, n) %>%  # 
  dplyr::rename(Count = n) %>%
  mutate(Percent = Count / sum(Count) * 100)  # Calculate percentages

data_gender <- data_gender %>% mutate(source = "Sample")
gender_ch <- gender_ch %>% mutate(source = "Population")

# Ensure Percent is numeric in both data frames
data_gender <- data_gender %>%
  mutate(Percent = as.numeric(Percent))

gender_ch <- gender_ch %>%
  mutate(Percent = gsub(",", ".", Percent)) %>% 
  mutate(Percent = as.numeric(Percent)) 


gender_ch <- gender_ch %>%
  rename(
    Type = GESCHLECHT,
    Count = Frequency
  )

combined_data_gender <- bind_rows(data_gender, gender_ch)

#create plot
ggplot(combined_data_gender, aes(x = Type, y = Percent, fill = source)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Gender", y = "Percentage (%)", title = "Gender Distribution Comparison") +
  scale_fill_manual(values = c("Sample" = "#4059AD", "Population" = "#CCA43B")) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 12, color = "black"),
    axis.text.y = element_text(size = 12, color = "black"),
    plot.title = element_text(size = 16, face = "bold"),
    legend.title = element_blank(),
    legend.text = element_text(size = 12),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  ) +
  coord_cartesian(ylim = c(45, 52)) 

ggsave("plots/SI_Figure_2.png", width = 5, height = 4)

### SI Figure 3 #######################################
# Language
#Import Gender Population Statistics
filename <- "data/Language_ch.csv"
language_ch <- read.csv(filename, header = TRUE, sep = ";")

table(data$UserLanguage)
#prepare sample data
data_language<- data %>%
  filter(!is.na(UserLanguage)) %>%  # Remove NA values
  count(UserLanguage) %>%  
  filter(!UserLanguage %in% c("EN")) %>%
  mutate(Language = recode(UserLanguage, "DE" = "German", "FR" = "French")) %>%
  dplyr::select(Language, n) %>%
  rename(Frequency = n) %>%
  mutate(Percent = Frequency / sum(Frequency) * 100)  # Calculate percentages


data_language <- data_language %>% mutate(source = "Sample")
language_ch <- language_ch %>% mutate(source = "Population")

# Ensure Percent is numeric in both data frames
data_language <- data_language %>%
  mutate(Percent = as.numeric(Percent))

language_ch <- language_ch %>%
  mutate(Percent = gsub(",", ".", Percent)) %>%  # Replace commas with dots
  mutate(Percent = as.numeric(Percent))  # Convert to numeric


combined_data_language <- bind_rows(data_language, language_ch)

#create plot
ggplot(combined_data_language, aes(x = Language, y = Percent, fill = source)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Language", y = "Percentage (%)", title = "Language Distribution") +
  scale_fill_manual(values = c("Sample" = "#4059AD", "Population" = "#CCA43B")) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 12, color = "black"),
    axis.text.y = element_text(size = 12, color = "black"),
    plot.title = element_text(size = 16, face = "bold"),
    legend.title = element_blank(),
    legend.text = element_text(size = 12),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  ) +
  coord_cartesian(ylim = c(0, 85))

ggsave("plots/SI_Figure_3.png", width = 5, height = 4)

### SI Figure 4 #######################################
## Urban Rural

#Import population statistics for age
filename <- "data/urban_rural_ch.csv"
urban_rural_ch <- read.csv(filename, header = TRUE, sep = ";")


data_urban_rural <- data %>%
  dplyr::filter(!is.na(urban_rural_true)) %>%  # Remove NA values
  dplyr::count(urban_rural_true) %>% 
  dplyr::mutate(Type = recode(urban_rural_true, `1` = "urban", `2` = "suburban", `3` = "rural")) %>%
  dplyr::select(Type, n) %>%  
  dplyr::rename(Count = n) %>%
  mutate(Percent = Count / sum(Count) * 100)  # Calculate percentages

data_urban_rural <- data_urban_rural %>% mutate(source = "Sample")
urban_rural_ch <- urban_rural_ch %>% mutate(source = "Population")

# Ensure Percent is numeric in both data frames
data_urban_rural <- data_urban_rural %>%
  mutate(Percent = as.numeric(Percent))

urban_rural_ch <- urban_rural_ch %>%
  mutate(Percent = gsub(",", ".", Percent)) %>%  
  mutate(Percent = as.numeric(Percent))  

urban_rural_ch <- urban_rural_ch %>%
  rename(
    Type = urbrur,
    Count = Frequency
  ) %>%
  select(Type, Count, Percent, source)

combined_data_urban_rural <- bind_rows(data_urban_rural, urban_rural_ch)

ggplot(combined_data_urban_rural, aes(x = Type, y = Percent, fill = source)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Category", y = "Percentage (%)", title = "Degree of Urbanization") +
  scale_fill_manual(values = c("Sample" = "#4059AD", "Population" = "#CCA43B")) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 12, color = "black"),
    axis.text.y = element_text(size = 12, color = "black"),
    plot.title = element_text(size = 14, face = "bold"),
    legend.title = element_blank(),
    legend.text = element_text(size = 12),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )

ggsave("plots/SI_Figure_4.png", width = 5, height = 3)

#Plot Fig. 1 in Main Manuscript ######################################################
#Fig. 1 shows the proportion of a representative sample of 2,155 Swiss residents with left-leaning and 
#right-leaning political ideology that clearly oppose (1) to clearly support (7) open-space PV and rooftop PV 

likert_colors <- c(
  "1" = "#d73027",  
  "2" = "#fc8d59",
  "3" = "#fee090",
  "4" = "#D3D3D3",  
  "5" = "#e0f3f8",
  "6" = "#91bfdb",
  "7" = "#4575b4"   
)

# Solar Roof + Solar Open Space 
data_plot <- data %>%
  select(solar_open_space, solar_roofs, left_right_binary) %>%
  pivot_longer(cols = c(solar_open_space, solar_roofs),
               names_to = "Question", values_to = "Response") %>%
  mutate(
    Question = recode(Question,
                      solar_open_space = "Open-Space PV",
                      solar_roofs = "Rooftop PV"),
    Group = recode(left_right_binary,
                   "Left" = "Left-Leaning Residents",
                   "Right" = "Right-Leaning Residents"),
    Response = factor(Response, levels = rev(as.character(1:7)))
  ) %>%
  filter(!is.na(Response), !is.na(Group)) %>%
  mutate(
    QuestionGroup = interaction(Question, Group, sep = ",\n"),
    QuestionGroup = factor(
      QuestionGroup,
      levels = c(
        "Rooftop PV,\nRight-Leaning Residents",
        "Rooftop PV,\nLeft-Leaning Residents",
        "Open-Space PV,\nRight-Leaning Residents",
        "Open-Space PV,\nLeft-Leaning Residents"
      )
    )
  ) %>%
  filter(QuestionGroup != "")
# Count and calculate proportions
df_plot <- data_plot %>%
  group_by(QuestionGroup, Response) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(QuestionGroup) %>%
  mutate(prop = n / sum(n))

# Plot
plot <- ggplot(df_plot, aes(x = QuestionGroup, y = prop, fill = Response)) +
  geom_bar(stat = "identity", position = "fill", width = 0.7) +
  coord_flip() +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_manual(values = likert_colors) +
  labs(
    y = "Proportion of Responses",
    x = NULL,
    fill = "1 = Completely oppose, 7 = Completely support"
  ) +
  guides(fill = guide_legend(nrow = 1, byrow = TRUE, reverse = TRUE)) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 12, color = "black"),
    axis.text.y = element_text(size = 12, color = "black"),
    plot.title = element_text(size = 16, face = "bold"),
    legend.text = element_text(size = 12),
    legend.position = "bottom",
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )

# Save the plot
ggsave("plots/Fig_1.png", plot = plot, width = 10, height = 4)

# Summary Statistics ###########################################

## SI Table 1 #################################
#Controls
labs <- c('Potential in Circle 1',
          'Potential in Circle 2',
          'Potential in Circle 3',
          'Age',
          'Gender',
          'Environmental Score',
          'Feelings towards Agri-PV',
          'Familiarity with Agri-PV',
          'Prior Agri-PV Preference',
          'Degree of Urbanization',
          'NIMBY',
          'Political Orientation')


st(data,
   vars = c('circle1', 'circle2', 'circle3', 'age', 'gender_f',
            'environment_score', 'feelings_agri_pv',
            'familiar_agri_pv', 'like_energy_agri_pv', 'urban_rural_true', 'NIMBY', 'left_right'),
   summ = c('notNA(x)', 
            'mean(x)', 
            'sd(x)', 
            'min(x)', 
            'max(x)'),
   summ.names = c('N','Mean','SD','Min','Max'),
   out = 'latex',
   file = 'tables/SI_Table_1.tex',
   labels = labs,
   title = NA,
   digits = 4)

## SI Table 2 #################################
#Comprehension Check and Dependent Variable 1 and 2

labs <- c('Knowledge of Potential in Circle 1',
         'Knowledge of Potential in Circle 2',
         'Knowledge of Potential in Circle 3',
         'Personal Advantage from Agri-PV',
         'Advantage for Switzerland')


st(data,
   vars = c('potential_know_1', 'potential_know_2', 'potential_know_3', 'personal_advantage', 'ch_advantage'),
   summ = c('notNA(x)', 
            'mean(x)', 
            'sd(x)', 
            'min(x)', 
            'max(x)'),
   summ.names = c('N','Mean','SD','Min','Max'),
   out = 'latex',
   file = 'tables/SI_Table_2.tex',
   labels = labs,
   title = NA,
   digits = 4)

## SI Table 3 ###############################
#Dependent Variables 3 to 7
labs <- c('Expansion \\textless{}1ha',
          'Expansion \\textless{}5ha',
          'Expansion \\textless{}10ha',
          'Expansion in Switzerland',
          'Expansion in Neighbourhood')


st(data,
   vars = c('attitude_expansion_small', 'attitude_expansion_medium', 'attitude_expansion_large', 'attitude_expansion', 'attitude_expansion_nearby'),
   summ = c('notNA(x)', 
            'mean(x)', 
            'sd(x)', 
            'min(x)', 
            'max(x)'),
   summ.names = c('N','Mean','SD','Min','Max'),
   out = 'latex',
   file = 'tables/SI_Table_3.tex',
   labels = labs,
   title = NA,
   digits = 4)

## SI Table 4 ###################################
#Dependent Variables 8 to 12
labs <- c('General Policy Support',
          'Simplified approval procedures',
          'Advisory Services for Farmers',
          'Increased One-Off Payments',
          'Financial Support for large Projects')

st(data,
   vars = c('support_policies', 'support_policy_1', 'support_policy_4', 'support_policy_6', 'support_policy_2'),
   summ = c('notNA(x)', 
            'mean(x)', 
            'sd(x)', 
            'min(x)', 
            'max(x)'),
   summ.names = c('N','Mean','SD','Min','Max'),
   out = 'latex',
   file = 'tables/SI_Table_4.tex',
   labels = labs,
   title = NA,
   digits = 4,
   col.width = c(35, rep(6, 5))
   )
