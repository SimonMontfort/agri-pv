# Replication Code for Descriptive and Regression Analysis "Agrivoltaics can reduce political polarization and local opposition to solar energy" #####################################################
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

data <- readRDS("data/data.rds")

# Packages #####################################################

library(tidyverse)
library(estimatr)
library(patchwork)
library(texreg)
library(emmeans)
library(scales)
library(vtable)


# Subset data #############################################

# Split by political orientation
data_left <- data[!is.na(data$left_right) &  data$left_right <=4, ]
data_centre <- data[!is.na(data$left_right) &  data$left_right >=5 &  data$left_right <=6, ]
data_right <- data[!is.na(data$left_right) & data$left_right >=7, ]

# Split by level of urbanization
data_urban <- data[data$urban_rural == 1, ]
data_suburban <- data[data$urban_rural == 2, ]
data_rural <- data[data$urban_rural == 3, ]


# Create relevant factor variables #################################

# Political orientation
data$left_right_centre <- cut(
  data$left_right,
  breaks = c(0, 4, 6, 10),
  labels = c("Left", "Centre", "Right"),
  include.lowest = TRUE,
  right = TRUE
)

# Degree of urbanization
data$urban_rural_f <- factor(
  data$urban_rural,
  levels = c(1, 2, 3),
  labels = c("Urban", "Suburban", "Rural")
)

# Summary Statistics ###########################################

## SI Table 1 #################################
#Controls
labs <- c('Potential in circle 1',
          'Potential in circle 2',
          'Potential in circle 3',
          'Age',
          'Gender',
          'Environmental score',
          'Familiarity with agrivoltaics',
          'Prior agrivoltaics preference',
          'Degree of urbanization',
          'Treatment',
          'Political orientation')


st(data,
   vars = c('circle1', 'circle2', 'circle3', 'age', 'gender_f',
            'environment_score',
            'familiar_agri_pv', 'like_energy_agri_pv', 'urban_rural', 'NIMBY', 'left_right'),
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

labs <- c('Knowledge of potential in circle 1',
          'Knowledge of potential in circle 2',
          'Knowledge of potential in circle 3',
          'Personal advantage from agrivoltaics',
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
          'Expansion in neighbourhood')


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
labs <- c('General policy support',
          'Simplified approval procedures',
          'Advisory services for farmers',
          'Increased one-off payments',
          'Financial support for large projects')

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

# Balance checks ####################################
#age
b_age <- lm_robust(age ~ NIMBY, data = data)

#gender, reference: Male
b_gender <- lm_robust(gender ~ NIMBY, data = data)

#Familiarity
b_familiar <- lm_robust(familiar_agri_pv ~ NIMBY, data = data)

#Environmental score (1-7)
b_environment <- lm_robust(environment_score ~ NIMBY, data = data)

#like_energy_agri_pv
b_like_energy <- lm_robust(like_energy_agri_pv ~ NIMBY, data = data)

#region, reference: urban
b_urbanization <- lm_robust(urban_rural ~ NIMBY, data = data)

#political spectrum (1-10)
b_left_right <- lm_robust(left_right ~ NIMBY, data = data)

#circle 1
b_circle1 <- lm_robust(circle1 ~ NIMBY, data = data)

#circle 2
b_circle2 <- lm_robust(circle2 ~ NIMBY, data = data)

#circle 3
b_circle3 <- lm_robust(circle3 ~ NIMBY, data = data)

## SI Table 5 ###############################################

models_balance_check <- list(
  b_age,
  b_gender,
  b_environment,
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
                                     "Treatment group"),
               custom.gof.names =c(NA, NA, "Observations", NA),
               reorder.gof =c(3, 1, 2, 4),
               file = "tables/SI_Balance.tex",
               use.packages = F,
               caption = "B1: Age, B2: Gender B3: Environmental score,
               B4: Familiarity with agrivoltaics, B5: Prior agrivoltaics preference, 
               B6: Degree of urbanization, B7: Political orientation, B8: Potential in circle 1 (0-500m),
               B9: Potential in circle 2 (500-1500m), B10: Potential in circle 3 (1500-4500m)"
)


# Regressions ##############################################
## Function for automated regression with controls
model_control <- function(dependent_var, main_independent_var, control_variables, data) {
  lm_robust(as.formula(paste(dependent_var, "~", main_independent_var, "+", paste(control_variables, collapse = " + "))), data = data)
}

### Controls ###########################
controls <- c("circle1", "circle2", "circle3", "age", "gender_f", "environment_score", "familiar_agri_pv", "like_energy_agri_pv", "urban_rural")
controls_c <- c("age", "gender_f", "environment_score", "familiar_agri_pv", "like_energy_agri_pv", "urban_rural")
controls_environment <- c("age", "gender_f", "familiar_agri_pv", "like_energy_agri_pv", "urban_rural")
controls_urban_rural <- c("age", "gender_f", "environment_score", "familiar_agri_pv", "like_energy_agri_pv")
controls_familiar <- c("age", "gender_f", "environment_score", "like_energy_agri_pv", "urban_rural")
controls_like_energy <- c("age", "gender_f", "environment_score", "familiar_agri_pv", "urban_rural")
controls_gender <- c("age", "environment_score", "familiar_agri_pv", "like_energy_agri_pv", "urban_rural")


controls_ur <- c("circle1", "circle2", "circle3", "age", "gender_f", "environment_score", "familiar_agri_pv", "like_energy_agri_pv", "left_right")
controls_ur_c <- c("age", "gender_f", "environment_score", "familiar_agri_pv", "like_energy_agri_pv", "left_right")
controls_ur_environment <- c("age", "gender_f", "familiar_agri_pv", "like_energy_agri_pv", "left_right")
controls_ur_left_right <- c("age", "gender_f", "environment_score", "familiar_agri_pv", "like_energy_agri_pv")
controls_ur_ur_familiar <- c("age", "gender_f", "environment_score", "like_energy_agri_pv", "left_right")
controls_ur_like_energy <- c("age", "gender_f", "environment_score", "familiar_agri_pv", "left_right")
controls_ur_gender <- c("age", "environment_score", "familiar_agri_pv", "like_energy_agri_pv", "left_right")


# Regressions #####################################################################

## Political orientaiton ##############################
### Comprehension check ######################################################

#### No Interactions ################################
# Knowledge circle 1
comp_potential_0_500_control_left <- model_control("potential_know_1", "NIMBY", controls, data_left)
comp_potential_0_500_control_centre <- model_control("potential_know_1", "NIMBY", controls, data_centre)
comp_potential_0_500_control_right <- model_control("potential_know_1", "NIMBY", controls, data_right)

comp_potential_0_500_circle_split_0_control_left <- model_control("potential_know_1", "NIMBY", controls_c, data_left [data_left$circle1 ==0,])
comp_potential_0_500_circle_split_0_control_centre <- model_control("potential_know_1", "NIMBY", controls_c, data_centre [data_centre$circle1 ==0,])
comp_potential_0_500_circle_split_0_control_right <- model_control("potential_know_1", "NIMBY", controls_c, data_right [data_right$circle1 ==0,])

comp_potential_0_500_circle_split_1_control_left <- model_control("potential_know_1", "NIMBY", controls_c, data_left [data_left$circle1 ==1,])
comp_potential_0_500_circle_split_1_control_centre <- model_control("potential_know_1", "NIMBY", controls_c, data_centre [data_centre$circle1 ==1,])
comp_potential_0_500_circle_split_1_control_right <- model_control("potential_know_1", "NIMBY", controls_c, data_right [data_right$circle1 ==1,])

#Knowledge circle 2
comp_potential_500_1500_control_left <- model_control("potential_know_2", "NIMBY", controls, data_left)
comp_potential_500_1500_control_centre <- model_control("potential_know_2", "NIMBY", controls, data_centre)
comp_potential_500_1500_control_right <- model_control("potential_know_2", "NIMBY", controls, data_right)

comp_potential_500_1500_circle_split_0_control_left <- model_control("potential_know_2", "NIMBY", controls_c, data_left [data_left$circle2 ==0,])
comp_potential_500_1500_circle_split_0_control_centre <- model_control("potential_know_2", "NIMBY", controls_c, data_centre [data_centre$circle2 ==0,])
comp_potential_500_1500_circle_split_0_control_right <- model_control("potential_know_2", "NIMBY", controls_c, data_right [data_right$circle2 ==0,])

comp_potential_500_1500_circle_split_1_control_left <- model_control("potential_know_2", "NIMBY", controls_c, data_left [data_left$circle2 ==1,])
comp_potential_500_1500_circle_split_1_control_centre <- model_control("potential_know_2", "NIMBY", controls_c, data_centre [data_centre$circle2 ==1,])
comp_potential_500_1500_circle_split_1_control_right <- model_control("potential_know_2", "NIMBY", controls_c, data_right [data_right$circle2 ==1,])

#Knowledge circle 3
comp_potential_1500_4500_control_left <- model_control("potential_know_3", "NIMBY", controls, data_left)
comp_potential_1500_4500_control_centre <- model_control("potential_know_3", "NIMBY", controls, data_centre)
comp_potential_1500_4500_control_right <- model_control("potential_know_3", "NIMBY", controls, data_right)

comp_potential_1500_4500_circle_split_0_control_left <- model_control("potential_know_3", "NIMBY", controls_c, data_left [data_left$circle3 ==0,])
comp_potential_1500_4500_circle_split_0_control_centre <- model_control("potential_know_3", "NIMBY", controls_c, data_centre [data_centre$circle3 ==0,])
comp_potential_1500_4500_circle_split_0_control_right <- model_control("potential_know_3", "NIMBY", controls_c, data_right [data_right$circle3 ==0,])

comp_potential_1500_4500_circle_split_1_control_left <- model_control("potential_know_3", "NIMBY", controls_c, data_left [data_left$circle3 ==1,])
comp_potential_1500_4500_circle_split_1_control_centre <- model_control("potential_know_3", "NIMBY", controls_c, data_centre [data_centre$circle3 ==1,])
comp_potential_1500_4500_circle_split_1_control_right <- model_control("potential_know_3", "NIMBY", controls_c, data_right [data_right$circle3 ==1,])

#### Interaction with circles #####################
dep_potential_know_1_control_int_circles_left <- model_control("potential_know_1", "NIMBY * circle1 + NIMBY * circle2 + NIMBY * circle3", controls, data_left)
dep_potential_know_1_control_int_circles_centre <- model_control("potential_know_1", "NIMBY * circle1 + NIMBY * circle2 + NIMBY * circle3", controls, data_centre)
dep_potential_know_1_control_int_circles_right <- model_control("potential_know_1", "NIMBY * circle1 + NIMBY * circle2 + NIMBY * circle3", controls, data_right)

dep_potential_know_2_control_int_circles_left <- model_control("potential_know_2", "NIMBY * circle1 + NIMBY * circle2 + NIMBY * circle3", controls, data_left)
dep_potential_know_2_control_int_circles_centre <- model_control("potential_know_2", "NIMBY * circle1 + NIMBY * circle2 + NIMBY * circle3", controls, data_centre)
dep_potential_know_2_control_int_circles_right <- model_control("potential_know_2", "NIMBY * circle1 + NIMBY * circle2 + NIMBY * circle3", controls, data_right)

dep_potential_know_3_control_int_circles_left <- model_control("potential_know_3", "NIMBY * circle1 + NIMBY * circle2 + NIMBY * circle3", controls, data_left)
dep_potential_know_3_control_int_circles_centre <- model_control("potential_know_3", "NIMBY * circle1 + NIMBY * circle2 + NIMBY * circle3", controls, data_centre)
dep_potential_know_3_control_int_circles_right <- model_control("potential_know_3", "NIMBY * circle1 + NIMBY * circle2 + NIMBY * circle3", controls, data_right)

### Manipulation check #############################
#### No interactions ############################
comp_personal_advantage_control_left <- model_control("personal_advantage", "NIMBY", controls, data_left)
comp_personal_advantage_control_centre <- model_control("personal_advantage", "NIMBY", controls, data_centre)
comp_personal_advantage_control_right <- model_control("personal_advantage", "NIMBY", controls, data_right)

comp_ch_advantage_control_left <- model_control("ch_advantage", "NIMBY", controls, data_left)
comp_ch_advantage_control_centre <- model_control("ch_advantage", "NIMBY", controls, data_centre)
comp_ch_advantage_control_right <- model_control("ch_advantage", "NIMBY", controls, data_right)

#### Interaction with circles #######################
dep_personal_advantage_control_int_circles_left <- model_control("personal_advantage", "NIMBY * circle1 + NIMBY * circle2 + NIMBY * circle3", controls, data_left)
dep_personal_advantage_control_int_circles_centre <- model_control("personal_advantage", "NIMBY * circle1 + NIMBY * circle2 + NIMBY * circle3", controls, data_centre)
dep_personal_advantage_control_int_circles_right <- model_control("personal_advantage", "NIMBY * circle1 + NIMBY * circle2 + NIMBY * circle3", controls, data_right)

dep_ch_advantage_control_int_circles_left <- model_control("ch_advantage", "NIMBY * circle1 + NIMBY * circle2 + NIMBY * circle3", controls, data_left)
dep_ch_advantage_control_int_circles_centre <- model_control("ch_advantage", "NIMBY * circle1 + NIMBY * circle2 + NIMBY * circle3", controls, data_centre)
dep_ch_advantage_control_int_circles_right <- model_control("ch_advantage", "NIMBY * circle1 + NIMBY * circle2 + NIMBY * circle3", controls, data_right)


### Attitude expansion ##############################

#### No Interactions #############################################
# attitude_expansion
comp_attitude_expansion_control_left <- model_control("attitude_expansion", "NIMBY", controls, data_left)
comp_attitude_expansion_control_centre <- model_control("attitude_expansion", "NIMBY", controls, data_centre)
comp_attitude_expansion_control_right <- model_control("attitude_expansion", "NIMBY", controls, data_right)

# attitude_expansion_small
comp_attitude_expansion_small_control_left <- model_control("attitude_expansion_small", "NIMBY", controls, data_left)
comp_attitude_expansion_small_control_centre <- model_control("attitude_expansion_small", "NIMBY", controls, data_centre)
comp_attitude_expansion_small_control_right <- model_control("attitude_expansion_small", "NIMBY", controls, data_right)

# attitude_expansion_medium
comp_attitude_expansion_medium_control_left <- model_control("attitude_expansion_medium", "NIMBY", controls, data_left)
comp_attitude_expansion_medium_control_centre <- model_control("attitude_expansion_medium", "NIMBY", controls, data_centre)
comp_attitude_expansion_medium_control_right <- model_control("attitude_expansion_medium", "NIMBY", controls, data_right)

# attitude_expansion_large
comp_attitude_expansion_large_control_left <- model_control("attitude_expansion_large", "NIMBY", controls, data_left)
comp_attitude_expansion_large_control_centre <- model_control("attitude_expansion_large", "NIMBY", controls, data_centre)
comp_attitude_expansion_large_control_right <- model_control("attitude_expansion_large", "NIMBY", controls, data_right)

# attitude_expansion_nearby
comp_attitude_expansion_nearby_control_left <- model_control("attitude_expansion_nearby", "NIMBY", controls, data_left)
comp_attitude_expansion_nearby_control_centre <- model_control("attitude_expansion_nearby", "NIMBY", controls, data_centre)
comp_attitude_expansion_nearby_control_right <- model_control("attitude_expansion_nearby", "NIMBY", controls, data_right)

#### Interaction with circles ####################################################
dep_attitude_expansion_control_int_circles_left <- model_control("attitude_expansion", "NIMBY * circle1 + NIMBY * circle2 + NIMBY * circle3", controls, data_left)
dep_attitude_expansion_control_int_circles_centre <- model_control("attitude_expansion", "NIMBY * circle1 + NIMBY * circle2 + NIMBY * circle3", controls, data_centre)
dep_attitude_expansion_control_int_circles_right <- model_control("attitude_expansion", "NIMBY * circle1 + NIMBY * circle2 + NIMBY * circle3", controls, data_right)

dep_attitude_expansion_small_control_int_circles_left <- model_control("attitude_expansion_small", "NIMBY * circle1 + NIMBY * circle2 + NIMBY * circle3", controls, data_left)
dep_attitude_expansion_small_control_int_circles_centre <- model_control("attitude_expansion_small", "NIMBY * circle1 + NIMBY * circle2 + NIMBY * circle3", controls, data_centre)
dep_attitude_expansion_small_control_int_circles_right <- model_control("attitude_expansion_small", "NIMBY * circle1 + NIMBY * circle2 + NIMBY * circle3", controls, data_right)

dep_attitude_expansion_medium_control_int_circles_left <- model_control("attitude_expansion_medium", "NIMBY * circle1 + NIMBY * circle2 + NIMBY * circle3", controls, data_left)
dep_attitude_expansion_medium_control_int_circles_centre <- model_control("attitude_expansion_medium", "NIMBY * circle1 + NIMBY * circle2 + NIMBY * circle3", controls, data_centre)
dep_attitude_expansion_medium_control_int_circles_right <- model_control("attitude_expansion_medium", "NIMBY * circle1 + NIMBY * circle2 + NIMBY * circle3", controls, data_right)

dep_attitude_expansion_large_control_int_circles_left <- model_control("attitude_expansion_large", "NIMBY * circle1 + NIMBY * circle2 + NIMBY * circle3", controls, data_left)
dep_attitude_expansion_large_control_int_circles_centre <- model_control("attitude_expansion_large", "NIMBY * circle1 + NIMBY * circle2 + NIMBY * circle3", controls, data_centre)
dep_attitude_expansion_large_control_int_circles_right <- model_control("attitude_expansion_large", "NIMBY * circle1 + NIMBY * circle2 + NIMBY * circle3", controls, data_right)

dep_attitude_expansion_nearby_control_int_circles_left <- model_control("attitude_expansion_nearby", "NIMBY * circle1 + NIMBY * circle2 + NIMBY * circle3", controls, data_left)
dep_attitude_expansion_nearby_control_int_circles_centre <- model_control("attitude_expansion_nearby", "NIMBY * circle1 + NIMBY * circle2 + NIMBY * circle3", controls, data_centre)
dep_attitude_expansion_nearby_control_int_circles_right <- model_control("attitude_expansion_nearby", "NIMBY * circle1 + NIMBY * circle2 + NIMBY * circle3", controls, data_right)

### Policy support ###################################################
#### No Interactions ###################################
dep_support_policies_control_left <- model_control("support_policies", "NIMBY", controls, data_left)
dep_support_policies_control_centre <- model_control("support_policies", "NIMBY", controls, data_centre)
dep_support_policies_control_right <- model_control("support_policies", "NIMBY", controls, data_right)

# support_policy individually
dep_support_policy_1_control_left <- model_control("support_policy_1", "NIMBY", controls, data_left)
dep_support_policy_1_control_centre <- model_control("support_policy_1", "NIMBY", controls, data_centre)
dep_support_policy_1_control_right <- model_control("support_policy_1", "NIMBY", controls, data_right)

dep_support_policy_2_control_left <- model_control("support_policy_2", "NIMBY", controls, data_left)
dep_support_policy_2_control_centre <- model_control("support_policy_2", "NIMBY", controls, data_centre)
dep_support_policy_2_control_right <- model_control("support_policy_2", "NIMBY", controls, data_right)

dep_support_policy_4_control_left <- model_control("support_policy_4", "NIMBY", controls, data_left)
dep_support_policy_4_control_centre <- model_control("support_policy_4", "NIMBY", controls, data_centre)
dep_support_policy_4_control_right <- model_control("support_policy_4", "NIMBY", controls, data_right)

dep_support_policy_6_control_left <- model_control("support_policy_6", "NIMBY", controls, data_left)
dep_support_policy_6_control_centre <- model_control("support_policy_6", "NIMBY", controls, data_centre)
dep_support_policy_6_control_right <- model_control("support_policy_6", "NIMBY", controls, data_right)

#### Interaction with circles #######################################

dep_support_policies_control_int_circles_left <- model_control("support_policies", "NIMBY * circle1 + NIMBY * circle2 + NIMBY * circle3", controls, data_left)
dep_support_policies_control_int_circles_centre <- model_control("support_policies", "NIMBY * circle1 + NIMBY * circle2 + NIMBY * circle3", controls, data_centre)
dep_support_policies_control_int_circles_right <- model_control("support_policies", "NIMBY * circle1 + NIMBY * circle2 + NIMBY * circle3", controls, data_right)

dep_support_policy_1_control_int_circles_left <- model_control("support_policy_1", "NIMBY * circle1 + NIMBY * circle2 + NIMBY * circle3", controls, data_left)
dep_support_policy_1_control_int_circles_centre <- model_control("support_policies", "NIMBY * circle1 + NIMBY * circle2 + NIMBY * circle3", controls, data_centre)
dep_support_policy_1_control_int_circles_right <- model_control("support_policy_1", "NIMBY * circle1 + NIMBY * circle2 + NIMBY * circle3", controls, data_right)

dep_support_policy_2_control_int_circles_left <- model_control("support_policy_2", "NIMBY * circle1 + NIMBY * circle2 + NIMBY * circle3", controls, data_left)
dep_support_policy_2_control_int_circles_centre <- model_control("support_policy_2", "NIMBY * circle1 + NIMBY * circle2 + NIMBY * circle3", controls, data_centre)
dep_support_policy_2_control_int_circles_right <- model_control("support_policy_2", "NIMBY * circle1 + NIMBY * circle2 + NIMBY * circle3", controls, data_right)

dep_support_policy_4_control_int_circles_left <- model_control("support_policy_4", "NIMBY * circle1 + NIMBY * circle2 + NIMBY * circle3", controls, data_left)
dep_support_policy_4_control_int_circles_centre <- model_control("support_policy_4", "NIMBY * circle1 + NIMBY * circle2 + NIMBY * circle3", controls, data_centre)
dep_support_policy_4_control_int_circles_right <- model_control("support_policy_4", "NIMBY * circle1 + NIMBY * circle2 + NIMBY * circle3", controls, data_right)

dep_support_policy_6_control_int_circles_left <- model_control("support_policy_6", "NIMBY * circle1 + NIMBY * circle2 + NIMBY * circle3", controls, data_left)
dep_support_policy_6_control_int_circles_centre <- model_control("support_policy_6", "NIMBY * circle1 + NIMBY * circle2 + NIMBY * circle3", controls, data_centre)
dep_support_policy_6_control_int_circles_right <- model_control("support_policy_6", "NIMBY * circle1 + NIMBY * circle2 + NIMBY * circle3", controls, data_right)

## Degree of urbanization #####################################
### Comprehension check ###########################
#### No Interactions ################################
# Knowledge Circle 1
comp_potential_0_500_control_urban <- model_control("potential_know_1", "NIMBY", controls_ur, data_urban)
comp_potential_0_500_control_suburban <- model_control("potential_know_1", "NIMBY", controls_ur, data_suburban)
comp_potential_0_500_control_rural <- model_control("potential_know_1", "NIMBY", controls_ur, data_rural)

comp_potential_0_500_circle_split_0_control_urban <- model_control("potential_know_1", "NIMBY", controls_ur_c, data_urban [data_urban$circle1 ==0,])
comp_potential_0_500_circle_split_0_control_suburban <- model_control("potential_know_1", "NIMBY", controls_ur_c, data_suburban [data_suburban$circle1 ==0,])
comp_potential_0_500_circle_split_0_control_rural <- model_control("potential_know_1", "NIMBY", controls_ur_c, data_rural [data_rural$circle1 ==0,])

comp_potential_0_500_circle_split_1_control_urban <- model_control("potential_know_1", "NIMBY", controls_ur_c, data_urban [data_urban$circle1 ==1,])
comp_potential_0_500_circle_split_1_control_suburban <- model_control("potential_know_1", "NIMBY", controls_ur_c, data_suburban [data_suburban$circle1 ==1,])
comp_potential_0_500_circle_split_1_control_rural <- model_control("potential_know_1", "NIMBY", controls_ur_c, data_rural [data_rural$circle1 ==1,])

#Knowledge Circle 2
comp_potential_500_1500_control_urban <- model_control("potential_know_2", "NIMBY", controls_ur, data_urban)
comp_potential_500_1500_control_suburban <- model_control("potential_know_2", "NIMBY", controls_ur, data_suburban)
comp_potential_500_1500_control_rural <- model_control("potential_know_2", "NIMBY", controls_ur, data_rural)

comp_potential_500_1500_circle_split_0_control_urban <- model_control("potential_know_2", "NIMBY", controls_ur_c, data_urban [data_urban$circle2 ==0,])
comp_potential_500_1500_circle_split_0_control_suburban <- model_control("potential_know_2", "NIMBY", controls_ur_c, data_suburban [data_suburban$circle2 ==0,])
comp_potential_500_1500_circle_split_0_control_rural <- model_control("potential_know_2", "NIMBY", controls_ur_c, data_rural [data_rural$circle2 ==0,])

comp_potential_500_1500_circle_split_1_control_urban <- model_control("potential_know_2", "NIMBY", controls_ur_c, data_urban [data_urban$circle2 ==1,])
comp_potential_500_1500_circle_split_1_control_suburban <- model_control("potential_know_2", "NIMBY", controls_ur_c, data_suburban [data_suburban$circle2 ==1,])
comp_potential_500_1500_circle_split_1_control_rural <- model_control("potential_know_2", "NIMBY", controls_ur_c, data_rural [data_rural$circle2 ==1,])

#Knowledge Circle 3
comp_potential_1500_4500_control_urban <- model_control("potential_know_3", "NIMBY", controls_ur, data_urban)
comp_potential_1500_4500_control_suburban <- model_control("potential_know_3", "NIMBY", controls_ur, data_suburban)
comp_potential_1500_4500_control_rural <- model_control("potential_know_3", "NIMBY", controls_ur, data_rural)

comp_potential_1500_4500_circle_split_0_control_urban <- model_control("potential_know_3", "NIMBY", controls_ur_c, data_urban [data_urban$circle3 ==0,])
comp_potential_1500_4500_circle_split_0_control_suburban <- model_control("potential_know_3", "NIMBY", controls_ur_c, data_suburban [data_suburban$circle3 ==0,])
comp_potential_1500_4500_circle_split_0_control_rural <- model_control("potential_know_3", "NIMBY", controls_ur_c, data_rural [data_rural$circle3 ==0,])

comp_potential_1500_4500_circle_split_1_control_urban <- model_control("potential_know_3", "NIMBY", controls_ur_c, data_urban [data_urban$circle3 ==1,])
comp_potential_1500_4500_circle_split_1_control_suburban <- model_control("potential_know_3", "NIMBY", controls_ur_c, data_suburban [data_suburban$circle3 ==1,])
comp_potential_1500_4500_circle_split_1_control_rural <- model_control("potential_know_3", "NIMBY", controls_ur_c, data_rural [data_rural$circle3 ==1,])

#### Interaction with Circles ################################
dep_potential_know_1_control_int_circles_urban <- model_control("potential_know_1", "NIMBY * circle1 + NIMBY * circle2 + NIMBY * circle3", controls_ur, data_urban)
dep_potential_know_1_control_int_circles_suburban <- model_control("potential_know_1", "NIMBY * circle1 + NIMBY * circle2 + NIMBY * circle3", controls_ur, data_suburban)
dep_potential_know_1_control_int_circles_rural <- model_control("potential_know_1", "NIMBY * circle1 + NIMBY * circle2 + NIMBY * circle3", controls_ur, data_rural)

dep_potential_know_2_control_int_circles_urban <- model_control("potential_know_2", "NIMBY * circle1 + NIMBY * circle2 + NIMBY * circle3", controls_ur, data_urban)
dep_potential_know_2_control_int_circles_suburban <- model_control("potential_know_2", "NIMBY * circle1 + NIMBY * circle2 + NIMBY * circle3", controls_ur, data_suburban)
dep_potential_know_2_control_int_circles_rural <- model_control("potential_know_2", "NIMBY * circle1 + NIMBY * circle2 + NIMBY * circle3", controls_ur, data_rural)

dep_potential_know_3_control_int_circles_urban <- model_control("potential_know_3", "NIMBY * circle1 + NIMBY * circle2 + NIMBY * circle3", controls_ur, data_urban)
dep_potential_know_3_control_int_circles_suburban <- model_control("potential_know_3", "NIMBY * circle1 + NIMBY * circle2 + NIMBY * circle3", controls_ur, data_suburban)
dep_potential_know_3_control_int_circles_rural <- model_control("potential_know_3", "NIMBY * circle1 + NIMBY * circle2 + NIMBY * circle3", controls_ur, data_rural)

### Manipulation Check #######################################
#### No Interactions ######################################

comp_personal_advantage_control_urban <- model_control("personal_advantage", "NIMBY", controls_ur, data_urban)
comp_personal_advantage_control_suburban <- model_control("personal_advantage", "NIMBY", controls_ur, data_suburban)
comp_personal_advantage_control_rural <- model_control("personal_advantage", "NIMBY", controls_ur, data_rural)

comp_ch_advantage_control_urban <- model_control("ch_advantage", "NIMBY", controls_ur, data_urban)
comp_ch_advantage_control_suburban <- model_control("ch_advantage", "NIMBY", controls_ur, data_suburban)
comp_ch_advantage_control_rural <- model_control("ch_advantage", "NIMBY", controls_ur, data_rural)

#### Interaction with Circles #########################

dep_personal_advantage_control_int_circles_urban <- model_control("personal_advantage", "NIMBY * circle1 + NIMBY * circle2 + NIMBY * circle3", controls_ur, data_urban)
dep_personal_advantage_control_int_circles_suburban <- model_control("personal_advantage", "NIMBY * circle1 + NIMBY * circle2 + NIMBY * circle3", controls_ur, data_suburban)
dep_personal_advantage_control_int_circles_rural <- model_control("personal_advantage", "NIMBY * circle1 + NIMBY * circle2 + NIMBY * circle3", controls_ur, data_rural)

dep_ch_advantage_control_int_circles_urban <- model_control("ch_advantage", "NIMBY * circle1 + NIMBY * circle2 + NIMBY * circle3", controls_ur, data_urban)
dep_ch_advantage_control_int_circles_suburban <- model_control("ch_advantage", "NIMBY * circle1 + NIMBY * circle2 + NIMBY * circle3", controls_ur, data_suburban)
dep_ch_advantage_control_int_circles_rural <- model_control("ch_advantage", "NIMBY * circle1 + NIMBY * circle2 + NIMBY * circle3", controls_ur, data_rural)

### Attitude Expansion ############################################

#### No Interactions #############################################
# attitude_expansion
comp_attitude_expansion_control_urban <- model_control("attitude_expansion", "NIMBY", controls_ur, data_urban)
comp_attitude_expansion_control_suburban <- model_control("attitude_expansion", "NIMBY", controls_ur, data_suburban)
comp_attitude_expansion_control_rural <- model_control("attitude_expansion", "NIMBY", controls_ur, data_rural)

# attitude_expansion_small
comp_attitude_expansion_small_control_urban <- model_control("attitude_expansion_small", "NIMBY", controls_ur, data_urban)
comp_attitude_expansion_small_control_suburban <- model_control("attitude_expansion_small", "NIMBY", controls_ur, data_suburban)
comp_attitude_expansion_small_control_rural <- model_control("attitude_expansion_small", "NIMBY", controls_ur, data_rural)

# attitude_expansion_medium
comp_attitude_expansion_medium_control_urban <- model_control("attitude_expansion_medium", "NIMBY", controls_ur, data_urban)
comp_attitude_expansion_medium_control_suburban <- model_control("attitude_expansion_medium", "NIMBY", controls_ur, data_suburban)
comp_attitude_expansion_medium_control_rural <- model_control("attitude_expansion_medium", "NIMBY", controls_ur, data_rural)

# attitude_expansion_large
comp_attitude_expansion_large_control_urban <- model_control("attitude_expansion_large", "NIMBY", controls_ur, data_urban)
comp_attitude_expansion_large_control_suburban <- model_control("attitude_expansion_large", "NIMBY", controls_ur, data_suburban)
comp_attitude_expansion_large_control_rural <- model_control("attitude_expansion_large", "NIMBY", controls_ur, data_rural)

# attitude_expansion_nearby
comp_attitude_expansion_nearby_control_urban <- model_control("attitude_expansion_nearby", "NIMBY", controls_ur, data_urban)
comp_attitude_expansion_nearby_control_suburban <- model_control("attitude_expansion_nearby", "NIMBY", controls_ur, data_suburban)
comp_attitude_expansion_nearby_control_rural <- model_control("attitude_expansion_nearby", "NIMBY", controls_ur, data_rural)

#### Interaction with Circles ####################################################
dep_attitude_expansion_control_int_circles_urban <- model_control("attitude_expansion", "NIMBY * circle1 + NIMBY * circle2 + NIMBY * circle3", controls_ur, data_urban)
dep_attitude_expansion_control_int_circles_suburban <- model_control("attitude_expansion", "NIMBY * circle1 + NIMBY * circle2 + NIMBY * circle3", controls_ur, data_suburban)
dep_attitude_expansion_control_int_circles_rural <- model_control("attitude_expansion", "NIMBY * circle1 + NIMBY * circle2 + NIMBY * circle3", controls_ur, data_rural)

dep_attitude_expansion_small_control_int_circles_urban <- model_control("attitude_expansion_small", "NIMBY * circle1 + NIMBY * circle2 + NIMBY * circle3", controls_ur, data_urban)
dep_attitude_expansion_small_control_int_circles_suburban <- model_control("attitude_expansion_small", "NIMBY * circle1 + NIMBY * circle2 + NIMBY * circle3", controls_ur, data_suburban)
dep_attitude_expansion_small_control_int_circles_rural <- model_control("attitude_expansion_small", "NIMBY * circle1 + NIMBY * circle2 + NIMBY * circle3", controls_ur, data_rural)

dep_attitude_expansion_medium_control_int_circles_urban <- model_control("attitude_expansion_medium", "NIMBY * circle1 + NIMBY * circle2 + NIMBY * circle3", controls_ur, data_urban)
dep_attitude_expansion_medium_control_int_circles_suburban <- model_control("attitude_expansion_medium", "NIMBY * circle1 + NIMBY * circle2 + NIMBY * circle3", controls_ur, data_suburban)
dep_attitude_expansion_medium_control_int_circles_rural <- model_control("attitude_expansion_medium", "NIMBY * circle1 + NIMBY * circle2 + NIMBY * circle3", controls_ur, data_rural)

dep_attitude_expansion_large_control_int_circles_urban <- model_control("attitude_expansion_large", "NIMBY * circle1 + NIMBY * circle2 + NIMBY * circle3", controls_ur, data_urban)
dep_attitude_expansion_large_control_int_circles_suburban <- model_control("attitude_expansion_large", "NIMBY * circle1 + NIMBY * circle2 + NIMBY * circle3", controls_ur, data_suburban)
dep_attitude_expansion_large_control_int_circles_rural <- model_control("attitude_expansion_large", "NIMBY * circle1 + NIMBY * circle2 + NIMBY * circle3", controls_ur, data_rural)

dep_attitude_expansion_nearby_control_int_circles_urban <- model_control("attitude_expansion_nearby", "NIMBY * circle1 + NIMBY * circle2 + NIMBY * circle3", controls_ur, data_urban)
dep_attitude_expansion_nearby_control_int_circles_suburban <- model_control("attitude_expansion_nearby", "NIMBY * circle1 + NIMBY * circle2 + NIMBY * circle3", controls_ur, data_suburban)
dep_attitude_expansion_nearby_control_int_circles_rural <- model_control("attitude_expansion_nearby", "NIMBY * circle1 + NIMBY * circle2 + NIMBY * circle3", controls_ur, data_rural)

### Policy Support ##################################################

#### No Interactions ###################################
dep_support_policies_control_urban <- model_control("support_policies", "NIMBY", controls_ur, data_urban)
dep_support_policies_control_suburban <- model_control("support_policies", "NIMBY", controls_ur, data_suburban)
dep_support_policies_control_rural <- model_control("support_policies", "NIMBY", controls_ur, data_rural)

# support_policy individually
dep_support_policy_1_control_urban <- model_control("support_policy_1", "NIMBY", controls_ur, data_urban)
dep_support_policy_1_control_suburban <- model_control("support_policy_1", "NIMBY", controls_ur, data_suburban)
dep_support_policy_1_control_rural <- model_control("support_policy_1", "NIMBY", controls_ur, data_rural)

dep_support_policy_2_control_urban <- model_control("support_policy_2", "NIMBY", controls_ur, data_urban)
dep_support_policy_2_control_suburban <- model_control("support_policy_2", "NIMBY", controls_ur, data_suburban)
dep_support_policy_2_control_rural <- model_control("support_policy_2", "NIMBY", controls_ur, data_rural)

dep_support_policy_4_control_urban <- model_control("support_policy_4", "NIMBY", controls_ur, data_urban)
dep_support_policy_4_control_suburban <- model_control("support_policy_4", "NIMBY", controls_ur, data_suburban)
dep_support_policy_4_control_rural <- model_control("support_policy_4", "NIMBY", controls_ur, data_rural)

dep_support_policy_6_control_urban <- model_control("support_policy_6", "NIMBY", controls_ur, data_urban)
dep_support_policy_6_control_suburban <- model_control("support_policy_6", "NIMBY", controls_ur, data_suburban)
dep_support_policy_6_control_rural <- model_control("support_policy_6", "NIMBY", controls_ur, data_rural)

#### Interaction with Circles ###################################################
dep_support_policies_control_int_circles_urban <- model_control("support_policies", "NIMBY * circle1 + NIMBY * circle2 + NIMBY * circle3", controls_ur, data_urban)
dep_support_policies_control_int_circles_suburban <- model_control("support_policies", "NIMBY * circle1 + NIMBY * circle2 + NIMBY * circle3", controls_ur, data_suburban)
dep_support_policies_control_int_circles_rural <- model_control("support_policies", "NIMBY * circle1 + NIMBY * circle2 + NIMBY * circle3", controls_ur, data_rural)

dep_support_policy_1_control_int_circles_urban <- model_control("support_policy_1", "NIMBY * circle1 + NIMBY * circle2 + NIMBY * circle3", controls_ur, data_urban)
dep_support_policy_1_control_int_circles_suburban <- model_control("support_policies", "NIMBY * circle1 + NIMBY * circle2 + NIMBY * circle3", controls_ur, data_suburban)
dep_support_policy_1_control_int_circles_rural <- model_control("support_policy_1", "NIMBY * circle1 + NIMBY * circle2 + NIMBY * circle3", controls_ur, data_rural)

dep_support_policy_2_control_int_circles_urban <- model_control("support_policy_2", "NIMBY * circle1 + NIMBY * circle2 + NIMBY * circle3", controls_ur, data_urban)
dep_support_policy_2_control_int_circles_suburban <- model_control("support_policy_2", "NIMBY * circle1 + NIMBY * circle2 + NIMBY * circle3", controls_ur, data_suburban)
dep_support_policy_2_control_int_circles_rural <- model_control("support_policy_2", "NIMBY * circle1 + NIMBY * circle2 + NIMBY * circle3", controls_ur, data_rural)

dep_support_policy_4_control_int_circles_urban <- model_control("support_policy_4", "NIMBY * circle1 + NIMBY * circle2 + NIMBY * circle3", controls_ur, data_urban)
dep_support_policy_4_control_int_circles_suburban <- model_control("support_policy_4", "NIMBY * circle1 + NIMBY * circle2 + NIMBY * circle3", controls_ur, data_suburban)
dep_support_policy_4_control_int_circles_rural <- model_control("support_policy_4", "NIMBY * circle1 + NIMBY * circle2 + NIMBY * circle3", controls_ur, data_rural)

dep_support_policy_6_control_int_circles_urban <- model_control("support_policy_6", "NIMBY * circle1 + NIMBY * circle2 + NIMBY * circle3", controls_ur, data_urban)
dep_support_policy_6_control_int_circles_suburban <- model_control("support_policy_6", "NIMBY * circle1 + NIMBY * circle2 + NIMBY * circle3", controls_ur, data_suburban)
dep_support_policy_6_control_int_circles_rural <- model_control("support_policy_6", "NIMBY * circle1 + NIMBY * circle2 + NIMBY * circle3", controls_ur, data_rural)




# Export as tables #####################################################

## Functions to generate column titles of tables #######################
model_names_lr <- function(dvs) {
  regions <- c("Left", "Centre", "Right")
  
  unlist(lapply(dvs, function(dv) {
    sapply(regions, function(region) {
      sprintf("\\shortstack{\\rule{0pt}{2.2ex}%s,\\\\%s}", dv, region)
    })
  }))
}

model_names_lr_wide <- function(dvs) {
  regions <- c("Left", "Centre", "Right")
  
  unlist(lapply(dvs, function(dv) {
    sapply(regions, function(region) {
      sprintf(
        "\\shortstack{\\rule{0pt}{2.2ex}\\hspace{0.25cm}%s,\\hspace{0.25cm}\\\\%s}",
        dv, region
      )
    })
  }))
}


model_names_ur <- function(dvs) {
  regions <- c("Urban", "Suburban", "Rural")
  
  unlist(lapply(dvs, function(dv) {
    sapply(regions, function(region) {
      sprintf("\\shortstack{\\rule{0pt}{2.2ex}%s,\\\\%s}", dv, region)
    })
  }))
}

model_names_ur_wide <- function(dvs) {
  regions <- c("Urban", "Suburban", "Rural")
  
  unlist(lapply(dvs, function(dv) {
    sapply(regions, function(region) {
      sprintf(
        "\\shortstack{\\rule{0pt}{2.2ex}\\hspace{0.25cm}%s,\\hspace{0.25cm}\\\\%s}",
        dv, region
      )
    })
  }))
}

## Political orientation #############################

### SI Table 5 ###################################################

# Comprehension check
models_comp <- list(
  comp_potential_0_500_control_left,
  comp_potential_0_500_control_centre,
  comp_potential_0_500_control_right,
  comp_potential_500_1500_control_left,
  comp_potential_500_1500_control_centre,
  comp_potential_500_1500_control_right,
  comp_potential_1500_4500_control_left,
  comp_potential_1500_4500_control_centre,
  comp_potential_1500_4500_control_right
)
print(comp_potential_0_500_control_left)
#LaTeX
texreg::texreg(lapply(models_comp, function(model) texreg::extract(model, include.ci = FALSE)), 
                digits = 3, stars = c(0.001, 0.01, 0.05, 0.1),
                custom.model.names = model_names_lr(c("CC1", "CC2", "CC3")),
                fontsize = "small", longtable = T, no.margin = T,
                custom.coef.names = c("Intercept",
                                      "Treatment",
                                      #Realisitc AgriPV Potential
                                      "Potential in circle 1",
                                      "Potential in circle 2",
                                      "Potential in circle 3",
                                      #Controls
                                      "Age",
                                      "Gender: Female",
                                      "Environmental score",
                                      "Familiarity with agrivoltaics",
                                      "Prior agrivoltaics preference",
                                      "Degree of urbanization"),
                groups = list("Realistic agrivoltaics potential" = 3:5, "Controls" = 6:11),
                custom.gof.names =c(NA, NA, "Observations", NA),
                reorder.gof =c(3, 1, 2, 4),
                custom.note = "\n",
                file = "tables/SI_Table_5.tex",
                use.packages = F,
                caption = "CC1: Knowledge of potential in circle 1 (0m-500m, binary), CC2: Knowledge of potential in circle 2 (500m-1500m, binary), CC3: Knowledge of potential in circle 3 (1500m-4500m, binary).
                Statistical significance was derived from linear least squares regression and two-sided tests. 
                Respective standard errors are reported in brackets and significant effects are indicated with: $^{***}$ for $p < 0.001$, $^{**}$ for $p < 0.01$, $^*$ for $p < 0.05$ and $\\cdot$ for $p < 0.1$."
)

### SI Table 6 ########################################
#Circle 1

models_comp_1 <- list(
  comp_potential_0_500_circle_split_0_control_left,
  comp_potential_0_500_circle_split_0_control_centre,
  comp_potential_0_500_circle_split_0_control_right,
  comp_potential_0_500_circle_split_1_control_left,
  comp_potential_0_500_circle_split_1_control_centre,
  comp_potential_0_500_circle_split_1_control_right
)
#Latex
texreg::texreg(lapply(models_comp_1, function(model) texreg::extract(model, include.ci = FALSE)), 
                digits = 3, stars = c(0.001, 0.01, 0.05, 0.1),
                custom.model.names = model_names_lr_wide(c("CC1, low", "CC1, high")),
                fontsize = "small", longtable = T, no.margin = T,
                custom.coef.names = c("Intercept",
                                      "Treatment",
                                      #Controls
                                      "Age",
                                      "Gender: Female",
                                      "Environmental score",
                                      "Familiarity with agrivoltaics",
                                      "Prior agrivoltaics preference",
                                      "Degree of urbanization"),
                groups = list("Controls" = 3:8),
                custom.gof.names =c(NA, NA, "Observations", NA),
                reorder.gof =c(3, 1, 2, 4),
                custom.note = "\n",
                file = "tables/SI_Table_6.tex",
                use.packages = F,
                caption = "CC1: Knowledge of potential in circle 1 (0m-500m, binary).Statistical significance was derived from linear least squares regression and two-sided tests. Respective standard errors are reported in brackets and significant effects are indicated with: $^{***}$ for $p < 0.001$, $^{**}$ for $p < 0.01$, $^*$ for $p < 0.05$ and $\\cdot$ for $p < 0.1$."
)


### SI Table 7##################################
#Circle 2
models_comp_2 <- list(
  comp_potential_500_1500_circle_split_0_control_left,
  comp_potential_500_1500_circle_split_0_control_centre,
  comp_potential_500_1500_circle_split_0_control_right,
  comp_potential_500_1500_circle_split_1_control_left,
  comp_potential_500_1500_circle_split_1_control_centre,
  comp_potential_500_1500_circle_split_1_control_right
)
#Latex
texreg::texreg(lapply(models_comp_2, function(model) texreg::extract(model, include.ci = FALSE)), 
               digits = 3, stars = c(0.001, 0.01, 0.05, 0.1),
               custom.model.names = model_names_lr_wide(c("CC2, low", "CC2, high")),
               fontsize = "small", longtable = T, no.margin = T,
               custom.coef.names = c("Intercept",
                                     "Treatment",
                                     #Controls
                                     "Age",
                                     "Gender: Female",
                                     "Environmental score",
                                     "Familiarity with agrivoltaics",
                                     "Prior agrivoltaics preference",
                                     "Degree of urbanization"),
               groups = list("Controls" = 3:8),
               custom.gof.names =c(NA, NA, "Observations", NA),
               reorder.gof =c(3, 1, 2, 4),
               custom.note = "\n",
               file = "tables/SI_Table_7.tex",
               use.packages = F,
               caption = "CC2: Knowledge of potential in circle 2 (500m-1500m, binary). Statistical significance was derived from linear least squares regression and two-sided tests. 
                Respective standard errors are reported in brackets and significant effects are indicated with: $^{***}$ for $p < 0.001$, $^{**}$ for $p < 0.01$, $^*$ for $p < 0.05$ and $\\cdot$ for $p < 0.1$."
)


### SI Table 8 #####################################
#Circle 3
models_comp_3 <- list(
  comp_potential_1500_4500_circle_split_0_control_left,
  comp_potential_1500_4500_circle_split_0_control_centre,
  comp_potential_1500_4500_circle_split_0_control_right,
  comp_potential_1500_4500_circle_split_1_control_left,
  comp_potential_1500_4500_circle_split_1_control_centre,
  comp_potential_1500_4500_circle_split_1_control_right
)
#Latex
texreg::texreg(lapply(models_comp_3, function(model) texreg::extract(model, include.ci = FALSE)), 
               digits = 3, stars = c(0.001, 0.01, 0.05, 0.1),
               custom.model.names = model_names_lr_wide(c("CC3, low", "CC3, high")),
               fontsize = "small", longtable = T, no.margin = T,
               custom.coef.names = c("Intercept",
                                     "Treatment",
                                     #Controls
                                     "Age",
                                     "Gender: Female",
                                     "Environmental score",
                                     "Familiarity with agrivoltaics",
                                     "Prior agrivoltaics preference",
                                     "Degree of urbanization"),
               groups = list("Controls" = 3:8),
               custom.gof.names =c(NA, NA, "Observations", NA),
               reorder.gof =c(3, 1, 2, 4),
               custom.note = "\n",
               file = "tables/SI_Table_8.tex",
               use.packages = F,
               caption = "CC1: Knowledge of potential in circle 3 (1500m-4500m, binary). Statistical significance was derived from linear least squares regression and two-sided tests. 
                Respective standard errors are reported in brackets and significant effects are indicated with: $^{***}$ for $p < 0.001$, $^{**}$ for $p < 0.01$, $^*$ for $p < 0.05$ and $\\cdot$ for $p < 0.1$."
)

### SI Table 9 ##################################################

models_manip <- list(
  comp_personal_advantage_control_left,
  comp_personal_advantage_control_centre,
  comp_personal_advantage_control_right,
  comp_ch_advantage_control_left,
  comp_ch_advantage_control_centre,
  comp_ch_advantage_control_right
)
#Latex
texreg::texreg(lapply(models_manip, function(model) texreg::extract(model, include.ci = FALSE)),
               digits = 3, stars = c(0.001, 0.01, 0.05, 0.1),
               custom.model.names = model_names_lr(c("DV1", "DV2")),
               fontsize = "small", longtable = T, no.margin = T,
               custom.coef.names = c("Intercept",
                                     "Treatment",
                                     #Realisitc AgriPV Potential
                                     "Potential in circle 1",
                                     "Potential in circle 2",
                                     "Potential in circle 3",
                                     #Controls
                                     "Age",
                                     "Gender: Female",
                                     "Environmental score",
                                     "Familiarity with agrivoltaics",
                                     "Prior agrivoltaics preference",
                                     "Degree of urbanization"),
               groups = list("Realistic agrivoltaics potential" = 3:5, "Controls" = 6:11),
               custom.gof.names =c(NA, NA, "Observations", NA),
               reorder.gof =c(3, 1, 2, 4),
               custom.note = "\n",
               file = "tables/SI_Table_9.tex",
               use.packages = F,
               caption = "DV1: Personal advantage from agrivoltaics (1-7 Likert scale), DV2: Advantage for Switzerland (1-7 Likert scale). Statistical significance was derived from linear least squares regression and two-sided tests. 
                Respective standard errors are reported in brackets and significant effects are indicated with: $^{***}$ for $p < 0.001$, $^{**}$ for $p < 0.01$, $^*$ for $p < 0.05$ and $\\cdot$ for $p < 0.1$."
)

### SI Table 10 #################################################
models_expansion <- list(
  comp_attitude_expansion_small_control_left,
  comp_attitude_expansion_small_control_centre,
  comp_attitude_expansion_small_control_right,
  comp_attitude_expansion_medium_control_left,
  comp_attitude_expansion_medium_control_centre,
  comp_attitude_expansion_medium_control_right,
  comp_attitude_expansion_large_control_left,
  comp_attitude_expansion_large_control_centre,
  comp_attitude_expansion_large_control_right,
  comp_attitude_expansion_control_left,
  comp_attitude_expansion_control_centre,
  comp_attitude_expansion_control_right,
  comp_attitude_expansion_nearby_control_left,
  comp_attitude_expansion_nearby_control_centre,
  comp_attitude_expansion_nearby_control_right
  
)

#LaTeX
texreg::texreg(lapply(models_expansion, function(model) texreg::extract(model, include.ci = FALSE)), 
               digits = 3, stars = c(0.001, 0.01, 0.05, 0.1),
               custom.model.names = model_names_lr(c("DV3", "DV4", "DV5", "DV6", "DV7")),
               fontsize = "small", longtable = T, no.margin = T,
               custom.coef.names = c("Intercept",
                                     "Treatment",
                                     #Realisitc AgriPV Potential
                                     "Potential in circle 1",
                                     "Potential in circle 2",
                                     "Potential in circle 3",
                                     #Controls
                                     "Age",
                                     "Gender: Female",
                                     "Environmental score",
                                     "Familiarity with agrivoltaics",
                                     "Prior agrivoltaics preference",
                                     "Degree of urbanization"),
               groups = list("Realistic agrivoltaics potential" = 3:5, "Controls" = 6:11),
               custom.gof.names =c(NA, NA, "Observations", NA),
               reorder.gof =c(3, 1, 2, 4),
               custom.note = "\n",
               file = "tables/SI_Table_10.tex",
               use.packages = F,
               caption = "DV3: Expansion \\textless{}1ha, DV4: Expansion \\textless{}5ha, DV5: Expansion \\textless{}10ha, DV6: Expansion in Switzerland, DV7: Expanion in neighbourhood. Statistical significance was derived from linear least squares regression and two-sided tests. 
                Respective standard errors are reported in brackets and significant effects are indicated with: $^{***}$ for $p < 0.001$, $^{**}$ for $p < 0.01$, $^*$ for $p < 0.05$ and $\\cdot$ for $p < 0.1$."
)

### SI Table 13 #####################################

models_policies <- list(
  dep_support_policies_control_left,
  dep_support_policies_control_centre,
  dep_support_policies_control_right,
  dep_support_policy_1_control_left,
  dep_support_policy_1_control_centre,
  dep_support_policy_1_control_right,
  dep_support_policy_4_control_left,
  dep_support_policy_4_control_centre,
  dep_support_policy_4_control_right,
  dep_support_policy_6_control_left,
  dep_support_policy_6_control_centre,
  dep_support_policy_6_control_right,
  dep_support_policy_2_control_left,
  dep_support_policy_2_control_centre,
  dep_support_policy_2_control_right
)
#LaTeX
texreg::texreg(lapply(models_policies, function(model) texreg::extract(model, include.ci = FALSE)), 
               digits = 3, stars = c(0.001, 0.01, 0.05, 0.1),
               custom.model.names = model_names_lr(c("DV8", "DV9", "DV10", "DV11", "DV12")),
               fontsize = "small", longtable = T, no.margin = T,
               custom.coef.names = c("Intercept",
                                     "Treatment",
                                     #Realisitc AgriPV Potential
                                     "Potential in circle 1",
                                     "Potential in circle 2",
                                     "Potential in circle 3",
                                     #Controls
                                     "Age",
                                     "Gender: Female",
                                     "Environmental score",
                                     "Familiarity with agrivoltaics",
                                     "Prior agrivoltaics preference",
                                     "Degree of urbanization"),
               groups = list("Realistic agrivoltaics potential" = 3:5, "Controls" = 6:11),
               custom.gof.names =c(NA, NA, "Observations", NA),
               reorder.gof =c(3, 1, 2, 4),
               custom.note = "\n",
               file = "tables/SI_Table_13.tex",
               use.packages = F,
               caption = "DV8: General policy support , DV9: Simplified approval procedures, DV10: Advisory services for farmers, DV11: Increased one-off payments, DV12: Financial support for large projects. Statistical significance was derived from linear least squares regression and two-sided tests. 
                Respective standard errors are reported in brackets and significant effects are indicated with: $^{***}$ for $p < 0.001$, $^{**}$ for $p < 0.01$, $^*$ for $p < 0.05$ and $\\cdot$ for $p < 0.1$."
)


### SI Table 14 ######################################
models_comp_int <- list(
  dep_potential_know_1_control_int_circles_left ,
  dep_potential_know_1_control_int_circles_centre ,
  dep_potential_know_1_control_int_circles_right ,
  dep_potential_know_2_control_int_circles_left ,
  dep_potential_know_2_control_int_circles_centre ,
  dep_potential_know_2_control_int_circles_right ,
  dep_potential_know_3_control_int_circles_left ,
  dep_potential_know_3_control_int_circles_centre ,
  dep_potential_know_3_control_int_circles_right 
)

#Latex
texreg::texreg(lapply(models_comp_int, function(model) texreg::extract(model, include.ci = FALSE)), 
                digits = 3, stars = c(0.001, 0.01, 0.05, 0.1),
                custom.model.names = model_names_lr(c("CC1", "CC2", "CC3")),
                fontsize = "small", longtable = T, no.margin = T,
                custom.coef.names = c("Intercept",
                                      "Treatment",
                                      #Realistic agrivoltaics potential
                                      "Potential in circle 1",
                                      "Potential in circle 2",
                                      "Potential in circle 3",
                                      #Controls
                                      "Age",
                                      "Gender: Female",
                                      "Environmental score",
                                      "Familiarity with agrivoltaics",
                                      "Prior agrivoltaics preference",
                                      "Degree of urbanization",
                                      #Interactions
                                      "NIMBY × Pot. in Circle1",
                                      "NIMBY × Pot. in Circle2",
                                      "NIMBY × Pot. in Circle3"),
                groups = list("Realistic agrivoltaics potential" = 3:5, "Controls" = 6:11, "Interactions" = 12:14),
                custom.gof.names =c(NA, NA, "Observations", NA),
                reorder.gof =c(3, 1, 2, 4),
                custom.note = "\n",
                file = "tables/SI_Table_14.tex",
                use.packages = F,
                caption = "CC1: Knowledge of potential in circle 1 (0m-500m, binary), CC2: Knowledge of potential in circle 2 (500m-1500m, binary), CC3: Knowledge of potential in circle 3 (1500m-4500m, binary). Statistical significance was derived from linear least squares regression and two-sided tests. 
                Respective standard errors are reported in brackets and significant effects are indicated with: $^{***}$ for $p < 0.001$, $^{**}$ for $p < 0.01$, $^*$ for $p < 0.05$ and $\\cdot$ for $p < 0.1$."
)

### SI Table 15 ##################

models_manip_int <- list(
  dep_personal_advantage_control_int_circles_left,
  dep_personal_advantage_control_int_circles_centre,
  dep_personal_advantage_control_int_circles_right,
  dep_ch_advantage_control_int_circles_left,
  dep_ch_advantage_control_int_circles_centre,
  dep_ch_advantage_control_int_circles_right
)

#Latex
texreg::texreg(lapply(models_manip_int, function(model) texreg::extract(model, include.ci = FALSE)),
               digits = 3, stars = c(0.001, 0.01, 0.05, 0.1),
               custom.model.names = model_names_lr(c("DV1", "DV2")),
               fontsize = "small", longtable = T, no.margin = T,
               custom.coef.names = c("Intercept",
                                     "Treatment",
                                     #Realistic agrivoltaics potential
                                     "Potential in circle 1",
                                     "Potential in circle 2",
                                     "Potential in circle 3",
                                     #Controls
                                     "Age",
                                     "Gender: Female",
                                     "Environmental score",
                                     "Familiarity with agrivoltaics",
                                     "Prior agrivoltaics preference",
                                     "Degree of urbanization",
                                     #Interactions
                                     "NIMBY × Pot. in Circle1",
                                     "NIMBY × Pot. in Circle2",
                                     "NIMBY × Pot. in Circle3"),
               groups = list("Realistic agrivoltaics potential" = 3:5, "Controls" = 6:11, "Interactions" = 12:14),
               custom.gof.names =c(NA, NA, "Observations", NA),
               reorder.gof =c(3, 1, 2, 4),
               custom.note = "\n",
               file = "tables/SI_Table_15.tex",
               use.packages = F,
               caption = "DV1: Personal advantage from agrivoltaics (1-7 Likert scale), DV2: Advantage for Switzerland (1-7 Likert scale). Statistical significance was derived from linear least squares regression and two-sided tests. 
                Respective standard errors are reported in brackets and significant effects are indicated with: $^{***}$ for $p < 0.001$, $^{**}$ for $p < 0.01$, $^*$ for $p < 0.05$ and $\\cdot$ for $p < 0.1$."
)

### SI Table 16 ##########################################

models_expansion_int <- list(
  dep_attitude_expansion_small_control_int_circles_left,
  dep_attitude_expansion_small_control_int_circles_centre,
  dep_attitude_expansion_small_control_int_circles_right,
  dep_attitude_expansion_medium_control_int_circles_left,
  dep_attitude_expansion_medium_control_int_circles_centre,
  dep_attitude_expansion_medium_control_int_circles_right,
  dep_attitude_expansion_large_control_int_circles_left,
  dep_attitude_expansion_large_control_int_circles_centre,
  dep_attitude_expansion_large_control_int_circles_right,
  dep_attitude_expansion_control_int_circles_left,
  dep_attitude_expansion_control_int_circles_centre,
  dep_attitude_expansion_control_int_circles_right,
  dep_attitude_expansion_nearby_control_int_circles_left,
  dep_attitude_expansion_nearby_control_int_circles_centre,
  dep_attitude_expansion_nearby_control_int_circles_right
)

#Latex

texreg::texreg(lapply(models_expansion_int, function(model) texreg::extract(model, include.ci = FALSE)), 
               digits = 3, stars = c(0.001, 0.01, 0.05, 0.1),
               custom.model.names = model_names_lr(c("DV3", "DV4", "DV5", "DV6", "DV7")),
               fontsize = "small", longtable = T, no.margin = T,
               custom.coef.names = c("Intercept",
                                     "Treatment",
                                     #Realistic agrivoltaics potential
                                     "Potential in circle 1",
                                     "Potential in circle 2",
                                     "Potential in circle 3",
                                     #Controls
                                     "Age",
                                     "Gender: Female",
                                     "Environmental score",
                                     "Familiarity with agrivoltaics",
                                     "Prior agrivoltaics preference",
                                     "Degree of urbanization",
                                     #Interactions
                                     "NIMBY × Pot. in Circle1",
                                     "NIMBY × Pot. in Circle2",
                                     "NIMBY × Pot. in Circle3"),
               groups = list("Realistic agrivoltaics potential" = 3:5, "Controls" = 6:11, "Interactions" = 12:14),
               custom.gof.names =c(NA, NA, "Observations", NA),
               reorder.gof =c(3, 1, 2, 4),
               custom.note = "\n",
               file = "tables/SI_Table_16.tex",
               use.packages = F,
               caption = "DV3: Expansion \\textless{}1ha, DV4: Expansion \\textless{}5ha, DV5: Expansion \\textless{}10ha, DV6: Expansion in Switzerland, DV7: Expanion in neighbourhood. Statistical significance was derived from linear least squares regression and two-sided tests. 
                Respective standard errors are reported in brackets and significant effects are indicated with: $^{***}$ for $p < 0.001$, $^{**}$ for $p < 0.01$, $^*$ for $p < 0.05$ and $\\cdot$ for $p < 0.1$."
)


### SI Table 17 #################################################

models_policies_int <- list(
  dep_support_policies_control_int_circles_left,
  dep_support_policies_control_int_circles_centre,
  dep_support_policies_control_int_circles_right,
  dep_support_policy_1_control_int_circles_left,
  dep_support_policy_1_control_int_circles_centre,
  dep_support_policy_1_control_int_circles_right,
  dep_support_policy_4_control_int_circles_left,
  dep_support_policy_4_control_int_circles_centre,
  dep_support_policy_4_control_int_circles_right,
  dep_support_policy_6_control_int_circles_left,
  dep_support_policy_6_control_int_circles_centre,
  dep_support_policy_6_control_int_circles_right,
  dep_support_policy_2_control_int_circles_left,
  dep_support_policy_2_control_int_circles_centre,
  dep_support_policy_2_control_int_circles_right
)

#LaTeX
texreg::texreg(lapply(models_policies_int, function(model) texreg::extract(model, include.ci = FALSE)), 
               digits = 3, stars = c(0.001, 0.01, 0.05, 0.1),
               custom.model.names = model_names_lr(c("DV8", "DV9", "DV10", "DV11", "DV12")),
               fontsize = "small", longtable = T, no.margin = T,
               custom.coef.names = c("Intercept",
                                     "Treatment",
                                     #Realistic agrivoltaics potential
                                     "Potential in circle 1",
                                     "Potential in circle 2",
                                     "Potential in circle 3",
                                     #Controls
                                     "Age",
                                     "Gender: Female",
                                     "Environmental score",
                                     "Familiarity with agrivoltaics",
                                     "Prior agrivoltaics preference",
                                     "Degree of urbanization",
                                     #Interactions
                                     "NIMBY × Pot. in Circle1",
                                     "NIMBY × Pot. in Circle2",
                                     "NIMBY × Pot. in Circle3"),
               groups = list("Realistic agrivoltaics potential" = 3:5, "Controls" = 6:11, "Interactions" = 12:14),
               custom.gof.names =c(NA, NA, "Observations", NA),
               reorder.gof =c(3, 1, 2, 4),
               custom.note = "\n",
               file = "tables/SI_Table_17.tex",
               use.packages = F,
               caption = "DV8: General policy support , DV9: Simplified approval procedures, DV10: Advisory services for farmers, DV11: Increased one-off payments, DV12: Financial support for large projects. Statistical significance was derived from linear least squares regression and two-sided tests. 
                Respective standard errors are reported in brackets and significant effects are indicated with: $^{***}$ for $p < 0.001$, $^{**}$ for $p < 0.01$, $^*$ for $p < 0.05$ and $\\cdot$ for $p < 0.1$."
)



## Degree of urbanization ##################################################



### SI Table 18 ###################################################

#### Comprehension Check
models_comp <- list(
  comp_potential_0_500_control_urban,
  comp_potential_0_500_control_suburban,
  comp_potential_0_500_control_rural,
  comp_potential_500_1500_control_urban,
  comp_potential_500_1500_control_suburban,
  comp_potential_500_1500_control_rural,
  comp_potential_1500_4500_control_urban,
  comp_potential_1500_4500_control_suburban,
  comp_potential_1500_4500_control_rural
)

#LaTeX
texreg::texreg(lapply(models_comp, function(model) texreg::extract(model, include.ci = FALSE)), 
               digits = 3, stars = c(0.001, 0.01, 0.05, 0.1),
               custom.model.names = model_names_ur(c("CC1", "CC2", "CC3")),
               fontsize = "small", longtable = T, no.margin = T,
               custom.coef.names = c("Intercept",
                                     "Treatment",
                                     #Realisitc AgriPV Potential
                                     "Potential in circle 1",
                                     "Potential in circle 2",
                                     "Potential in circle 3",
                                     #Controls
                                     "Age",
                                     "Gender: Female",
                                     "Environmental score",
                                     "Familiarity with agrivoltaics",
                                     "Prior agrivoltaics preference",
                                     "Political orientation"),
               groups = list("Realistic agrivoltaics potential" = 3:5, "Controls" = 6:11),
               custom.gof.names =c(NA, NA, "Observations", NA),
               reorder.gof =c(3, 1, 2, 4),
               custom.note = "\n",
               file = "tables/SI_Table_18.tex",
               use.packages = F,
               caption = "CC1: Knowledge of potential in circle 1 (0m-500m, binary), CC2: Knowledge of potential in circle 2 (500m-1500m, binary), CC3: Knowledge of potential in circle 3 (1500m-4500m, binary). Statistical significance was derived from linear least squares regression and two-sided tests. Respective standard errors are reported in brackets and significant effects are indicated with: $^{***}$ for $p < 0.001$, $^{**}$ for $p < 0.01$, $^*$ for $p < 0.05$ and $\\cdot$ for $p < 0.1$."
)


### SI Table 19 ########################################
#Circle 1

models_comp_1 <- list(
  comp_potential_0_500_circle_split_0_control_urban,
  comp_potential_0_500_circle_split_0_control_suburban,
  comp_potential_0_500_circle_split_0_control_rural,
  comp_potential_0_500_circle_split_1_control_urban,
  comp_potential_0_500_circle_split_1_control_suburban,
  comp_potential_0_500_circle_split_1_control_rural
)
#Latex
texreg::texreg(lapply(models_comp_1, function(model) texreg::extract(model, include.ci = FALSE)), 
               digits = 3, stars = c(0.001, 0.01, 0.05, 0.1),
               custom.model.names = model_names_ur_wide(c("CC1, low", "CC1, high")),
               fontsize = "small", longtable = T, no.margin = T,
               custom.coef.names = c("Intercept",
                                     "Treatment",
                                     #Controls
                                     "Age",
                                     "Gender: Female",
                                     "Environmental score",
                                     "Familiarity with agrivoltaics",
                                     "Prior agrivoltaics preference",
                                     "Political orientation"),
               groups = list("Controls" = 3:8),
               custom.gof.names =c(NA, NA, "Observations", NA),
               reorder.gof =c(3, 1, 2, 4),
               custom.note = "\n",
               file = "tables/SI_Table_19.tex",
               use.packages = F,
               caption = "CC1: Knowledge of potential in circle 1 (0m-500m, binary). Statistical significance was derived from linear least squares regression and two-sided tests. Respective standard errors are reported in brackets and significant effects are indicated with: $^{***}$ for $p < 0.001$, $^{**}$ for $p < 0.01$, $^*$ for $p < 0.05$ and $\\cdot$ for $p < 0.1$."
)


### SI Table 20##################################
#Circle 2
models_comp_2 <- list(
  comp_potential_500_1500_circle_split_0_control_urban,
  comp_potential_500_1500_circle_split_0_control_suburban,
  comp_potential_500_1500_circle_split_0_control_rural,
  comp_potential_500_1500_circle_split_1_control_urban,
  comp_potential_500_1500_circle_split_1_control_suburban,
  comp_potential_500_1500_circle_split_1_control_rural
)
#Latex
texreg::texreg(lapply(models_comp_2, function(model) texreg::extract(model, include.ci = FALSE)), 
               digits = 3, stars = c(0.001, 0.01, 0.05, 0.1),
               custom.model.names = model_names_ur_wide(c("CC2, low", "CC2, high")),
               fontsize = "small", longtable = T, no.margin = T,
               custom.coef.names = c("Intercept",
                                     "Treatment",
                                     #Controls
                                     "Age",
                                     "Gender: Female",
                                     "Environmental score",
                                     "Familiarity with agrivoltaics",
                                     "Prior agrivoltaics preference",
                                     "Political orientation"),
               groups = list("Controls" = 3:8),
               custom.gof.names =c(NA, NA, "Observations", NA),
               reorder.gof =c(3, 1, 2, 4),
               custom.note = "\n",
               file = "tables/SI_Table_20.tex",
               use.packages = F,
               caption = "CC2: Knowledge of potential in circle 2 (500m-1500m, binary). Statistical significance was derived from linear least squares regression and two-sided tests. Respective standard errors are reported in brackets and significant effects are indicated with: $^{***}$ for $p < 0.001$, $^{**}$ for $p < 0.01$, $^*$ for $p < 0.05$ and $\\cdot$ for $p < 0.1$."
)


### SI Table 21 #####################################
#Circle 3
models_comp_3 <- list(
  comp_potential_1500_4500_circle_split_0_control_urban,
  comp_potential_1500_4500_circle_split_0_control_suburban,
  comp_potential_1500_4500_circle_split_0_control_rural,
  comp_potential_1500_4500_circle_split_1_control_urban,
  comp_potential_1500_4500_circle_split_1_control_suburban,
  comp_potential_1500_4500_circle_split_1_control_rural
)
#Latex
texreg::texreg(lapply(models_comp_3, function(model) texreg::extract(model, include.ci = FALSE)), 
               digits = 3, stars = c(0.001, 0.01, 0.05, 0.1),
               custom.model.names = model_names_ur_wide(c("CC3, low", "CC3, high")),
               fontsize = "small", longtable = T, no.margin = T,
               custom.coef.names = c("Intercept",
                                     "Treatment",
                                     #Controls
                                     "Age",
                                     "Gender: Female",
                                     "Environmental score",
                                     "Familiarity with agrivoltaics",
                                     "Prior agrivoltaics preference",
                                     "Political orientation"),
               groups = list("Controls" = 3:8),
               custom.gof.names =c(NA, NA, "Observations", NA),
               reorder.gof =c(3, 1, 2, 4),
               custom.note = "\n",
               file = "tables/SI_Table_21.tex",
               use.packages = F,
               caption = "CC1: Knowledge of potential in circle 3 (1500m-4500m, binary). Statistical significance was derived from linear least squares regression and two-sided tests. Respective standard errors are reported in brackets and significant effects are indicated with: $^{***}$ for $p < 0.001$, $^{**}$ for $p < 0.01$, $^*$ for $p < 0.05$ and $\\cdot$ for $p < 0.1$."
)

### SI Table 22 ##################################################

models_manip <- list(
  comp_personal_advantage_control_urban,
  comp_personal_advantage_control_suburban,
  comp_personal_advantage_control_rural,
  comp_ch_advantage_control_urban,
  comp_ch_advantage_control_suburban,
  comp_ch_advantage_control_rural
)
#Latex
texreg::texreg(lapply(models_manip, function(model) texreg::extract(model, include.ci = FALSE)),
               digits = 3, stars = c(0.001, 0.01, 0.05, 0.1),
               custom.model.names = model_names_ur(c("DV1", "DV2")),
               fontsize = "small", longtable = T, no.margin = T,
               custom.coef.names = c("Intercept",
                                     "Treatment",
                                     #Realisitc AgriPV Potential
                                     "Potential in circle 1",
                                     "Potential in circle 2",
                                     "Potential in circle 3",
                                     #Controls
                                     "Age",
                                     "Gender: Female",
                                     "Environmental score",
                                     "Familiarity with agrivoltaics",
                                     "Prior agrivoltaics preference",
                                     "Political orientation"),
               groups = list("Realistic agrivoltaics potential" = 3:5, "Controls" = 6:11),
               custom.gof.names =c(NA, NA, "Observations", NA),
               reorder.gof =c(3, 1, 2, 4),
               custom.note = "\n",
               file = "tables/SI_Table_22.tex",
               use.packages = F,
               caption = "DV1: Personal advantage from agrivoltaics (1-7 Likert scale), DV2: Advantage for Switzerland (1-7 Likert scale). Statistical significance was derived from linear least squares regression and two-sided tests. Respective standard errors are reported in brackets and significant effects are indicated with: $^{***}$ for $p < 0.001$, $^{**}$ for $p < 0.01$, $^*$ for $p < 0.05$ and $\\cdot$ for $p < 0.1$."
)

### SI Table 23 #################################################
models_expansion <- list(
  comp_attitude_expansion_small_control_urban,
  comp_attitude_expansion_small_control_suburban,
  comp_attitude_expansion_small_control_rural,
  comp_attitude_expansion_medium_control_urban,
  comp_attitude_expansion_medium_control_suburban,
  comp_attitude_expansion_medium_control_rural,
  comp_attitude_expansion_large_control_urban,
  comp_attitude_expansion_large_control_suburban,
  comp_attitude_expansion_large_control_rural,
  comp_attitude_expansion_control_urban,
  comp_attitude_expansion_control_suburban,
  comp_attitude_expansion_control_rural,
  comp_attitude_expansion_nearby_control_urban,
  comp_attitude_expansion_nearby_control_suburban,
  comp_attitude_expansion_nearby_control_rural
  
)

#LaTeX
texreg::texreg(lapply(models_expansion, function(model) texreg::extract(model, include.ci = FALSE)), 
               digits = 3, stars = c(0.001, 0.01, 0.05, 0.1),
               custom.model.names = model_names_ur(c("DV3", "DV4", "DV5", "DV6", "DV7")),
               fontsize = "small", longtable = T, no.margin = T,
               custom.coef.names = c("Intercept",
                                     "Treatment",
                                     #Realisitc AgriPV Potential
                                     "Potential in circle 1",
                                     "Potential in circle 2",
                                     "Potential in circle 3",
                                     #Controls
                                     "Age",
                                     "Gender: Female",
                                     "Environmental score",
                                     "Familiarity with agrivoltaics",
                                     "Prior agrivoltaics preference",
                                     "Political orientation"),
               groups = list("Realistic agrivoltaics potential" = 3:5, "Controls" = 6:11),
               custom.gof.names =c(NA, NA, "Observations", NA),
               reorder.gof =c(3, 1, 2, 4),
               custom.note = "\n",
               file = "tables/SI_Table_23.tex",
               use.packages = F,
               caption = "DV3: Expansion \\textless{}1ha, DV4: Expansion \\textless{}5ha, DV5: Expansion \\textless{}10ha, DV6: Expansion in Switzerland, DV7: Expanion in neighbourhood. Statistical significance was derived from linear least squares regression and two-sided tests. Respective standard errors are reported in brackets and significant effects are indicated with: $^{***}$ for $p < 0.001$, $^{**}$ for $p < 0.01$, $^*$ for $p < 0.05$ and $\\cdot$ for $p < 0.1$."
)


### SI Table 26 #####################################

models_policies <- list(
  dep_support_policies_control_urban,
  dep_support_policies_control_suburban,
  dep_support_policies_control_rural,
  dep_support_policy_1_control_urban,
  dep_support_policy_1_control_suburban,
  dep_support_policy_1_control_rural,
  dep_support_policy_4_control_urban,
  dep_support_policy_4_control_suburban,
  dep_support_policy_4_control_rural,
  dep_support_policy_6_control_urban,
  dep_support_policy_6_control_suburban,
  dep_support_policy_6_control_rural,
  dep_support_policy_2_control_urban,
  dep_support_policy_2_control_suburban,
  dep_support_policy_2_control_rural
)
#LaTeX
texreg::texreg(lapply(models_policies, function(model) texreg::extract(model, include.ci = FALSE)), 
               digits = 3, stars = c(0.001, 0.01, 0.05, 0.1),
               custom.model.names = model_names_ur(c("DV8", "DV9", "DV10", "DV11", "DV12")),
               fontsize = "small", longtable = T, no.margin = T,
               custom.coef.names = c("Intercept",
                                     "Treatment",
                                     #Realisitc AgriPV Potential
                                     "Potential in circle 1",
                                     "Potential in circle 2",
                                     "Potential in circle 3",
                                     #Controls
                                     "Age",
                                     "Gender: Female",
                                     "Environmental score",
                                     "Familiarity with agrivoltaics",
                                     "Prior agrivoltaics preference",
                                     "Political orientation"),
               groups = list("Realistic agrivoltaics potential" = 3:5, "Controls" = 6:11),
               custom.gof.names =c(NA, NA, "Observations", NA),
               reorder.gof =c(3, 1, 2, 4),
               custom.note = "\n",
               file = "tables/SI_Table_26.tex",
               use.packages = F,
               caption = "DV8: General policy support , DV9: Simplified approval procedures, DV10: Advisory services for farmers, DV11: Increased one-off payments, DV12: Financial support for large projects. Statistical significance was derived from linear least squares regression and two-sided tests. Respective standard errors are reported in brackets and significant effects are indicated with: $^{***}$ for $p < 0.001$, $^{**}$ for $p < 0.01$, $^*$ for $p < 0.05$ and $\\cdot$ for $p < 0.1$."
)




### SI Table 27 ######################################
models_comp_int <- list(
  dep_potential_know_1_control_int_circles_urban ,
  dep_potential_know_1_control_int_circles_suburban ,
  dep_potential_know_1_control_int_circles_rural ,
  dep_potential_know_2_control_int_circles_urban ,
  dep_potential_know_2_control_int_circles_suburban ,
  dep_potential_know_2_control_int_circles_rural ,
  dep_potential_know_3_control_int_circles_urban ,
  dep_potential_know_3_control_int_circles_suburban ,
  dep_potential_know_3_control_int_circles_rural 
)

#Latex
texreg::texreg(lapply(models_comp_int, function(model) texreg::extract(model, include.ci = FALSE)), 
               digits = 3, stars = c(0.001, 0.01, 0.05, 0.1),
               custom.model.names = model_names_ur(c("CC1", "CC2", "CC3")),
               fontsize = "small", longtable = T, no.margin = T,
               custom.coef.names = c("Intercept",
                                     "Treatment",
                                     #Realistic agrivoltaics potential
                                     "Potential in circle 1",
                                     "Potential in circle 2",
                                     "Potential in circle 3",
                                     #Controls
                                     "Age",
                                     "Gender: Female",
                                     "Environmental score",
                                     "Familiarity with agrivoltaics",
                                     "Prior agrivoltaics preference",
                                     "Political orientation",
                                     #Interactions
                                     "NIMBY × Pot. in Circle1",
                                     "NIMBY × Pot. in Circle2",
                                     "NIMBY × Pot. in Circle3"),
               groups = list("Realistic agrivoltaics potential" = 3:5, "Controls" = 6:11, "Interactions" = 12:14),
               custom.gof.names =c(NA, NA, "Observations", NA),
               reorder.gof =c(3, 1, 2, 4),
               custom.note = "\n",
               file = "tables/SI_Table_27.tex",
               use.packages = F,
               caption = "CC1: Knowledge of potential in circle 1 (0m-500m, binary), CC2: Knowledge of potential in circle 2 (500m-1500m, binary), CC3: Knowledge of potential in circle 3 (1500m-4500m, binary). Statistical significance was derived from linear least squares regression and two-sided tests. Respective standard errors are reported in brackets and significant effects are indicated with: $^{***}$ for $p < 0.001$, $^{**}$ for $p < 0.01$, $^*$ for $p < 0.05$ and $\\cdot$ for $p < 0.1$."
)






### SI Table 28 ##################

models_manip_int <- list(
  dep_personal_advantage_control_int_circles_urban,
  dep_personal_advantage_control_int_circles_suburban,
  dep_personal_advantage_control_int_circles_rural,
  dep_ch_advantage_control_int_circles_urban,
  dep_ch_advantage_control_int_circles_suburban,
  dep_ch_advantage_control_int_circles_rural
)

#Latex
texreg::texreg(lapply(models_manip_int, function(model) texreg::extract(model, include.ci = FALSE)),
               digits = 3, stars = c(0.001, 0.01, 0.05, 0.1),
               custom.model.names = model_names_ur(c("DV1", "DV2")),
               fontsize = "small", longtable = T, no.margin = T,
               custom.coef.names = c("Intercept",
                                     "Treatment",
                                     #Realistic agrivoltaics potential
                                     "Potential in circle 1",
                                     "Potential in circle 2",
                                     "Potential in circle 3",
                                     #Controls
                                     "Age",
                                     "Gender: Female",
                                     "Environmental score",
                                     "Familiarity with agrivoltaics",
                                     "Prior agrivoltaics preference",
                                     "Political orientation",
                                     #Interactions
                                     "NIMBY × Pot. in Circle1",
                                     "NIMBY × Pot. in Circle2",
                                     "NIMBY × Pot. in Circle3"),
               groups = list("Realistic agrivoltaics potential" = 3:5, "Controls" = 6:11, "Interactions" = 12:14),
               custom.gof.names =c(NA, NA, "Observations", NA),
               reorder.gof =c(3, 1, 2, 4),
               custom.note = "\n",
               file = "tables/SI_Table_28.tex",
               use.packages = F,
               caption = "DV1: Personal advantage from agrivoltaics (1-7 Likert scale), DV2: Advantage for Switzerland (1-7 Likert scale). Statistical significance was derived from linear least squares regression and two-sided tests. Respective standard errors are reported in brackets and significant effects are indicated with: $^{***}$ for $p < 0.001$, $^{**}$ for $p < 0.01$, $^*$ for $p < 0.05$ and $\\cdot$ for $p < 0.1$."
)




### SI Table 29 ##########################################

models_expansion_int <- list(
  dep_attitude_expansion_small_control_int_circles_urban,
  dep_attitude_expansion_small_control_int_circles_suburban,
  dep_attitude_expansion_small_control_int_circles_rural,
  dep_attitude_expansion_medium_control_int_circles_urban,
  dep_attitude_expansion_medium_control_int_circles_suburban,
  dep_attitude_expansion_medium_control_int_circles_rural,
  dep_attitude_expansion_large_control_int_circles_urban,
  dep_attitude_expansion_large_control_int_circles_suburban,
  dep_attitude_expansion_large_control_int_circles_rural,
  dep_attitude_expansion_control_int_circles_urban,
  dep_attitude_expansion_control_int_circles_suburban,
  dep_attitude_expansion_control_int_circles_rural,
  dep_attitude_expansion_nearby_control_int_circles_urban,
  dep_attitude_expansion_nearby_control_int_circles_suburban,
  dep_attitude_expansion_nearby_control_int_circles_rural
)

#Latex

texreg::texreg(lapply(models_expansion_int, function(model) texreg::extract(model, include.ci = FALSE)), 
               digits = 3, stars = c(0.001, 0.01, 0.05, 0.1),
               custom.model.names = model_names_ur(c("DV3", "DV4", "DV5", "DV6", "DV7")),
               fontsize = "small", longtable = T, no.margin = T,
               custom.coef.names = c("Intercept",
                                     "Treatment",
                                     #Realistic agrivoltaics potential
                                     "Potential in circle 1",
                                     "Potential in circle 2",
                                     "Potential in circle 3",
                                     #Controls
                                     "Age",
                                     "Gender: Female",
                                     "Environmental score",
                                     "Familiarity with agrivoltaics",
                                     "Prior agrivoltaics preference",
                                     "Political orientation",
                                     #Interactions
                                     "NIMBY × Pot. in Circle1",
                                     "NIMBY × Pot. in Circle2",
                                     "NIMBY × Pot. in Circle3"),
               groups = list("Realistic agrivoltaics potential" = 3:5, "Controls" = 6:11, "Interactions" = 12:14),
               custom.gof.names =c(NA, NA, "Observations", NA),
               reorder.gof =c(3, 1, 2, 4),
               custom.note = "\n",
               file = "tables/SI_Table_29.tex",
               use.packages = F,
               caption = "DV3: Expansion \\textless{}1ha, DV4: Expansion \\textless{}5ha, DV5: Expansion \\textless{}10ha, DV6: Expansion in Switzerland, DV7: Expanion in neighbourhood. Statistical significance was derived from linear least squares regression and two-sided tests. Respective standard errors are reported in brackets and significant effects are indicated with: $^{***}$ for $p < 0.001$, $^{**}$ for $p < 0.01$, $^*$ for $p < 0.05$ and $\\cdot$ for $p < 0.1$."
)







### SI Table 30 #################################################

models_policies_int <- list(
  dep_support_policies_control_int_circles_urban,
  dep_support_policies_control_int_circles_suburban,
  dep_support_policies_control_int_circles_rural,
  dep_support_policy_1_control_int_circles_urban,
  dep_support_policy_1_control_int_circles_suburban,
  dep_support_policy_1_control_int_circles_rural,
  dep_support_policy_4_control_int_circles_urban,
  dep_support_policy_4_control_int_circles_suburban,
  dep_support_policy_4_control_int_circles_rural,
  dep_support_policy_6_control_int_circles_urban,
  dep_support_policy_6_control_int_circles_suburban,
  dep_support_policy_6_control_int_circles_rural,
  dep_support_policy_2_control_int_circles_urban,
  dep_support_policy_2_control_int_circles_suburban,
  dep_support_policy_2_control_int_circles_rural
)

#LaTeX
texreg::texreg(lapply(models_policies_int, function(model) texreg::extract(model, include.ci = FALSE)), 
               digits = 3, stars = c(0.001, 0.01, 0.05, 0.1),
               custom.model.names = model_names_ur(c("DV8", "DV9", "DV10", "DV11", "DV12")),
               fontsize = "small", longtable = T, no.margin = T,
               custom.coef.names = c("Intercept",
                                     "Treatment",
                                     #Realistic agrivoltaics potential
                                     "Potential in circle 1",
                                     "Potential in circle 2",
                                     "Potential in circle 3",
                                     #Controls
                                     "Age",
                                     "Gender: Female",
                                     "Environmental score",
                                     "Familiarity with agrivoltaics",
                                     "Prior agrivoltaics preference",
                                     "Political orientation",
                                     #Interactions
                                     "NIMBY × Pot. in Circle1",
                                     "NIMBY × Pot. in Circle2",
                                     "NIMBY × Pot. in Circle3"),
               groups = list("Realistic agrivoltaics potential" = 3:5, "Controls" = 6:11, "Interactions" = 12:14),
               custom.gof.names =c(NA, NA, "Observations", NA),
               reorder.gof =c(3, 1, 2, 4),
               custom.note = "\n",
               file = "tables/SI_Table_30.tex",
               use.packages = F,
               caption = "DV8: General policy support , DV9: Simplified approval procedures, DV10: Advisory services for farmers, DV11: Increased one-off payments, DV12: Financial support for large projects. Statistical significance was derived from linear least squares regression and two-sided tests. Respective standard errors are reported in brackets and significant effects are indicated with: $^{***}$ for $p < 0.001$, $^{**}$ for $p < 0.01$, $^*$ for $p < 0.05$ and $\\cdot$ for $p < 0.1$."
)


# Figures #####################################################################

theme_plot <- theme_light() +
  theme(legend.position = "right",
        plot.title = element_text(size = 9, face = "bold", hjust = 0.5),
        axis.text = element_text(size = 8, color = "black"),
        axis.title = element_text(size = 8),
        strip.text = element_text(size = 8),
        legend.title = element_text(size = 9),
        legend.text = element_text(size = 8),
        axis.text.x = element_text(angle = 0, hjust = 0.5),
        axis.title.x = element_text(margin = margin(t=6)))

vals_legend <- c("#4059AD", "grey", "#CCA43B")


## Functions ##############################

create_emm_plot <- function(data, title, y_label = NULL, y_limits, y_breaks, show_legend = FALSE) {
  p <- data %>%
    mutate(NIMBY = factor(NIMBY, levels = c(0,1), labels = c("Control", "Treatment")),
           left_right = factor(left_right, levels = c("Left", "Centre", "Right"))) %>%
    ggplot(aes(x = NIMBY, y = emmean, group = left_right, col = left_right)) +
    geom_point(size = 2, position = position_dodge(0.3)) +
    geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.0, linewidth = 0.8, position = position_dodge(0.3)) +
    scale_y_continuous(limits = y_limits, 
                       breaks = y_breaks) +
    scale_color_manual(name = "Political orientation", values = vals_legend) +
    scale_fill_manual(name = "Political orientation", values = vals_legend) +
    labs(title = title, x = NULL, y = y_label) +
    theme_plot
  
  if (!show_legend) {
    p <- p + theme(legend.position = "none")
  }
  return(p)
}

create_emm_plot_ur <- function(data, title, y_label = NULL, y_limits, y_breaks, show_legend = FALSE) {
  p <- data %>%
    mutate(NIMBY = factor(NIMBY, levels = c(0,1), labels = c("Control", "Treatment")),
           urban_rural = factor(urban_rural, levels = c("Urban", "Suburban", "Rural"))) %>%
    ggplot(aes(x = NIMBY, y = emmean, group = urban_rural, col = urban_rural)) +
    geom_point(size = 2, position = position_dodge(0.3)) +
    geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.0, linewidth = 0.8, position = position_dodge(0.3)) +
    scale_y_continuous(limits = y_limits, 
                       breaks = y_breaks) +
    scale_color_manual(name = "Degree of urbanization", values = vals_legend) +
    scale_fill_manual(name = "Degree of urbanization", values = vals_legend) +
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

## Manuscript figures ########################
### Figure 3 Attitude #######################################
# Expansion <1ha
means_expansion_small_left <- emmeans(comp_attitude_expansion_small_control_left, "NIMBY")
means_expansion_small_centre <- emmeans(comp_attitude_expansion_small_control_centre, "NIMBY")
means_expansion_small_right <- emmeans(comp_attitude_expansion_small_control_right, "NIMBY")

# Expansion <5ha
means_expansion_medium_left <- emmeans(comp_attitude_expansion_medium_control_left, "NIMBY")
means_expansion_medium_centre <- emmeans(comp_attitude_expansion_medium_control_centre, "NIMBY")
means_expansion_medium_right <- emmeans(comp_attitude_expansion_medium_control_right, "NIMBY")

# Expansion <10ha
means_expansion_large_left <- emmeans(comp_attitude_expansion_large_control_left, "NIMBY")
means_expansion_large_centre <- emmeans(comp_attitude_expansion_large_control_centre, "NIMBY")
means_expansion_large_right <- emmeans(comp_attitude_expansion_large_control_right, "NIMBY")

# Expansion in CH
means_expansion_ch_left <- emmeans(comp_attitude_expansion_control_left, "NIMBY")
means_expansion_ch_centre <- emmeans(comp_attitude_expansion_control_centre, "NIMBY")
means_expansion_ch_right <- emmeans(comp_attitude_expansion_control_right, "NIMBY")

#Expansion in Neighbourhood
means_expansion_nearby_left <- emmeans(comp_attitude_expansion_nearby_control_left, "NIMBY")
means_expansion_nearby_centre <- emmeans(comp_attitude_expansion_nearby_control_centre, "NIMBY")
means_expansion_nearby_right <- emmeans(comp_attitude_expansion_nearby_control_right, "NIMBY")

means_expansion_small <- bind_rows(
  as.data.frame(means_expansion_small_left) %>% mutate(left_right = "Left"),
  as.data.frame(means_expansion_small_centre) %>% mutate(left_right = "Centre"),
  as.data.frame(means_expansion_small_right) %>% mutate(left_right = "Right")
)

means_expansion_medium <- bind_rows(
  as.data.frame(means_expansion_medium_left) %>% mutate(left_right = "Left"),
  as.data.frame(means_expansion_medium_centre) %>% mutate(left_right = "Centre"),
  as.data.frame(means_expansion_medium_right) %>% mutate(left_right = "Right")
)

means_expansion_large <- bind_rows(
  as.data.frame(means_expansion_large_left) %>% mutate(left_right = "Left"),
  as.data.frame(means_expansion_large_centre) %>% mutate(left_right = "Centre"),
  as.data.frame(means_expansion_large_right) %>% mutate(left_right = "Right")
)

means_expansion_ch <- bind_rows(
  as.data.frame(means_expansion_ch_left) %>% mutate(left_right = "Left"),
  as.data.frame(means_expansion_ch_centre) %>% mutate(left_right = "Centre"),
  as.data.frame(means_expansion_ch_right) %>% mutate(left_right = "Right")
)

means_expansion_nearby <- bind_rows(
  as.data.frame(means_expansion_nearby_left) %>% mutate(left_right = "Left"),
  as.data.frame(means_expansion_nearby_centre) %>% mutate(left_right = "Centre"),
  as.data.frame(means_expansion_nearby_right) %>% mutate(left_right = "Right")
)

# Small expansion (<1ha)
attitude_small <- create_emm_plot(
  data = means_expansion_small,
  title = "Expansion <1ha",
  y_label = "Estimated mean",
  y_limits = c(3, 5.8),
  y_breaks = seq(3, 5.5, 0.5)
)

# Medium expansion (<5ha)
attitude_medium <- create_emm_plot(
  data = means_expansion_medium,
  title = "Expansion <5ha",
  y_label = NULL,
  y_limits = c(3, 5.8),
  y_breaks = seq(3, 5.5, 0.5)
)

# Large expansion (<10ha)
attitude_large <- create_emm_plot(
  data = means_expansion_large,
  title = "Expansion <10ha",
  y_label = NULL,
  y_limits = c(3, 5.8),
  y_breaks = seq(3, 5.5, 0.5)
)

# Expansion in CH
attitude_CH <- create_emm_plot(
  data = means_expansion_ch,
  title = "Expansion in Switzerland",
  y_label = "Estimated mean",
  y_limits = c(3, 5.8),
  y_breaks = seq(3, 5.5, 0.5)
)

# Expansion in Neighbourhood
attitude_nearby <- create_emm_plot(
  data = means_expansion_nearby,
  title = "Expansion in neighbourhood",
  y_label = NULL,
  y_limits = c(3, 5.8),
  y_breaks = seq(3, 5.5, 0.5),
  show_legend = TRUE
)

empty_plot <- ggplot() + theme_void()

attitude_small <- attitude_small + labs(tag = "a)") + theme(plot.tag = element_text(face = "bold", size = 10))
attitude_medium <- attitude_medium + labs(tag = "b)") + theme(plot.tag = element_text(face = "bold", size = 10))
attitude_large <- attitude_large + labs(tag = "c)") + theme(plot.tag = element_text(face = "bold", size = 10))
attitude_CH <- attitude_CH + labs(tag = "d)") + theme(plot.tag = element_text(face = "bold", size = 10))
attitude_nearby <- attitude_nearby + labs(tag = "e)") + theme(plot.tag = element_text(face = "bold", size = 10))

design <- "abc \n def"
final_plot <- attitude_small + attitude_medium + attitude_large + attitude_CH + attitude_nearby + guide_area() + 
  
  plot_layout(design = design, 
              guides = "collect" ) +
  plot_annotation(
    title = "",
    theme = theme(
      plot.title = element_text(size = 11, face = "bold")
    )
  ) &
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    legend.text = element_text(size=8)
  )


ggsave(filename = "figures/Figure_3.pdf", plot = final_plot, width = 180, height = 120, units = "mm")




### Figure 5 Policies ###############################################
# Get estimated marginal means
means_support_policies_left <- emmeans(dep_support_policies_control_left, "NIMBY")
means_support_policies_centre <- emmeans(dep_support_policies_control_centre, "NIMBY")
means_support_policies_right <- emmeans(dep_support_policies_control_right, "NIMBY")

means_support_policy_1_left <- emmeans(dep_support_policy_1_control_left, "NIMBY")
means_support_policy_1_centre <- emmeans(dep_support_policy_1_control_centre, "NIMBY")
means_support_policy_1_right <- emmeans(dep_support_policy_1_control_right, "NIMBY")

means_support_policy_4_left <- emmeans(dep_support_policy_4_control_left, "NIMBY")
means_support_policy_4_centre <- emmeans(dep_support_policy_4_control_centre, "NIMBY")
means_support_policy_4_right <- emmeans(dep_support_policy_4_control_right, "NIMBY")

means_support_policy_6_left <- emmeans(dep_support_policy_6_control_left, "NIMBY")
means_support_policy_6_centre <- emmeans(dep_support_policy_6_control_centre, "NIMBY")
means_support_policy_6_right <- emmeans(dep_support_policy_6_control_right, "NIMBY")

means_support_policy_2_left <- emmeans(dep_support_policy_2_control_left, "NIMBY")
means_support_policy_2_centre <- emmeans(dep_support_policy_2_control_centre, "NIMBY")
means_support_policy_2_right <- emmeans(dep_support_policy_2_control_right, "NIMBY")

# Combine them with left/right labels

means_support_policies <- bind_rows(
  as.data.frame(means_support_policies_left) %>% mutate(left_right = "Left"),
  as.data.frame(means_support_policies_centre) %>% mutate(left_right = "Centre"),
  as.data.frame(means_support_policies_right) %>% mutate(left_right = "Right")
)

means_support_policy_1 <- bind_rows(
  as.data.frame(means_support_policy_1_left) %>% mutate(left_right = "Left"),
  as.data.frame(means_support_policy_1_centre) %>% mutate(left_right = "Centre"),
  as.data.frame(means_support_policy_1_right) %>% mutate(left_right = "Right")
)

means_support_policy_4 <- bind_rows(
  as.data.frame(means_support_policy_4_left) %>% mutate(left_right = "Left"),
  as.data.frame(means_support_policy_4_centre) %>% mutate(left_right = "Centre"),
  as.data.frame(means_support_policy_4_right) %>% mutate(left_right = "Right")
)

means_support_policy_6 <- bind_rows(
  as.data.frame(means_support_policy_6_left) %>% mutate(left_right = "Left"),
  as.data.frame(means_support_policy_6_centre) %>% mutate(left_right = "Centre"),
  as.data.frame(means_support_policy_6_right) %>% mutate(left_right = "Right")
)

means_support_policy_2 <- bind_rows(
  as.data.frame(means_support_policy_2_left) %>% mutate(left_right = "Left"),
  as.data.frame(means_support_policy_2_centre) %>% mutate(left_right = "Centre"),
  as.data.frame(means_support_policy_2_right) %>% mutate(left_right = "Right")
)
  
plot_policy <- create_emm_plot(
  data = means_support_policies,
  title = "General policy support",
  y_label = "Estimated mean",
  y_limits = c(3.7, 6),
  y_breaks = seq(4, 6, 0.5)
)

plot_policy_1 <- create_emm_plot(
  data = means_support_policy_1,
  title = "Simplified approval procedures",
  y_limits = c(3.7, 6),
  y_breaks = seq(4, 6, 0.5)
)

plot_policy_4 <- create_emm_plot(
  data = means_support_policy_4,
  title = "Advisory services for farmers",
  y_limits = c(3.7, 6),
  y_breaks = seq(4, 6, 0.5)
)

plot_policy_6 <- create_emm_plot(
  data = means_support_policy_6,
  title = "Increased one-off payments",
  y_label = "Estimated mean",
  y_limits = c(3.7, 6),
  y_breaks = seq(4, 6, 0.5)
)

plot_policy_2 <- create_emm_plot(
  data = means_support_policy_2,
  title = "Financial support for large projects",
  y_limits = c(3.7, 6),
  y_breaks = seq(4, 6, 0.5),
  show_legend = TRUE
)
empty_plot <- ggplot() + theme_void()

plot_policy <- plot_policy + labs(tag = "a)") + theme(plot.tag = element_text(face = "bold", size = 10))
plot_policy_1 <- plot_policy_1 + labs(tag = "b)") + theme(plot.tag = element_text(face = "bold", size = 10))
plot_policy_4 <- plot_policy_4 + labs(tag = "c)") + theme(plot.tag = element_text(face = "bold", size = 10))
plot_policy_6 <- plot_policy_6 + labs(tag = "d)") + theme(plot.tag = element_text(face = "bold", size = 10))
plot_policy_2 <- plot_policy_2 + labs(tag = "e)") + theme(plot.tag = element_text(face = "bold", size = 10))

design <- "abc \n def"
final_plot <- plot_policy + plot_policy_1 + plot_policy_4 + plot_policy_6 + plot_policy_2 + guide_area() + 
  
  plot_layout(design = design, 
              guides = "collect" ) +
  plot_annotation(
    title = "",
    theme = theme(
      plot.title = element_text(size = 11, face = "bold")
    )
  ) &
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    legend.text = element_text(size=8)
  )


ggsave(filename = "figures/Figure_5.pdf", plot = final_plot, width = 180, height = 120, units = "mm")


## SI Figures ###########################

### SI Figure 8 Attitude #######################################
# Expansion <1ha
means_expansion_small_urban <- emmeans(comp_attitude_expansion_small_control_urban, "NIMBY")
means_expansion_small_suburban <- emmeans(comp_attitude_expansion_small_control_suburban, "NIMBY")
means_expansion_small_rural <- emmeans(comp_attitude_expansion_small_control_rural, "NIMBY")

# Expansion <5ha
means_expansion_medium_urban <- emmeans(comp_attitude_expansion_medium_control_urban, "NIMBY")
means_expansion_medium_suburban <- emmeans(comp_attitude_expansion_medium_control_suburban, "NIMBY")
means_expansion_medium_rural <- emmeans(comp_attitude_expansion_medium_control_rural, "NIMBY")

# Expansion <10ha
means_expansion_large_urban <- emmeans(comp_attitude_expansion_large_control_urban, "NIMBY")
means_expansion_large_suburban <- emmeans(comp_attitude_expansion_large_control_suburban, "NIMBY")
means_expansion_large_rural <- emmeans(comp_attitude_expansion_large_control_rural, "NIMBY")

# Expansion in CH
means_expansion_ch_urban <- emmeans(comp_attitude_expansion_control_urban, "NIMBY")
means_expansion_ch_suburban <- emmeans(comp_attitude_expansion_control_suburban, "NIMBY")
means_expansion_ch_rural <- emmeans(comp_attitude_expansion_control_rural, "NIMBY")

#Expansion in Neighbourhood
means_expansion_nearby_urban <- emmeans(comp_attitude_expansion_nearby_control_urban, "NIMBY")
means_expansion_nearby_suburban <- emmeans(comp_attitude_expansion_nearby_control_suburban, "NIMBY")
means_expansion_nearby_rural <- emmeans(comp_attitude_expansion_nearby_control_rural, "NIMBY")



means_expansion_small <- bind_rows(
  as.data.frame(means_expansion_small_urban) %>% mutate(urban_rural = "Urban"),
  as.data.frame(means_expansion_small_suburban) %>% mutate(urban_rural = "Suburban"),
  as.data.frame(means_expansion_small_rural) %>% mutate(urban_rural = "Rural")
)

means_expansion_medium <- bind_rows(
  as.data.frame(means_expansion_medium_urban) %>% mutate(urban_rural = "Urban"),
  as.data.frame(means_expansion_medium_suburban) %>% mutate(urban_rural = "Suburban"),
  as.data.frame(means_expansion_medium_rural) %>% mutate(urban_rural = "Rural")
)

means_expansion_large <- bind_rows(
  as.data.frame(means_expansion_large_urban) %>% mutate(urban_rural = "Urban"),
  as.data.frame(means_expansion_large_suburban) %>% mutate(urban_rural = "Suburban"),
  as.data.frame(means_expansion_large_rural) %>% mutate(urban_rural = "Rural")
)

means_expansion_ch <- bind_rows(
  as.data.frame(means_expansion_ch_urban) %>% mutate(urban_rural = "Urban"),
  as.data.frame(means_expansion_ch_suburban) %>% mutate(urban_rural = "Suburban"),
  as.data.frame(means_expansion_ch_rural) %>% mutate(urban_rural = "Rural")
)

means_expansion_nearby <- bind_rows(
  as.data.frame(means_expansion_nearby_urban) %>% mutate(urban_rural = "Urban"),
  as.data.frame(means_expansion_nearby_suburban) %>% mutate(urban_rural = "Suburban"),
  as.data.frame(means_expansion_nearby_rural) %>% mutate(urban_rural = "Rural")
)

# Small expansion (<1ha)
attitude_small <- create_emm_plot_ur(
  data = means_expansion_small,
  title = "Expansion <1ha",
  y_label = "Estimated mean",
  y_limits = c(3.2, 5.55),
  y_breaks = seq(3.5, 5.5, 0.5)
)

# Medium expansion (<5ha)
attitude_medium <- create_emm_plot_ur(
  data = means_expansion_medium,
  title = "Expansion <5ha",
  y_label = NULL,
  y_limits = c(3.2, 5.55),
  y_breaks = seq(3.5, 5.5, 0.5)
)

# Large expansion (<10ha)
attitude_large <- create_emm_plot_ur(
  data = means_expansion_large,
  title = "Expansion <10ha",
  y_label = NULL,
  y_limits = c(3.2, 5.55),
  y_breaks = seq(3.5, 5.5, 0.5)
)

# Expansion in CH
attitude_CH <- create_emm_plot_ur(
  data = means_expansion_ch,
  title = "Expansion in Switzerland",
  y_label = "Estimated mean",
  y_limits = c(3.2, 5.55),
  y_breaks = seq(3.5, 5.5, 0.5)
)

# Expansion in Neighbourhood
attitude_nearby <- create_emm_plot_ur(
  data = means_expansion_nearby,
  title = "Expansion in neighbourhood",
  y_label = NULL,
  y_limits = c(3.2, 5.55),
  y_breaks = seq(3.5, 5.5, 0.5),
  show_legend = TRUE
)

empty_plot <- ggplot() + theme_void()

attitude_small <- attitude_small + labs(tag = "a)") + theme(plot.tag = element_text(face = "bold", size = 10))
attitude_medium <- attitude_medium + labs(tag = "b)") + theme(plot.tag = element_text(face = "bold", size = 10))
attitude_large <- attitude_large + labs(tag = "c)") + theme(plot.tag = element_text(face = "bold", size = 10))
attitude_CH <- attitude_CH + labs(tag = "d)") + theme(plot.tag = element_text(face = "bold", size = 10))
attitude_nearby <- attitude_nearby + labs(tag = "e)") + theme(plot.tag = element_text(face = "bold", size = 10))

design <- "abc \n def"
final_plot <- attitude_small + attitude_medium + attitude_large + attitude_CH + attitude_nearby + guide_area() + 
  
  plot_layout(design = design, 
              guides = "collect" ) +
  plot_annotation(
    title = "",
    theme = theme(
      plot.title = element_text(size = 11, face = "bold")
    )
  ) &
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    legend.text = element_text(size = 8)
  )


ggsave(filename = "figures/SI_Figure_8.pdf", plot = final_plot, width = 180, height = 120, units = "mm")

### SI Figure 11 Policies ###############################################
# Get estimated marginal means
means_support_policies_urban <- emmeans(dep_support_policies_control_urban, "NIMBY")
means_support_policies_suburban <- emmeans(dep_support_policies_control_suburban, "NIMBY")
means_support_policies_rural <- emmeans(dep_support_policies_control_rural, "NIMBY")

means_support_policy_1_urban <- emmeans(dep_support_policy_1_control_urban, "NIMBY")
means_support_policy_1_suburban <- emmeans(dep_support_policy_1_control_suburban, "NIMBY")
means_support_policy_1_rural <- emmeans(dep_support_policy_1_control_rural, "NIMBY")

means_support_policy_4_urban <- emmeans(dep_support_policy_4_control_urban, "NIMBY")
means_support_policy_4_suburban <- emmeans(dep_support_policy_4_control_suburban, "NIMBY")
means_support_policy_4_rural <- emmeans(dep_support_policy_4_control_rural, "NIMBY")

means_support_policy_6_urban <- emmeans(dep_support_policy_6_control_urban, "NIMBY")
means_support_policy_6_suburban <- emmeans(dep_support_policy_6_control_suburban, "NIMBY")
means_support_policy_6_rural <- emmeans(dep_support_policy_6_control_rural, "NIMBY")

means_support_policy_2_urban <- emmeans(dep_support_policy_2_control_urban, "NIMBY")
means_support_policy_2_suburban <- emmeans(dep_support_policy_2_control_suburban, "NIMBY")
means_support_policy_2_rural <- emmeans(dep_support_policy_2_control_rural, "NIMBY")

# Combine them with labels urban / suburban / rural
means_support_policies <- bind_rows(
  as.data.frame(means_support_policies_urban) %>% mutate(urban_rural = "Urban"),
  as.data.frame(means_support_policies_suburban) %>% mutate(urban_rural = "Suburban"),
  as.data.frame(means_support_policies_rural) %>% mutate(urban_rural = "Rural")
)

means_support_policy_1 <- bind_rows(
  as.data.frame(means_support_policy_1_urban) %>% mutate(urban_rural = "Urban"),
  as.data.frame(means_support_policy_1_suburban) %>% mutate(urban_rural = "Suburban"),
  as.data.frame(means_support_policy_1_rural) %>% mutate(urban_rural = "Rural")
)

means_support_policy_4 <- bind_rows(
  as.data.frame(means_support_policy_4_urban) %>% mutate(urban_rural = "Urban"),
  as.data.frame(means_support_policy_4_suburban) %>% mutate(urban_rural = "Suburban"),
  as.data.frame(means_support_policy_4_rural) %>% mutate(urban_rural = "Rural")
)

means_support_policy_6 <- bind_rows(
  as.data.frame(means_support_policy_6_urban) %>% mutate(urban_rural = "Urban"),
  as.data.frame(means_support_policy_6_suburban) %>% mutate(urban_rural = "Suburban"),
  as.data.frame(means_support_policy_6_rural) %>% mutate(urban_rural = "Rural")
)

means_support_policy_2 <- bind_rows(
  as.data.frame(means_support_policy_2_urban) %>% mutate(urban_rural = "Urban"),
  as.data.frame(means_support_policy_2_suburban) %>% mutate(urban_rural = "Suburban"),
  as.data.frame(means_support_policy_2_rural) %>% mutate(urban_rural = "Rural")
)

plot_policy <- create_emm_plot_ur(
  data = means_support_policies,
  title = "General policy support",
  y_label = "Estimated mean",
  y_limits = c(3.4, 5.75),
  y_breaks = seq(3.5, 5.5, 0.5)
)

plot_policy_1 <- create_emm_plot_ur(
  data = means_support_policy_1,
  title = "Simplified approval procedures",
  y_limits = c(3.4, 5.75),
  y_breaks = seq(3.5, 5.5, 0.5)
)

plot_policy_4 <- create_emm_plot_ur(
  data = means_support_policy_4,
  title = "Advisory services for farmers",
  y_limits = c(3.4, 5.75),
  y_breaks = seq(3.5, 5.5, 0.5)
)

plot_policy_6 <- create_emm_plot_ur(
  data = means_support_policy_6,
  title = "Increased one-off payments",
  y_label = "Estimated mean",
  y_limits = c(3.4, 5.75),
  y_breaks = seq(3.5, 5.5, 0.5)
)

plot_policy_2 <- create_emm_plot_ur(
  data = means_support_policy_2,
  title = "Financial support for large projects",
  y_limits = c(3.4, 5.75),
  y_breaks = seq(3.5, 5.5, 0.5),
  show_legend = TRUE
)
empty_plot <- ggplot() + theme_void()

plot_policy <- plot_policy + labs(tag = "a)") + theme(plot.tag = element_text(face = "bold", size = 10))
plot_policy_1 <- plot_policy_1 + labs(tag = "b)") + theme(plot.tag = element_text(face = "bold", size = 10))
plot_policy_4 <- plot_policy_4 + labs(tag = "c)") + theme(plot.tag = element_text(face = "bold", size = 10))
plot_policy_6 <- plot_policy_6 + labs(tag = "d)") + theme(plot.tag = element_text(face = "bold", size = 10))
plot_policy_2 <- plot_policy_2 + labs(tag = "e)") + theme(plot.tag = element_text(face = "bold", size = 10))

design <- "abc \n def"
final_plot <- plot_policy + plot_policy_1 + plot_policy_4 + plot_policy_6 + plot_policy_2 + guide_area() + 
  
  plot_layout(design = design, 
              guides = "collect" ) +
  plot_annotation(
    title = "",
    theme = theme(
      plot.title = element_text(size = 11, face = "bold")
    )
  ) &
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    legend.text = element_text(size=8)
  )


ggsave(filename = "figures/SI_Figure_11.pdf", plot = final_plot, width = 180, height = 120, units = "mm")





# Descriptive Figures ##############################################

## General Discriptives ######################################
### SI Figure 2 ################################
#age
data_age <- data %>%
  mutate(age_bin = cut(age, breaks = c(seq(18, 73, by = 5), Inf), right = FALSE, include.lowest = TRUE)) %>%
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
  mutate(age_bin = cut(ALTER, breaks = c(seq(18, 73, by = 5), Inf), right = FALSE, include.lowest = TRUE)) %>%
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
  labs(x = "Age", y = "Percentage (%)", title = "Age distribution comparison") +
  scale_fill_manual(values = c("Sample" = "#4059AD", "Population" = "#CCA43B")) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 9),
    axis.title.y = element_text(size = 9),
    axis.text.x = element_text(size = 8, angle = 45, hjust = 1, color = "black"),
    axis.text.y = element_text(size = 8, color = "black"),
    plot.title = element_text(size = 10, face = "bold"),
    legend.title = element_blank(),
    legend.text = element_text(size = 8),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )

ggsave("figures/SI_Figure_2.pdf", width = 120, height = 80, units = "mm")

### SI Figure 3 ######################
#Gender

#Import Gender Population Statistics
filename <- "data/gender_ch.csv"
gender_ch <- read.csv(filename, header = TRUE, sep = ";")

table(data$gender)
#prepare sample data
data_gender<- data %>%
  filter(!is.na(gender)) %>%  # Remove NA values
  dplyr::count(gender) %>% 
  mutate(Type = recode(gender, `1` = "Male", `2` = "Female")) %>%
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
  dplyr::rename(
    Type = GESCHLECHT,
    Count = Frequency
  ) %>%
  mutate(Type = recode (Type,
                        "male" = "Male",
                        "female" = "Female"))

combined_data_gender <- bind_rows(data_gender, gender_ch)

#create plot
ggplot(combined_data_gender, aes(x = Type, y = Percent, fill = source)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Gender", y = "Percentage (%)", title = "Gender distribution comparison") +
  scale_fill_manual(values = c("Sample" = "#4059AD", "Population" = "#CCA43B")) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 9),
    axis.title.y = element_text(size = 9),
    axis.text.x = element_text(size = 8, color = "black"),
    axis.text.y = element_text(size = 8, color = "black"),
    plot.title = element_text(size = 10, face = "bold"),
    legend.title = element_blank(),
    legend.text = element_text(size = 8),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  ) +
  coord_cartesian(ylim = c(45, 52)) 

ggsave("figures/SI_Figure_3.pdf", width = 120, height = 80, units = "mm")

### SI Figure 4 #######################################
# Language
#Import Gender Population Statistics
filename <- "data/Language_ch.csv"
language_ch <- read.csv(filename, header = TRUE, sep = ";")

table(data$UserLanguage)
#prepare sample data
data_language<- data %>%
  filter(!is.na(UserLanguage)) %>%  # Remove NA values
  dplyr::count(UserLanguage) %>%  
  filter(!UserLanguage %in% c("EN")) %>%
  mutate(Language = recode(UserLanguage, "DE" = "German", "FR" = "French")) %>%
  dplyr::select(Language, n) %>%
  dplyr::rename(Frequency = n) %>%
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
  labs(x = "Language", y = "Percentage (%)", title = "Language distribution") +
  scale_fill_manual(values = c("Sample" = "#4059AD", "Population" = "#CCA43B")) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 9),
    axis.title.y = element_text(size = 9),
    axis.text.x = element_text(size = 8, color = "black"),
    axis.text.y = element_text(size = 8, color = "black"),
    plot.title = element_text(size = 10, face = "bold"),
    legend.title = element_blank(),
    legend.text = element_text(size = 8),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  ) +
  coord_cartesian(ylim = c(0, 85))

ggsave("figures/SI_Figure_4.pdf", width = 120, height = 80, units = "mm")

### SI Figure 5 #######################################
## urban rural

#Import population statistics for age
filename <- "data/urban_rural_ch.csv"
urban_rural_ch <- read.csv(filename, header = TRUE, sep = ";")


data_urban_rural <- data %>%
  dplyr::filter(!is.na(urban_rural)) %>%  # Remove NA values
  dplyr::count(urban_rural) %>% 
  dplyr::mutate(Type = recode(urban_rural, `1` = "Urban", `2` = "Suburban", `3` = "Rural")) %>%
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
  dplyr::rename(
    Type = urbrur,
    Count = Frequency
  ) %>%
  dplyr::select(Type, Count, Percent, source) %>%
  mutate(Type = recode (Type,
                        "urban" = "Urban",
                        "suburban" = "Suburban",
                        "rural" = "Rural"))

combined_data_urban_rural <- bind_rows(data_urban_rural, urban_rural_ch)

ggplot(combined_data_urban_rural, aes(x = Type, y = Percent, fill = source)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Category", y = "Percentage (%)", title = "Degree of urbanization") +
  scale_fill_manual(values = c("Sample" = "#4059AD", "Population" = "#CCA43B")) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 9),
    axis.title.y = element_text(size = 9),
    axis.text.x = element_text(size = 8, color = "black"),
    axis.text.y = element_text(size = 8, color = "black"),
    plot.title = element_text(size = 10, face = "bold"),
    legend.title = element_blank(),
    legend.text = element_text(size = 8),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )

ggsave("figures/SI_Figure_5.pdf",  width = 120, height = 80, units = "mm")

## Descriptive Figure 2  #################################################

likert_colors <- c(
  "1" = "#7f0000",
  "2" = "#b2182b",
  "3" = "#ef8a62",
  "4" = "#f0f0f0",
  "5" = "#92c5de",
  "6" = "#4393c3",
  "7" = "#2166ac"
)
### Political Orientation ################################################

#### Figure 2 ##############################################################

data_plot <- data %>%
  dplyr::filter(NIMBY == 0) %>%
  dplyr::select(solar_open_space, solar_roofs, attitude_expansion, left_right_centre) %>%
  pivot_longer(
    cols = c(solar_open_space, solar_roofs, attitude_expansion),
    names_to = "Question",
    values_to = "Response"
  ) %>%
  mutate(
    Question = dplyr::recode(
      Question,
      solar_open_space = "Open-space PV",
      solar_roofs      = "Rooftop PV",
      attitude_expansion = "Agrivoltaics"
    ),
    Response = factor(Response, levels = rev(as.character(1:7)))
  ) %>%
  filter(!is.na(Response), !is.na(left_right_centre)) %>%
  mutate(
    QuestionGroup = interaction(Question, left_right_centre, sep = ",\n")
  )%>%
  filter(QuestionGroup != "")%>%
  dplyr::count(QuestionGroup, Response, name = "n", .drop = FALSE)

order_vec <- c(
  "Rooftop PV,\nLeft",
  "Rooftop PV,\nCentre",
  "Rooftop PV,\nRight",
  "Open-space PV,\nLeft",
  "Open-space PV,\nCentre",
  "Open-space PV,\nRight",
  "Agrivoltaics,\nLeft",
  "Agrivoltaics,\nCentre",
  "Agrivoltaics,\nRight"
)

data_plot <- data_plot %>%
  mutate(
    QuestionGroup = factor(QuestionGroup, levels = rev(order_vec))
  )%>%
  group_by(QuestionGroup) %>%
  dplyr::mutate(prop = n / sum(n))


# Plot
plot <- ggplot(data_plot, aes(x = QuestionGroup, y = prop, fill = Response)) +
  geom_bar(stat = "identity", position = "fill", width = 0.7) +
  coord_flip() +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_manual(values = likert_colors) +
  labs(
    y = "Proportion of responses",
    x = NULL,
    fill = "1 = Completely oppose, 7 = Completely support"
  ) +
  guides(fill = guide_legend(nrow = 1, byrow = TRUE, reverse = TRUE)) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 8),
    axis.title.y = element_text(size = 8),
    axis.text.x = element_text(size = 8, color = "black"),
    axis.text.y = element_text(size = 8, color = "black"),
    plot.title = element_text(size = 9, face = "bold"),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 8),
    legend.position = "bottom",
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )

# Save the plot
ggsave("figures/Figure_2.pdf", plot = plot, width = 180, height = 100, units = "mm")

### Urban rural #############################################

#### SI Figure 6  ###########################
data_plot <- data %>%
  dplyr::filter(NIMBY == 0) %>%
  dplyr::select(solar_open_space, solar_roofs, attitude_expansion, urban_rural_f) %>%
  pivot_longer(
    cols = c(solar_open_space, solar_roofs, attitude_expansion),
    names_to = "Question",
    values_to = "Response"
  ) %>%
  mutate(
    Question = dplyr::recode(
      Question,
      solar_open_space = "Open-space PV",
      solar_roofs      = "Rooftop PV",
      attitude_expansion = "Agrivoltaics"
    ),
    Response = factor(Response, levels = rev(as.character(1:7)))
  ) %>%
  filter(!is.na(Response), !is.na(urban_rural_f)) %>%
  mutate(
    QuestionGroup = interaction(Question, urban_rural_f, sep = ",\n")
  )%>%
  filter(QuestionGroup != "")%>%
  dplyr::count(QuestionGroup, Response, name = "n", .drop = FALSE)

order_vec <- c(
  "Rooftop PV,\nUrban",
  "Rooftop PV,\nSuburban",
  "Rooftop PV,\nRural",
  "Open-space PV,\nUrban",
  "Open-space PV,\nSuburban",
  "Open-space PV,\nRural",
  "Agrivoltaics,\nUrban",
  "Agrivoltaics,\nSuburban",
  "Agrivoltaics,\nRural"
)

data_plot <- data_plot %>%
  mutate(
    QuestionGroup = factor(QuestionGroup, levels = rev(order_vec))
  )%>%
  group_by(QuestionGroup) %>%
  dplyr::mutate(prop = n / sum(n))


# Plot
plot <- ggplot(data_plot, aes(x = QuestionGroup, y = prop, fill = Response)) +
  geom_bar(stat = "identity", position = "fill", width = 0.7) +
  coord_flip() +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_manual(values = likert_colors) +
  labs(
    y = "Proportion of responses",
    x = NULL,
    fill = "1 = Completely oppose, 7 = Completely support"
  ) +
  guides(fill = guide_legend(nrow = 1, byrow = TRUE, reverse = TRUE)) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 8),
    axis.title.y = element_text(size = 8),
    axis.text.x = element_text(size = 8, color = "black"),
    axis.text.y = element_text(size = 8, color = "black"),
    plot.title = element_text(size = 9, face = "bold"),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 8),
    legend.position = "bottom",
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )

# Save the plot
ggsave("figures/SI_Figure_6.pdf", plot = plot, width = 180, height = 100, units = "mm")


