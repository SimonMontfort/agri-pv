# Replication Code for Conjoint Analysis "Agrivoltaics can reduce political polarization and local opposition to solar energy" #####################################################
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

setwd("/Users/simon/Documents/repo/agri-pv")
      

# Packages ####################################
library(cjoint)
library(cregg)
library(dplyr)
library(emmeans)
library(ggpubr)
library(ggsci)
library(plyr) 
library(readr)
library(readxl)
library(sandwich)
library(sparsereg)
library(stargazer)
library(tibble)
library(tidyr)
library(cowplot)
library(knitr)
library(kableExtra)
library(stringi)
library(cowplot)
library(FindIt)

# Load Data ###############################################
vote <- read.csv("data/data_cleaned[1].csv", header = TRUE, sep = ",")

any(sapply(stri_split_fixed(vote$QID1214859317_cjp1, ","), function(x){length(x)}) != 7)
any(sapply(stri_split_fixed(vote$QID1214859317_cjp2, ","), function(x){length(x)}) != 7)  
any(sapply(stri_split_fixed(vote$QID246_cjp1, ","), function(x){length(x)}) != 7)  
any(sapply(stri_split_fixed(vote$QID246_cjp2, ","), function(x){length(x)}) != 7)  
any(sapply(stri_split_fixed(vote$QID251_cjp1, ","), function(x){length(x)}) != 7)  
any(sapply(stri_split_fixed(vote$QID251_cjp2, ","), function(x){length(x)}) != 7) 
any(sapply(stri_split_fixed(vote$QID255_cjp1, ","), function(x){length(x)}) != 7)  
any(sapply(stri_split_fixed(vote$QID255_cjp2, ","), function(x){length(x)}) != 7) 

vote <- vote %>% 
  filter_at(vars(QID1214859317_cjp1,	QID1214859317_cjp2,	QID246_cjp1, QID246_cjp2, 
                 QID251_cjp1, QID251_cjp2, QID255_cjp1, QID255_cjp2, 
                 Q15.9_1, Q15.9_2, Q15.15_1, Q15.15_2, Q15.21_1, Q15.21_2, 
                 Q15.27_1, Q15.27_2, Q15.8, Q15.14, Q15.20, Q15.26,
                 personal_advantage, ch_advantage), ~ !is.na(.))

# assign id
vote$id <- 1:nrow(vote)
# to df
vote <- as.data.frame(vote)


# Code Transformation #################################################
# Code transformation for Marginal Means Analyis of Attributes 
## Transform the data from qualtrics to the formate required by the cjoint package

split_cjp <- function(vote, vals, support, choice){
  # initialise list
  shown_conj_values <- list()
  # loop through all values
  for (i in 1:length(vals)){
    # split strings
    shown_conj_values[[vals[i]]] <- strsplit(vote[ , as.character(vals[i])], ",")
    # bind the list together 
    shown_conj_values[[vals[i]]] <- do.call(rbind, shown_conj_values[[vals[i]]])
    # replace col labels
    colnames(shown_conj_values[[vals[i]]]) <- c("attrib1_lab", "attrib2_lab", "attrib3_lab", "attrib4_lab", "attrib5_lab", "attrib6_lab", "attrib7_lab")
    # bind the support variable to it
    shown_conj_values[[vals[i]]] <- cbind(shown_conj_values[[vals[i]]], vote[, as.character(support[i])])
    # rename cols
    colnames(shown_conj_values[[vals[i]]])[length(colnames(shown_conj_values[[vals[i]]]))] <- "rate"
    # choice outcome only appears 4 times, rate outcomes appear 8 times, therefore change the counter
    j <- if(i %in% 1:2) 1 else if (i %in% 3:4) 2 else if (i %in% 5:6) 3 else if (i %in% 7:8) 4
    # bind the choice variable to it
    shown_conj_values[[vals[i]]] <- cbind(shown_conj_values[[vals[i]]], vote[, as.character(choice[j])])
    # rename cols
    colnames(shown_conj_values[[vals[i]]])[length(colnames(shown_conj_values[[vals[i]]]))] <- "choice"
    # # bind vars to keep to it
    shown_conj_values[[vals[i]]] <- cbind(shown_conj_values[[vals[i]]], vote)
  }
  # bind elements in list of conjoint features of package 1
  df_cjp_1 <- do.call(rbind, shown_conj_values[grepl("_cjp1|id", names(shown_conj_values))])
  
  df_cjp_1[, "choice_id"] <- 1
  # bind elements in list of conjoint features of package 2
  df_cjp_2 <- do.call(rbind, shown_conj_values[grepl("_cjp2|id", names(shown_conj_values))])
  
  df_cjp_2[, "choice_id"] <- 2
  # bind these together
  df_cjp <- as.data.frame(rbind(df_cjp_1, df_cjp_2))
  # write this inot the results object
  df_cjp[grepl(paste(qid_values[1], qid_values[2], sep= "|"), rownames(df_cjp)) ,"round"] <- 1
  df_cjp[grepl(paste(qid_values[3], qid_values[4], sep= "|"), rownames(df_cjp)) ,"round"] <- 2
  df_cjp[grepl(paste(qid_values[5], qid_values[6], sep= "|"), rownames(df_cjp)) ,"round"] <- 3
  df_cjp[grepl(paste(qid_values[7], qid_values[8], sep= "|"), rownames(df_cjp)) ,"round"] <- 4
  # order
  df_cjp <- df_cjp[order(df_cjp$id, df_cjp$round),]
  # add choice_id
  # df_cjp$choice_id <- rep(1:2, nrow(df_cjp)/2)
  # recode choice depending on choice id
  df_cjp$choice[df_cjp$choice_id == 1 & df_cjp$choice == "Vorschlag A"] <- 1
  df_cjp$choice[df_cjp$choice_id == 2 & df_cjp$choice == "Vorschlag A"] <- 0
  df_cjp$choice[df_cjp$choice_id == 1 & df_cjp$choice == "Vorschlag B"] <- 0
  df_cjp$choice[df_cjp$choice_id == 2 & df_cjp$choice == "Vorschlag B"] <- 1
  df_cjp$choice <- as.numeric(df_cjp$choice)
  # output object of the function
  df_cjp
}

qid_values <- c("QID1214859317_cjp1",	"QID1214859317_cjp2",	"QID246_cjp1",	"QID246_cjp2", "QID251_cjp1",	"QID251_cjp2", "QID255_cjp1", "QID255_cjp2") # Werte
support <- c("Q15.9_1", "Q15.9_2", "Q15.15_1", "Q15.15_2", "Q15.21_1",	"Q15.21_2", "Q15.27_1", "Q15.27_2") # Werte
# activism <- c("QID389_1", "QID389_2", "QID415_1", "QID415_2", "QID412_1",	"QID412_2", "QID414_1", "QID414_2") # Werte
choice <- c("Q15.8", "Q15.14", "Q15.20", "Q15.26") # Werte
vals <- qid_values

# transform the data
dat <- split_cjp(vote, qid_values, support, choice)

# Transform Data for Conjoint ###########################################
## (2) recode variable format and content (languages)
# to character
dat[,  c("attrib1_lab", "attrib2_lab", "attrib3_lab", "attrib4_lab", "attrib5_lab", "attrib6_lab", "attrib7_lab")] <- sapply(dat[,  c("attrib1_lab", "attrib2_lab", "attrib3_lab", "attrib4_lab", "attrib5_lab", "attrib6_lab", "attrib7_lab")], as.character)

# recode labels of the conjoint (French / German) for uniform labelling
dat$attrib1_lab[dat$attrib1_lab %in% c("Agri-PV-Anlage auf Gewächshäusern und Ersatz von Folientunneln", "Installations agri-PV sur des serres et remplacement de tunnels en plastique", "Agri-PV systems on greenhouses and replacement of polytunnels")] <- "Agri-PV systems on greenhouses\nand replacement of polytunnels"
dat$attrib1_lab[dat$attrib1_lab %in% c("Horizontale Freiflächen Agri-PV-Anlage auf Weide- oder Ackerland", "Surfaces libres horizontales pour installations agri-PV sur des pâturage ou des terres cultivées", "Horizontal open space Agri-PV systems on pasture or arable land")] <-  "Horizontal open space Agri-PV\nsystems on pasture or arable land"
dat$attrib1_lab[dat$attrib1_lab %in% c("Vertikale Freiflächen Agri-PV-Anlage auf Weide- oder Ackerland", "Surfaces libres verticales pour installations agri-PV sur des pâturages ou des terres cultivées ", "Vertical open space Agri-PV systems on pasture or arable land")] <-  "Vertical open space Agri-PV\nsystems on pasture or arable land"

dat$attrib2_lab[grepl("1ha)", dat$attrib2_lab, fixed = T)] <- "Up to one football pitch (approx. 1ha)"
dat$attrib2_lab[grepl("5ha)", dat$attrib2_lab, fixed = T)] <- "Up to 5 football pitches (approx. 5ha)"
dat$attrib2_lab[grepl("10ha)", dat$attrib2_lab, fixed = T)] <- "Up to 10 football pitches (approx. 10ha)"

dat$attrib3_lab[grepl("0-500", dat$attrib3_lab, fixed = T)] <- "0-500 meters" 
dat$attrib3_lab[grepl("500-1500", dat$attrib3_lab, fixed = T)] <- "500-1500 meters"
dat$attrib3_lab[grepl("1500-4500", dat$attrib3_lab, fixed = T)] <- "1500-4500 meters"

dat$attrib4_lab[dat$attrib4_lab %in% c("Gemeinde", "Commune")] <- "Municipality"
dat$attrib4_lab[dat$attrib4_lab %in% c("Regionaler Energieversorger", "Fournisseur régional d'énergie")] <-  "Regional energy supplier"
dat$attrib4_lab[dat$attrib4_lab %in% c("Bäuerinnen und Bauern", "Paysans et paysannes")] <-  "Farmers"
dat$attrib4_lab[dat$attrib4_lab %in% c("Grundstückseigentümer", "Propriétaire du terrain")] <- "Landowner"
dat$attrib4_lab[dat$attrib4_lab %in% c("Energiegenossenschaft (z.B. lokale Bevölkerung)", "Coopérative énergétique (par ex. citoyens locaux)")] <- "Energy cooperative (e.g. local population)"
dat$attrib4_lab[dat$attrib4_lab %in% c("Externe (nicht lokale) Investoren", "Investisseurs externes (non locaux)")] <- "External (non-local) investors"

dat$attrib5_lab[grepl("0-5%", dat$attrib5_lab, fixed = T)] <- "0-5% reduction in crop yield"
dat$attrib5_lab[grepl("6-10%", dat$attrib5_lab, fixed = T)] <- "6-10% reduction in crop yield"  
dat$attrib5_lab[grepl("11-20%", dat$attrib5_lab, fixed = T)] <- "11-20% reduction in crop yield"
dat$attrib5_lab[grepl("21-40%", dat$attrib5_lab, fixed = T)] <- "21-40% reduction in crop yield"
dat$attrib5_lab[grepl("41-80%", dat$attrib5_lab, fixed = T)] <- "41-80% reduction in crop yield"

dat$attrib6_lab[grepl("0-5%", dat$attrib6_lab, fixed = T)] <- "0-5% increase in own production"
dat$attrib6_lab[grepl("6-10%", dat$attrib6_lab, fixed = T)] <- "6-10% increase in own production "  
dat$attrib6_lab[grepl("11-20%", dat$attrib6_lab, fixed = T)] <- "11-20% increase in own production"
dat$attrib6_lab[grepl("21-40%", dat$attrib6_lab, fixed = T)] <- "21-40% increase in own production"
dat$attrib6_lab[grepl("41-80%", dat$attrib6_lab, fixed = T)] <- "41-80% increase in own production"

dat$attrib7_lab[grepl("0-5%", dat$attrib7_lab, fixed = T)] <- "0-5% higher income"
dat$attrib7_lab[grepl("6-10%", dat$attrib7_lab, fixed = T)] <- "6-10% higher income"  
dat$attrib7_lab[grepl("11-20%", dat$attrib7_lab, fixed = T)] <- "11-20% higher income" 
dat$attrib7_lab[grepl("21-40%", dat$attrib7_lab, fixed = T)] <- "21-40% higher income"
dat$attrib7_lab[grepl("41-80%", dat$attrib7_lab, fixed = T)] <- "41-80% higher income"

# check that none are NA, should be FALSE
any(sapply(dat[, c("attrib1_lab", "attrib2_lab", "attrib3_lab", "attrib4_lab", "attrib5_lab", "attrib6_lab")], is.na))


## (3) transform DVs

# create numeric rate variable
dat$rate <- as.numeric(dat$rate)

## (4) create numeric attributes
dat[, "Attrib1"] <- ""
dat$Attrib1[grepl("Agri-PV systems on greenhouses\nand replacement of polytunnels", dat$attrib1_lab)] <- 1
dat$Attrib1[grepl("Horizontal open space Agri-PV\nsystems on pasture or arable land", dat$attrib1_lab)] <- 2
dat$Attrib1[grepl("Vertical open space Agri-PV\nsystems on pasture or arable land", dat$attrib1_lab)] <- 3

dat[, "Attrib2"] <- ""
dat$Attrib2[grepl('1ha', dat$attrib2_lab)] <- 1
dat$Attrib2[grep('5ha', dat$attrib2_lab)] <- 2
dat$Attrib2[grepl('10ha', dat$attrib2_lab)] <- 3

dat[, "Attrib3"] <- ""
dat$Attrib3[grepl('0-500', dat$attrib3_lab)] <- 1
dat$Attrib3[grepl('500-1500', dat$attrib3_lab)] <- 2
dat$Attrib3[grepl('1500-4500', dat$attrib3_lab)] <- 3

dat[, "Attrib4"] <- ""
dat$Attrib4[grepl('Municipality', dat$attrib4_lab)] <- 1
dat$Attrib4[grepl('Regional energy supplier', dat$attrib4_lab)] <- 2
dat$Attrib4[grepl('Farmers', dat$attrib4_lab)] <- 3
dat$Attrib4[grepl('Landowner', dat$attrib4_lab)] <- 4
dat$Attrib4[grepl('Energy cooperative', dat$attrib4_lab)] <- 5
dat$Attrib4[grepl('External', dat$attrib4_lab)] <- 6

dat[, "Attrib5"] <- ""
dat$Attrib5[grepl('0-5%', dat$attrib5_lab)] <- 1
dat$Attrib5[grepl('6-10%', dat$attrib5_lab)] <- 2
dat$Attrib5[grepl('11-20%', dat$attrib5_lab)] <- 3
dat$Attrib5[grepl('21-40%', dat$attrib5_lab)] <- 4
dat$Attrib5[grepl('41-80%', dat$attrib5_lab)] <- 5

dat[, "Attrib6"] <- ""
dat$Attrib6[grepl('0-5%', dat$attrib6_lab)] <- 1
dat$Attrib6[grepl('6-10%', dat$attrib6_lab)] <- 2
dat$Attrib6[grepl('11-20%', dat$attrib6_lab)] <- 3
dat$Attrib6[grepl('21-40%', dat$attrib6_lab)] <- 4
dat$Attrib6[grepl('41-80%', dat$attrib6_lab)] <- 5

dat[, "Attrib7"] <- ""
dat$Attrib7[grepl('0-5%', dat$attrib7_lab)] <- 1
dat$Attrib7[grepl('6-10%', dat$attrib7_lab)] <- 2
dat$Attrib7[grepl('11-20%', dat$attrib7_lab)] <- 3
dat$Attrib7[grepl('21-40%', dat$attrib7_lab)] <- 4
dat$Attrib7[grepl('41-80%', dat$attrib7_lab)] <- 5

sapply(dat[, c("attrib1_lab", "attrib2_lab", "attrib3_lab", "attrib4_lab", "attrib5_lab", "attrib6_lab", "attrib7_lab")], 
       function(x){table(is.na(x))/length(x)})
sapply(dat[, c("Attrib1", "Attrib2", "Attrib3", "Attrib4", "Attrib5", "Attrib6", "Attrib7")], 
       function(x){table(is.na(x))/length(x)})

# replace parentheses because they seem to create problems when reading the data into the function
dat$attrib1_lab <- gsub("\\s*\\([^\\)]+\\)", "", as.character(dat$attrib1_lab))
dat$attrib2_lab <- gsub("\\s*\\([^\\)]+\\)", "", as.character(dat$attrib2_lab))
dat$attrib3_lab <- gsub("\\s*\\([^\\)]+\\)", "", as.character(dat$attrib3_lab))
dat$attrib4_lab <- gsub("\\s*\\([^\\)]+\\)", "", as.character(dat$attrib4_lab))
dat$attrib5_lab <- gsub("\\s*\\([^\\)]+\\)", "", as.character(dat$attrib5_lab))
dat$attrib6_lab <- gsub("\\s*\\([^\\)]+\\)", "", as.character(dat$attrib6_lab))
dat$attrib7_lab <- gsub("\\s*\\([^\\)]+\\)", "", as.character(dat$attrib7_lab))

sapply(dat[, c("attrib1_lab", "attrib2_lab", "attrib3_lab", "attrib4_lab", "attrib5_lab", "attrib6_lab", "attrib7_lab")], 
       function(x){table(is.na(x))/length(x)})

## (4) reformat
# transform all to factor
dat[,  c("Attrib1", "Attrib2", "Attrib3", "Attrib4", "Attrib5", "Attrib6", "Attrib7")] <- sapply(dat[,  c("Attrib1", "Attrib2", "Attrib3", "Attrib4", "Attrib5", "Attrib6", "Attrib7")], as.factor)

# transform all to factor with ordered levels
dat$attrib1_lab <- factor(dat$attrib1_lab, levels = c("Agri-PV systems on greenhouses\nand replacement of polytunnels", "Horizontal open space Agri-PV\nsystems on pasture or arable land", "Vertical open space Agri-PV\nsystems on pasture or arable land"))
dat$attrib2_lab <- factor(dat$attrib2_lab, levels = c("Up to one football pitch", "Up to 5 football pitches", "Up to 10 football pitches"))
dat$attrib3_lab <- factor(dat$attrib3_lab, levels = c("0-500 meters", "500-1500 meters", "1500-4500 meters"))
dat$attrib4_lab <- factor(dat$attrib4_lab, levels = c("Municipality", "Regional energy supplier", "Farmers", "Landowner", "Energy cooperative", "External investors"))
dat$attrib5_lab <- factor(dat$attrib5_lab, levels = c("0-5% reduction in crop yield", "6-10% reduction in crop yield", "11-20% reduction in crop yield", "21-40% reduction in crop yield", "41-80% reduction in crop yield"))
dat$attrib6_lab <- factor(dat$attrib6_lab, levels = c("0-5% increase in own production", "6-10% increase in own production ", "11-20% increase in own production", "21-40% increase in own production", "41-80% increase in own production"))
dat$attrib7_lab <- factor(dat$attrib7_lab, levels = c("0-5% higher income", "6-10% higher income", "11-20% higher income", "21-40% higher income", "41-80% higher income"))

sapply(dat[, c("attrib1_lab", "attrib2_lab", "attrib3_lab", "attrib4_lab", "attrib5_lab", "attrib6_lab", "attrib7_lab")], 
       function(x){table(is.na(x))/length(x)})

dat$choice <- ifelse(dat$choice == 2, 1, 0)

dat$rate_binary <- ifelse(dat$rate > 4, 1, 0)


# Recode Variables ##############################

dat2 <- dat %>% mutate(NIMBY = as.factor(NIMBY),
                       environment_score = ifelse(environment_score >3.5, 1, 0))

dat2 <- dat2 %>% 
  mutate(left_right_bins = case_when(left_right <=4 ~ "Left",
                                     left_right >=5 & left_right <=6 ~ "Centre",
                                     left_right >=7 ~ "Right")) %>% 
  mutate(left_right_bins = factor(left_right_bins, levels = c("Left", "Centre", "Right"))) 


dat2 <- dat2 %>% 
  #filter(circle_match == 1) %>% # Limiting data to correctly treated
  mutate(left_right_binary = case_when(left_right <=5 ~ "Left",
                                       left_right >=6 ~ "Right")) %>% 
  mutate(left_right_binary = factor(left_right_binary, levels = c("Left", "Right"))) 

dat2 <- dat2 %>%
  mutate(circle = factor(circle, 
                         levels = c(0, 1)),
         urban_rural_true = factor(urban_rural_true,
                                 levels = c(1, 2, 3))
         )


cut_points <- quantile(dat2$age, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE) #Cut points to create Age group variable

dat2$age_group <- cut(
  dat2$age, 
  breaks = cut_points, 
  labels = c("Young", "Middle-aged", "Old"), 
  include.lowest = TRUE
)

# Create exp_dat Dataset ########################################################
exp_dat <- dat2 %>% 
  dplyr::select(attrib1_lab, attrib2_lab, attrib3_lab, attrib4_lab, attrib5_lab, attrib6_lab, attrib7_lab,
                age_group, gender_f, environment_score, feelings_agri_pv, familiar_agri_pv, like_energy_agri_pv,
                urban_rural_true, 
                NIMBY, circle, circle1, circle2, circle3,
                rate_binary, id,
                left_right_binary, left_right_bins
                ) 

# check that the numbers are right and no NAs
nrow(exp_dat)/8
nrow(drop_na(exp_dat))/8
sapply(exp_dat, function(x){table(is.na(x))/length(x)})

# check that no NAs are dropped
nrow(drop_na(exp_dat)) == nrow(exp_dat) #should be TRUE


#exp_dat$age_group <- factor(exp_dat$age_group, levels = c("Young","Middle-aged", "Old"))
exp_dat$gender_f <- factor(exp_dat$gender_f, levels = c("Male", "Female", "Prefer not to say", "Other"))
exp_dat$environment_score <- factor(exp_dat$environment_score, 
                                    levels = c("0", "1"), 
                                    labels = c("0_environment_score", "1_environment_score"))
exp_dat$feelings_agri_pv <- factor(exp_dat$feelings_agri_pv, 
                                   levels = c("1", "2", "3", "4", "5", "6", "7"),
                                   labels = c("1_feelings", "2_feelings", "3_feelings", "4_feelings", "5_feelings", "6_feelings", "7_feelings"))
exp_dat$familiar_agri_pv <- factor(exp_dat$familiar_agri_pv, 
                                   levels = c("1", "2", "3", "4", "5", "6", "7"),
                                   labels = c("1_familiar", "2_familiar", "3_familiar", "4_familiar", "5_familiar", "6_familiar", "7_familiar"))
exp_dat$like_energy_agri_pv <- factor(exp_dat$like_energy_agri_pv, 
                                      levels = c("1", "2", "3", "4", "5", "6", "7"),
                                      labels = c("1_like", "2_like", "3_like", "4_like", "5_like", "6_like", "7_like"))
exp_dat$urban_rural_true <- factor(exp_dat$urban_rural_true, levels = c("1", "2", "3"))
#exp_dat$left_right_binary <- factor(exp_dat$left_right_binary, levels = c("1", "2", "3"))


exp_dat$circle1 <- factor(exp_dat$circle1, levels = c("0", "1"),
                          labels = c("circle1_0", "circle1_1"))
exp_dat$circle2 <- factor(exp_dat$circle2, levels = c("0", "1"),
                          labels = c("circle2_0", "circle2_1"))
exp_dat$circle3 <- factor(exp_dat$circle3, levels = c("0", "1"),
                          labels = c("circle3_0", "circle3_1"))

exp_dat_2 <- drop_na(exp_dat)


exp_dat <- exp_dat %>% 
  mutate(
    NIMBY_true = case_when(
      NIMBY == 1 & circle == 1 ~ 1,
      TRUE ~ 0
    )
  )

exp_dat$NIMBY_true <- factor(exp_dat $NIMBY_true, 
                             levels = c("0", "1"),
                             labels = c("Untreated", "Treated"))

exp_dat$NIMBY <- factor(exp_dat $NIMBY, 
                        levels = c("0", "1"),
                        labels = c("Control", "Treatment"))



# Perform Conjoint Analysis ################################################################
left_only <- exp_dat %>% 
  filter(left_right_binary == "Left")

right_only <- exp_dat %>% 
  filter(left_right_binary == "Right")


p <- cj(
  exp_dat, 
  rate_binary ~ attrib1_lab + attrib2_lab + attrib3_lab + attrib4_lab + attrib5_lab + attrib6_lab + attrib7_lab + 
    circle1 + circle2 + circle3 + age_group + gender_f + environment_score + feelings_agri_pv + familiar_agri_pv +
    like_energy_agri_pv + urban_rural_true, 
  id = ~id, 
  estimate = "mm", 
  by = ~NIMBY)

p <- p  %>% 
  filter(feature %in% c("attrib1_lab", "attrib2_lab", "attrib3_lab", "attrib4_lab", "attrib5_lab", "attrib6_lab", "attrib7_lab"))

left_p <- cj(
  left_only, 
  rate_binary ~ attrib1_lab + attrib2_lab + attrib3_lab + attrib4_lab + attrib5_lab + attrib6_lab + attrib7_lab +
    circle1 + circle2 + circle3 + age_group + gender_f + environment_score + feelings_agri_pv + familiar_agri_pv +
    like_energy_agri_pv + urban_rural_true, 
  id = ~id, 
  estimate = "mm", 
  by = ~NIMBY)


left_p  <- left_p  %>% 
  filter(feature %in% c("attrib1_lab", "attrib2_lab", "attrib3_lab", "attrib4_lab", "attrib5_lab", "attrib6_lab", "attrib7_lab"))


right_p <- cj(
  right_only, 
  rate_binary ~ attrib1_lab + attrib2_lab + attrib3_lab + attrib4_lab + attrib5_lab + attrib6_lab + attrib7_lab +
    circle1 + circle2 + circle3 + age_group + gender_f + environment_score + feelings_agri_pv + familiar_agri_pv +
    like_energy_agri_pv + urban_rural_true, 
  id = ~id, 
  estimate = "mm", 
  by = ~NIMBY)


right_p <- right_p %>% 
  filter(feature %in% c("attrib1_lab", "attrib2_lab", "attrib3_lab", "attrib4_lab", "attrib5_lab", "attrib6_lab", "attrib7_lab"))

#For Plot without NIMBY
mm_lf_binary_2 <- cj(
  exp_dat, 
  rate_binary ~ attrib1_lab + attrib2_lab + attrib3_lab + attrib4_lab + attrib5_lab + attrib6_lab + attrib7_lab +
    age_group + gender_f + environment_score + feelings_agri_pv + familiar_agri_pv +
    like_energy_agri_pv + urban_rural_true , 
  id = ~id, 
  estimate = "mm", 
  by = ~left_right_bins)


mm_lf_binary_2 <- mm_lf_binary_2 %>% 
  filter(feature %in% c("attrib1_lab", "attrib2_lab", "attrib3_lab", "attrib4_lab", "attrib5_lab", "attrib6_lab", "attrib7_lab"))


custom_labels <- c(
  attrib1_lab = "Agri-PV Type",
  attrib2_lab = "Approximate Size of Agri-PV",
  attrib3_lab = "Distance to Residency",
  attrib4_lab = "Ownership",
  attrib5_lab = "Impact on Food Production",
  attrib6_lab = "Impact on Local Energy Production",
  attrib7_lab = "Impact on Farmers Income"
)

# Export Tables #############################################
## SI- Table 11 - Marginal Means for Left_Right Interaction (corresponding to Fig. 5 in Manuscript)  ####################################################

mm_lf_binary_t <- mm_lf_binary_2

#change levels of first plot to save vertical space
levels(mm_lf_binary_t$level)[levels(mm_lf_binary_t$level) == "Agri-PV systems on greenhouses\nand replacement of polytunnels"] <- "On greenhouse/replace polytunnels"
levels(mm_lf_binary_t$level)[levels(mm_lf_binary_t$level) == "Horizontal open space Agri-PV\nsystems on pasture or arable land"] <- "Horizontal on pasture/arable land"
levels(mm_lf_binary_t$level)[levels(mm_lf_binary_t$level) == "Vertical open space Agri-PV\nsystems on pasture or arable land"] <- "Vertical on pasture/arable land"

#Make sure Latex can read the % signs
levels(mm_lf_binary_t$level) <- gsub("%", "\\\\%", levels(mm_lf_binary_t$level))

custom_labels <- c(
  attrib1_lab = "Agri-PV Type",
  attrib2_lab = "Approximate Size of Agri-PV",
  attrib3_lab = "Distance to Residency",
  attrib4_lab = "Ownership",
  attrib5_lab = "Impact on Food Production",
  attrib6_lab = "Impact on Local Energy Production",
  attrib7_lab = "Impact on Farmers Income"
)



data_table <- mm_lf_binary_t %>%
  dplyr::select(feature, level, left_right_bins, estimate, std.error, z, p) %>%
  arrange(feature, level, left_right_bins) %>%
  dplyr::mutate(
    feature = recode(feature, !!!custom_labels),
    Significance = case_when(
      p < 0.001 ~ "***",
      p < 0.01  ~ "**",
      p < 0.05  ~ "*",
      p < 0.1   ~ ".",
      TRUE      ~ ""
    )
  )  %>%
  dplyr::mutate(
    p = ifelse(p < 0.001, "$<$0.001", round(p, 3))
  )


tbl <- data_table %>%
  kbl(
    format = "latex",
    col.names = c(
      "Feature", "Level", "Political Orientation",
      "Marginal Mean", "SE", "Z-Value", "p-Value", "Significance"
    ),
    digits = 3,
    booktabs = TRUE,
    longtable = TRUE,
    escape = FALSE
  ) %>%
  column_spec(1, width = "4cm", latex_valign = "p") %>%
  column_spec(2, width = "4cm", latex_valign = "p") %>%
  column_spec(3, width = "2.5cm", latex_valign = "p") %>%
  collapse_rows(columns = c(1, 2))

save_kable(tbl, file = "tables/SI_Table_11.tex")


## SI- Table 12 - Marginal Means for NIMBY & Left_Right Interaction (corresponding to SI-Fig. 5) ################################################## 
left_p_1 <- left_p %>%
  mutate(left_right = "Left")

centre_p_1 <- left_p %>%
  mutate(left_right = "Centre")

right_p_1 <- right_p %>%
  mutate(left_right = "Right")

p_2 <- p %>%
  mutate(left_right = "Overall")

overall_mm <- rbind(left_p_1, centre_p_1, right_p_1, p_2)

#change levels of first plot to save vertical space
levels(overall_mm$level)[levels(overall_mm$level) == "Agri-PV systems on greenhouses\nand replacement of polytunnels"] <- "On greenhouse/replace polytunnels"
levels(overall_mm$level)[levels(overall_mm$level) == "Horizontal open space Agri-PV\nsystems on pasture or arable land"] <- "Horizontal on pasture/arable land"
levels(overall_mm$level)[levels(overall_mm$level) == "Vertical open space Agri-PV\nsystems on pasture or arable land"] <- "Vertical on pasture/arable land"

#Make sure Latex can read the % signs
levels(overall_mm$level) <- gsub("%", "\\\\%", levels(overall_mm$level))

custom_labels <- c(
  attrib1_lab = "Agri-PV Type",
  attrib2_lab = "Approximate Size of Agri-PV",
  attrib3_lab = "Distance to Residency",
  attrib4_lab = "Ownership",
  attrib5_lab = "Impact on Food Production",
  attrib6_lab = "Impact on Local Energy Production",
  attrib7_lab = "Impact on Farmers Income"
)



data_table <- overall_mm %>%
  dplyr::select(feature, level, NIMBY, left_right, estimate, std.error, z, p) %>%
  arrange(feature, level, NIMBY, left_right) %>%
  dplyr::mutate(
    feature = recode(feature, !!!custom_labels),
    Significance = case_when(
      p < 0.001 ~ "***",
      p < 0.01  ~ "**",
      p < 0.05  ~ "*",
      p < 0.1   ~ ".",
      TRUE      ~ ""
    )
  ) %>%
  dplyr::mutate(
    p = ifelse(p < 0.001, "$<$0.001", round(p, 3))
  )


tbl <- data_table %>%
  kbl(
    format = "latex",
    col.names = c("Feature", "Level", "Group", "Political Orientation", "Marginal Mean", "SE", "Z-Value", "p-Value", "Significance"),
    digits = 3,
    booktabs = TRUE,
    longtable = TRUE, 
    escape = FALSE,
    latex_column_spec = c(
      ">{\\raggedright\\arraybackslash}p{4cm}",  # Feature column
      ">{\\raggedright\\arraybackslash}p{4cm}",  # Level column
      ">{\\raggedright\\arraybackslash}p{2.5cm}",  # Group
      "l", "l", "l", "l", "l", "l"
    )
  ) %>%
  collapse_rows(columns = c(1, 2, 3), valign = "top")

save_kable(tbl, file = "tables/SI_Table_12.tex")


#Plot Fig. 5 in Main Manuscript ######################################################
#Figure 5 displays the marginal means for clear support – defined as a support rating greater than 5 on a 7-point Likert scale – for differently designed Agri-PV projects among left-leaning (blue) and right-leaning (yellow) 


#change levels of first plot to save vertical space
levels(mm_lf_binary_2$level)[levels(mm_lf_binary_2$level) == "Agri-PV systems on greenhouse\nand replacement of polytunnels"] <- "On greenhouse/replace polytunnels"
levels(mm_lf_binary_2$level)[levels(mm_lf_binary_2$level) == "Horizontal open space Agri-PV\nsystems on pasture or arable land"] <- "Horizontal on pasture/arable land"
levels(mm_lf_binary_2$level)[levels(mm_lf_binary_2$level) == "Vertical open space Agri-PV\nsystems on pasture or arable land"] <- "Vertical on pasture/arable land"



make_plot <- function(attr, show_ylabel = TRUE, show_legend = FALSE) {
  p <- ggplot(subset(mm_lf_binary_2, feature == attr), 
              aes(x = level, y = estimate, colour = left_right_bins)) +
    geom_pointrange(aes(ymin = estimate - 1.96 * std.error, 
                        ymax = estimate + 1.96 * std.error), 
                    position = position_dodge(width = .2)) +
    scale_x_discrete(labels = scales::label_wrap(18)) +
    theme_bw() + 
    theme(
      legend.position = if (show_legend) "bottom" else "none",
      axis.text.x = element_text(size = 14, angle = 20, color = "black", margin = margin(t = 17, b = -20)),
      axis.text.y = element_text(size = if (show_ylabel) 12 else 0),
      axis.title.y = element_text(size = if (show_ylabel) 16 else 0),
      legend.text = element_text(size = 16),
      legend.title = element_text(size = 16),
      strip.text.x = element_text(size = 16),
      strip.background = element_blank(),
      plot.margin = margin(b = -40, t = 0, l = 5, r = 5)
    ) +
    ylab(if (show_ylabel) "Marginal Mean" else NULL) +
    xlab("") +
    # coord_cartesian(ylim = c(0.33, 0.65)) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    geom_hline(yintercept = 0.4, lty = "dashed") +
    geom_hline(yintercept = 0.5, lty = "dashed") +
    geom_hline(yintercept = 0.6, lty = "dashed") +
    scale_color_manual(name = "Political Orientation", 
                       values = c("Left" = "#4059AD", "Centre" = "grey", "Right" = "#CCA43B")) +
    facet_wrap(~ feature, scales = "fixed", nrow = 1,
               labeller = labeller(feature = custom_labels))
  return(p)
}

# Generate plots
plot_attrib1 <- make_plot("attrib1_lab", show_ylabel = TRUE)
plot_attrib2 <- make_plot("attrib2_lab", show_ylabel = FALSE)
plot_attrib3 <- make_plot("attrib3_lab", show_ylabel = FALSE)
plot_attrib4 <- make_plot("attrib4_lab", show_ylabel = TRUE)
plot_attrib5 <- make_plot("attrib5_lab", show_ylabel = FALSE)
plot_attrib6 <- make_plot("attrib6_lab", show_ylabel = TRUE)
plot_attrib7 <- make_plot("attrib7_lab", show_ylabel = FALSE, show_legend = TRUE)

# First row: 3 plots
row1 <- ggarrange(
  plot_attrib1, plot_attrib2, plot_attrib3,
  ncol = 3,
  align = "h"
)

# Second row: 2 plots
row2 <- ggarrange(
  plot_attrib4, plot_attrib5,
  ncol = 2,
  align = "h"
)

# Third row: 2 plots
row3 <- ggarrange(
  plot_attrib6, plot_attrib7,
  ncol = 2,
  align = "h",
  common.legend = TRUE,
  legend = "bottom"
)

# Now combine all rows into one final plot
combined_plot <- plot_grid(
  row1, row2, row3,
  ncol = 1,
  rel_heights = c(1, 1, 1)  # optional: adjust row heights if needed
)


title <- ggdraw() + 
  draw_label("Effects of Interaction with Political Ideology (binary, right >= 6)", 
             fontface = 'bold', 
             size = 20, 
             x = 0.5, 
             hjust = 0.5)

caption <- ggdraw() + 
  draw_label("Number of Observations: 2155, Number of Unique IDs.: 17240",
             size = 12, 
             x = 0, 
             hjust = 0)

title_plot <- plot_grid(title, combined_plot, caption,  ncol=1, rel_heights=c(0.07, 1))

final_plot <- ggdraw(title_plot) + 
  theme(plot.background = element_rect(fill = "white", color = NA))

ggsave("plots/Fig_5_conj_left_right.png", plot = final_plot, width = 16, height = 12)


#Plot SI-Fig. 5 ######################################################
#SI-Figure 5 displays the marginal means for clear support – defined as a support rating greater than 5 on a 7-point Likert scale – for differently designed Agri-PV projects among left-leaning (blue) and right-leaning (yellow) in the NIMBY treatment group (reactangles), the control group (circles) and the overall sample (grey)

left_p_2 <- left_p %>%
  mutate(left_right = "Left")

centre_p_2 <- right_p %>%
  mutate(left_right = "Centre")

right_p_2 <- right_p %>%
  mutate(left_right = "Right")

p_2 <- p %>%
  mutate(left_right = "Overall")


overall_mm <- rbind(left_p_2, centre_p_2, right_p_2, p_2)

#change levels of first plot to save vertical space
levels(overall_mm$level)[levels(overall_mm$level) == "Agri-PV systems on greenhouses\nand replacement of polytunnels"] <- "On greenhouse/replace polytunnels"
levels(overall_mm$level)[levels(overall_mm$level) == "Horizontal open space Agri-PV\nsystems on pasture or arable land"] <- "Horizontal on pasture/arable land"
levels(overall_mm$level)[levels(overall_mm$level) == "Vertical open space Agri-PV\nsystems on pasture or arable land"] <- "Vertical on pasture/arable land"

overall_mm$group_id <- interaction(overall_mm$left_right, overall_mm$NIMBY)


make_plot <- function(attr, show_ylabel = TRUE, show_legend = FALSE) {
  p <- ggplot(subset(overall_mm, feature == attr), 
              aes(x = level, y = estimate,
                  group = group_id, 
                  shape = NIMBY, 
                  colour = left_right)) +
    geom_pointrange(aes(ymin = estimate - 1.96 * std.error, 
                        ymax = estimate + 1.96 * std.error), 
                    position = position_dodge(width = .4)) +
    scale_x_discrete(labels = scales::label_wrap(18)) +
    theme_bw() + 
    theme(
      legend.position = if (show_legend) "bottom" else "none",
      axis.text.x = element_text(size = 14, angle = 20, color = "black", margin = margin(t = 17, b = -20)),
      axis.text.y = element_text(size = if (show_ylabel) 12 else 0),
      axis.title.y = element_text(size = if (show_ylabel) 16 else 0),
      legend.text = element_text(size = 16),
      legend.title = element_text(size = 16),
      strip.text.x = element_text(size = 16),
      strip.background = element_blank(),
      plot.margin = margin(b = -40, t = 0, l = 5, r = 5)
    ) +
    ylab(if (show_ylabel) "Marginal Mean" else NULL) +
    xlab("") +
    # coord_cartesian(ylim = c(0.30, 0.72)) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    geom_hline(yintercept = 0.3, lty = "dashed") +
    geom_hline(yintercept = 0.4, lty = "dashed") +
    geom_hline(yintercept = 0.5, lty = "dashed") +
    geom_hline(yintercept = 0.6, lty = "dashed") +
    geom_hline(yintercept = 0.7, lty = "dashed") +
    scale_color_manual(name = "Political Orientation:", 
                       values = c("Left" = "#4059AD", "Centre" = "grey", "Right" = "#CCA43B", "Overall" = "black")) +
    scale_shape_discrete(name="Group:") + 
    facet_wrap(~ feature, scales = "fixed", nrow = 1,
               labeller = labeller(feature = custom_labels))
  return(p)
}

# Generate plots
plot_attrib1 <- make_plot("attrib1_lab", show_ylabel = TRUE)
plot_attrib2 <- make_plot("attrib2_lab", show_ylabel = FALSE)
plot_attrib3 <- make_plot("attrib3_lab", show_ylabel = FALSE)
plot_attrib4 <- make_plot("attrib4_lab", show_ylabel = TRUE)
plot_attrib5 <- make_plot("attrib5_lab", show_ylabel = FALSE)
plot_attrib6 <- make_plot("attrib6_lab", show_ylabel = TRUE)
plot_attrib7 <- make_plot("attrib7_lab", show_ylabel = FALSE, show_legend = TRUE)

# First row: 3 plots
row1 <- ggarrange(
  plot_attrib1, plot_attrib2, plot_attrib3,
  ncol = 3,
  align = "h"
)

# Second row: 2 plots
row2 <- ggarrange(
  plot_attrib4, plot_attrib5,
  ncol = 2,
  align = "h"
)

# Third row: 2 plots
row3 <- ggarrange(
  plot_attrib6, plot_attrib7,
  ncol = 2,
  align = "h",
  common.legend = TRUE,
  legend = "bottom"
)

# Now combine all rows into one final plot
combined_plot <- plot_grid(
  row1, row2, row3,
  ncol = 1,
  rel_heights = c(1, 1, 1)
)


title <- ggdraw() + 
  draw_label("Effects of Interaction with Political Ideology (binary, right >= 6) by Group", 
             fontface = 'bold', 
             size = 20, 
             x = 0.5, 
             hjust = 0.5)

caption <- ggdraw() + 
  draw_label("Number of Observations: 2155, Number of Unique IDs.: 17240",
             size = 12, 
             x = 0, 
             hjust = 0)

title_plot <- plot_grid(title, combined_plot, caption, ncol=1, rel_heights=c(0.07, 1))

final_plot <- ggdraw(title_plot) + 
  theme(plot.background = element_rect(fill = "white", color = NA))

ggsave("plots/SI_Fig_5.png", plot = final_plot, width = 16, height = 12)


###############################################################################
# FOR PLOT WITHOUT NIMBY — URBAN / RURAL INTERACTION
###############################################################################

mm_ur_binary_2 <- cj(
  exp_dat,
  rate_binary ~ attrib1_lab + attrib2_lab + attrib3_lab + attrib4_lab +
    attrib5_lab + attrib6_lab + attrib7_lab +
    age_group + gender_f + environment_score + feelings_agri_pv +
    familiar_agri_pv + like_energy_agri_pv,
  id = ~id,
  estimate = "mm",
  by = ~urban_rural_true
)

mm_ur_binary_2 <- mm_ur_binary_2 %>%
  filter(feature %in% c(
    "attrib1_lab", "attrib2_lab", "attrib3_lab",
    "attrib4_lab", "attrib5_lab", "attrib6_lab", "attrib7_lab"
  ))

custom_labels <- c(
  attrib1_lab = "Agri-PV Type",
  attrib2_lab = "Approximate Size of Agri-PV",
  attrib3_lab = "Distance to Residency",
  attrib4_lab = "Ownership",
  attrib5_lab = "Impact on Food Production",
  attrib6_lab = "Impact on Local Energy Production",
  attrib7_lab = "Impact on Farmers Income"
)


###############################################################################
# EXPORT TABLE — equivalent to SI-TABLE 11 but for URBAN/RURAL
###############################################################################

mm_ur_binary_t <- mm_ur_binary_2

levels(mm_ur_binary_t$level)[levels(mm_ur_binary_t$level) ==
                               "Agri-PV systems on greenhouses\nand replacement of polytunnels"] <-
  "On greenhouse/replace polytunnels"
levels(mm_ur_binary_t$level)[levels(mm_ur_binary_t$level) ==
                               "Horizontal open space Agri-PV\nsystems on pasture or arable land"] <-
  "Horizontal on pasture/arable land"
levels(mm_ur_binary_t$level)[levels(mm_ur_binary_t$level) ==
                               "Vertical open space Agri-PV\nsystems on pasture or arable land"] <-
  "Vertical on pasture/arable land"

levels(mm_ur_binary_t$level) <- gsub("%", "\\\\%", levels(mm_ur_binary_t$level))

data_table <- mm_ur_binary_t %>%
  dplyr::select(feature, level, urban_rural_true, estimate, std.error, z, p) %>%
  arrange(feature, level, urban_rural_true) %>%
  dplyr::mutate(
    feature = recode(feature, !!!custom_labels),
    Significance = case_when(
      p < 0.001 ~ "***",
      p < 0.01 ~ "**",
      p < 0.05 ~ "*",
      p < 0.1 ~ ".",
      TRUE ~ ""
    ),
    p = ifelse(p < 0.001, "$<$0.001", round(p, 3))
  )

tbl <- data_table %>%
  kbl(
    format = "latex",
    col.names = c(
      "Feature", "Level", "Urban–Rural Group",
      "Marginal Mean", "SE", "Z-Value", "p-Value", "Significance"
    ),
    digits = 3,
    booktabs = TRUE,
    longtable = TRUE,
    escape = FALSE
  ) %>%
  column_spec(1, width = "4cm", latex_valign = "p") %>%
  column_spec(2, width = "4cm", latex_valign = "p") %>%
  column_spec(3, width = "2.5cm", latex_valign = "p") %>%
  collapse_rows(columns = c(1, 2))

save_kable(tbl, file = "tables/SI_Table_11_URBAN_RURAL.tex")


###############################################################################
# PLOT FIGURE — FULL REPLACEMENT OF FIG. 5 (Urban vs Rural)
###############################################################################

levels(mm_ur_binary_2$level)[levels(mm_ur_binary_2$level) ==
                               "Agri-PV systems on greenhouse\nand replacement of polytunnels"] <-
  "On greenhouse/replace polytunnels"
levels(mm_ur_binary_2$level)[levels(mm_ur_binary_2$level) ==
                               "Horizontal open space Agri-PV\nsystems on pasture or arable land"] <-
  "Horizontal on pasture/arable land"
levels(mm_ur_binary_2$level)[levels(mm_ur_binary_2$level) ==
                               "Vertical open space Agri-PV\nsystems on pasture or arable land"] <-
  "Vertical on pasture/arable land"

make_plot_ur <- function(attr, show_ylabel = TRUE, show_legend = FALSE) {
  ggplot(subset(mm_ur_binary_2, feature == attr),
         aes(
           x = level, y = estimate,
           colour = urban_rural_true
         )
  ) +
    geom_pointrange(aes(
      ymin = estimate - 1.96 * std.error,
      ymax = estimate + 1.96 * std.error
    ),
    position = position_dodge(width = .2)
    ) +
    scale_x_discrete(labels = scales::label_wrap(18)) +
    theme_bw() +
    theme(
      legend.position = if (show_legend) "bottom" else "none",
      axis.text.x = element_text(size = 14, angle = 20, colour = "black",
                                 margin = margin(t = 17, b = -20)
      ),
      axis.text.y = element_text(size = if (show_ylabel) 12 else 0),
      axis.title.y = element_text(size = if (show_ylabel) 16 else 0),
      legend.text = element_text(size = 16),
      legend.title = element_text(size = 16),
      strip.text.x = element_text(size = 16),
      strip.background = element_blank(),
      plot.margin = margin(b = -40, t = 0, l = 5, r = 5)
    ) +
    ylab(if (show_ylabel) "Marginal Mean" else NULL) +
    xlab("") +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    geom_hline(yintercept = 0.4, lty = "dashed") +
    geom_hline(yintercept = 0.5, lty = "dashed") +
    geom_hline(yintercept = 0.6, lty = "dashed") +
    scale_color_manual(
      name = "Urban–Rural Classification",
      labels = c(
        "1" = "Urban",
        "2" = "Suburban",
        "3" = "Rural"
      ),
      values = c(
        "1" = "#4059AD",
        "2" = "grey",
        "3" = "#CCA43B"
      )
    ) +
    facet_wrap(~feature,
               scales = "fixed", nrow = 1,
               labeller = labeller(feature = custom_labels)
    )
}

# Generate 7 attribute plots
plot_attrib1 <- make_plot_ur("attrib1_lab", show_ylabel = TRUE)
plot_attrib2 <- make_plot_ur("attrib2_lab", show_ylabel = FALSE)
plot_attrib3 <- make_plot_ur("attrib3_lab", show_ylabel = FALSE)
plot_attrib4 <- make_plot_ur("attrib4_lab", show_ylabel = TRUE)
plot_attrib5 <- make_plot_ur("attrib5_lab", show_ylabel = FALSE)
plot_attrib6 <- make_plot_ur("attrib6_lab", show_ylabel = TRUE)
plot_attrib7 <- make_plot_ur("attrib7_lab", show_ylabel = FALSE, show_legend = TRUE)

# Arrange into rows
row1 <- ggarrange(plot_attrib1, plot_attrib2, plot_attrib3, ncol = 3, align = "h")
row2 <- ggarrange(plot_attrib4, plot_attrib5, ncol = 2, align = "h")
row3 <- ggarrange(plot_attrib6, plot_attrib7, ncol = 2, align = "h",
                  common.legend = TRUE, legend = "bottom"
)

combined_plot <- plot_grid(row1, row2, row3, ncol = 1)

# Title and caption
title <- ggdraw() +
  draw_label(
    "Effects of Interaction with Urban–Rural Classification",
    fontface = "bold", size = 20, x = 0.5, hjust = 0.5
  )

caption <- ggdraw() +
  draw_label(
    "Number of Observations: 2155, Number of Unique IDs: 17240",
    size = 12, x = 0, hjust = 0
  )

title_plot <- plot_grid(title, combined_plot, caption,
                        ncol = 1, rel_heights = c(0.07, 1)
)

final_plot <- ggdraw(title_plot) +
  theme(plot.background = element_rect(fill = "white", colour = NA))

ggsave("plots/Fig_URBAN_RURAL.png",
       plot = final_plot, width = 16, height = 12
)







# Average Marginal Interaction Effect (AMIE)##############################################
# This allows us to estimate the most and least preferred Agri-PV project design options
# Building on nonparametric estimation strategy proposed by Egami, N., & Imai, K. (2019). Causal interaction in factorial experiments: Application to conjoint analysis. Journal of the American Statistical Association.


# Find interactions
F1  <- FindIt(model.treat = rate_binary ~ attrib1_lab + attrib2_lab + attrib3_lab + attrib4_lab + attrib5_lab + attrib6_lab + attrib7_lab,
              data = exp_dat,  
              type = "binary",
              nway = "multiple"
) 
summary(F1) 

## Returns predicted values for unique treatment combinations.
pred2 <- predict(F1, unique=TRUE)

## Top 50 most preferred Agri-PV project design options
head_df <- head(pred2$data, n=50)
colnames(head_df) <- NA
rownames(head_df) <- NULL

## Bottom 50 least preferred Agri-PV project design options
tail_df <- tail(pred2$data, n=50)
colnames(tail_df) <- NA
rownames(tail_df) <- NULL

# print table (SI- Tables 18 and 19)
tab_higest <- kable(head_df, "html", booktabs = TRUE, caption = "50 Treatment Combinations with the Highest Average Marginal Interaction Effect (AMIE)", col.names = NULL) %>% 
  add_header_above(c("AMIE" = 1, "Agri-PV Type" = 1, "Approximate Size of Agri-PV"= 1, 
                     "Distance to Residency"= 1, "Ownership","Impact on Food Production", "Impact on Local Energy Production", "Impact on Farmers Income"), angle = "0") 
tab_lowest <- kable(tail_df, "html", booktabs = TRUE, caption = "50 Treatment Combinations with the Lowest Average Marginal Interaction Effect (AMIE)", col.names = NULL) %>% 
  add_header_above(c("AMIE" = 1, "Agri-PV Type" = 1, "Approximate Size of Agri-PV"= 1, 
                     "Distance to Residency"= 1, "Ownership","Impact on Food Production", "Impact on Local Energy Production", "Impact on Farmers Income"), angle = "0") 
writeLines(tab_higest, "tables/tab_higest.html")
writeLines(tab_lowest, "tables/tab_lowest.html")

tab_higest <- kable(head_df, "latex", booktabs = TRUE, caption = "50 Treatment Combinations with the Highest Average Marginal Interaction Effect (AMIE)", col.names = NULL) %>% 
  add_header_above(c("AMIE" = 1, "Agri-PV Type" = 1, "Approximate Size of Agri-PV"= 1, 
                     "Distance to Residency"= 1, "Ownership","Impact on Food Production", "Impact on Local Energy Production", "Impact on Farmers Income"), angle = "0") 
tab_lowest <- kable(tail_df, "latex", booktabs = TRUE, caption = "50 Treatment Combinations with the Lowest Average Marginal Interaction Effect (AMIE)", col.names = NULL) %>% 
  add_header_above(c("AMIE" = 1, "Agri-PV Type" = 1, "Approximate Size of Agri-PV"= 1, 
                     "Distance to Residency"= 1, "Ownership","Impact on Food Production", "Impact on Local Energy Production", "Impact on Farmers Income"), angle = "0") 
writeLines(tab_higest, "tables/tab_higest.tex")
writeLines(tab_lowest, "tables/tab_lowest.tex")

findit_plot_dat <- pred2$data %>% 
  as.data.frame() %>% 
  dplyr::mutate(index = row_number())

write.csv(findit_plot_dat, "data/findit_plot_dat.csv")
# findit_plot_dat <- read.csv("data/findit_plot_dat.csv")

#Plot the treatment-treatment interactions with the highest and lowest treatment effects (SI-Figure 6)
indifference <- findit_plot_dat %>% filter(Treatment.effect == 0)

library(ggthemes)
p_type <- findit_plot_dat %>% 
  ggplot(aes(x = index, y = Treatment.effect, col = attrib2_lab)) + 
  geom_point(alpha = .3) + 
  geom_vline(xintercept = indifference %>% slice(1) %>% pull(index), lty = 2) + 
  geom_hline(yintercept = 0, lty = 3) + 
  theme_light() + 
  facet_wrap(~attrib1_lab) +
  scale_color_colorblind() +
  scale_color_manual(values = c("grey", "darkblue", "darkred")) + 
  ylim(min(findit_plot_dat$Treatment.effect), max(findit_plot_dat$Treatment.effect)) +
  labs(y = "Treatment effect", x = "Combination of treatment (index)", col = "") + 
  theme(panel.grid = element_blank(),
        strip.text.y = element_text(angle = 0),
        strip.background = element_rect(fill = "white"),
        strip.text = element_text(colour = "black", size =9),
        axis.ticks.length = unit(.25, "cm"),
        legend.position = "none")
p_type

p_distance <- findit_plot_dat %>% 
  ggplot(aes(x = index, y = Treatment.effect, col = attrib2_lab)) + 
  geom_point(alpha = .3) + 
  geom_vline(xintercept = indifference %>% slice(1) %>% pull(index), lty = 2) + 
  geom_hline(yintercept = 0, lty = 3) + 
  theme_light() + 
  facet_wrap(~attrib3_lab) +
  scale_color_colorblind() +
  scale_color_manual(values = c("grey", "darkblue", "darkred")) + 
  ylim(min(findit_plot_dat$Treatment.effect), max(findit_plot_dat$Treatment.effect)) +
  labs(y = "Treatment effect", x = "Combination of treatment (index)", col = "") + 
  theme(panel.grid = element_blank(),
        strip.text.y = element_text(angle = 0),
        strip.background = element_rect(fill = "white"),
        strip.text = element_text(colour = "black", size =9),
        axis.ticks.length = unit(.25, "cm"),
        legend.position = "none")
p_distance

p_owner <- findit_plot_dat %>% 
  ggplot(aes(x = index, y = Treatment.effect, col = attrib2_lab)) + 
  geom_point(alpha = .3) + 
  geom_vline(xintercept = indifference %>% slice(1) %>% pull(index), lty = 2) + 
  geom_hline(yintercept = 0, lty = 3) + 
  facet_wrap(~attrib4_lab) +
  scale_color_manual(values = c("grey", "darkblue", "darkred")) + 
  theme_light() + 
  ylim(min(findit_plot_dat$Treatment.effect), max(findit_plot_dat$Treatment.effect)) +
  labs(y = "Treatment effect", x = "Combination of treatment (index)", col = "Approximate Size of Agri-PV") + 
  theme(panel.grid = element_blank(),
        strip.text.y = element_text(angle = 0),
        strip.background = element_rect(fill = "white"),
        strip.text = element_text(colour = "black", size =9),
        axis.ticks.length = unit(.25, "cm"), 
        legend.position = c(.8,.1))
p_owner


legend <- get_legend(p_size)

# Combine the plots (without legend), arranged vertically
combined <- plot_grid(p_type, p_distance, p_owner, labels = c("a", "b", "c"), ncol = 1, rel_heights = c(1.2,1.2, 2))

# Add the legend at a custom position (e.g., x = 0.8, y = 0.1)
p_treat_interactions <- ggdraw() +
  draw_plot(combined, 0, 0, 1, 1) +
  draw_plot(legend, x = 0.8, y = 0.1, width = 0.2, height = 0.2)

# Print the final result
print(p_treat_interactions)

ggsave(p_treat_interactions, file = "plots/SI_Fig_p_treat_interactions.pdf", width = 10, height = 14)
  
