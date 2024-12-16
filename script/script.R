######################################################
#### Agri-PV
## Script compiled by Simon Montfort
# 16.12.2022
######################################################
# > R.version
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

rm(list = ls())

# packages
library(cjoint)
# library(gdata)
library(readxl)
library(plyr) 
library(dplyr)
# library(ggthemes)
# library(ggtext)
# library(ggplot2)
# library(gridExtra)
library(tidyr)
# library(lubridate)
# library(zoo)
# library(jtools)
library(readr)
library(sandwich)
library(sparsereg)
library(tibble)
library(ggpubr)
library(emmeans)
library(stringi)
emm_options(rg.limit = 5.67e+15)

setwd("/Users/simon/Documents/repo/agri-pv")
# load data
vote <- read.csv("data/data.csv")

vote$QID1214859317_cjp1
vote$Q15.15

#############notes###################
# first, only use those in the control group of the NIMBY experiment
# vote %>% 
#   filter(NIMBY) 

# environment_score --> interactions
# 1) environmentally friendly behaviour 
# 2) env crisis
# 3) catastrophe

# circle_match --> 1 = right treatement
# circle1 --> 1 = high, 0 = low
# circle2 --> 1 = high, 0 = low
# circle3 --> 1 = high, 0 = low

# circle --> if any is 1, then 1 otherwise 0

############# notes ###################

# vote %>% 
#   select(vars("QID1214859317_cjp1",	"QID1214859317_cjp2",	"QID246_cjp1",	"QID246_cjp2", 
#               "QID251_cjp1",	"QID251_cjp2", "QID255_cjp1", "QID255_cjp2", 
#               "Q15.9_1", "Q15.9_2", "Q15.15_1", "Q15.15_2", "Q15.21_1",	"Q15.21_2", 
#               "Q15.27_1", "Q15.27_2", "Q15.8", "Q15.14", "Q15.20", "Q15.26")) %>% 
#   summary()
# subset to full completes
# vote <- vote[vote$Status == "IP Address",]
# vote <- vote[vote$QID6 == "Ja",] # only respondents who are eligible to vote
# subset missing values for conjoint (using the conjoint attribute values shown to the respondent)
# vote <- vote[rowSums(is.na(vote[,c("QID1214859317_cjp1",	"QID1214859317_cjp2",	"QID246_cjp1",	"QID246_cjp2", "QID251_cjp1",	"QID251_cjp2", "QID255_cjp1", "QID255_cjp2",
                                   # "Q15.8", "Q15.9", "Q15.14", "Q15.15", "Q15.20",	"Q15.21", "Q15.26", "Q15.27")])) == 0,]

# filter all with NA
vote <- vote %>% 
  filter_at(vars(QID1214859317_cjp1,	QID1214859317_cjp2,	QID246_cjp1, QID246_cjp2, 
                QID251_cjp1, QID251_cjp2, QID255_cjp1, QID255_cjp2, 
                Q15.9_1, Q15.9_2, Q15.15_1, Q15.15_2, Q15.21_1,	Q15.21_2, 
                Q15.27_1, Q15.27_2, Q15.8, Q15.14, Q15.20, Q15.26), ~ !is.na(.))

# assign id
vote$id <- 1:nrow(vote)
# to df
vote <- as.data.frame(vote)
# # convert to date
# vote$StartDate <- as_datetime(as.numeric(vote$StartDate)* 3600*24, origin='1900-01-01')
# # subset to all those respondents after the pretest, this does nothing because 
# vote <- vote[vote$StartDate > as.Date("2021-06-05", "%Y-%m-%d"),]
# table(vote$QID14, useNA = "always")
# table(vote$QID82_1, useNA = "always")
# table(vote$QID618_1, useNA = "always")
# table(vote$QID565, useNA = "always")
# table(vote$QID16, useNA = "always")
# table(vote$QID19, useNA = "always")
# table(vote$QID22, useNA = "always") # plz question
# sum(is.na(vote$QID22))
# table(is.na(vote$no_of_stations_ev))
# nrow(vote)

## info on survey used in the text:
# start date
min(openxlsx::convertToDateTime(vote$EndDate))
# end date
max(openxlsx::convertToDateTime(vote$EndDate))
# median time
summary(as.numeric(vote$`Duration (in seconds)`))[3]/60
# # speeding
# gsub("0.", "", )
# library(openxlsx)
# vote$EndDate <- openxlsx::convertToDateTime(vote$EndDate)
# vote$StartDate <- openxlsx::convertToDateTime(vote$StartDate)
# vote$`Duration (in seconds)` <- as.numeric(vote$`Duration (in seconds)`)/60
# med_below40 <- summary(vote$`Duration (in seconds)`)[3]*.4
# # vote <- vote[vote$`Duration (in seconds)` > med_below40,]
# 
# med_below40_prior_benefit <- summary(as.numeric(vote$`QID566_Page Submit`))[3]*.4
# vote <- vote[vote$`QID566_Page Submit` < med_below40_prior_benefit,]
######################################################
# functions and custom theme
######################################################
# function to transform the data from qualtrics to the formate required by the cjoint package
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
    # bind the activism variable to it
    # shown_conj_values[[vals[i]]] <- cbind(shown_conj_values[[vals[i]]], vote[, as.character(activism[i])])
    # rename cols
    # colnames(shown_conj_values[[vals[i]]])[length(colnames(shown_conj_values[[vals[i]]]))] <- "rate_activism"
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
# dat %>% group_by(id, round) %>% summarise(choice = sum(choice)) %>% ungroup() %>% distinct(choice) # should be one
# ## to double check that function works correctly -- values are the same. All good
# # for DV rate
# vote %>% arrange(id) %>% select(id, "QID98_1", "QID98_2", "QID270_1", "QID270_2", "QID282_1", "QID282_2", "QID281_1", "QID281_2", "QID96", "QID269", "QID277", "QID279") %>% head(4)
# dat %>% arrange(id, round) %>% select(id, round, choice_id, rate, activism, choice) %>% head(32)
# # for DV support
# vote %>% arrange(id) %>% select(id, "QID389_1", "QID389_2", "QID415_1", "QID415_2", "QID412_1",	"QID412_2", "QID414_1", "QID414_2") %>% head(4)
# dat %>% arrange(id, round) %>% select(id, round, rate_activism) %>% head(32)
# # for covariate
# vote %>% arrange(id) %>% select(id, "QID519") %>% head(4)
# dat %>% arrange(id, round) %>% select(id, round, QID519) %>% head(32)

# to extract results object from an amce object with an option to retrieve interaction effects supplied through respondent.varying
results <- function(rate, option) {
  # initialise df to store results
  value <- data.frame(matrix(nrow = length(rate$user.levels), ncol = 2))
  # name cols
  colnames(value) <- c("lab", "value")
  # sotre value shown to respondent
  value$value <- unlist(rate$user.levels)
  # transform to factor
  value$value <- factor(value$value, levels = rev(unique(value$value)))
  # give it a label
  value$lab <- names(unlist(rate$user.levels))
  # retrieve results
  s <- base::summary(rate)
  # how many interactions? 1 = no interactions, 3 = binary, 4 = continous
  if (option == 4){
    # create list of results for each subgroup
    l <- list(s[[1]], s[[3]], s[[4]], s[[5]])
  } else if (option  == 3){
    # create list of results for each subgroup
    l <- list(s[[1]], s[[3]], s[[4]])
  } else if (option  == 1){
    # create list of results for baseline
    l <- list(s[[1]])
  } else {
    # error if incorrect option is chosen
    stop("musst supply number of marginal effect results: 1 = only baseline, 3 = unconditional and two conditional, 4 unconditional and three conditional")
  }
  # create ids
  l <- lapply(l, function(x){
    # create id for each variable
    x <- transform(x, id = as.numeric(factor(x$Attribute)))
    # to factor
    # x$Level <- factor(x$Level, levels = unique(value[,"value"]))
    # create id1 for each subgroup
    x$id1 <- rep(parent.frame()$i[], nrow(x)) 
    # output
    x
  })
  # bind lists together
  res <- do.call(rbind, l)
  # unique id for each variable within group
  res$id2 <- as.numeric(paste0(res$id, res$id1))
  # create a new row for each variable that constitutes the baseline
  res <- do.call(rbind, lapply(split(res, res$id2), function(x) rbind(NA, within(x, {NA}))))
  # add the corresponding level used as baseline and shown to the respondent
  res$Level[is.na(res$Level)] <- rep(s$baselines_amce$Level, each = option)
  # add Attribute label 
  res$Attribute <- na.locf(res$Attribute, fromLast = T)
  # add id
  res$id <- ifelse(is.na(res$id), lead(res$id), res$id)
  # add id1
  res$id1 <- ifelse(is.na(res$id1), lead(res$id1), res$id1)
  # mark which is the baseline attribute
  res$Level[is.na(res$Estimate)] <- paste("Baseline:", res$Level[is.na(res$Estimate)])
  # reorder levels
  res$Level <- factor(res$Level, levels = unique(rev(res$Level)))
  # output object of the function
  res
}

######################################################
# transform data for conjoint
######################################################

## (1) use user-defined function to transform data to long
# variables which will be supplied ot the function below for transformation
# qid_values <- c("QID430_cjp1",	"QID430_cjp2",	"QID431_cjp1",	"QID431_cjp2", "QID392_cjp1",	"QID392_cjp2", "QID393_cjp1", "QID393_cjp2") # Werte
# support <- c("QID98_1", "QID98_2", "QID270_1", "QID270_2", "QID282_1",	"QID282_2", "QID281_1", "QID281_2") # Werte
# activism <- c("QID389_1", "QID389_2", "QID415_1", "QID415_2", "QID412_1",	"QID412_2", "QID414_1", "QID414_2") # Werte
# choice <- c("QID96", "QID269", "QID277", "QID279") # Werte
# vals <- qid_values

# transform the data
dat <- split_cjp(vote, qid_values, support, choice)

## (2) recode variable format and content (languages)
# to character
dat[,  c("attrib1_lab", "attrib2_lab", "attrib3_lab", "attrib4_lab", "attrib5_lab", "attrib6_lab", "attrib7_lab")] <- sapply(dat[,  c("attrib1_lab", "attrib2_lab", "attrib3_lab", "attrib4_lab", "attrib5_lab", "attrib6_lab", "attrib7_lab")], as.character)

# recode labels of the conjoint (French / German) for uniform labelling
dat$attrib1_lab[dat$attrib1_lab %in% c("Agri-PV-Anlage auf Gewächshäusern und Ersatz von Folientunneln", "Installations agri-PV sur des serres et remplacement de tunnels en plastique")] <- "Agri-PV systems on greenhouses and replacement of polytunnels"
dat$attrib1_lab[dat$attrib1_lab %in% c("Horizontale Freiflächen Agri-PV-Anlage auf Weide- oder Ackerland", "Surfaces libres horizontales pour installations agri-PV sur des pâturage ou des terres cultivées")] <-  "Horizontal open space Agri-PV systems on pasture or arable land"
dat$attrib1_lab[dat$attrib1_lab %in% c("Vertikale Freiflächen Agri-PV-Anlage auf Weide- oder Ackerland", "Surfaces libres verticales pour installations agri-PV sur des pâturages ou des terres cultivées ")] <-  "Vertical open space Agri-PV systems on pasture or arable land"

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
any(sapply(dat[, c("attrib2_lab", "attrib3_lab", "attrib4_lab", "attrib5_lab", "attrib6_lab")], is.na))


## (3) transform DVs

# create numeric rate variable
dat$rate <- as.numeric(dat$rate)

## (4) create numeric attributes
dat[, "Attrib1"] <- ""
dat$Attrib1[grepl("Agri-PV systems on greenhouses and replacement of polytunnels", dat$attrib1_lab)] <- 1
dat$Attrib1[grepl("Horizontal open space Agri-PV systems on pasture or arable land", dat$attrib1_lab)] <- 2
dat$Attrib1[grepl("Vertical open space Agri-PV systems on pasture or arable land", dat$attrib1_lab)] <- 3

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

# replace parentheses because they seem to create problems when reading the data into the function
dat$attrib2_lab <- gsub("\\s*\\([^\\)]+\\)", "", as.character(dat$attrib2_lab))
dat$attrib3_lab <- gsub("\\s*\\([^\\)]+\\)", "", as.character(dat$attrib3_lab))
dat$attrib4_lab <- gsub("\\s*\\([^\\)]+\\)", "", as.character(dat$attrib4_lab))
dat$attrib5_lab <- gsub("\\s*\\([^\\)]+\\)", "", as.character(dat$attrib5_lab))
dat$attrib6_lab <- gsub("\\s*\\([^\\)]+\\)", "", as.character(dat$attrib6_lab))
dat$attrib7_lab <- gsub("\\s*\\([^\\)]+\\)", "", as.character(dat$attrib7_lab))

## (4) reformat
# transform all to factor
dat[,  c("Attrib1", "Attrib2", "Attrib3", "Attrib4", "Attrib5", "Attrib6", "Attrib7")] <- sapply(dat[,  c("Attrib1", "Attrib2", "Attrib3", "Attrib4", "Attrib5", "Attrib6", "Attrib7")], as.factor)

# transform all to factor with ordered levels
dat$attrib1_lab <- factor(dat$attrib1_lab, levels = c("Agri-PV systems on greenhouses and replacement of polytunnels", "Horizontal open space Agri-PV systems on pasture or arable land", "Vertical open space Agri-PV systems on pasture or arable land"))
dat$attrib2_lab <- factor(dat$attrib2_lab, levels = c("Up to one football pitch", "Up to 5 football pitches", "Up to 10 football pitches"))
dat$attrib3_lab <- factor(dat$attrib3_lab, levels = c("0-500 meters", "500-1500 meters", "1500-4500 meters"))
dat$attrib4_lab <- factor(dat$attrib4_lab, levels = c("Municipality", "Regional energy supplier", "Farmers", "Landowner", "Energy cooperative", "External investors"))
dat$attrib5_lab <- factor(dat$attrib5_lab, levels = c("0-5% reduction in crop yield", "6-10% reduction in crop yield", "11-20% reduction in crop yield", "21-40% reduction in crop yield", "41-80% reduction in crop yield"))
dat$attrib6_lab <- factor(dat$attrib6_lab, levels = c("0-5% increase in own production", "6-10% increase in own production ", "11-20% increase in own production", "21-40% increase in own production", "41-80% increase in own production"))
dat$attrib7_lab <- factor(dat$attrib7_lab, levels = c("0-5% higher income", "6-10% higher income", "11-20% higher income", "21-40% higher income", "41-80% higher income"))

dat$choice <- ifelse(dat$choice == 2, 1, 0)

dat$rate_binary <- ifelse(dat$rate > 4, 1, 0)
######################################################
# recode explanatory variables
######################################################

# dat_desc <- dat %>% 
#   dplyr::select(rate, choice, prior_benefit_2, ratio_ev_to_muni_area, driver, home_owner, age, educ, language, empl_sect, fin_cond, 
#                 left_right, sal_glob, sal_env, region, urban_rural) %>% 
#   mutate("Region: Geneva" = ifelse(region == 1, 1, 0), 
#          "Region: Middle Land" = ifelse(region == 2, 1, 0),
#          "Region: North East" = ifelse(region == 3, 1, 0),
#          "Region: Zurich" = ifelse(region == 4, 1, 0),
#          "Region: East" = ifelse(region == 5, 1, 0),
#          "Region: Central" = ifelse(region == 6, 1, 0),
#          "Region: Ticino" = ifelse(region == 7, 1, 0),
#          "Urban Area" = ifelse(urban_rural == 1, 1, 0),
#          "Intermediate Area" = ifelse(urban_rural == 2, 1, 0),
#          "Rural Area" = ifelse(urban_rural == 3, 1, 0),) %>% 
#   dplyr::select(-region, -urban_rural) %>% 
#   mutate_all(as.character) %>% 
#   mutate_all(as.numeric) 
# 
# p_desc <- dat_desc %>% 
#   mutate(ratio_ev_to_muni_area = round(ratio_ev_to_muni_area, 0)) %>% 
#   pivot_longer(., everything(), names_to = "Question", values_to = "Response") %>% 
#   mutate(Question = factor(Question, levels = c("rate", "choice", "prior_benefit_2", "ratio_ev_to_muni_area", "driver", "home_owner", "age",
#                                                 "educ", "language", "empl_sect", "fin_cond", 
#                                                 "left_right", "sal_glob", "sal_env",
#                                                 "Region: Geneva",
#                                                 "Region: Middle Land",
#                                                 "Region: North East",
#                                                 "Region: Zurich",
#                                                 "Region: East",
#                                                 "Region: Central",
#                                                 "Region: Ticino",
#                                                 "Urban Area",
#                                                 "Intermediate Area",
#                                                 "Rural Area"))) %>% 
#   group_by(Question, Response) %>% 
#   count(name = "freq") %>% 
#   mutate(Response = factor(Response, levels = as.character(c(0:14, NA)))) %>% 
#   ggplot(aes(x = Response, y = freq)) +
#   geom_col() + labs(x = "", y = "") +
#   facet_wrap(~Question, scales = "free_x", 
#              labeller = labeller(Question = c("driver" = "Driver",
#                                               "educ" = "Education",
#                                               "age" = "Age",
#                                               "empl_sect" = "Employment Sector",
#                                               "fin_cond" = "Financial Condition",
#                                               "home_owner" = "Home Owner",
#                                               "left_right" = "Left−Right",
#                                               "prior_benefit_2" = "Perceived Effectiveness\n of Prior Benefits",
#                                               "rate" = "Support \n(Rate Outcome)",
#                                               "choice" = "Support \n(Choice Outcome)",
#                                               "ratio_ev_to_muni_area" = "EV Charging Stations",
#                                               "sal_env" = "Salience: \nEnvironment and Climate",
#                                               "sal_glob" = "Salience: \nGlobalisation",
#                                               "language" = "French",
#                                               "Urban Area" = "Urban Area",
#                                               "Intermediate Area" = "Intermediate Area",
#                                               "Rural Area"  = "Rural Area",
#                                               "Region: Geneva" = "Region: Geneva",
#                                               "Region: Middle Land" = "Region: Middle Land",
#                                               "Region: North East" = "Region: North East",
#                                               "Region: Zurich" = "Region: Zurich",
#                                               "Region: East" = "Region: East",
#                                               "Region: Central" = "Region: Central",
#                                               "Region: Ticino" = "Region: Ticino"))) +
#   theme_light()
# p_desc
# ggsave(p_desc, filename = "Plots/p_desc.pdf", height = 16, width = 10)
# 
# 
# labs_desc <- c("Support (Rate Outcome)", "Support (Choice Outcome)", "Perceived Effectiveness of Prior Benefits",
#                "EV Charging Stations",  "Driver", "Home Owner", "Age", "Education", "French",
#                "Employment Sector", "Financial Condition", "Left-Right", "Salience: Globalisation", "Salience: Environment and Climate",
#                "Region: Geneva", "Region: Middle Land", "Region: North East", "Region: Zurich", "Region: East", "Region: Central", "Region: Ticino", "Urban Area", "Intermediate Area", "Rural Area")
# 
# library(stargazer)
# stargazer(dat_desc,
#           out.header = F,
#           no.space = TRUE, 
#           label = "tab:summary_stats",
#           column.sep.width = "3pt",
#           font.size = "footnotesize",
#           covariate.labels = labs_desc,
#           out = "Tables/summary_stats.tex"
#           )
# 
# library(Hmisc)
# correlation_matrix <- cor(dat_desc %>% mutate_all(., as.numeric), use = "pairwise.complete.obs")
# correlation_matrix <- round(correlation_matrix, 2)
# correlation_matrix[upper.tri(correlation_matrix)] <- NA
# diag(correlation_matrix) <- NA
# colnames(correlation_matrix) <- rownames(correlation_matrix) <- labs_desc
# correlation_matrix1 <- correlation_matrix[,1:12]
# correlation_matrix2 <- correlation_matrix[,13:ncol(correlation_matrix)]
# 
# stargazer(correlation_matrix1, title="Correlation Matrix Part 1", 
#           float.env = "sidewaystable", 
#           type = "latex", 
#           out.header = F,
#           no.space = TRUE, # to remove the spaces after each line of coefficients
#           column.sep.width = "1pt", # to reduce column width
#           font.size = "footnotesize", # to make font size smaller
#           label = "tab:correlation_pt1",
#           out = "Tables/correlation_pt1.tex"
#           # covariate.labels = labs_desc,
#           # dep.var.labels = labs_desc
# )
# stargazer(correlation_matrix2, title="Correlation Matrix  Part 2", 
#           float.env = "sidewaystable", 
#           type = "latex", 
#           out.header = F,
#           no.space = TRUE, # to remove the spaces after each line of coefficients
#           column.sep.width = "1pt", # to reduce column width
#           font.size = "footnotesize", # to make font size smaller
#           label = "tab:correlation_pt2",
#           out = "Tables/correlation_pt2.tex"
# )
library(cregg)
library(ggsci)
dat <- dat %>% mutate(NIMBY = as.factor(NIMBY),
                      environment_score = ifelse(environment_score >3.5, 1, 0),
                      environment_score = factor(environment_score, levels = c(0, 1)))

dat <- dat %>% 
  mutate(left_right_bins = case_when(left_right <=3 ~ "Left",
                                left_right >=4 & left_right <=6 ~ "Centre",
                                left_right >=7 ~ "Right")) %>% 
  mutate(left_right_bins = factor(left_right_bins, levels = c("Left", "Centre", "Right")))

model1 <- lm(choice ~ attrib1_lab + attrib2_lab +  attrib3_lab +  attrib4_lab + attrib5_lab + attrib6_lab + attrib7_lab, data = dat %>% filter(NIMBY == 0))
summary(model1)

amce <- cj(data = dat, rate_binary ~ attrib1_lab + attrib2_lab + attrib3_lab + attrib4_lab + attrib5_lab + attrib6_lab + attrib7_lab, id = ~id, estimate = "amce")
amce

p_baseline <- amce %>% 
  mutate(
    level = factor(level, levels = c("Agri-PV systems on greenhouses and replacement of polytunnels", "Horizontal open space Agri-PV systems on pasture or arable land", "Vertical open space Agri-PV systems on pasture or arable land", 
                                     "Up to one football pitch", "Up to 5 football pitches", "Up to 10 football pitches", 
                                     "0-500 meters", "500-1500 meters", "1500-4500 meters", 
                                     "Municipality", "Regional energy supplier", "Farmers", "Landowner", "Energy cooperative", "External investors", 
                                     "0-5% reduction in crop yield", "6-10% reduction in crop yield", "11-20% reduction in crop yield", "21-40% reduction in crop yield", "41-80% reduction in crop yield", 
                                     "0-5% increase in own production", "6-10% increase in own production ", "11-20% increase in own production", "21-40% increase in own production", "41-80% increase in own production", 
                                     "0-5% higher income", "6-10% higher income", "11-20% higher income", "21-40% higher income", "41-80% higher income"))
    # by = factor(BY, levels = c("0", "1")),
    # feature_lab = "",
    # feature_lab = ifelse(feature == "attrib1_lab", "Recipient developing country", feature_lab),
    # feature_lab = ifelse(feature == "attrib2_lab", "Number of climate\nmigrants to accept\nfrom this country per year", feature_lab),
    # feature_lab = ifelse(feature == "attrib3_lab", "Climate aid to give\nto this country\n(CHF) per year", feature_lab),
    # feature_lab = ifelse(feature == "attrib4_lab", "Value of Swiss\ntrade with this\ncountry", feature_lab),
    # feature_lab = ifelse(feature == "attrib5_lab", "Extreme weather event", feature_lab),
    # feature_lab = ifelse(feature == "attrib6_lab", "Percentage of this\ncountry's votes\nin line with Switzerland's\nposition at the UN\nSecurity Council", feature_lab),
    # feature_lab = factor(feature_lab, levels = c("Recipient developing country", "Number of climate\nmigrants to accept\nfrom this country per year",
    #                                              "Climate aid to give\nto this country\n(CHF) per year", "Value of Swiss\ntrade with this\ncountry",
    #                                              "Extreme weather event", "Percentage of this\ncountry's votes\nin line with Switzerland's\nposition at the UN\nSecurity Council"))
  ) %>% 
  ggplot(aes(level, estimate)) + 
  geom_pointrange(aes(ymin = lower, ymax = upper)) +
  scale_x_discrete(limits=rev) +
  facet_grid(feature~., scales = "free_y", space = "free_y") +
  theme_light() + 
  coord_flip() +
  labs(y ="AMCE", x = "", subtitle = "all (NIMBY treated and control)") +
  theme(panel.grid = element_blank(),
        strip.text.y = element_text(angle = 0),
        strip.background = element_rect(fill = "white"),
        strip.text = element_text(colour = "black", size =9)) +
  geom_hline(yintercept = 0, lty = 2, alpha = 0.7, color = "gray50") 

ggsave(p_baseline, file = "plots/p_baseline.pdf", width = 7, height = 7)

amce <- cj(data = dat %>% filter(NIMBY == 0), rate_binary ~ attrib1_lab + attrib2_lab + attrib3_lab + attrib4_lab + attrib5_lab + attrib6_lab + attrib7_lab, id = ~id, estimate = "amce")
amce

p_baseline_control <- amce %>% 
  mutate(
    level = factor(level, levels = c("Agri-PV systems on greenhouses and replacement of polytunnels", "Horizontal open space Agri-PV systems on pasture or arable land", "Vertical open space Agri-PV systems on pasture or arable land", 
                                     "Up to one football pitch", "Up to 5 football pitches", "Up to 10 football pitches", 
                                     "0-500 meters", "500-1500 meters", "1500-4500 meters", 
                                     "Municipality", "Regional energy supplier", "Farmers", "Landowner", "Energy cooperative", "External investors", 
                                     "0-5% reduction in crop yield", "6-10% reduction in crop yield", "11-20% reduction in crop yield", "21-40% reduction in crop yield", "41-80% reduction in crop yield", 
                                     "0-5% increase in own production", "6-10% increase in own production ", "11-20% increase in own production", "21-40% increase in own production", "41-80% increase in own production", 
                                     "0-5% higher income", "6-10% higher income", "11-20% higher income", "21-40% higher income", "41-80% higher income"))
    # by = factor(BY, levels = c("0", "1")),
         # feature_lab = "",
         # feature_lab = ifelse(feature == "attrib1_lab", "Recipient developing country", feature_lab),
         # feature_lab = ifelse(feature == "attrib2_lab", "Number of climate\nmigrants to accept\nfrom this country per year", feature_lab),
         # feature_lab = ifelse(feature == "attrib3_lab", "Climate aid to give\nto this country\n(CHF) per year", feature_lab),
         # feature_lab = ifelse(feature == "attrib4_lab", "Value of Swiss\ntrade with this\ncountry", feature_lab),
         # feature_lab = ifelse(feature == "attrib5_lab", "Extreme weather event", feature_lab),
         # feature_lab = ifelse(feature == "attrib6_lab", "Percentage of this\ncountry's votes\nin line with Switzerland's\nposition at the UN\nSecurity Council", feature_lab),
         # feature_lab = factor(feature_lab, levels = c("Recipient developing country", "Number of climate\nmigrants to accept\nfrom this country per year",
         #                                              "Climate aid to give\nto this country\n(CHF) per year", "Value of Swiss\ntrade with this\ncountry",
         #                                              "Extreme weather event", "Percentage of this\ncountry's votes\nin line with Switzerland's\nposition at the UN\nSecurity Council"))
  ) %>% 
  ggplot(aes(level, estimate)) + 
  geom_pointrange(aes(ymin = lower, ymax = upper)) +
  scale_x_discrete(limits=rev) +
  facet_grid(feature~., scales = "free_y", space = "free_y") +
  theme_light() + 
  coord_flip() +
  labs(y ="AMCE", x = "", subtitle = "only NIMBY control") +
  theme(panel.grid = element_blank(),
        strip.text.y = element_text(angle = 0),
        strip.background = element_rect(fill = "white"),
        strip.text = element_text(colour = "black", size =9)) +
  geom_hline(yintercept = 0, lty = 2, alpha = 0.7, color = "gray50") 

ggsave(p_baseline_control, file = "plots/p_baseline_control.pdf", width = 7, height = 7)

amce <- cj(data = dat %>% filter(NIMBY == 1), rate_binary ~ attrib1_lab + attrib2_lab + attrib3_lab + attrib4_lab + attrib5_lab + attrib6_lab + attrib7_lab, id = ~id, estimate = "amce")
amce

p_baseline_treated <- amce %>% 
  mutate(
    level = factor(level, levels = c("Agri-PV systems on greenhouses and replacement of polytunnels", "Horizontal open space Agri-PV systems on pasture or arable land", "Vertical open space Agri-PV systems on pasture or arable land", 
                                     "Up to one football pitch", "Up to 5 football pitches", "Up to 10 football pitches", 
                                     "0-500 meters", "500-1500 meters", "1500-4500 meters", 
                                     "Municipality", "Regional energy supplier", "Farmers", "Landowner", "Energy cooperative", "External investors", 
                                     "0-5% reduction in crop yield", "6-10% reduction in crop yield", "11-20% reduction in crop yield", "21-40% reduction in crop yield", "41-80% reduction in crop yield", 
                                     "0-5% increase in own production", "6-10% increase in own production ", "11-20% increase in own production", "21-40% increase in own production", "41-80% increase in own production", 
                                     "0-5% higher income", "6-10% higher income", "11-20% higher income", "21-40% higher income", "41-80% higher income"))
    # by = factor(BY, levels = c("0", "1")),
    # feature_lab = "",
    # feature_lab = ifelse(feature == "attrib1_lab", "Recipient developing country", feature_lab),
    # feature_lab = ifelse(feature == "attrib2_lab", "Number of climate\nmigrants to accept\nfrom this country per year", feature_lab),
    # feature_lab = ifelse(feature == "attrib3_lab", "Climate aid to give\nto this country\n(CHF) per year", feature_lab),
    # feature_lab = ifelse(feature == "attrib4_lab", "Value of Swiss\ntrade with this\ncountry", feature_lab),
    # feature_lab = ifelse(feature == "attrib5_lab", "Extreme weather event", feature_lab),
    # feature_lab = ifelse(feature == "attrib6_lab", "Percentage of this\ncountry's votes\nin line with Switzerland's\nposition at the UN\nSecurity Council", feature_lab),
    # feature_lab = factor(feature_lab, levels = c("Recipient developing country", "Number of climate\nmigrants to accept\nfrom this country per year",
    #                                              "Climate aid to give\nto this country\n(CHF) per year", "Value of Swiss\ntrade with this\ncountry",
    #                                              "Extreme weather event", "Percentage of this\ncountry's votes\nin line with Switzerland's\nposition at the UN\nSecurity Council"))
  ) %>% 
  ggplot(aes(level, estimate)) + 
  geom_pointrange(aes(ymin = lower, ymax = upper)) +
  scale_x_discrete(limits=rev) +
  facet_grid(feature~., scales = "free_y", space = "free_y") +
  theme_light() + 
  coord_flip() +
  labs(y ="AMCE", x = "", subtitle = "only NIMBY treated") +
  theme(panel.grid = element_blank(),
        strip.text.y = element_text(angle = 0),
        strip.background = element_rect(fill = "white"),
        strip.text = element_text(colour = "black", size =9)) +
  geom_hline(yintercept = 0, lty = 2, alpha = 0.7, color = "gray50") 

ggsave(p_baseline_treated, file = "plots/p_baseline_treated.pdf", width = 7, height = 7)

export_table <- function(name, col_label, formula, by){
  require(jtools)
  require(kableExtra)
  require(dplyr)
  if (!is.null(by)) {
    by_cregg <- as.formula(by)
  } else {
    by_cregg <- by
  }
  formula_cregg <- as.formula(formula)
  amce_by <- cj(dat, formula_cregg, id = ~id, by = by_cregg, estimate = "amce")
  mm_by <- cj(dat, formula_cregg, id = ~id, by = by_cregg, estimate = "mm", h0 = 0.5)
  amce_by <- amce_by %>% dplyr::select(any_of(c("BY", "feature", "level", "estimate", "p"))) %>% dplyr::rename("estimate AMCE" = "estimate", "p-value AMCE" = "p")
  mm_by <- mm_by %>% dplyr::select(any_of(c("BY", "feature", "level", "estimate", "p"))) %>% dplyr::rename("estimate MM" = "estimate", "p-value MM" = "p")
  if (!is.null(by)) {
    formula_lm <- paste0(unlist(strsplit(as.character(formula), "+",  fixed = T)), collapse = paste0("*", gsub("~", "", by), " +"))
  } else {
    formula_lm <- formula_cregg
  }
  res <- lm(formula_lm, dat)
  x <- summary(res)
  x <- coeftest(res, vcov = vcovCL(res, cluster = ~id, type = "HC"))
  
  if (!is.null(by)) {
    out <- mm_by %>% dplyr::select(level, feature, BY, `estimate MM`, `p-value MM`) %>% left_join(amce_by, by = c("feature", "level", "BY")) 
  } else {
    out <- mm_by %>% dplyr::select(level, feature, `estimate MM`, `p-value MM`) %>% left_join(amce_by, by = c("feature", "level"))
  }
  gof <- as.data.frame(rbind(c("Number of observations", "", "",  "",nobs(res), ""),
                             c("R2", "", "",  "",sapply(lapply(list(res), summ), attr, "rsq"), ""),
                             c("Adj.R2", "", "",  "",sapply(lapply(list(res), summ), attr, "arsq"), ""))) %>% 
    mutate(V2 = factor(V2),
           V5 = round(as.numeric(V5), 4)) %>% 
    mutate_if(is.double, as.character)
  if (!is.null(by)) {gof <- cbind(gof[,1:2], rep("", nrow(gof)), gof[,3:6])}
  colnames(gof) <- colnames(out)
  
  options(scipen = 999)
  c_names <- if(!is.null(by)) {
    c("Attribute levels", col_label, "Estimate", "p-value", "Estimate", "p-value")
  } else {
    c("Attribute levels", "Estimate", "p-value", "Estimate", "p-value")
  }
  tab <- out %>%
    dplyr::select(-feature) %>% 
    mutate_at(vars("estimate MM", "p-value MM", "estimate AMCE", "p-value AMCE"), round, 4) %>%
    mutate_at(vars("estimate MM", "p-value MM", "estimate AMCE", "p-value AMCE"), as.character) %>% 
    bind_rows(gof %>% dplyr::select(-feature)) %>%
    # dplyr::select(all_of("Attribute levels", col_label, c("Estimate", "p-value", "Estimate", "p-value")))
    mutate(`estimate AMCE` = ifelse(is.na(`p-value AMCE`), "baseline", `estimate AMCE`)) %>%
    mutate(`p-value AMCE` = ifelse(is.na(`p-value AMCE`), "", `p-value AMCE`)) %>%
    mutate(`estimate MM` = ifelse(is.na(`estimate MM`), "", `estimate MM`)) %>%
    kable(format = 'latex', booktabs = TRUE, col.names = c_names, caption = paste(name, "estimates"), longtable = T) %>% 
    kable_styling(font_size = 9) %>% 
    footnote("Standard errors for the computation of p-values were clustered by respondent id. For MM estimates that are derived from AMCEs (for details, see Leeper et al., 2018), p-values are computed under null hypothesis that the estimate is equal to 0.5 for the binary choice outcome where respondents choose the preferred policy package when presented with two policy pairs with randomized attribute levels. The label for the attribute level ’Percentage of this country's votes in line with Switzerland's position at the UN Security Council’ was replaced with to ’UN Security Council votes in line with Switzerland’ for better readability.",
             footnote_as_chunk = T,
             threeparttable = TRUE,
    )
  
  if (!is.null(by)){
    tab <- tab %>% add_header_above(c(" " = 2, "MM" = 2, "AMCE" = 2))
  } else {
    tab <- tab %>% add_header_above(c(" " = 1, "MM" = 2, "AMCE" = 2))
  }
  
  attr <- data.frame(attribute_levels = c("Recipient developing country", "Number of climate migrants to accept from this country per year",
                                          "Climate aid to give to this country (CHF) per year", "Value of Swiss trade with this country",
                                          "Extreme weather event", "UN Security Council votes in line with Switzerland"),
                     len = c(4,6,5,3,4,3))
  
  
  
  attributes_to_keep <- as.numeric(unlist(stri_extract_all_regex(unique(out$feature), "[0-9]")))
  attr <- attr[attributes_to_keep,]
  
  attr$end <- cumsum(attr$len)
  attr$start <- attr$end - attr$len + 1
  
  
  len <- ifelse(!is.null(by), length(unique(out$BY)), 1)
  for (i in 1:len){
    add <- (i-1)*length(levels(out$level))
    for (j in 1:length(attr$attribute_levels)){
      tab <- tab %>% pack_rows(attr$attribute_levels[j], attr$start[j] + add, attr$end[j] + add)
    }
  }
  tab <- tab %>% kableExtra::row_spec(length(levels(out$level))*len, extra_latex_after = "\\midrule")
  writeLines(tab, paste0("tables/", name, ".tex"))
}
export_table("Baseline", "", "choice ~ attrib1_lab + attrib2_lab + attrib3_lab + attrib4_lab + attrib5_lab + attrib6_lab", NULL)

mm_by <- cj(dat, rate_binary ~ attrib1_lab + attrib2_lab + attrib3_lab + attrib4_lab + attrib5_lab + attrib6_lab + attrib7_lab, id = ~id, estimate = "mm", by = ~NIMBY)

p_NIMBY <- mm_by %>% 
  mutate(by = factor(BY, levels = c("0", "1")),
         # feature_lab = "",
         # feature_lab = ifelse(feature == "attrib1_lab", "Recipient developing country", feature_lab),
         # feature_lab = ifelse(feature == "attrib2_lab", "Number of climate\nmigrants to accept\nfrom this country per year", feature_lab),
         # feature_lab = ifelse(feature == "attrib3_lab", "Climate aid to give\nto this country\n(CHF) per year", feature_lab),
         # feature_lab = ifelse(feature == "attrib4_lab", "Value of Swiss\ntrade with this\ncountry", feature_lab),
         # feature_lab = ifelse(feature == "attrib5_lab", "Extreme weather event", feature_lab),
         # feature_lab = ifelse(feature == "attrib6_lab", "Percentage of this\ncountry's votes\nin line with Switzerland's\nposition at the UN\nSecurity Council", feature_lab),
         # feature_lab = factor(feature_lab, levels = c("Recipient developing country", "Number of climate\nmigrants to accept\nfrom this country per year",
         #                                              "Climate aid to give\nto this country\n(CHF) per year", "Value of Swiss\ntrade with this\ncountry",
         #                                              "Extreme weather event", "Percentage of this\ncountry's votes\nin line with Switzerland's\nposition at the UN\nSecurity Council"))
  ) %>% 
  ggplot(aes(level, estimate, shape = by, col = by)) + 
  geom_pointrange(aes(ymin = lower, ymax = upper), position = position_dodge(.5)) +
  scale_x_discrete(limits=rev) +
  facet_grid(feature~ ., scales = "free_y", 
             space = "free_y"
  ) +
  theme_light() + 
  coord_flip() +
  scale_colour_npg() +
  labs(y ="MM", x = "", subtitle = "Interaction with NIMBY treatment, binary recoding of rate outcoome") +
  theme(panel.grid = element_blank(),
        legend.position = "bottom",
        strip.text.y = element_text(angle = 0),
        strip.background = element_rect(fill = "white"),
        strip.text = element_text(colour = "black", size =9)) +
  geom_hline(yintercept = 0.5, lty = 2, alpha = 0.7, color = "gray50") 
ggsave(p_NIMBY, file = "plots/p_NIMBY.pdf", width = 7, height = 7)


dat <- dat %>% 
  mutate(circle1 = factor(circle1, levels = c(0,1)),
         circle2 = factor(circle2, levels = c(0,1)),
         circle3 = factor(circle3, levels = c(0,1)),
         circle = factor(circle, levels = c(0,1)))
mm_by <- cj(dat, rate_binary ~ attrib1_lab + attrib2_lab + attrib3_lab + attrib4_lab + attrib5_lab + attrib6_lab + attrib7_lab, id = ~id, estimate = "mm", by = ~circle)

p_environment_score <- mm_by %>% 
  mutate(by = factor(BY, levels = c("0", "1")),
         # feature_lab = "",
         # feature_lab = ifelse(feature == "attrib1_lab", "Recipient developing country", feature_lab),
         # feature_lab = ifelse(feature == "attrib2_lab", "Number of climate\nmigrants to accept\nfrom this country per year", feature_lab),
         # feature_lab = ifelse(feature == "attrib3_lab", "Climate aid to give\nto this country\n(CHF) per year", feature_lab),
         # feature_lab = ifelse(feature == "attrib4_lab", "Value of Swiss\ntrade with this\ncountry", feature_lab),
         # feature_lab = ifelse(feature == "attrib5_lab", "Extreme weather event", feature_lab),
         # feature_lab = ifelse(feature == "attrib6_lab", "Percentage of this\ncountry's votes\nin line with Switzerland's\nposition at the UN\nSecurity Council", feature_lab),
         # feature_lab = factor(feature_lab, levels = c("Recipient developing country", "Number of climate\nmigrants to accept\nfrom this country per year",
         #                                              "Climate aid to give\nto this country\n(CHF) per year", "Value of Swiss\ntrade with this\ncountry",
         #                                              "Extreme weather event", "Percentage of this\ncountry's votes\nin line with Switzerland's\nposition at the UN\nSecurity Council"))
  ) %>% 
  ggplot(aes(level, estimate, shape = by, col = by)) + 
  geom_pointrange(aes(ymin = lower, ymax = upper), position = position_dodge(.5)) +
  scale_x_discrete(limits=rev) +
  facet_grid(feature~ ., scales = "free_y", 
             space = "free_y"
  ) +
  theme_light() + 
  coord_flip() +
  scale_colour_npg() +
  labs(y ="MM", x = "", subtitle = "Interaction with environment_score, binary recoding of rate outcoome") +
  theme(panel.grid = element_blank(),
        legend.position = "bottom",
        strip.text.y = element_text(angle = 0),
        strip.background = element_rect(fill = "white"),
        strip.text = element_text(colour = "black", size =9)) +
  geom_hline(yintercept = 0.5, lty = 2, alpha = 0.7, color = "gray50") 
ggsave(p_environment_score, file = "plots/p_environment_score.pdf", width = 7, height = 7)


mm_by <- cj(dat, rate_binary ~ attrib1_lab + attrib2_lab + attrib3_lab + attrib4_lab + attrib5_lab + attrib6_lab + attrib7_lab, id = ~id, estimate = "mm", by = ~left_right_bins)

p_left_right_bins <- mm_by %>% 
  mutate(by = factor(BY, levels = c("Left", "Centre", "Right")),
         # feature_lab = "",
         # feature_lab = ifelse(feature == "attrib1_lab", "Recipient developing country", feature_lab),
         # feature_lab = ifelse(feature == "attrib2_lab", "Number of climate\nmigrants to accept\nfrom this country per year", feature_lab),
         # feature_lab = ifelse(feature == "attrib3_lab", "Climate aid to give\nto this country\n(CHF) per year", feature_lab),
         # feature_lab = ifelse(feature == "attrib4_lab", "Value of Swiss\ntrade with this\ncountry", feature_lab),
         # feature_lab = ifelse(feature == "attrib5_lab", "Extreme weather event", feature_lab),
         # feature_lab = ifelse(feature == "attrib6_lab", "Percentage of this\ncountry's votes\nin line with Switzerland's\nposition at the UN\nSecurity Council", feature_lab),
         # feature_lab = factor(feature_lab, levels = c("Recipient developing country", "Number of climate\nmigrants to accept\nfrom this country per year",
         #                                              "Climate aid to give\nto this country\n(CHF) per year", "Value of Swiss\ntrade with this\ncountry",
         #                                              "Extreme weather event", "Percentage of this\ncountry's votes\nin line with Switzerland's\nposition at the UN\nSecurity Council"))
  ) %>% 
  ggplot(aes(level, estimate, shape = by, col = by)) + 
  geom_pointrange(aes(ymin = lower, ymax = upper), position = position_dodge(.5)) +
  scale_x_discrete(limits=rev) +
  facet_grid(feature~ ., scales = "free_y", 
             space = "free_y"
  ) +
  theme_light() + 
  coord_flip() +
  scale_colour_npg() +
  labs(y ="AMCE", x = "", subtitle = "Interaction with left-right, binary recoding of rate outcoome") +
  theme(panel.grid = element_blank(),
        legend.position = "bottom",
        strip.text.y = element_text(angle = 0),
        strip.background = element_rect(fill = "white"),
        strip.text = element_text(colour = "black", size =9)) +
  geom_hline(yintercept = 0.5, lty = 2, alpha = 0.7, color = "gray50") 
ggsave(p_left_right_bins, file = "plots/p_left_right_bins.pdf", width = 7, height = 7)

dat <- dat %>% 
  mutate(left_right_binary = ifelse(left_right >= 5, "Right", "Left")) %>% 
  mutate(left_right_binary = factor(left_right_binary, levels = c("Right", "Left")))

mm_by <- cj(dat, rate_binary ~ attrib1_lab + attrib2_lab + attrib3_lab + attrib4_lab + attrib5_lab + attrib6_lab + attrib7_lab, id = ~id, estimate = "mm", by = ~left_right_binary)

p_left_right_binary <- mm_by %>% 
  mutate(by = factor(BY, levels = c("Left", "Right")),
         # feature_lab = "",
         # feature_lab = ifelse(feature == "attrib1_lab", "Recipient developing country", feature_lab),
         # feature_lab = ifelse(feature == "attrib2_lab", "Number of climate\nmigrants to accept\nfrom this country per year", feature_lab),
         # feature_lab = ifelse(feature == "attrib3_lab", "Climate aid to give\nto this country\n(CHF) per year", feature_lab),
         # feature_lab = ifelse(feature == "attrib4_lab", "Value of Swiss\ntrade with this\ncountry", feature_lab),
         # feature_lab = ifelse(feature == "attrib5_lab", "Extreme weather event", feature_lab),
         # feature_lab = ifelse(feature == "attrib6_lab", "Percentage of this\ncountry's votes\nin line with Switzerland's\nposition at the UN\nSecurity Council", feature_lab),
         # feature_lab = factor(feature_lab, levels = c("Recipient developing country", "Number of climate\nmigrants to accept\nfrom this country per year",
         #                                              "Climate aid to give\nto this country\n(CHF) per year", "Value of Swiss\ntrade with this\ncountry",
         #                                              "Extreme weather event", "Percentage of this\ncountry's votes\nin line with Switzerland's\nposition at the UN\nSecurity Council"))
  ) %>% 
  ggplot(aes(level, estimate, shape = by, col = by)) + 
  geom_pointrange(aes(ymin = lower, ymax = upper), position = position_dodge(.5)) +
  scale_x_discrete(limits=rev) +
  facet_grid(feature~ ., scales = "free_y", 
             space = "free_y"
  ) +
  theme_light() + 
  coord_flip() +
  scale_colour_npg() +
  labs(y ="MM", x = "", subtitle = "Interaction with left-right (Right >=5), binary recoding of rate outcoome") +
  theme(panel.grid = element_blank(),
        legend.position = "bottom",
        strip.text.y = element_text(angle = 0),
        strip.background = element_rect(fill = "white"),
        strip.text = element_text(colour = "black", size =9)) +
  geom_hline(yintercept = 0.5, lty = 2, alpha = 0.7, color = "gray50") 
ggsave(p_left_right_binary, file = "plots/p_left_right_binary.pdf", width = 7, height = 7)


dat <- dat %>% 
  mutate(urban_rural = factor(urban_rural, levels = c(1,2,3)))
mm_by <- cj(dat, rate_binary ~ attrib1_lab + attrib2_lab + attrib3_lab + attrib4_lab + attrib5_lab + attrib6_lab + attrib7_lab, id = ~id, estimate = "mm", by = ~urban_rural)

p_urban_rural <- mm_by %>% 
  mutate(by = factor(BY, levels = c(1, 2, 3)),
         # feature_lab = "",
         # feature_lab = ifelse(feature == "attrib1_lab", "Recipient developing country", feature_lab),
         # feature_lab = ifelse(feature == "attrib2_lab", "Number of climate\nmigrants to accept\nfrom this country per year", feature_lab),
         # feature_lab = ifelse(feature == "attrib3_lab", "Climate aid to give\nto this country\n(CHF) per year", feature_lab),
         # feature_lab = ifelse(feature == "attrib4_lab", "Value of Swiss\ntrade with this\ncountry", feature_lab),
         # feature_lab = ifelse(feature == "attrib5_lab", "Extreme weather event", feature_lab),
         # feature_lab = ifelse(feature == "attrib6_lab", "Percentage of this\ncountry's votes\nin line with Switzerland's\nposition at the UN\nSecurity Council", feature_lab),
         # feature_lab = factor(feature_lab, levels = c("Recipient developing country", "Number of climate\nmigrants to accept\nfrom this country per year",
         #                                              "Climate aid to give\nto this country\n(CHF) per year", "Value of Swiss\ntrade with this\ncountry",
         #                                              "Extreme weather event", "Percentage of this\ncountry's votes\nin line with Switzerland's\nposition at the UN\nSecurity Council"))
  ) %>% 
  ggplot(aes(level, estimate, shape = by, col = by)) + 
  geom_pointrange(aes(ymin = lower, ymax = upper), position = position_dodge(.5)) +
  scale_x_discrete(limits=rev) +
  facet_grid(feature~ ., scales = "free_y", 
             space = "free_y"
  ) +
  theme_light() + 
  coord_flip() +
  scale_colour_npg() +
  labs(y ="MM", x = "", subtitle = "Interaction with urban_rural, binary recoding of rate outcoome") +
  theme(panel.grid = element_blank(),
        legend.position = "bottom",
        strip.text.y = element_text(angle = 0),
        strip.background = element_rect(fill = "white"),
        strip.text = element_text(colour = "black", size =9)) +
  geom_hline(yintercept = 0.5, lty = 2, alpha = 0.7, color = "gray50") 
ggsave(p_urban_rural, file = "plots/p_urban_rural.pdf", width = 7, height = 7)

