# This script automatically runs the scripts to clean and analyze the data and to produce all outputs used in the main manuscript and the supplementary information
# Authors: Lukas Fesenfeld, Leon Sistek, Simon Montfort, Dionis Anderegg, Jürg Rohrer, Tobias Schmidt 
# Date: 06.05.2026

#Adjust working directory here
setwd("[Specify working directory here]")


# Run cleaning steps
source("code/Cleaning.R", echo = TRUE)

# Run regression anaylsis
source("code/Analysis.R",echo = TRUE)

# Run conjoint anaylsis
source("code/Conjoint.R", echo = TRUE)