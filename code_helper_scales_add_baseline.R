
# ----- R UNIVERSE PREP -----

rm(list = ls()) # remove everything old that might be in my var space
knitr::opts_chunk$set(echo = TRUE) # this is to connect markdown and r - i don't think i need this but whatever
pacman::p_load(tidyverse, nlme, CorrMixed, lme4, lmerTest, geepack, ggplot2, geomtextpath, car, emmeans, effectsize, simr, matrixStats, haven) # pacman is a package manager for R (!!! useful). Install packages if missing and load them 
options(scipen=999) # strongly avoid scientific notations (none of the e bs)

# ----- LOAD DATA -----

data_eo <- read.table("/Users/abhishekanand/Documents/OTPH_TF/dat_EO_relPower_withbehav_missval.csv", header=T, sep=",")
data_ec <- read.table("/Users/abhishekanand/Documents/OTPH_TF/dat_EC_relPower_withbehav_missval.csv", header=T, sep=",")

# open baseline behav data
data_behav <- read_sav("/Users/abhishekanand/Documents/OTPH_TF/dat_OTPH_Responses.sav")
#View(data_behav)

# enrich my data with baseline vals
# Create the new columns
data_eo$Alertness_base <- NA_real_
data_eo$Excitement_base <- NA_real_
data_eo$Sociability_base <- NA_real_

data_ec$Alertness_base <- NA_real_
data_ec$Excitement_base <- NA_real_
data_ec$Sociability_base <- NA_real_

#assign vals (skipping the excluded subj 5)
for (id_val in unique(data_eo$id)) {
  if (id_val <= 4) {row_behav <- id_val} else {row_behav <- id_val + 1}
  # ec and eo share the same behav data (exp design: EC --> EO --> behav ratings)
  # --> OT data
  # ----> Alertness
  data_eo$Alertness_base[data_eo$Drug == 1 & data_eo$id == id_val] <- data_behav$Gr1_Scale_1_0[row_behav]
  data_ec$Alertness_base[data_ec$Drug == 1 & data_ec$id == id_val] <- data_behav$Gr1_Scale_1_0[row_behav]
  # ----> Excitement
  data_eo$Excitement_base[data_eo$Drug == 1 & data_eo$id == id_val] <- data_behav$Gr1_Scale_2_0[row_behav]
  data_ec$Excitement_base[data_ec$Drug == 1 & data_ec$id == id_val] <- data_behav$Gr1_Scale_2_0[row_behav] 
  # ----> Sociability
  data_eo$Sociability_base[data_eo$Drug == 1 & data_eo$id == id_val] <- data_behav$Gr1_Scale_3_0[row_behav]
  data_ec$Sociability_base[data_ec$Drug == 1 & data_ec$id == id_val] <- data_behav$Gr1_Scale_3_0[row_behav] 
  # --> PL data
  # ----> Alertness
  data_eo$Alertness_base[data_eo$Drug == 2 & data_eo$id == id_val] <- data_behav$Gr2_Scale_1_0[row_behav]
  data_ec$Alertness_base[data_ec$Drug == 2 & data_ec$id == id_val] <- data_behav$Gr2_Scale_1_0[row_behav]
  # ----> Excitement
  data_eo$Excitement_base[data_eo$Drug == 2 & data_eo$id == id_val] <- data_behav$Gr2_Scale_2_0[row_behav]
  data_ec$Excitement_base[data_ec$Drug == 2 & data_ec$id == id_val] <- data_behav$Gr2_Scale_2_0[row_behav] 
  # ----> Sociability
  data_eo$Sociability_base[data_eo$Drug == 2 & data_eo$id == id_val] <- data_behav$Gr2_Scale_3_0[row_behav]
  data_ec$Sociability_base[data_ec$Drug == 2 & data_ec$id == id_val] <- data_behav$Gr2_Scale_3_0[row_behav] 
}

# reversed baseline scores
# -- EO
data_eo$Alertness_base_reversed <- 6 - data_eo$Alertness_base
data_eo$Excitement_base_reversed <- 6 - data_eo$Excitement_base
data_eo$Sociability_base_reversed <- 6 - data_eo$Sociability_base
# -- EC
data_ec$Alertness_base_reversed <- 6 - data_ec$Alertness_base
data_ec$Excitement_base_reversed <- 6 - data_ec$Excitement_base
data_ec$Sociability_base_reversed <- 6 - data_ec$Sociability_base

# save them
write.csv(data_ec, "/Users/abhishekanand/Documents/OTPH_TF/RR/data_rr/data_ec_behavbl.csv", row.names = FALSE)
write.csv(data_eo, "/Users/abhishekanand/Documents/OTPH_TF/RR/data_rr/data_eo_behavbl.csv", row.names = FALSE)