#### D2: Insects Main ####
## Function: A 3class LCM with MNL parameters and no time preferences
## Author: Dr Peter King (p.king1@leeds.ac.uk)
## Last change: 28/01/2025
## - No MNL, No discounting, new class-allocation


# ****************************
# Replication Information: ####
# ****************************



# ****************************
# Setup Environment: ####
# ****************************


## Libraries that will come in handy later
library(apollo)
library(dplyr)
library(magrittr)
library(ggplot2)
library(ggridges)
library(reshape2)
library(mded)
library(here)
library(data.table)
library(MASS)
library(janitor)
library(tidyverse)


# ****************************
# Import Data: ####
# ****************************


database <- here("Data/Main", "database_Step3.csv") %>% fread() %>% data.frame()


database$Insect_PlanA_Wasp <- ifelse(database$Insect_PlanA == 1, 1, 0)
database$Insect_PlanA_Bee <- ifelse(database$Insect_PlanA == 2, 1, 0)
database$Insect_PlanA_Beetle <- ifelse(database$Insect_PlanA == 3, 1, 0)

database$Insect_PlanB_Wasp <- ifelse(database$Insect_PlanB == 1, 1, 0)
database$Insect_PlanB_Bee <- ifelse(database$Insect_PlanB == 2, 1, 0)
database$Insect_PlanB_Beetle <- ifelse(database$Insect_PlanB == 3, 1, 0)

## Rescaling to ~[0,1]
database$Biowell_Bee1_Mean = database$Biowell_Bee1_Mean %>% scale() %>% as.numeric()
database$Biowell_Bee2_Mean = database$Biowell_Bee2_Mean %>% scale() %>% as.numeric()
database$Biowell_Bee3_Mean = database$Biowell_Bee3_Mean %>% scale() %>% as.numeric()
database$Biowell_Beetle1_Mean = database$Biowell_Beetle1_Mean %>% scale() %>% as.numeric()
database$Biowell_Beetle2_Mean = database$Biowell_Beetle2_Mean %>% scale() %>% as.numeric()
database$Biowell_Beetle3_Mean = database$Biowell_Beetle3_Mean %>% scale() %>% as.numeric()
database$Biowell_Wasp1_Mean = database$Biowell_Wasp1_Mean %>% scale() %>% as.numeric()
database$Biowell_Wasp2_Mean = database$Biowell_Wasp2_Mean %>% scale() %>% as.numeric()
database$Biowell_Wasp3_Mean = database$Biowell_Wasp3_Mean %>% scale() %>% as.numeric()

## Recode for ease of interpretation
database$DiscountRate_Scaled_Recoded <- dplyr::recode(database$DiscountRate,
                                                      '0.00' = "1",
                                                      '5.26' = "2",
                                                      '33.33' = "3",
                                                      '100.00' = "4",
                                                      '300.00' = "5",
                                                      '700.00' = "6",
                                                      '1000.00' = "7",
                                                      '1900.00' = "8") %>% 
  as.numeric() %>% scale() %>% as.numeric()


database$IncomeMidpoints_PlusMissing_Recoded <- ifelse(database$IncomeMidpoints_PlusMissing < 500, 
                                                       250, 
                                                       database$IncomeMidpoints_PlusMissing) %>% 
  as.numeric() %>% 
  scale() %>% 
  as.numeric()



database$Q3Country_England <- ifelse( database$Q3Country == 1, 
                                      1, 
                                      0)


database$Biowell_Debrief_BeeCertain <- database$Biowell_Debrief_BeeCertain %>% 
  scale() %>% 
  as.numeric()

database$Biowell_Debrief_BeetleCertain <- database$Biowell_Debrief_BeetlesCertain %>% 
  scale() %>% 
  as.numeric()

database$Biowell_Debrief_WaspCertain <- database$Biowell_Debrief_WaspCertain %>% 
  scale() %>% 
  as.numeric()

# 
# database$Biowell_Debrief_BeeCertain <- ifelse(database$Biowell_Debrief_BeeCertain < median(database$Biowell_Debrief_BeeCertain), 0, 1)
# database$Biowell_Debrief_BeetlesCertain <- ifelse(database$Biowell_Debrief_BeetlesCertain < median(database$Biowell_Debrief_BeetlesCertain), 0, 1)
# database$Biowell_Debrief_WaspCertain <- ifelse(database$Biowell_Debrief_WaspCertain < median(database$Biowell_Debrief_WaspCertain), 0, 1)


database$CE_Debrief_Certain_scaled <- database$CE_Debrief_Certain %>% scale() %>% as.numeric()
database$CE_Debrief_Confident_scaled <- database$CE_Debrief_Confident %>% scale() %>% as.numeric()
# 
# database$CE_Debrief_Certain <- ifelse(database$CE_Debrief_Certain < median(database$CE_Debrief_Certain), 0, 1)
# database$CE_Debrief_Confident <- ifelse(database$CE_Debrief_Confident < median(database$CE_Debrief_Confident), 0, 1)

## Flag people who chose £0 over £200
database$AlwaysZero <- ifelse(database$QDiscounting_7 == 2, 1, 0)


# ## Drop weird responses
database <- database %>% dplyr::filter(SerialSQ != 1 &
                                         Protest_True == 1 &
                                         CE_ANA_None != 1)

# ## Keep only relevant columns
database <- database[, c(
  "Choice",
  "Respondent",
  "Task",
  "ID",
  "Tax_PlanA",
  "Tax_PlanB",
  "Tax_PlanA_Values",
  "Tax_PlanB_Values",
  "Encounter_PlanA_Values",
  "Encounter_PlanB_Values",
  "Existence_PlanA_Values",
  "Existence_PlanB_Values",
  "Bequest_PlanA_Values",
  "Bequest_PlanB_Values",
  "Insect_PlanA",
  "Insect_PlanB",
  "Insect_PlanC",
  "Insect_PlanA_Wasp",
  "Insect_PlanA_Bee",
  "Insect_PlanB_Wasp",
  "Insect_PlanB_Bee",
  "Insect_PlanA_Beetle",
  "Insect_PlanB_Beetle",
  
  
  "Biowell_Bee1_Mean",
  "Biowell_Bee2_Mean",
  "Biowell_Bee3_Mean",
  "Biowell_Beetle1_Mean",
  "Biowell_Beetle2_Mean",
  "Biowell_Beetle3_Mean",
  "Biowell_Wasp1_Mean",
  "Biowell_Wasp2_Mean",
  "Biowell_Wasp3_Mean",
  
  "Q1Age",
  "IncomeMidpoints_PlusMissing_Recoded",
  "Female_dummy",
  "Q3Country_England",
  "CE_Debrief_Certain_scaled",
  "CE_Debrief_Confident_scaled",
  "Q43_Citizen",
  "AlwaysZero",
  "Biowell_Debrief_BeeCertain",
  "Biowell_Debrief_BeetleCertain",
  "Biowell_Debrief_WaspCertain"
)]


## Necessary to get apollo working
apollo_initialise()


# ****************************
# Estimation Basics: ####
# ****************************


## Note 10 cores as I'm using the University of Kent 'Tesla' HPC:
apollo_control = list(
  nCores = 30,
  mixing = TRUE,
  modelName = "D2_Truncated_LC_3C_MXL_NoDR_V1",
  modelDescr  = "So hybrid idea but not hybrid",
  indivID    = "Respondent",
  ## This is the name of a column in the database indicating each unique respondent
  outputDirectory = "CEOutput/Main/LCM"
)


# ################################################################# #
#### DEFINE MODEL PARAMETERS                                     ####
# ################################################################# #



## Define parameters starting values:
apollo_beta=c(
  asc_C_Class1 = -0.397,
  asc_C_Class2 = -0.092,
  asc_C_Class3 = -2.751,
  beta_Tax_Class1 = -0.022,
  beta_Tax_Class2 = -0.084,
  beta_Tax_Class3 = -0.106,
  mu_Encounter_Medium_Class1 = 20.793,
  mu_Encounter_High_Class1 = 24.312,
  mu_Existence_Medium_Class1 = 11.477,
  mu_Existence_High_Class1 = 20.437,
  mu_Bequest_Medium_Class1 = 27.039,
  mu_Bequest_High_Class1 = 53.011,
  mu_Int_Encounter_Medium_Bee_Class1 = 10.838,
  mu_Int_Encounter_High_Bee_Class1 = 44.871,
  mu_Int_Existence_Medium_Bee_Class1 = -9.793,
  mu_Int_Existence_High_Bee_Class1 = 44.658,
  mu_Int_Bequest_Medium_Bee_Class1 = 33.771,
  mu_Int_Bequest_High_Bee_Class1 = 64.063,
  mu_Int_Existence_Medium_Wasp_Class1 = 23.917,
  mu_Int_Existence_High_Wasp_Class1 = 39.130,
  mu_Int_Encounter_Medium_Wasp_Class1 = 42.142,
  mu_Int_Encounter_High_Wasp_Class1 = 41.193,
  mu_Int_Bequest_Medium_Wasp_Class1 = 5.705,
  mu_Int_Bequest_High_Wasp_Class1 = 28.955,
  mu_Encounter_Medium_Class2 = -5.861,
  mu_Encounter_High_Class2 = -11.672,
  mu_Existence_Medium_Class2 = -10.459,
  mu_Existence_High_Class2 = -20.718,
  mu_Bequest_Medium_Class2 = 13.272,
  mu_Bequest_High_Class2 = -2.094,
  mu_Int_Encounter_Medium_Bee_Class2 = -10.991,
  mu_Int_Encounter_High_Bee_Class2 = 4.540,
  mu_Int_Existence_Medium_Bee_Class2 = 5.885,
  mu_Int_Existence_High_Bee_Class2 = -5.410,
  mu_Int_Bequest_Medium_Bee_Class2 = -5.257,
  mu_Int_Bequest_High_Bee_Class2 = 3.034,
  mu_Int_Existence_Medium_Wasp_Class2 = 1.206,
  mu_Int_Existence_High_Wasp_Class2 = -15.896,
  mu_Int_Encounter_Medium_Wasp_Class2 = -4.536,
  mu_Int_Encounter_High_Wasp_Class2 = -21.933,
  mu_Int_Bequest_Medium_Wasp_Class2 = 5.057,
  mu_Int_Bequest_High_Wasp_Class2 = -2.112,
  mu_Encounter_Medium_Class3 = -0.848,
  mu_Encounter_High_Class3 = 2.547,
  mu_Existence_Medium_Class3 = 10.687,
  mu_Existence_High_Class3 = 2.801,
  mu_Bequest_Medium_Class3 = -14.494,
  mu_Bequest_High_Class3 = 2.160,
  mu_Int_Encounter_Medium_Bee_Class3 = -5.848,
  mu_Int_Encounter_High_Bee_Class3 = 0.221,
  mu_Int_Existence_Medium_Bee_Class3 = -0.135,
  mu_Int_Existence_High_Bee_Class3 = -0.343,
  mu_Int_Bequest_Medium_Bee_Class3 = 7.710,
  mu_Int_Bequest_High_Bee_Class3 = -1.106,
  mu_Int_Existence_Medium_Wasp_Class3 = -7.369,
  mu_Int_Existence_High_Wasp_Class3 = -12.268,
  mu_Int_Encounter_Medium_Wasp_Class3 = 8.714,
  mu_Int_Encounter_High_Wasp_Class3 = 0.070,
  mu_Int_Bequest_Medium_Wasp_Class3 = -15.511,
  mu_Int_Bequest_High_Wasp_Class3 = -10.697,
  
  
  sig_Encounter_Medium_Class1 = -3.01629,
  sig_Encounter_High_Class1 = -6.83844,
  sig_Existence_Medium_Class1 = 4.12786,
  sig_Existence_High_Class1 = 0.38219,
  sig_Bequest_Medium_Class1 = -0.55572,
  sig_Bequest_High_Class1 = -0.07438,
  sig_Int_Encounter_Medium_Bee_Class1 = 3.45219,
  sig_Int_Encounter_High_Bee_Class1 = -1.62648,
  sig_Int_Existence_Medium_Bee_Class1 = 2.02641,
  sig_Int_Existence_High_Bee_Class1 = 4.01376,
  sig_Int_Bequest_Medium_Bee_Class1 = 4.71025,
  sig_Int_Bequest_High_Bee_Class1 = 2.89755,
  sig_Int_Existence_Medium_Wasp_Class1 = 0.71182,
  sig_Int_Existence_High_Wasp_Class1 = 0.71530,
  sig_Int_Encounter_Medium_Wasp_Class1 = -0.68743,
  sig_Int_Encounter_High_Wasp_Class1 = -0.31232,
  sig_Int_Bequest_Medium_Wasp_Class1 = -0.71290,
  sig_Int_Bequest_High_Wasp_Class1 = 0.70360,
  sig_Encounter_Medium_Class2 = -0.68762,
  sig_Encounter_High_Class2 = 3.16067,
  sig_Existence_Medium_Class2 = -2.85425,
  sig_Existence_High_Class2 = 1.96458,
  sig_Bequest_Medium_Class2 = -17.66053,
  sig_Bequest_High_Class2 = -0.30720,
  sig_Int_Encounter_Medium_Bee_Class2 = 12.54186,
  sig_Int_Encounter_High_Bee_Class2 = -6.87402,
  sig_Int_Existence_Medium_Bee_Class2 = 10.45593,
  sig_Int_Existence_High_Bee_Class2 = -12.59647,
  sig_Int_Bequest_Medium_Bee_Class2 = 102.54571,
  sig_Int_Bequest_High_Bee_Class2 = -1.28872,
  sig_Int_Existence_Medium_Wasp_Class2 = -1.66039,
  sig_Int_Existence_High_Wasp_Class2 = 4.34946,
  sig_Int_Encounter_Medium_Wasp_Class2 = 0.56529,
  sig_Int_Encounter_High_Wasp_Class2 = -0.78766,
  sig_Int_Bequest_Medium_Wasp_Class2 = -3.43458,
  sig_Int_Bequest_High_Wasp_Class2 = 0.75787,
  sig_Encounter_Medium_Class3 = -30.89581,
  sig_Encounter_High_Class3 = 9.55760,
  sig_Existence_Medium_Class3 = 21.02805,
  sig_Existence_High_Class3 = 24.97377,
  sig_Bequest_Medium_Class3 = -16.76171,
  sig_Bequest_High_Class3 = 40.28992,
  sig_Int_Encounter_Medium_Bee_Class3 = -16.83690,
  sig_Int_Encounter_High_Bee_Class3 = -2.43720,
  sig_Int_Existence_Medium_Bee_Class3 = 12.78753,
  sig_Int_Existence_High_Bee_Class3 = -28.73304,
  sig_Int_Bequest_Medium_Bee_Class3 = 4.56519,
  sig_Int_Bequest_High_Bee_Class3 = -29.17512,
  sig_Int_Existence_Medium_Wasp_Class3 = -6.44470,
  sig_Int_Existence_High_Wasp_Class3 = -17.43312,
  sig_Int_Encounter_Medium_Wasp_Class3 = 25.57567,
  sig_Int_Encounter_High_Wasp_Class3 = 35.88289,
  sig_Int_Bequest_Medium_Wasp_Class3 = 50.28280,
  sig_Int_Bequest_High_Wasp_Class3 = 2.16350,
  
  
  delta_Class1 = 2.890,
  delta_Class2 = -0.109,
  Int_LV_Bee_Encounter_Medium_Class1 = 1.572,
  Int_LV_Bee_Existence_Medium_Class1 = -6.484,
  Int_LV_Bee_Bequest_Medium_Class1 = -2.040,
  Int_LV_Bee_Encounter_High_Class1 = 0.138,
  Int_LV_Bee_Existence_High_Class1 = 0.400,
  Int_LV_Bee_Bequest_High_Class1 = 6.462,
  Int_LV_Beetle_Encounter_Medium_Class1 = 0.632,
  Int_LV_Beetle_Existence_Medium_Class1 = 8.299,
  Int_LV_Beetle_Bequest_Medium_Class1 = 9.266,
  Int_LV_Beetle_Encounter_High_Class1 = 1.527,
  Int_LV_Beetle_Existence_High_Class1 = 12.760,
  Int_LV_Beetle_Bequest_High_Class1 = 7.029,
  Int_LV_Wasp_Encounter_Medium_Class1 = 18.750,
  Int_LV_Wasp_Existence_Medium_Class1 = 8.114,
  Int_LV_Wasp_Bequest_Medium_Class1 = -2.837,
  Int_LV_Wasp_Encounter_High_Class1 = 15.572,
  Int_LV_Wasp_Existence_High_Class1 = 4.573,
  Int_LV_Wasp_Bequest_High_Class1 = 2.407,
  Int_LV_Bee_Encounter_Medium_Class2 = 8.836,
  Int_LV_Bee_Existence_Medium_Class2 = 5.379,
  Int_LV_Bee_Bequest_Medium_Class2 = -0.593,
  Int_LV_Bee_Encounter_High_Class2 = 5.055,
  Int_LV_Bee_Existence_High_Class2 = 3.598,
  Int_LV_Bee_Bequest_High_Class2 = 1.483,
  Int_LV_Beetle_Encounter_Medium_Class2 = 2.366,
  Int_LV_Beetle_Existence_Medium_Class2 = -0.249,
  Int_LV_Beetle_Bequest_Medium_Class2 = 1.615,
  Int_LV_Beetle_Encounter_High_Class2 = 4.897,
  Int_LV_Beetle_Existence_High_Class2 = -5.056,
  Int_LV_Beetle_Bequest_High_Class2 = 1.213,
  Int_LV_Wasp_Encounter_Medium_Class2 = 2.488,
  Int_LV_Wasp_Existence_Medium_Class2 = 3.570,
  Int_LV_Wasp_Bequest_Medium_Class2 = 1.518,
  Int_LV_Wasp_Encounter_High_Class2 = 1.433,
  Int_LV_Wasp_Existence_High_Class2 = 8.514,
  Int_LV_Wasp_Bequest_High_Class2 = -0.367,
  Int_LV_Bee_Encounter_Medium_Class3 = 3.616,
  Int_LV_Bee_Existence_Medium_Class3 = -1.699,
  Int_LV_Bee_Bequest_Medium_Class3 = 4.264,
  Int_LV_Bee_Encounter_High_Class3 = 4.721,
  Int_LV_Bee_Existence_High_Class3 = 3.407,
  Int_LV_Bee_Bequest_High_Class3 = 3.511,
  Int_LV_Beetle_Encounter_Medium_Class3 = 5.488,
  Int_LV_Beetle_Existence_Medium_Class3 = 2.235,
  Int_LV_Beetle_Bequest_Medium_Class3 = -1.629,
  Int_LV_Beetle_Encounter_High_Class3 = 8.148,
  Int_LV_Beetle_Existence_High_Class3 = -1.492,
  Int_LV_Beetle_Bequest_High_Class3 = 0.484,
  Int_LV_Wasp_Encounter_Medium_Class3 = 6.771,
  Int_LV_Wasp_Existence_Medium_Class3 = 2.079,
  Int_LV_Wasp_Bequest_Medium_Class3 = 2.264,
  Int_LV_Wasp_Encounter_High_Class3 = 8.078,
  Int_LV_Wasp_Existence_High_Class3 = 7.103,
  Int_LV_Wasp_Bequest_High_Class3 = 1.692,
  ClassAllocation_C1_Q1Age = -0.033,
  ClassAllocation_C1_Income = 0.116,
  ClassAllocation_C1_Female_dummy = -0.332,
  ClassAllocation_C1_Q3Country_England = 0.257,
  ClassAllocation_C1_CE_Debrief_Certain_scaled = -0.130,
  ClassAllocation_C1_CE_Debrief_Confident_scaled = 0.128,
  ClassAllocation_C1_Q43_Citizen = -1.508,
  ClassAllocation_C1_AlwaysZero = 0.000,
  ClassAllocation_C2_Q1Age = -0.003,
  ClassAllocation_C2_Income = -0.035,
  ClassAllocation_C2_Female_dummy = 0.361,
  ClassAllocation_C2_Q3Country_England = -0.310,
  ClassAllocation_C2_CE_Debrief_Certain_scaled = 0.068,
  ClassAllocation_C2_CE_Debrief_Confident_scaled = -0.236,
  ClassAllocation_C2_Q43_Citizen = 0.122,
  ClassAllocation_C2_AlwaysZero = 0.000
)

apollo_fixed = c()


# ################################################################# #
# S4: Random draws ####
# ################################################################# #


apollo_draws = list(
  interDrawsType = "sobol",## Robust to using MLHS or Sobol draws
  interNDraws    = 1000, ## more the better - but also HPC kills too many
  interUnifDraws = c(),
  interNormDraws = c(
    "draws_Encounter_Medium",
    "draws_Encounter_High",
    "draws_Existence_Medium",
    "draws_Existence_High",
    "draws_Bequest_Medium",
    "draws_Bequest_High",
    
    "draws_Int_Encounter_Medium_Bee",
    "draws_Int_Encounter_High_Bee",
    "draws_Int_Existence_Medium_Bee",
    "draws_Int_Existence_High_Bee",
    "draws_Int_Bequest_Medium_Bee",
    "draws_Int_Bequest_High_Bee",
    
    "draws_Int_Encounter_Medium_Wasp",
    "draws_Int_Encounter_High_Wasp",
    "draws_Int_Existence_Medium_Wasp",
    "draws_Int_Existence_High_Wasp",
    "draws_Int_Bequest_Medium_Wasp",
    "draws_Int_Bequest_High_Wasp"
  ))

## Create random parameters
### Note lognormal for tax attribute to impose negative signs
apollo_randCoeff = function(apollo_beta, apollo_inputs){
  randcoeff = list()
  
  ## Beetle attributes _Class1 # ################################################################# #
  randcoeff[["b_Encounter_Medium_Class1"]] = (mu_Encounter_Medium_Class1 + 
                                                sig_Encounter_Medium_Class1 * draws_Encounter_Medium + 
                                                Int_LV_Beetle_Encounter_Medium_Class1 * Biowell_Beetle1_Mean)
  
  randcoeff[["b_Encounter_High_Class1"]] = (mu_Encounter_High_Class1 + 
                                              sig_Encounter_High_Class1 * draws_Encounter_High + 
                                              Int_LV_Beetle_Encounter_High_Class1 * Biowell_Beetle1_Mean)
  
  randcoeff[["b_Existence_Medium_Class1"]] = (mu_Existence_Medium_Class1 + 
                                                sig_Existence_Medium_Class1 * draws_Existence_Medium + 
                                                Int_LV_Beetle_Existence_Medium_Class1 * Biowell_Beetle2_Mean )
  
  randcoeff[["b_Existence_High_Class1"]] = (mu_Existence_High_Class1 + 
                                              sig_Existence_High_Class1 * draws_Existence_High + 
                                              Int_LV_Beetle_Existence_High_Class1 * Biowell_Beetle2_Mean)
  
  randcoeff[["b_Bequest_Medium_Class1"]] = (mu_Bequest_Medium_Class1 + 
                                              sig_Bequest_Medium_Class1 * draws_Bequest_Medium + 
                                              Int_LV_Beetle_Bequest_Medium_Class1 * Biowell_Beetle3_Mean)
  
  randcoeff[["b_Bequest_High_Class1"]] = (mu_Bequest_High_Class1 + 
                                            sig_Bequest_High_Class1 * draws_Bequest_High + 
                                            Int_LV_Beetle_Bequest_High_Class1 * Biowell_Beetle3_Mean)
  
  
  ## Beetle attributes _Class2 # ################################################################# #
  randcoeff[["b_Encounter_Medium_Class2"]] = (mu_Encounter_Medium_Class2 + 
                                                sig_Encounter_Medium_Class2 * draws_Encounter_Medium + 
                                                Int_LV_Beetle_Encounter_Medium_Class2 * Biowell_Beetle1_Mean)
  
  randcoeff[["b_Encounter_High_Class2"]] = (mu_Encounter_High_Class2 + 
                                              sig_Encounter_High_Class2 * draws_Encounter_High + 
                                              Int_LV_Beetle_Encounter_High_Class2 * Biowell_Beetle1_Mean)
  
  randcoeff[["b_Existence_Medium_Class2"]] = (mu_Existence_Medium_Class2 + 
                                                sig_Existence_Medium_Class2 * draws_Existence_Medium + 
                                                Int_LV_Beetle_Existence_Medium_Class2 * Biowell_Beetle2_Mean)
  
  randcoeff[["b_Existence_High_Class2"]] = (mu_Existence_High_Class2 + 
                                              sig_Existence_High_Class2 * draws_Existence_High + 
                                              Int_LV_Beetle_Existence_High_Class2 * Biowell_Beetle2_Mean)
  
  randcoeff[["b_Bequest_Medium_Class2"]] = (mu_Bequest_Medium_Class2 + 
                                              sig_Bequest_Medium_Class2 * draws_Bequest_Medium + 
                                              Int_LV_Beetle_Bequest_Medium_Class2 * Biowell_Beetle3_Mean)
  
  randcoeff[["b_Bequest_High_Class2"]] = (mu_Bequest_High_Class2 + 
                                            sig_Bequest_High_Class2 * draws_Bequest_High + 
                                            Int_LV_Beetle_Bequest_High_Class2 * Biowell_Beetle3_Mean)
  
  ## Beetle attributes _Class3 # ################################################################# #
  randcoeff[["b_Encounter_Medium_Class3"]] = (mu_Encounter_Medium_Class3 + 
                                                sig_Encounter_Medium_Class3 * draws_Encounter_Medium + 
                                                Int_LV_Beetle_Encounter_Medium_Class3 * Biowell_Beetle1_Mean)
  
  randcoeff[["b_Encounter_High_Class3"]] = (mu_Encounter_High_Class3 + 
                                              sig_Encounter_High_Class3 * draws_Encounter_High + 
                                              Int_LV_Beetle_Encounter_High_Class3 * Biowell_Beetle1_Mean)
  
  randcoeff[["b_Existence_Medium_Class3"]] = (mu_Existence_Medium_Class3 + 
                                                sig_Existence_Medium_Class3 * draws_Existence_Medium + 
                                                Int_LV_Beetle_Existence_Medium_Class3 * Biowell_Beetle2_Mean )
  
  randcoeff[["b_Existence_High_Class3"]] = (mu_Existence_High_Class3 + 
                                              sig_Existence_High_Class3 * draws_Existence_High + 
                                              Int_LV_Beetle_Existence_High_Class3 * Biowell_Beetle2_Mean)
  
  randcoeff[["b_Bequest_Medium_Class3"]] = (mu_Bequest_Medium_Class3 + 
                                              sig_Bequest_Medium_Class3 * draws_Bequest_Medium + 
                                              Int_LV_Beetle_Bequest_Medium_Class3 * Biowell_Beetle3_Mean)
  
  randcoeff[["b_Bequest_High_Class3"]] = (mu_Bequest_High_Class3 + 
                                            sig_Bequest_High_Class3 * draws_Bequest_High + 
                                            Int_LV_Beetle_Bequest_High_Class3 * Biowell_Beetle3_Mean)
  
  ## Bee attributes _Class1 # ################################################################# #
  randcoeff[["Int_Encounter_Medium_Bee_Class1"]] = (mu_Int_Encounter_Medium_Bee_Class1 + 
                                                      sig_Int_Encounter_Medium_Bee_Class1 * draws_Int_Encounter_Medium_Bee + 
                                                      Int_LV_Bee_Encounter_Medium_Class1 * Biowell_Bee1_Mean)
  
  randcoeff[["Int_Encounter_High_Bee_Class1"]] = (mu_Int_Encounter_High_Bee_Class1 + 
                                                    sig_Int_Encounter_High_Bee_Class1 * draws_Int_Encounter_High_Bee + 
                                                    Int_LV_Bee_Encounter_High_Class1 * Biowell_Bee1_Mean)
  
  randcoeff[["Int_Existence_Medium_Bee_Class1"]] = (mu_Int_Existence_Medium_Bee_Class1 + 
                                                      sig_Int_Existence_Medium_Bee_Class1 * draws_Int_Existence_Medium_Bee + 
                                                      Int_LV_Bee_Existence_Medium_Class1 * Biowell_Bee2_Mean)
  
  randcoeff[["Int_Existence_High_Bee_Class1"]] = (mu_Int_Existence_High_Bee_Class1 + 
                                                    sig_Int_Existence_High_Bee_Class1 * draws_Int_Existence_High_Bee + 
                                                    Int_LV_Bee_Existence_High_Class1 * Biowell_Bee2_Mean)
  
  randcoeff[["Int_Bequest_Medium_Bee_Class1"]] = (mu_Int_Bequest_Medium_Bee_Class1 + 
                                                    sig_Int_Bequest_Medium_Bee_Class1 * draws_Int_Bequest_Medium_Bee + 
                                                    Int_LV_Bee_Bequest_Medium_Class1 * Biowell_Bee3_Mean)
  
  randcoeff[["Int_Bequest_High_Bee_Class1"]] = (mu_Int_Bequest_High_Bee_Class1 + 
                                                  sig_Int_Bequest_High_Bee_Class1 * draws_Int_Bequest_High_Bee + 
                                                  Int_LV_Bee_Bequest_High_Class1 * Biowell_Bee3_Mean)
  
  
  
  ## Bee attributes _Class2 # ################################################################# #
  randcoeff[["Int_Encounter_Medium_Bee_Class2"]] = (mu_Int_Encounter_Medium_Bee_Class2 + 
                                                      sig_Int_Encounter_Medium_Bee_Class2 * draws_Int_Encounter_Medium_Bee + 
                                                      Int_LV_Bee_Encounter_Medium_Class2 * Biowell_Bee1_Mean)
  
  randcoeff[["Int_Encounter_High_Bee_Class2"]] = (mu_Int_Encounter_High_Bee_Class2 + 
                                                    sig_Int_Encounter_High_Bee_Class2 * draws_Int_Encounter_High_Bee + 
                                                    Int_LV_Bee_Encounter_High_Class2 * Biowell_Bee1_Mean)
  
  randcoeff[["Int_Existence_Medium_Bee_Class2"]] = (mu_Int_Existence_Medium_Bee_Class2 + 
                                                      sig_Int_Existence_Medium_Bee_Class2 * draws_Int_Existence_Medium_Bee + 
                                                      Int_LV_Bee_Existence_Medium_Class2 * Biowell_Bee2_Mean)
  
  randcoeff[["Int_Existence_High_Bee_Class2"]] = (mu_Int_Existence_High_Bee_Class2 + 
                                                    sig_Int_Existence_High_Bee_Class2 * draws_Int_Existence_High_Bee + 
                                                    Int_LV_Bee_Existence_High_Class2 * Biowell_Bee2_Mean )
  
  
  randcoeff[["Int_Bequest_Medium_Bee_Class2"]] = (mu_Int_Bequest_Medium_Bee_Class2 + 
                                                    sig_Int_Bequest_Medium_Bee_Class2 * draws_Int_Bequest_Medium_Bee + 
                                                    Int_LV_Bee_Bequest_Medium_Class2 * Biowell_Bee3_Mean)
  
  randcoeff[["Int_Bequest_High_Bee_Class2"]] = (mu_Int_Bequest_High_Bee_Class2 + 
                                                  sig_Int_Bequest_High_Bee_Class2 * draws_Int_Bequest_High_Bee + 
                                                  Int_LV_Bee_Bequest_High_Class2 * Biowell_Bee3_Mean)
  
  
  ## Bee attributes _Class3 # ################################################################# #
  randcoeff[["Int_Encounter_Medium_Bee_Class3"]] = (mu_Int_Encounter_Medium_Bee_Class3 + 
                                                      sig_Int_Encounter_Medium_Bee_Class3 * draws_Int_Encounter_Medium_Bee + 
                                                      Int_LV_Bee_Encounter_Medium_Class3 * Biowell_Bee1_Mean)
  
  randcoeff[["Int_Encounter_High_Bee_Class3"]] = (mu_Int_Encounter_High_Bee_Class3 + 
                                                    sig_Int_Encounter_High_Bee_Class3 * draws_Int_Encounter_High_Bee + 
                                                    Int_LV_Bee_Encounter_High_Class3 * Biowell_Bee1_Mean)
  
  randcoeff[["Int_Existence_Medium_Bee_Class3"]] = (mu_Int_Existence_Medium_Bee_Class3 + 
                                                      sig_Int_Existence_Medium_Bee_Class3 * draws_Int_Existence_Medium_Bee + 
                                                      Int_LV_Bee_Existence_Medium_Class3 * Biowell_Bee2_Mean)
  
  randcoeff[["Int_Existence_High_Bee_Class3"]] = (mu_Int_Existence_High_Bee_Class3 + 
                                                    sig_Int_Existence_High_Bee_Class3 * draws_Int_Existence_High_Bee + 
                                                    Int_LV_Bee_Existence_High_Class3 * Biowell_Bee2_Mean)
  
  randcoeff[["Int_Bequest_Medium_Bee_Class3"]] = (mu_Int_Bequest_Medium_Bee_Class3 + 
                                                    sig_Int_Bequest_Medium_Bee_Class3 * draws_Int_Bequest_Medium_Bee + 
                                                    Int_LV_Bee_Bequest_Medium_Class3 * Biowell_Bee3_Mean)
  
  randcoeff[["Int_Bequest_High_Bee_Class3"]] = (mu_Int_Bequest_High_Bee_Class3 + 
                                                  sig_Int_Bequest_High_Bee_Class3 * draws_Int_Bequest_High_Bee + 
                                                  Int_LV_Bee_Bequest_High_Class3 * Biowell_Bee3_Mean)
  
  
  ## Wasp attributes _Class1 # ################################################################# #
  randcoeff[["Int_Encounter_Medium_Wasp_Class1"]] = (mu_Int_Encounter_Medium_Wasp_Class1 + 
                                                       sig_Int_Encounter_Medium_Wasp_Class1 * draws_Int_Encounter_Medium_Wasp + 
                                                       Int_LV_Wasp_Encounter_Medium_Class1 * Biowell_Wasp1_Mean)
  
  randcoeff[["Int_Encounter_High_Wasp_Class1"]] = (mu_Int_Encounter_High_Wasp_Class1 + 
                                                     sig_Int_Encounter_High_Wasp_Class1 * draws_Int_Encounter_High_Wasp + 
                                                     Int_LV_Wasp_Encounter_High_Class1 * Biowell_Wasp1_Mean)
  
  randcoeff[["Int_Existence_Medium_Wasp_Class1"]] = (mu_Int_Existence_Medium_Wasp_Class1 + 
                                                       sig_Int_Existence_Medium_Wasp_Class1 * draws_Int_Existence_Medium_Wasp + 
                                                       Int_LV_Wasp_Existence_Medium_Class1 * Biowell_Wasp2_Mean)
  
  randcoeff[["Int_Existence_High_Wasp_Class1"]] = (mu_Int_Existence_High_Wasp_Class1 + 
                                                     sig_Int_Existence_High_Wasp_Class1 * draws_Int_Existence_High_Wasp + 
                                                     Int_LV_Wasp_Existence_High_Class1 * Biowell_Wasp2_Mean)
  
  randcoeff[["Int_Bequest_Medium_Wasp_Class1"]] = (mu_Int_Bequest_Medium_Wasp_Class1 + 
                                                     sig_Int_Bequest_Medium_Wasp_Class1 * draws_Int_Bequest_Medium_Wasp + 
                                                     Int_LV_Wasp_Bequest_Medium_Class1 * Biowell_Wasp3_Mean)
  
  
  randcoeff[["Int_Bequest_High_Wasp_Class1"]] = (mu_Int_Bequest_High_Wasp_Class1 + 
                                                   sig_Int_Bequest_High_Wasp_Class1 * draws_Int_Bequest_High_Wasp + 
                                                   Int_LV_Wasp_Bequest_High_Class1 * Biowell_Wasp3_Mean)
  
  
  
  ## Wasp attributes _Class2 # ################################################################# #
  randcoeff[["Int_Encounter_Medium_Wasp_Class2"]] = (mu_Int_Encounter_Medium_Wasp_Class2 + 
                                                       sig_Int_Encounter_Medium_Wasp_Class2 * draws_Int_Encounter_Medium_Wasp + 
                                                       Int_LV_Wasp_Encounter_Medium_Class2 * Biowell_Wasp1_Mean)
  
  randcoeff[["Int_Encounter_High_Wasp_Class2"]] = (mu_Int_Encounter_High_Wasp_Class2 + 
                                                     sig_Int_Encounter_High_Wasp_Class2 * draws_Int_Encounter_High_Wasp + 
                                                     Int_LV_Wasp_Encounter_High_Class2 * Biowell_Wasp1_Mean)
  
  randcoeff[["Int_Existence_Medium_Wasp_Class2"]] = (mu_Int_Existence_Medium_Wasp_Class2 + 
                                                       sig_Int_Existence_Medium_Wasp_Class2 * draws_Int_Existence_Medium_Wasp + 
                                                       Int_LV_Wasp_Existence_Medium_Class2 * Biowell_Wasp2_Mean )
  
  randcoeff[["Int_Existence_High_Wasp_Class2"]] = (mu_Int_Existence_High_Wasp_Class2 + 
                                                     sig_Int_Existence_High_Wasp_Class2 * draws_Int_Existence_High_Wasp + 
                                                     Int_LV_Wasp_Existence_High_Class2 * Biowell_Wasp2_Mean)
  
  randcoeff[["Int_Bequest_Medium_Wasp_Class2"]] = (mu_Int_Bequest_Medium_Wasp_Class2 + 
                                                     sig_Int_Bequest_Medium_Wasp_Class2 * draws_Int_Bequest_Medium_Wasp + 
                                                     Int_LV_Wasp_Bequest_Medium_Class2 * Biowell_Wasp3_Mean)
  
  randcoeff[["Int_Bequest_High_Wasp_Class2"]] = (mu_Int_Bequest_High_Wasp_Class2 + 
                                                   sig_Int_Bequest_High_Wasp_Class2 * draws_Int_Bequest_High_Wasp + 
                                                   Int_LV_Wasp_Bequest_High_Class2 * Biowell_Wasp3_Mean)
  
  ## Wasp attributes _Class3 # ################################################################# #
  randcoeff[["Int_Encounter_Medium_Wasp_Class3"]] = (mu_Int_Encounter_Medium_Wasp_Class3 + 
                                                       sig_Int_Encounter_Medium_Wasp_Class3 * draws_Int_Encounter_Medium_Wasp + 
                                                       Int_LV_Wasp_Encounter_Medium_Class3 * Biowell_Wasp1_Mean)
  
  randcoeff[["Int_Encounter_High_Wasp_Class3"]] = (mu_Int_Encounter_High_Wasp_Class3 + 
                                                     sig_Int_Encounter_High_Wasp_Class3 * draws_Int_Encounter_High_Wasp + 
                                                     Int_LV_Wasp_Encounter_High_Class3 * Biowell_Wasp1_Mean)
  
  randcoeff[["Int_Existence_Medium_Wasp_Class3"]] = (mu_Int_Existence_Medium_Wasp_Class3 + 
                                                       sig_Int_Existence_Medium_Wasp_Class3 * draws_Int_Existence_Medium_Wasp + 
                                                       Int_LV_Wasp_Existence_Medium_Class3 * Biowell_Wasp2_Mean )
  
  randcoeff[["Int_Existence_High_Wasp_Class3"]] = (mu_Int_Existence_High_Wasp_Class3 + 
                                                     sig_Int_Existence_High_Wasp_Class3 * draws_Int_Existence_High_Wasp + 
                                                     Int_LV_Wasp_Existence_High_Class3 * Biowell_Wasp2_Mean)
  
  randcoeff[["Int_Bequest_Medium_Wasp_Class3"]] = (mu_Int_Bequest_Medium_Wasp_Class3 + 
                                                     sig_Int_Bequest_Medium_Wasp_Class3 * draws_Int_Bequest_Medium_Wasp + 
                                                     Int_LV_Wasp_Bequest_Medium_Class3 * Biowell_Wasp3_Mean)
  
  randcoeff[["Int_Bequest_High_Wasp_Class3"]] = (mu_Int_Bequest_High_Wasp_Class3 + 
                                                   sig_Int_Bequest_High_Wasp_Class3 * draws_Int_Bequest_High_Wasp + 
                                                   Int_LV_Wasp_Bequest_High_Class3 * Biowell_Wasp3_Mean)
  
  return(randcoeff)
}

# ################################################################# #
# S5: lcpars ####
# ################################################################# #

apollo_lcPars = function(apollo_beta, apollo_inputs) {
  lcpars = list()
  
  lcpars[["Int_LV_Bee1_Medium"]] = list(Int_LV_Bee_Encounter_Medium_Class1, Int_LV_Bee_Encounter_Medium_Class2, Int_LV_Bee_Encounter_Medium_Class3)
  lcpars[["Int_LV_Bee2_Medium"]] = list(Int_LV_Bee_Existence_Medium_Class1, Int_LV_Bee_Existence_Medium_Class2, Int_LV_Bee_Existence_Medium_Class3)
  lcpars[["Int_LV_Bee3_Medium"]] = list(Int_LV_Bee_Bequest_Medium_Class1, Int_LV_Bee_Bequest_Medium_Class2, Int_LV_Bee_Bequest_Medium_Class3)
  lcpars[["Int_LV_Bee1_High"]] = list(Int_LV_Bee_Encounter_High_Class1, Int_LV_Bee_Encounter_High_Class2, Int_LV_Bee_Encounter_High_Class3)
  lcpars[["Int_LV_Bee2_High"]] = list(Int_LV_Bee_Existence_High_Class1, Int_LV_Bee_Existence_High_Class2, Int_LV_Bee_Existence_High_Class3)
  lcpars[["Int_LV_Bee3_High"]] = list(Int_LV_Bee_Bequest_High_Class1, Int_LV_Bee_Bequest_High_Class2, Int_LV_Bee_Bequest_High_Class3)
  
  lcpars[["Int_LV_Beetle1_Medium"]] = list(Int_LV_Beetle_Encounter_Medium_Class1, Int_LV_Beetle_Encounter_Medium_Class2, Int_LV_Beetle_Encounter_Medium_Class3)
  lcpars[["Int_LV_Beetle2_Medium"]] = list(Int_LV_Beetle_Existence_Medium_Class1, Int_LV_Beetle_Existence_Medium_Class2, Int_LV_Beetle_Existence_Medium_Class3)
  lcpars[["Int_LV_Beetle3_Medium"]] = list(Int_LV_Beetle_Bequest_Medium_Class1, Int_LV_Beetle_Bequest_Medium_Class2, Int_LV_Beetle_Bequest_Medium_Class3)
  lcpars[["Int_LV_Beetle1_High"]] = list(Int_LV_Beetle_Encounter_High_Class1, Int_LV_Beetle_Encounter_High_Class2, Int_LV_Beetle_Encounter_High_Class3)
  lcpars[["Int_LV_Beetle2_High"]] = list(Int_LV_Beetle_Existence_High_Class1, Int_LV_Beetle_Existence_High_Class2, Int_LV_Beetle_Existence_High_Class3)
  lcpars[["Int_LV_Beetle3_High"]] = list(Int_LV_Beetle_Bequest_High_Class1, Int_LV_Beetle_Bequest_High_Class2, Int_LV_Beetle_Bequest_High_Class3)
  
  lcpars[["Int_LV_Wasp1_Medium"]] = list(Int_LV_Wasp_Encounter_Medium_Class1, Int_LV_Wasp_Encounter_Medium_Class2, Int_LV_Wasp_Encounter_Medium_Class3)
  lcpars[["Int_LV_Wasp2_Medium"]] = list(Int_LV_Wasp_Existence_Medium_Class1, Int_LV_Wasp_Existence_Medium_Class2, Int_LV_Wasp_Existence_Medium_Class3)
  lcpars[["Int_LV_Wasp3_Medium"]] = list(Int_LV_Wasp_Bequest_Medium_Class1, Int_LV_Wasp_Bequest_Medium_Class2, Int_LV_Wasp_Bequest_Medium_Class3)
  lcpars[["Int_LV_Wasp1_High"]] = list(Int_LV_Wasp_Encounter_High_Class1, Int_LV_Wasp_Encounter_High_Class2, Int_LV_Wasp_Encounter_High_Class3)
  lcpars[["Int_LV_Wasp2_High"]] = list(Int_LV_Wasp_Existence_High_Class1, Int_LV_Wasp_Existence_High_Class2, Int_LV_Wasp_Existence_High_Class3)
  lcpars[["Int_LV_Wasp3_High"]] = list(Int_LV_Wasp_Bequest_High_Class1, Int_LV_Wasp_Bequest_High_Class2, Int_LV_Wasp_Bequest_High_Class3)
  
  lcpars[["asc_C"]] = list(asc_C_Class1, asc_C_Class2, asc_C_Class3)
  lcpars[["beta_Tax"]] = list(beta_Tax_Class1, beta_Tax_Class2, beta_Tax_Class3)
  
  lcpars[["b_Encounter_Medium"]] = list(b_Encounter_Medium_Class1, b_Encounter_Medium_Class2, b_Encounter_Medium_Class3)
  lcpars[["b_Encounter_High"]] = list(b_Encounter_High_Class1, b_Encounter_High_Class2, b_Encounter_High_Class3)
  lcpars[["b_Existence_Medium"]] = list(b_Existence_Medium_Class1, b_Existence_Medium_Class2, b_Existence_Medium_Class3)
  lcpars[["b_Existence_High"]] = list(b_Existence_High_Class1, b_Existence_High_Class2, b_Existence_High_Class3)
  lcpars[["b_Bequest_Medium"]]  = list(b_Bequest_Medium_Class1, b_Bequest_Medium_Class2, b_Bequest_Medium_Class3)
  lcpars[["b_Bequest_High"]]  = list(b_Bequest_High_Class1, b_Bequest_High_Class2, b_Bequest_High_Class3)
  
  lcpars[["Int_Encounter_Medium_Bee"]] = list(Int_Encounter_Medium_Bee_Class1, Int_Encounter_Medium_Bee_Class2, Int_Encounter_Medium_Bee_Class3)
  lcpars[["Int_Encounter_High_Bee"]] = list(Int_Encounter_High_Bee_Class1, Int_Encounter_High_Bee_Class2, Int_Encounter_High_Bee_Class3)
  lcpars[["Int_Existence_Medium_Bee"]] = list(Int_Existence_Medium_Bee_Class1, Int_Existence_Medium_Bee_Class2, Int_Existence_Medium_Bee_Class3)
  lcpars[["Int_Existence_High_Bee"]] = list(Int_Existence_High_Bee_Class1, Int_Existence_High_Bee_Class2, Int_Existence_High_Bee_Class3)
  lcpars[["Int_Bequest_Medium_Bee"]]  = list(Int_Bequest_Medium_Bee_Class1, Int_Bequest_Medium_Bee_Class2, Int_Bequest_Medium_Bee_Class3)
  lcpars[["Int_Bequest_High_Bee"]]  = list(Int_Bequest_High_Bee_Class1, Int_Bequest_High_Bee_Class2, Int_Bequest_High_Bee_Class3)
  
  lcpars[["Int_Existence_Medium_Wasp"]] = list(Int_Existence_Medium_Wasp_Class1, Int_Existence_Medium_Wasp_Class2, Int_Existence_Medium_Wasp_Class3)
  lcpars[["Int_Existence_High_Wasp"]] = list(Int_Existence_High_Wasp_Class1, Int_Existence_High_Wasp_Class2, Int_Existence_High_Wasp_Class3)
  lcpars[["Int_Encounter_Medium_Wasp"]] = list(Int_Encounter_Medium_Wasp_Class1, Int_Encounter_Medium_Wasp_Class2, Int_Encounter_Medium_Wasp_Class3)
  lcpars[["Int_Encounter_High_Wasp"]] = list(Int_Encounter_High_Wasp_Class1, Int_Encounter_High_Wasp_Class2, Int_Encounter_High_Wasp_Class3)
  lcpars[["Int_Bequest_Medium_Wasp"]]  = list(Int_Bequest_Medium_Wasp_Class1, Int_Bequest_Medium_Wasp_Class2, Int_Bequest_Medium_Wasp_Class3)
  lcpars[["Int_Bequest_High_Wasp"]] = list(Int_Bequest_High_Wasp_Class1, Int_Bequest_High_Wasp_Class2, Int_Bequest_High_Wasp_Class3)
  
  
  ### Utilities of class allocation model
  V = list()
  V[["Class1"]] = delta_Class1 + 
    ClassAllocation_C1_Q1Age * Q1Age +
    ClassAllocation_C1_Income * IncomeMidpoints_PlusMissing_Recoded +
    ClassAllocation_C1_Female_dummy * Female_dummy +
    ClassAllocation_C1_Q3Country_England * Q3Country_England +
    ClassAllocation_C1_CE_Debrief_Certain_scaled * CE_Debrief_Certain_scaled +
    ClassAllocation_C1_CE_Debrief_Confident_scaled * CE_Debrief_Confident_scaled +
    ClassAllocation_C1_Q43_Citizen * Q43_Citizen +
    ClassAllocation_C1_AlwaysZero * AlwaysZero
  
  V[["Class2"]] = delta_Class2 + 
    ClassAllocation_C2_Q1Age * Q1Age +
    ClassAllocation_C2_Income * IncomeMidpoints_PlusMissing_Recoded +
    ClassAllocation_C2_Female_dummy * Female_dummy +
    ClassAllocation_C2_Q3Country_England * Q3Country_England +
    ClassAllocation_C2_CE_Debrief_Certain_scaled * CE_Debrief_Certain_scaled +
    ClassAllocation_C2_CE_Debrief_Confident_scaled * CE_Debrief_Confident_scaled +
    ClassAllocation_C2_Q43_Citizen * Q43_Citizen +
    ClassAllocation_C2_AlwaysZero * AlwaysZero
  
  V[["Class3"]] = 0
  
  ### Settings for class allocation models
  classAlloc_settings = list(classes      = c(Class1 = 1, 
                                              Class2 = 2,
                                              Class3 = 3),
                             utilities    = V)
  
  lcpars[["pi_values"]] = apollo_classAlloc(classAlloc_settings)
  
  return(lcpars)
}

# ################################################################# #
#### GROUP AND VALIDATE INPUTS                                   ####
# ################################################################# #

apollo_inputs = apollo_validateInputs()
apollo_sink()

# ################################################################# #
#### DEFINE MODEL AND LIKELIHOOD FUNCTION                        ####
# ################################################################# #

apollo_probabilities = function(apollo_beta,
                                apollo_inputs,
                                functionality = "estimate") {
  ### Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  ### Create list of probabilities P
  P = list()
  
  ### Define settings for MNL model component that are generic across classes
  mnl_settings = list(
    alternatives = c(A = 1, B = 2, C = 3),
    avail        = list(A = 1, B = 1, C = 1),
    choiceVar    = Choice
  )
  
  ### Loop over classes
  S <- 3
  for(s in 1:S){
    ### Compute class-specific utilities
    V = list()
    
    V[['A']]  = -beta_Tax[[s]] * (
      
      ## Beetle Encounter:
      (b_Encounter_Medium[[s]] * (Encounter_PlanA_Values == 15)) +
        (b_Encounter_High[[s]] * (Encounter_PlanA_Values == 30)) +
        
        ## Beetle existence:
        (b_Existence_Medium[[s]] * (Existence_PlanA_Values == 15)) +
        (b_Existence_High[[s]] * (Existence_PlanA_Values == 30)) +
        
        # Beetle bequest
        (b_Bequest_Medium[[s]] * (Bequest_PlanA_Values == 15)) +
        (b_Bequest_High[[s]] * (Bequest_PlanA_Values == 30)) +
        
        ## Bee Encounter
        (Int_Encounter_Medium_Bee[[s]] * (Encounter_PlanA_Values == 15) * Insect_PlanA_Bee) +
        (Int_Encounter_High_Bee[[s]] * (Encounter_PlanA_Values == 30) * Insect_PlanA_Bee) +
        
        ## Wasp Encounter
        (Int_Encounter_Medium_Wasp[[s]] * (Encounter_PlanA_Values == 15) * Insect_PlanA_Wasp) +
        (Int_Encounter_High_Wasp[[s]] * (Encounter_PlanA_Values == 30) * Insect_PlanA_Wasp) +
        
        ## Bee existence
        (Int_Existence_Medium_Bee[[s]] * (Existence_PlanA_Values == 15) * Insect_PlanA_Bee) +
        (Int_Existence_High_Bee[[s]] * (Existence_PlanA_Values == 30) * Insect_PlanA_Bee) +
        
        ## Wasp existence
        (Int_Existence_Medium_Wasp[[s]] * (Existence_PlanA_Values == 15) * Insect_PlanA_Wasp) +
        (Int_Existence_High_Wasp[[s]] * (Existence_PlanA_Values == 30) * Insect_PlanA_Wasp) +
        
        ## Beequest
        (Int_Bequest_Medium_Bee[[s]] * (Bequest_PlanA_Values == 15) * Insect_PlanA_Bee) +
        (Int_Bequest_High_Bee[[s]] * (Bequest_PlanA_Values == 30) * Insect_PlanA_Bee) +
        
        ## Wasp bequest  
        (Int_Bequest_Medium_Wasp[[s]] * (Bequest_PlanA_Values == 15) * Insect_PlanA_Wasp) +
        (Int_Bequest_High_Wasp[[s]] * (Bequest_PlanA_Values == 30) * Insect_PlanA_Wasp)  -
        Tax_PlanA_Values
    )
    
    
    
    V[['B']]  = -beta_Tax[[s]] * (
      
      ## Beetle Encounter:
      (b_Encounter_Medium[[s]] * (Encounter_PlanB_Values == 15)) +
        (b_Encounter_High[[s]] * (Encounter_PlanB_Values == 30)) +
        
        ## Beetle existence:
        (b_Existence_Medium[[s]] * (Existence_PlanB_Values == 15) ) +
        (b_Existence_High[[s]] * (Existence_PlanB_Values == 30) ) +
        
        # Beetle bequest
        (b_Bequest_Medium[[s]] * (Bequest_PlanB_Values == 15)) +
        (b_Bequest_High[[s]] * (Bequest_PlanB_Values == 30)) +
        
        ## Bee Encounter
        (Int_Encounter_Medium_Bee[[s]] * (Encounter_PlanB_Values == 15) * Insect_PlanB_Bee) +
        (Int_Encounter_High_Bee[[s]] * (Encounter_PlanB_Values == 30) * Insect_PlanB_Bee) +
        
        ## Wasp Encounter
        (Int_Encounter_Medium_Wasp[[s]] * (Encounter_PlanB_Values == 15) * Insect_PlanB_Wasp) +
        (Int_Encounter_High_Wasp[[s]] * (Encounter_PlanB_Values == 30) * Insect_PlanB_Wasp) +
        
        ## Bee existence
        (Int_Existence_Medium_Bee[[s]] * (Existence_PlanB_Values == 15) * Insect_PlanB_Bee) +
        (Int_Existence_High_Bee[[s]] * (Existence_PlanB_Values == 30) * Insect_PlanB_Bee) +
        
        ## Wasp existence
        (Int_Existence_Medium_Wasp[[s]] * (Existence_PlanB_Values == 15) * Insect_PlanB_Wasp) +
        (Int_Existence_High_Wasp[[s]] * (Existence_PlanB_Values == 30) * Insect_PlanB_Wasp) +
        
        ## Beequest
        (Int_Bequest_Medium_Bee[[s]] * (Bequest_PlanB_Values == 15) * Insect_PlanB_Bee) +
        (Int_Bequest_High_Bee[[s]] * (Bequest_PlanB_Values == 30) * Insect_PlanB_Bee) +
        
        ## Wasp bequest  
        (Int_Bequest_Medium_Wasp[[s]] * (Bequest_PlanB_Values == 15) * Insect_PlanB_Wasp) +
        (Int_Bequest_High_Wasp[[s]] * (Bequest_PlanB_Values == 30) * Insect_PlanB_Wasp) -
        
        Tax_PlanB_Values
    )
    
    V[['C']]  = asc_C[[s]] 
    
    
    mnl_settings$utilities     = V
    mnl_settings$componentName = paste0("Class", s)
    
    ### Compute within-class choice probabilities using MNL model
    P[[paste0("Class", s)]] = apollo_mnl(mnl_settings, functionality)
    
    ### Take product across observation for same individual
    P[[paste0("Class", s)]] = apollo_panelProd(P[[paste0("Class", s)]], 
                                               apollo_inputs , functionality)
  }
  
  ### Compute latent class model probabilities
  lc_settings  = list(inClassProb = P, classProb = pi_values)
  P[["model"]] = apollo_lc(lc_settings, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_avgInterDraws(P, apollo_inputs, functionality)
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}


# ################################################################# #
#### MODEL ESTIMATION                                            ####
# ################################################################# #

### Estimate model
D2_Truncated_LC_3C_MXL_NoDR_V1 = apollo_estimate(apollo_beta,
                                                 apollo_fixed,
                                                 apollo_probabilities,
                                                 apollo_inputs)

### Show output in screen
apollo_modelOutput(D2_Truncated_LC_3C_MXL_NoDR_V1,
                   modelOutput_settings = list(printPVal = 2))


### Save output to file(s)
apollo_saveOutput(D2_Truncated_LC_3C_MXL_NoDR_V1,
                  saveOutput_settings = list(printPVal = 2))

apollo_sink()

# *************************************************************************
#### Section 2: Model Summary Function ####
# *************************************************************************



## So this code outputs a table of estimate,  p.v stars and s.e in brackets ##
### To make it easy,  just change the model name here and the code will output the table for your model:
ModelOutputs <- function(Estimates) {
  data.frame("Variable" =  Estimates$V1,
             "Estimate" =  paste(
               ifelse(
                 Estimates$Rob.p.val.0. < 0.01,
                 paste0(Estimates$Estimate %>% round(3) %>% sprintf("%.3f", .),  "***"),
                 ifelse(
                   Estimates$Rob.p.val.0. < 0.05,
                   paste0(Estimates$Estimate %>% round(3) %>% sprintf("%.3f", .),  "**"),
                   ifelse(
                     Estimates$Rob.p.val.0. < 0.1,
                     paste0(Estimates$Estimate %>% round(3) %>% sprintf("%.3f", .),  "*"),
                     paste0(Estimates$Estimate %>% round(3) %>% sprintf("%.3f", .)))
                 )),
               paste0("(", Estimates$Rob.std.err %>% round(3) %>% sprintf("%.3f", .), ")")
             ))
}


## New function to take model and report model stats
Diagnostics <- function(Model) {
  rbind(
    "N" = Model$nIndivs,
    "AIC" = Model$AIC %>% round(3) %>% sprintf("%.3f", .),
    "Adj.R2" = Model$adjRho2_C %>% round(3) %>% sprintf("%.3f", .),
    "LogLik" = Model$LLout %>% as.numeric() %>% round(3) %>% sprintf("%.3f", .)
  )
}




Estimates <- here("CEOutput/Main/LCM", 
                  "D2_Truncated_LC_3C_MXL_NoDR_V1_estimates.csv") %>%
  fread() %>% 
  data.frame()


ModelOutputs(Estimates)


ModelOutputs(Estimates) %>% 
  data.frame() %>% 
  fwrite(sep = ",", 
         here("CEOutput/Main/LCM", 
              "D2_Truncated_LC_3C_MXL_NoDR_V1_MyModelOutputs.txt"))



# ----------------------------------------------------------------- #
#---- POSTERIOR ANALYSIS                                     ----
# ----------------------------------------------------------------- #


Model <- here("CEOutput/Main/LCM", 
              "D2_Truncated_LC_3C_MXL_NoDR_V1_model.rds") %>% readRDS()


Test <- apollo_unconditionals(Model, apollo_probabilities, apollo_inputs)

Test %>% 
  saveRDS(file = here("CEOutput/Main/LCM",
                      "D2_Truncated_LC_3C_MXL_NoDR_V1_model_UCWTP.rds"))
# 
# 
# Encounter_Medium_Beetle_Class1 = Test$continuous$b_Encounter_Medium_Class1 %>% rowMeans() %>% mean()
# Encounter_High_Beetle_Class1 = Test$continuous$b_Encounter_High_Class1 %>% rowMeans() %>% mean()
# Existence_Medium_Beetle_Class1 = Test$continuous$b_Existence_Medium_Class1 %>% rowMeans() %>% mean()
# Existence_High_Beetle_Class1 = Test$continuous$b_Existence_High_Class1 %>% rowMeans() %>% mean()
# Bequest_Medium_Beetle_Class1 = Test$continuous$b_Bequest_Medium_Class1 %>% rowMeans() %>% mean()
# Bequest_High_Beetle_Class1 = Test$continuous$b_Bequest_High_Class1 %>% rowMeans() %>% mean()
# Encounter_Medium_Beetle_Class2 = Test$continuous$b_Encounter_Medium_Class2 %>% rowMeans() %>% mean()
# Encounter_High_Beetle_Class2 = Test$continuous$b_Encounter_High_Class2 %>% rowMeans() %>% mean()
# Existence_Medium_Beetle_Class2 = Test$continuous$b_Existence_Medium_Class2 %>% rowMeans() %>% mean()
# Existence_High_Beetle_Class2 = Test$continuous$b_Existence_High_Class2 %>% rowMeans() %>% mean()
# Bequest_Medium_Beetle_Class2 = Test$continuous$b_Bequest_Medium_Class2 %>% rowMeans() %>% mean()
# Bequest_High_Beetle_Class2 = Test$continuous$b_Bequest_High_Class2 %>% rowMeans() %>% mean()
# 
# 
# Encounter_Medium_Bee_Class1 = (Test$continuous$b_Encounter_Medium_Class1 + Test$continuous$Int_Encounter_Medium_Bee_Class1) %>% rowMeans() %>% mean()
# Encounter_High_Bee_Class1 = (Test$continuous$b_Encounter_High_Class1 + Test$continuous$Int_Encounter_High_Bee_Class1) %>% rowMeans() %>% mean()
# Existence_Medium_Bee_Class1 = (Test$continuous$b_Existence_Medium_Class1 + Test$continuous$Int_Existence_Medium_Bee_Class1) %>% rowMeans() %>% mean()
# Existence_High_Bee_Class1 = (Test$continuous$b_Existence_High_Class1 + Test$continuous$Int_Existence_High_Bee_Class1) %>% rowMeans() %>% mean()
# Bequest_Medium_Bee_Class1 = (Test$continuous$b_Bequest_Medium_Class1 + Test$continuous$Int_Bequest_Medium_Bee_Class1) %>% rowMeans() %>% mean()
# Bequest_High_Bee_Class1 = (Test$continuous$b_Bequest_High_Class1 + Test$continuous$Int_Bequest_High_Bee_Class1) %>% rowMeans() %>% mean()
# Encounter_Medium_Bee_Class2 = (Test$continuous$b_Encounter_Medium_Class2 + Test$continuous$Int_Encounter_Medium_Bee_Class2) %>% rowMeans() %>% mean()
# Encounter_High_Bee_Class2 = (Test$continuous$b_Encounter_High_Class2 + Test$continuous$Int_Encounter_High_Bee_Class2) %>% rowMeans() %>% mean()
# Existence_Medium_Bee_Class2 = (Test$continuous$b_Existence_Medium_Class2 + Test$continuous$Int_Existence_Medium_Bee_Class2) %>% rowMeans() %>% mean()
# Existence_High_Bee_Class2 = (Test$continuous$b_Existence_High_Class2 + Test$continuous$Int_Existence_High_Bee_Class2) %>% rowMeans() %>% mean()
# Bequest_Medium_Bee_Class2 = (Test$continuous$b_Bequest_Medium_Class2 + Test$continuous$Int_Bequest_Medium_Bee_Class2) %>% rowMeans() %>% mean()
# Bequest_High_Bee_Class2 = (Test$continuous$b_Bequest_High_Class2 + Test$continuous$Int_Bequest_High_Bee_Class2) %>% rowMeans() %>% mean()
# 
# Encounter_Medium_Wasp_Class1 = (Test$continuous$b_Encounter_Medium_Class1 + Test$continuous$Int_Encounter_Medium_Wasp_Class1) %>% rowMeans() %>% mean()
# Encounter_High_Wasp_Class1 = (Test$continuous$b_Encounter_High_Class1 + Test$continuous$Int_Encounter_High_Wasp_Class1) %>% rowMeans() %>% mean()
# Existence_Medium_Wasp_Class1 = (Test$continuous$b_Existence_Medium_Class1 + Test$continuous$Int_Existence_Medium_Wasp_Class1) %>% rowMeans() %>% mean()
# Existence_High_Wasp_Class1 = (Test$continuous$b_Existence_High_Class1 + Test$continuous$Int_Existence_High_Wasp_Class1) %>% rowMeans() %>% mean()
# Bequest_Medium_Wasp_Class1 = (Test$continuous$b_Bequest_Medium_Class1 + Test$continuous$Int_Bequest_Medium_Wasp_Class1) %>% rowMeans() %>% mean()
# Bequest_High_Wasp_Class1 = (Test$continuous$b_Bequest_High_Class1 + Test$continuous$Int_Bequest_High_Wasp_Class1) %>% rowMeans() %>% mean()
# Encounter_Medium_Wasp_Class2 = (Test$continuous$b_Encounter_Medium_Class2 + Test$continuous$Int_Encounter_Medium_Wasp_Class2) %>% rowMeans() %>% mean()
# Encounter_High_Wasp_Class2 = (Test$continuous$b_Encounter_High_Class2 + Test$continuous$Int_Encounter_High_Wasp_Class2) %>% rowMeans() %>% mean()
# Existence_Medium_Wasp_Class2 = (Test$continuous$b_Existence_Medium_Class2 + Test$continuous$Int_Existence_Medium_Wasp_Class2) %>% rowMeans() %>% mean()
# Existence_High_Wasp_Class2 = (Test$continuous$b_Existence_High_Class2 + Test$continuous$Int_Existence_High_Wasp_Class2) %>% rowMeans() %>% mean()
# Bequest_Medium_Wasp_Class2 = (Test$continuous$b_Bequest_Medium_Class2 + Test$continuous$Int_Bequest_Medium_Wasp_Class2) %>% rowMeans() %>% mean()
# Bequest_High_Wasp_Class2 = (Test$continuous$b_Bequest_High_Class2 + Test$continuous$Int_Bequest_High_Wasp_Class2) %>% rowMeans() %>% mean()
# 
# 
# #
# WTPOutput <- cbind(
#   "Class1" =
#     rbind(
#       Encounter_Medium_Beetle_Class1,
#       Encounter_High_Beetle_Class1,
#       Existence_Medium_Beetle_Class1,
#       Existence_High_Beetle_Class1,
#       Bequest_Medium_Beetle_Class1,
#       Bequest_High_Beetle_Class1,
#       Encounter_Medium_Bee_Class1,
#       Encounter_High_Bee_Class1,
#       Existence_Medium_Bee_Class1,
#       Existence_High_Bee_Class1,
#       Bequest_Medium_Bee_Class1,
#       Bequest_High_Bee_Class1,
#       Encounter_Medium_Wasp_Class1,
#       Encounter_High_Wasp_Class1,
#       Existence_Medium_Wasp_Class1,
#       Existence_High_Wasp_Class1,
#       Bequest_Medium_Wasp_Class1,
#       Bequest_High_Wasp_Class1
#     ),
#   "Class2" =
#     rbind(
#       Encounter_Medium_Beetle_Class2,
#       Encounter_High_Beetle_Class2,
#       Existence_Medium_Beetle_Class2,
#       Existence_High_Beetle_Class2,
#       Bequest_Medium_Beetle_Class2,
#       Bequest_High_Beetle_Class2,
#       Encounter_Medium_Bee_Class2,
#       Encounter_High_Bee_Class2,
#       Existence_Medium_Bee_Class2,
#       Existence_High_Bee_Class2,
#       Bequest_Medium_Bee_Class2,
#       Bequest_High_Bee_Class2,
#       Encounter_Medium_Wasp_Class2,
#       Encounter_High_Wasp_Class2,
#       Existence_Medium_Wasp_Class2,
#       Existence_High_Wasp_Class2,
#       Bequest_Medium_Wasp_Class2,
#       Bequest_High_Wasp_Class2
#     )
# ) %>% data.frame()
# 
# 
# colnames(WTPOutput) <- c("Class1", "Class2")
# 
# 
# Variables <- rbind(
#   "Encounter_Medium_Beetle",
#   "Encounter_High_Beetle",
#   "Existence_Medium_Beetle",
#   "Existence_High_Beetle",
#   "Bequest_Medium_Beetle",
#   "Bequest_High_Beetle",
#   "Encounter_Medium_Bee",
#   "Encounter_High_Bee",
#   "Existence_Medium_Bee",
#   "Existence_High_Bee",
#   "Bequest_Medium_Bee",
#   "Bequest_High_Bee",
#   "Encounter_Medium_Wasp",
#   "Encounter_High_Wasp",
#   "Existence_Medium_Wasp",
#   "Existence_High_Wasp",
#   "Bequest_Medium_Wasp",
#   "Bequest_High_Wasp"
# )
# 
# OutputTable <-
#   data.frame(WTPOutput$Class1 %>% round(2),
#              WTPOutput$Class2 %>% round(2),
#              Variables
#   )
# 
# OutputTable %>% write.csv(quote = FALSE)
# 
# 


# 
# 
# RawWTP <-
#   cbind(
#     "Encounter_Medium_Beetle_Class1" = Encounter_Medium_Beetle_Class1,
#     "Encounter_High_Beetle_Class1" = Encounter_High_Beetle_Class1,
#     "Existence_Medium_Beetle_Class1" = Existence_Medium_Beetle_Class1,
#     "Existence_High_Beetle_Class1" = Existence_High_Beetle_Class1,
#     "Bequest_Medium_Beetle_Class1" = Bequest_Medium_Beetle_Class1,
#     "Bequest_High_Beetle_Class1" = Bequest_High_Beetle_Class1,
#     "Encounter_Medium_Beetle_Class2" = Encounter_Medium_Beetle_Class2,
#     "Encounter_High_Beetle_Class2" = Encounter_High_Beetle_Class2,
#     "Existence_Medium_Beetle_Class2" = Existence_Medium_Beetle_Class2,
#     "Existence_High_Beetle_Class2" = Existence_High_Beetle_Class2,
#     "Bequest_Medium_Beetle_Class2" = Bequest_Medium_Beetle_Class2,
#     "Bequest_High_Beetle_Class2" = Bequest_High_Beetle_Class2,
#     "Encounter_Medium_Bee_Class1" = Encounter_Medium_Bee_Class1,
#     "Encounter_High_Bee_Class1" = Encounter_High_Bee_Class1,
#     "Existence_Medium_Bee_Class1" = Existence_Medium_Bee_Class1,
#     "Existence_High_Bee_Class1" = Existence_High_Bee_Class1,
#     "Bequest_Medium_Bee_Class1" = Bequest_Medium_Bee_Class1,
#     "Bequest_High_Bee_Class1" = Bequest_High_Bee_Class1,
#     "Encounter_Medium_Bee_Class2" = Encounter_Medium_Bee_Class2,
#     "Encounter_High_Bee_Class2" = Encounter_High_Bee_Class2,
#     "Existence_Medium_Bee_Class2" = Existence_Medium_Bee_Class2,
#     "Existence_High_Bee_Class2" = Existence_High_Bee_Class2,
#     "Bequest_Medium_Bee_Class2" = Bequest_Medium_Bee_Class2,
#     "Bequest_High_Bee_Class2" = Bequest_High_Bee_Class2,
#     "Encounter_Medium_Wasp_Class1" = Encounter_Medium_Wasp_Class1,
#     "Encounter_High_Wasp_Class1" = Encounter_High_Wasp_Class1,
#     "Existence_Medium_Wasp_Class1" = Existence_Medium_Wasp_Class1,
#     "Existence_High_Wasp_Class1" = Existence_High_Wasp_Class1,
#     "Bequest_Medium_Wasp_Class1" = Bequest_Medium_Wasp_Class1,
#     "Bequest_High_Wasp_Class1" = Bequest_High_Wasp_Class1,
#     "Encounter_Medium_Wasp_Class2" = Encounter_Medium_Wasp_Class2,
#     "Encounter_High_Wasp_Class2" = Encounter_High_Wasp_Class2,
#     "Existence_Medium_Wasp_Class2" = Existence_Medium_Wasp_Class2,
#     "Existence_High_Wasp_Class2" = Existence_High_Wasp_Class2,
#     "Bequest_Medium_Wasp_Class2" = Bequest_Medium_Wasp_Class2,
#     "Bequest_High_Wasp_Class2" = Bequest_High_Wasp_Class2
#   ) %>% data.frame()
# 
# 
# ## Rearrange
# WTP_Transformed <- RawWTP %>%
#   pivot_longer(cols = everything()) %>%
#   tidyr::separate(
#     name,
#     into = c("attribute", "level", "insect", "Class"),
#     sep = "_"
#   ) %>%
#   mutate(
#     attribute = factor(attribute, levels = c("Encounter", "Existence", "Bequest")),
#     level = factor(level, levels = c("Medium", "High"))
#   ) %>%
#   group_By(attribute, level, insect, Class) %>%
#   summarize(mean_value = mean(value))
# 
# 
# WTP_Transformed %>% 
#   data.frame() %>% 
#   fwrite(sep = ",", 
#          here("CEOutput/Main/LCM", 
#               "D2_Truncated_LC_3C_MXL_NoDR_V1_WTP_Transformed.txt"))

# WTP_Transformed <- here("CEOutput/Main/LCM", 
#                         "D2_Truncated_LC_3C_MXL_NoDR_V1_WTP_Transformed.txt") %>% 
#   fread(sep = ",") %>% 
#   data.frame()


# TextSize <- 12
# 
# 
# TextSetup <- element_text(size = TextSize,
#                           colour = "black",
#                           family = "serif")
# 
# # custom_colors <- RColorBrewer::brewer.pal(n = 9, name = "Purples")[c(3, 6, 9)]
# custom_colors <- RColorBrewer::brewer.pal(9, "Blues")[c(4, 6, 8)]
# 


# **********************************************************************************
#### Section 5: Plot ####
# **********************************************************************************



# Figure_Y <- 
#   WTP_Transformed %>%
#   ggplot(aes(x = attribute, 
#              y = mean_value,
#              shape = insect)) +
#   geom_point(size = 4,  
#              aes(colour = factor(insect),
#                  fill = factor(insect)),
#              position = position_dodge(width = 1)) +
#   facet_grid(Class ~ level, scales = "free_y") +
#   theme_Bw() +
#   geom_hline(yintercept = 0, alpha = 0.45) +
#   geom_vline(xintercept = 1.5, alpha = 0.45, linetype = "dotted") +
#   geom_vline(xintercept = 2.5, alpha = 0.45, linetype = "dotted") +
#   labs(
#     x = "Cultural Ecosystem Service",
#     y = "Marginal WTP"
#   ) +
#   scale_colour_manual(name = "Insect", values = custom_colors) +
#   scale_fill_manual(name = "Insect", values = custom_colors) +
#   scale_shape_manual(name = "Insect",values = c(21, 22, 23)
#   ) +
#   guides(
#     fill = guide_legend("Insect"),
#     shape = guide_legend("Insect")
#   ) +
#   theme(
#     legend.position = "bottom",
#     legend.text = TextSetup,
#     legend.title = TextSetup,
#     axis.text.x = TextSetup,
#     axis.text.y = TextSetup,
#     axis.title.x = TextSetup,
#     axis.title.y = TextSetup,
#     strip.background = element_rect(fill = "white"),
#     strip.text = TextSetup,
#     legend.background = element_Blank(),
#     panel.grid.major.x = element_Blank(),
#     panel.grid.minor.x = element_Blank(),
#     panel.grid.major.y = element_Blank()
#   )
# 
# 
# 
# # Figure_Y <- 
# WTP_Transformed %>%
#   ggplot(aes(x = attribute, 
#              y = mean_value,
#              shape = level)) +
#   geom_point(size = 4,  
#              aes(colour = factor(level),
#                  fill = factor(level)),
#              position = position_dodge(width = 1)) +
#   facet_grid(Class ~ insect, scales = "free_y") +
#   theme_Bw() +
#   geom_hline(yintercept = 0, alpha = 0.45) +
#   geom_vline(xintercept = 1.5, alpha = 0.45, linetype = "dotted") +
#   geom_vline(xintercept = 2.5, alpha = 0.45, linetype = "dotted") +
#   labs(
#     x = "Cultural Ecosystem Service",
#     y = "Marginal WTP"
#   ) +
#   scale_colour_manual(name = "level", values = custom_colors) +
#   scale_fill_manual(name = "level", values = custom_colors) +
#   scale_shape_manual(name = "level",values = c(21, 23)
#   ) +
#   guides(
#     fill = guide_legend("level"),
#     shape = guide_legend("level")
#   ) +
#   theme(
#     legend.position = "bottom",
#     legend.text = TextSetup,
#     legend.title = TextSetup,
#     axis.text.x = TextSetup,
#     axis.text.y = TextSetup,
#     axis.title.x = TextSetup,
#     axis.title.y = TextSetup,
#     strip.background = element_rect(fill = "white"),
#     strip.text = TextSetup,
#     legend.background = element_Blank(),
#     panel.grid.major.x = element_Blank(),
#     panel.grid.minor.x = element_Blank(),
#     panel.grid.major.y = element_Blank()
#   )


# 
#   WTP_Transformed %>%
#   ggplot(aes(x = value,
#              y = interaction(attribute, level) %>% fct_relevel(c("Encounter.Medium", "Encounter.High", 
#                                                                  "Existence.Medium", "Existence.High", 
#                                                                  "Bequest.Medium", "Bequest.High")),
#              color = insect %>% as.factor(),
#              fill = insect %>% as.factor(),
#              group = insect %>% as.factor())) +
#   ggdist::stat_histinterval(position = "dodge", 
#                             scale = 0.75, 
#                             outline_Bars = TRUE, 
#                             slab_colour = "black", 
#                             linewidth = 0.1,
#                             normalize = "groups") +
#   facet_grid( ~ Class) +
#   theme_Bw() +
#   geom_hline(yintercept = 2.6, alpha = 0.45) +
#   geom_hline(yintercept = 4.6, alpha = 0.45) +
#   geom_hline(yintercept = 1.6, alpha = 0.25, linetype = "dotted") +
#   geom_hline(yintercept = 5.6, alpha = 0.25, linetype = "dotted") +
#   geom_vline(xintercept = 0, alpha = 0.45) +
#   labs(
#     x = "WTP",
#     y = "Attribute",
#     color = "Insect"
#   ) +
#   scale_color_manual(
#     name = "Insect",
#     values = c("red", "blue", "green")
#   ) +
#   scale_fill_manual(
#     name = "Insect",
#     values = c("red", "blue", "green")
#   ) +
#   theme(
#     legend.position = "bottom",
#     legend.text = TextSetup,
#     legend.title = TextSetup,
#     axis.text.x = element_text(angle = 45, vjust = 0.6),
#     axis.text.y = TextSetup,
#     axis.title.y = TextSetup,
#     strip.background = element_rect(fill = "white"),
#     strip.text = TextSetup,
#     legend.background = element_Blank(),
#     panel.grid.major.x = element_Blank(),
#     panel.grid.minor.x = element_Blank(),
#     panel.grid.major.y = element_Blank()
#   ) +
#   coord_flip()


# Date <- gsub(pattern = "-",replacement = "_",Sys.Date())
# Save output in highest DPI
# ggsave(
#   Figure_Y,
#   device = "jpeg",
#   filename = here("OtherOutput/Figures",
#                   "D2_Truncated_LC_3C_MXL_NoDR_V1_Unconditionals_Boxplot.jpg"),
#   width = 20,
#   height = 25,
#   units = "cm",
#   dpi = 500
# )

