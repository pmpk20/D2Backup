#### D2: Insects Main ####
## Function: A 3class LCM with MNL parameters and no time preferences
## Author: Dr Peter King (p.king1@leeds.ac.uk)
## Last change: 28/01/2025
## - No MNL, No discounting, new class-allocation
## - TODO: Tidy up and comment


# ****************************
# Replication Information: ####
# ****************************

# here() = "C:/Users/earpkin/OneDrive - University of Leeds/DRUID/D2/Analysis/D2Backup"
# ─ Session info ─────────────────────────────────────
# setting  value
# version  R version 4.5.0 (2025-04-11 ucrt)
# os       Windows 11 x64 (build 22631)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United Kingdom.utf8
# ctype    English_United Kingdom.utf8
# tz       Europe/London
# date     2025-05-20
# rstudio  2023.06.2+561 Mountain Hydrangea (desktop)
# pandoc   NA
# quarto   ERROR: Unknown command "TMPDIR=C:/Users/earpkin/AppData/Local/Temp/RtmpKgCcs8/file7187aba4677". Did you mean command "create-project"? @ C:\\PROGRA~1\\RStudio\\RESOUR~1\\app\\bin\\quarto\\bin\\quarto.exe
# 
# ─ Packages ─────────────────────────────────────────
# package      * version    date (UTC) lib source
# apollo       * 0.3.5      2025-03-12 [1] CRAN (R 4.5.0)
# bgw            0.1.3      2024-03-29 [1] CRAN (R 4.5.0)
# cli            3.6.5      2025-04-23 [1] CRAN (R 4.5.0)
# coda           0.19-4.1   2024-01-31 [1] CRAN (R 4.5.0)
# data.table   * 1.17.2     2025-05-12 [1] CRAN (R 4.5.0)
# dichromat      2.0-0.1    2022-05-02 [1] CRAN (R 4.5.0)
# digest         0.6.37     2024-08-19 [1] CRAN (R 4.5.0)
# dplyr        * 1.1.4      2023-11-17 [1] CRAN (R 4.5.0)
# farver         2.1.2      2024-05-13 [1] CRAN (R 4.5.0)
# forcats      * 1.0.0      2023-01-29 [1] CRAN (R 4.5.0)
# generics       0.1.4      2025-05-09 [1] CRAN (R 4.5.0)
# ggplot2      * 3.5.2      2025-04-09 [1] CRAN (R 4.5.0)
# ggridges     * 0.5.6      2024-01-23 [1] CRAN (R 4.5.0)
# glue           1.8.0      2024-09-30 [1] CRAN (R 4.5.0)
# gtable         0.3.6      2024-10-25 [1] CRAN (R 4.5.0)
# here         * 1.0.1      2020-12-13 [1] CRAN (R 4.5.0)
# hms            1.1.3      2023-03-21 [1] CRAN (R 4.5.0)
# janitor      * 2.2.1      2024-12-22 [1] CRAN (R 4.5.0)
# lattice        0.22-7     2025-04-02 [1] CRAN (R 4.5.0)
# lifecycle      1.0.4      2023-11-07 [1] CRAN (R 4.5.0)
# lubridate    * 1.9.4      2024-12-08 [1] CRAN (R 4.5.0)
# magrittr     * 2.0.3      2022-03-30 [1] CRAN (R 4.5.0)
# MASS         * 7.3-65     2025-02-28 [1] CRAN (R 4.5.0)
# Matrix         1.7-3      2025-03-11 [1] CRAN (R 4.5.0)
# MatrixModels   0.5-4      2025-03-26 [1] CRAN (R 4.5.0)
# matrixStats    1.5.0      2025-01-07 [1] CRAN (R 4.5.0)
# maxLik         1.5-2.1    2024-03-24 [1] CRAN (R 4.5.0)
# mcmc           0.9-8      2023-11-16 [1] CRAN (R 4.5.0)
# MCMCpack       1.7-1      2024-08-27 [1] CRAN (R 4.5.0)
# miscTools      0.6-28     2023-05-03 [1] CRAN (R 4.5.0)
# mnormt         2.1.1      2022-09-26 [1] CRAN (R 4.5.0)
# mvtnorm        1.3-3      2025-01-10 [1] CRAN (R 4.5.0)
# numDeriv       2016.8-1.1 2019-06-06 [1] CRAN (R 4.5.0)
# pillar         1.10.2     2025-04-05 [1] CRAN (R 4.5.0)
# pkgconfig      2.0.3      2019-09-22 [1] CRAN (R 4.5.0)
# plyr           1.8.9      2023-10-02 [1] CRAN (R 4.5.0)
# purrr        * 1.0.4      2025-02-05 [1] CRAN (R 4.5.0)
# quantreg       6.1        2025-03-10 [1] CRAN (R 4.5.0)
# R6             2.6.1      2025-02-15 [1] CRAN (R 4.5.0)
# randtoolbox    2.0.5      2024-10-18 [1] CRAN (R 4.5.0)
# RColorBrewer   1.1-3      2022-04-03 [1] CRAN (R 4.5.0)
# Rcpp           1.0.14     2025-01-12 [1] CRAN (R 4.5.0)
# readr        * 2.1.5      2024-01-10 [1] CRAN (R 4.5.0)
# reshape2     * 1.4.4      2020-04-09 [1] CRAN (R 4.5.0)
# rlang          1.1.6      2025-04-11 [1] CRAN (R 4.5.0)
# rngWELL        0.10-10    2024-10-17 [1] CRAN (R 4.5.0)
# rprojroot      2.0.4      2023-11-05 [1] CRAN (R 4.5.0)
# RSGHB          1.2.2      2019-07-03 [1] CRAN (R 4.5.0)
# Rsolnp         1.16       2015-12-28 [1] CRAN (R 4.5.0)
# rstudioapi     0.17.1     2024-10-22 [1] CRAN (R 4.5.0)
# sandwich       3.1-1      2024-09-15 [1] CRAN (R 4.5.0)
# scales         1.4.0      2025-04-24 [1] CRAN (R 4.5.0)
# sessioninfo  * 1.2.3      2025-02-05 [1] CRAN (R 4.5.0)
# snakecase      0.11.1     2023-08-27 [1] CRAN (R 4.5.0)
# SparseM        1.84-2     2024-07-17 [1] CRAN (R 4.5.0)
# stringi        1.8.7      2025-03-27 [1] CRAN (R 4.5.0)
# stringr      * 1.5.1      2023-11-14 [1] CRAN (R 4.5.0)
# survival       3.8-3      2024-12-17 [1] CRAN (R 4.5.0)
# tibble       * 3.2.1      2023-03-20 [1] CRAN (R 4.5.0)
# tidyr        * 1.3.1      2024-01-24 [1] CRAN (R 4.5.0)
# tidyselect     1.2.1      2024-03-11 [1] CRAN (R 4.5.0)
# tidyverse    * 2.0.0      2023-02-22 [1] CRAN (R 4.5.0)
# timechange     0.3.0      2024-01-18 [1] CRAN (R 4.5.0)
# truncnorm      1.0-9      2023-03-20 [1] CRAN (R 4.5.0)
# tzdb           0.5.0      2025-03-15 [1] CRAN (R 4.5.0)
# utf8           1.2.5      2025-05-01 [1] CRAN (R 4.5.0)
# vctrs          0.6.5      2023-12-01 [1] CRAN (R 4.5.0)
# withr          3.0.2      2024-10-28 [1] CRAN (R 4.5.0)
# zoo            1.8-14     2025-04-10 [1] CRAN (R 4.5.0)
# 
# [1] C:/Users/earpkin/AppData/Local/Programs/R/R-4.5.0/library
# * ── Packages attached to the search path.


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


## TRUNCATE FIRST
database <- database %>% dplyr::filter(SerialSQ != 1 &
                                         Protest_True == 1 &
                                         CE_ANA_None != 1)


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
                                                       500, 
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
  nCores = 40,
  mixing = TRUE,
  # analyticGrad    = TRUE, ## Backup if convergence an issue
  modelName = "D2_Truncated_LC_3C_MXL_NoDR_V3",
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
  asc_C_Class1 = -0.050499,
  asc_C_Class2 = -3.436000,
  asc_C_Class3 = -3.629333,
  beta_Tax_Class1 = -0.022257,
  beta_Tax_Class2 = -0.186967,
  beta_Tax_Class3 = -0.125080,
  mu_Encounter_Medium_Class1 =  20.255928,
  mu_Encounter_High_Class1 =  30.992926,
  mu_Existence_Medium_Class1 =  14.582946,
  mu_Existence_High_Class1 =  19.068855,
  mu_Bequest_Medium_Class1 =  26.120187,
  mu_Bequest_High_Class1 =  55.543362,
  mu_Int_Encounter_Medium_Bee_Class1 = -7.518903,
  mu_Int_Encounter_High_Bee_Class1 =  20.176504,
  mu_Int_Existence_Medium_Bee_Class1 = -36.047518,
  mu_Int_Existence_High_Bee_Class1 =  27.166582,
  mu_Int_Bequest_Medium_Bee_Class1 =  20.300756,
  mu_Int_Bequest_High_Bee_Class1 =  23.915712,
  mu_Int_Existence_Medium_Wasp_Class1 =   7.632834,
  mu_Int_Existence_High_Wasp_Class1 =  22.995987,
  mu_Int_Encounter_Medium_Wasp_Class1 =  29.484080,
  mu_Int_Encounter_High_Wasp_Class1 =   7.975582,
  mu_Int_Bequest_Medium_Wasp_Class1 = -20.813370,
  mu_Int_Bequest_High_Wasp_Class1 = -28.521636,
  mu_Encounter_Medium_Class2 = -19.571922,
  mu_Encounter_High_Class2 = -21.123839,
  mu_Existence_Medium_Class2 = -9.354278,
  mu_Existence_High_Class2 = -25.266627,
  mu_Bequest_Medium_Class2 = -3.638017,
  mu_Bequest_High_Class2 = -15.162056,
  mu_Int_Encounter_Medium_Bee_Class2 =   3.012287,
  mu_Int_Encounter_High_Bee_Class2 =  10.088446,
  mu_Int_Existence_Medium_Bee_Class2 =  12.525473,
  mu_Int_Existence_High_Bee_Class2 = -113.325420,
  mu_Int_Bequest_Medium_Bee_Class2 = -92.771150,
  mu_Int_Bequest_High_Bee_Class2 =   6.656949,
  mu_Int_Existence_Medium_Wasp_Class2 =   5.887375,
  mu_Int_Existence_High_Wasp_Class2 =   0.319496,
  mu_Int_Encounter_Medium_Wasp_Class2 =   2.182677,
  mu_Int_Encounter_High_Wasp_Class2 = -62.368546,
  mu_Int_Bequest_Medium_Wasp_Class2 =   8.421744,
  mu_Int_Bequest_High_Wasp_Class2 =  10.807129,
  mu_Encounter_Medium_Class3 =   1.014642,
  mu_Encounter_High_Class3 =   5.403047,
  mu_Existence_Medium_Class3 =  10.949814,
  mu_Existence_High_Class3 =   4.347735,
  mu_Bequest_Medium_Class3 = -16.368749,
  mu_Bequest_High_Class3 = -0.882910,
  mu_Int_Encounter_Medium_Bee_Class3 = -7.076033,
  mu_Int_Encounter_High_Bee_Class3 = -7.978586,
  mu_Int_Existence_Medium_Bee_Class3 = -10.915133,
  mu_Int_Existence_High_Bee_Class3 = -5.132854,
  mu_Int_Bequest_Medium_Bee_Class3 =  13.821862,
  mu_Int_Bequest_High_Bee_Class3 = -1.766574,
  mu_Int_Existence_Medium_Wasp_Class3 = -17.681089,
  mu_Int_Existence_High_Wasp_Class3 = -15.745729,
  mu_Int_Encounter_Medium_Wasp_Class3 =  21.059849,
  mu_Int_Encounter_High_Wasp_Class3 =   2.580938,
  mu_Int_Bequest_Medium_Wasp_Class3 = -7.445184,
  mu_Int_Bequest_High_Wasp_Class3 = -12.503127,
  sig_Encounter_Medium_Class1 = -31.009107,
  sig_Encounter_High_Class1 = -43.813437,
  sig_Existence_Medium_Class1 = -11.291455,
  sig_Existence_High_Class1 = -25.709695,
  sig_Bequest_Medium_Class1 = -10.169952,
  sig_Bequest_High_Class1 = -30.890094,
  sig_Int_Encounter_Medium_Bee_Class1 =   6.653772,
  sig_Int_Encounter_High_Bee_Class1 =   0.693637,
  sig_Int_Existence_Medium_Bee_Class1 =  93.595561,
  sig_Int_Existence_High_Bee_Class1 =  37.120414,
  sig_Int_Bequest_Medium_Bee_Class1 = -9.563833,
  sig_Int_Bequest_High_Bee_Class1 =  70.332345,
  sig_Int_Existence_Medium_Wasp_Class1 =  27.812087,
  sig_Int_Existence_High_Wasp_Class1 =  15.162814,
  sig_Int_Encounter_Medium_Wasp_Class1 = -27.358389,
  sig_Int_Encounter_High_Wasp_Class1 =   5.016673,
  sig_Int_Bequest_Medium_Wasp_Class1 = -30.655928,
  sig_Int_Bequest_High_Wasp_Class1 =  80.877177,
  sig_Encounter_Medium_Class2 = -2.803825,
  sig_Encounter_High_Class2 = -1.727358,
  sig_Existence_Medium_Class2 =   4.374289,
  sig_Existence_High_Class2 =   5.298439,
  sig_Bequest_Medium_Class2 = -11.431259,
  sig_Bequest_High_Class2 = -0.579045,
  sig_Int_Encounter_Medium_Bee_Class2 = -5.217648,
  sig_Int_Encounter_High_Bee_Class2 =  10.004118,
  sig_Int_Existence_Medium_Bee_Class2 = -10.567992,
  sig_Int_Existence_High_Bee_Class2 = -113.107214,
  sig_Int_Bequest_Medium_Bee_Class2 =   145.622970,
  sig_Int_Bequest_High_Bee_Class2 = -10.055321,
  sig_Int_Existence_Medium_Wasp_Class2 = -1.226547,
  sig_Int_Existence_High_Wasp_Class2 =   4.979817,
  sig_Int_Encounter_Medium_Wasp_Class2 =   4.067193,
  sig_Int_Encounter_High_Wasp_Class2 = -60.287546,
  sig_Int_Bequest_Medium_Wasp_Class2 = -6.388930,
  sig_Int_Bequest_High_Wasp_Class2 =  13.695479,
  sig_Encounter_Medium_Class3 = -8.622836,
  sig_Encounter_High_Class3 =   5.200118,
  sig_Existence_Medium_Class3 = -0.495759,
  sig_Existence_High_Class3 =   9.295085,
  sig_Bequest_Medium_Class3 = -7.237200,
  sig_Bequest_High_Class3 =   1.143410,
  sig_Int_Encounter_Medium_Bee_Class3 =   1.505260,
  sig_Int_Encounter_High_Bee_Class3 = -0.218744,
  sig_Int_Existence_Medium_Bee_Class3 =   1.071008,
  sig_Int_Existence_High_Bee_Class3 = -12.013279,
  sig_Int_Bequest_Medium_Bee_Class3 =   3.434330,
  sig_Int_Bequest_High_Bee_Class3 = -1.843550,
  sig_Int_Existence_Medium_Wasp_Class3 = -10.748702,
  sig_Int_Existence_High_Wasp_Class3 =   0.486008,
  sig_Int_Encounter_Medium_Wasp_Class3 =  17.408295,
  sig_Int_Encounter_High_Wasp_Class3 = -1.849642,
  sig_Int_Bequest_Medium_Wasp_Class3 =  15.261151,
  sig_Int_Bequest_High_Wasp_Class3 =   0.674235,
  delta_Class1 =   4.475292,
  delta_Class2 = -0.193658,
  Int_LV_Bee_Encounter_Medium_Class1 =   5.019432,
  Int_LV_Bee_Existence_Medium_Class1 = -6.008531,
  Int_LV_Bee_Bequest_Medium_Class1 =   2.691087,
  Int_LV_Bee_Encounter_High_Class1 =   4.002235,
  Int_LV_Bee_Existence_High_Class1 =   5.298108,
  Int_LV_Bee_Bequest_High_Class1 =  13.373153,
  Int_LV_Beetle_Encounter_Medium_Class1 =   1.974021,
  Int_LV_Beetle_Existence_Medium_Class1 =   7.922487,
  Int_LV_Beetle_Bequest_Medium_Class1 =  10.746965,
  Int_LV_Beetle_Encounter_High_Class1 =   9.795678,
  Int_LV_Beetle_Existence_High_Class1 =  11.354150,
  Int_LV_Beetle_Bequest_High_Class1 =  12.407041,
  Int_LV_Wasp_Encounter_Medium_Class1 =  11.113868,
  Int_LV_Wasp_Existence_Medium_Class1 =   7.395505,
  Int_LV_Wasp_Bequest_Medium_Class1 =   1.477757,
  Int_LV_Wasp_Encounter_High_Class1 =  13.749173,
  Int_LV_Wasp_Existence_High_Class1 =  13.632927,
  Int_LV_Wasp_Bequest_High_Class1 =  11.163785,
  Int_LV_Bee_Encounter_Medium_Class2 =   8.149498,
  Int_LV_Bee_Existence_Medium_Class2 =   6.670131,
  Int_LV_Bee_Bequest_Medium_Class2 =  20.259030,
  Int_LV_Bee_Encounter_High_Class2 =   3.621121,
  Int_LV_Bee_Existence_High_Class2 =  14.621758,
  Int_LV_Bee_Bequest_High_Class2 =   1.457275,
  Int_LV_Beetle_Encounter_Medium_Class2 = -0.586765,
  Int_LV_Beetle_Existence_Medium_Class2 = -1.555620,
  Int_LV_Beetle_Bequest_Medium_Class2 =   6.036216,
  Int_LV_Beetle_Encounter_High_Class2 =   2.439514,
  Int_LV_Beetle_Existence_High_Class2 = -1.451295,
  Int_LV_Beetle_Bequest_High_Class2 =   2.140912,
  Int_LV_Wasp_Encounter_Medium_Class2 =   2.820431,
  Int_LV_Wasp_Existence_Medium_Class2 =   2.082228,
  Int_LV_Wasp_Bequest_Medium_Class2 = -0.369792,
  Int_LV_Wasp_Encounter_High_Class2 =   6.269815,
  Int_LV_Wasp_Existence_High_Class2 =   4.990568,
  Int_LV_Wasp_Bequest_High_Class2 =   0.025399,
  Int_LV_Bee_Encounter_Medium_Class3 =   3.396395,
  Int_LV_Bee_Existence_Medium_Class3 = -0.700293,
  Int_LV_Bee_Bequest_Medium_Class3 =   2.482800,
  Int_LV_Bee_Encounter_High_Class3 =   3.239375,
  Int_LV_Bee_Existence_High_Class3 =   1.389163,
  Int_LV_Bee_Bequest_High_Class3 =   2.355599,
  Int_LV_Beetle_Encounter_Medium_Class3 =   0.057343,
  Int_LV_Beetle_Existence_Medium_Class3 =   1.136175,
  Int_LV_Beetle_Bequest_Medium_Class3 =   0.261052,
  Int_LV_Beetle_Encounter_High_Class3 = -0.989700,
  Int_LV_Beetle_Existence_High_Class3 = -0.213162,
  Int_LV_Beetle_Bequest_High_Class3 =   0.584706,
  Int_LV_Wasp_Encounter_Medium_Class3 =  11.483456,
  Int_LV_Wasp_Existence_Medium_Class3 =   3.666930,
  Int_LV_Wasp_Bequest_Medium_Class3 = -1.425245,
  Int_LV_Wasp_Encounter_High_Class3 =  12.514323,
  Int_LV_Wasp_Existence_High_Class3 =   5.315859,
  Int_LV_Wasp_Bequest_High_Class3 = -1.562000,
  ClassAllocation_C1_Q1Age = -0.034483,
  ClassAllocation_C1_Income =   0.044871,
  ClassAllocation_C1_Female_dummy = -0.325341,
  ClassAllocation_C1_Q3Country_England =   0.188292,
  ClassAllocation_C1_CE_Debrief_Certain_scaled = -0.185568,
  ClassAllocation_C1_CE_Debrief_Confident_scaled =   0.143141,
  ClassAllocation_C1_Q43_Citizen = -1.335075,
  ClassAllocation_C1_AlwaysZero = -1.764607,
  ClassAllocation_C2_Q1Age =   0.002836,
  ClassAllocation_C2_Income = -0.033031,
  ClassAllocation_C2_Female_dummy =   0.511249,
  ClassAllocation_C2_Q3Country_England = -0.328144,
  ClassAllocation_C2_CE_Debrief_Certain_scaled =   0.140673,
  ClassAllocation_C2_CE_Debrief_Confident_scaled = -0.328531,
  ClassAllocation_C2_Q43_Citizen =   0.388497,
  ClassAllocation_C2_AlwaysZero = -0.632926
)

apollo_fixed = c()


# ################################################################# #
# S4: Random draws ####
# ################################################################# #


apollo_draws = list(
  interDrawsType = "sobol",## Robust to using MLHS or Sobol draws
  interNDraws    = 1000, ## more the better - but also HPC kills too many
  # interNDraws    = 2000, ## more the better - but also HPC kills too many
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
D2_Truncated_LC_3C_MXL_NoDR_V3 = apollo_estimate(apollo_beta,
                                                 apollo_fixed,
                                                 apollo_probabilities,
                                                 apollo_inputs)

### Show output in screen
apollo_modelOutput(D2_Truncated_LC_3C_MXL_NoDR_V3,
                   modelOutput_settings = list(printPVal = 2))


### Save output to file(s)
apollo_saveOutput(D2_Truncated_LC_3C_MXL_NoDR_V3,
                  saveOutput_settings = list(printPVal = 2))

apollo_sink()


Model <- here("CEOutput/Main/LCM", 
              "D2_Truncated_LC_3C_MXL_NoDR_V3_model.rds") %>% readRDS()


Test <- apollo_unconditionals(Model, apollo_probabilities, apollo_inputs)

Test$latentClass$pi_values %>% 
  saveRDS(file = here("CEOutput/Main/LCM",
                      "D2_Truncated_LC_3C_MXL_NoDR_V3_model_PiValues.rds"))

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
    "BIC" = Model$BIC %>% round(3) %>% sprintf("%.3f", .),
    "Adj.R2" = Model$adjRho2_C %>% round(3) %>% sprintf("%.3f", .),
    "LogLik" = Model$LLout["model"] %>% as.numeric() %>% round(3) %>% sprintf("%.3f", .)
  )
}




Estimates <- here("CEOutput/Main/LCM", 
                  "D2_Truncated_LC_3C_MXL_NoDR_V3_estimates.csv") %>%
  fread() %>% 
  data.frame()


ModelOutputs(Estimates)


ModelOutputs(Estimates) %>% 
  data.frame() %>% 
  fwrite(sep = ",", 
         here("CEOutput/Main/LCM", 
              "D2_Truncated_LC_3C_MXL_NoDR_V3_MyModelOutputs.txt"))



# ----------------------------------------------------------------- #
#---- POSTERIOR ANALYSIS                                     ----
# ----------------------------------------------------------------- #


Model <- here("CEOutput/Main/LCM", 
              "D2_Truncated_LC_3C_MXL_NoDR_V3_model.rds") %>% readRDS()


# Set up parallel environment
n_cores <- parallel::detectCores() - 1
RNGkind("L'Ecuyer-CMRG")  # For reproducible parallel random numbers
set.seed(123)

# Load model
Model <- here("CEOutput/Main/LCM", "D2_Truncated_LC_3C_MXL_NoDR_V3_model.rds") %>% readRDS()

# Simulator function - optimised
Simulator <- function(Model, Class, insect, wellbeing) {
  # Reduced iterations for testing - increase for production
  N_Reps_KrinskyRobb <- 10000
  N_Reps_Draws <- 10000
  
  # Define attributes and levels
  names <- c("Encounter_Medium", "Encounter_High", "Existence_Medium", 
             "Existence_High", "Bequest_Medium", "Bequest_High")
  
  # Extract model parameters once
  mean_v <- Model$estimate
  covar <- as.matrix(Model$robvarcov)
  
  # Draw from multivariate normal distribution - do this once
  model_draws <- mvtnorm::rmvnorm(n = N_Reps_KrinskyRobb,
                                  mean = mean_v,
                                  sigma = covar)
  
  # Pre-generate all random draws at once
  all_params <- c(names, paste0("Int_", 
                                rep(names, each=2), 
                                "_", 
                                rep(c("Bee", "Wasp"), 
                                    times=length(names))))
  
  # Generate all normal draws at once
  set.seed(123 + Class + match(insect, Insects) + wellbeing*10)  # Different seed for each combo
  all_draws <- matrix(rnorm(N_Reps_Draws * length(all_params)), 
                      nrow = N_Reps_Draws, 
                      ncol = length(all_params))
  colnames(all_draws) <- all_params
  
  # Pre-allocate results matrix
  result_mat <- matrix(NA, nrow = N_Reps_KrinskyRobb, ncol = length(names))
  colnames(result_mat) <- names
  
  # Main simulation loop - vectorised where possible
  for (i in 1:N_Reps_KrinskyRobb) {
    for (j in seq_along(names)) {
      param <- names[j]
      
      # Parameter names
      mu_name <- paste0("mu_", param, "_Class", Class)
      sig_name <- paste0("sig_", param, "_Class", Class)
      
      Int_name_Insect <- ifelse(insect == "Beetle", 
                                "Int_LV_Beetle_", 
                                paste0("Int_LV_", insect, "_"))
      Int_name <- paste0(Int_name_Insect, param, "_Class", Class)
      
      # Calculate base WTP
      base_wtp <- model_draws[i, mu_name] + 
        model_draws[i, sig_name] * all_draws[, param] +
        model_draws[i, Int_name] * wellbeing
      
      # Adjust for insect-specific effects
      if (insect != "Beetle") {
        int_mu_name <- paste0("mu_Int_", param, "_", insect, "_Class", Class)
        int_sig_name <- paste0("sig_Int_", param, "_", insect, "_Class", Class)
        
        insect_effect <- model_draws[i, int_mu_name] + 
          model_draws[i, int_sig_name] * all_draws[, paste0("Int_", param, "_", insect)]
        
        total_wtp <- base_wtp + insect_effect
      } else {
        total_wtp <- base_wtp
      }
      
      # Store mean WTP
      result_mat[i, j] <- mean(total_wtp)
    }
  }
  
  # Convert to data.table directly (avoid data.frame conversion)
  Output <- as.data.table(result_mat)
  Output[, insect := insect]
  
  return(Output)
}

# Define parameter grid
Classes <- c(1, 2, 3)
Insects <- c("Beetle", "Bee", "Wasp")
wellbeing_levels <- c(-2, -1, 0, 1, 2)

param_grid <- expand.grid(
  Class = Classes,
  insect = Insects,
  wellbeing = wellbeing_levels
)

# Setup parallel backend with proper RNG
plan(multisession, workers = detectCores() - 1)

# Add future.seed=TRUE to ensure proper random number generation
results <- future_lapply(1:nrow(param_grid), function(i) {
  class <- param_grid$Class[i]
  insect <- param_grid$insect[i]
  wellbeing <- param_grid$wellbeing[i]
  
  sim_result <- Simulator(Model = Model, 
                          Class = class, 
                          insect = insect, 
                          wellbeing = wellbeing)
  
  # Add metadata
  sim_result[, Class := class]
  sim_result[, wellbeing := wellbeing]
  
  return(sim_result)
}, future.seed = TRUE)  # This is the key addition


# Combine results efficiently
all_results <- rbindlist(results, fill = TRUE)


# Make sure all columns are properly formatted
all_results <- all_results[, lapply(.SD, function(x) if(is.numeric(x)) round(x, 4) else x)] %>% 
  data.frame


# Get column names (excluding the metadata columns)
value_cols <- setdiff(colnames(all_results), c("Class", "insect", "wellbeing"))

# Set column order
setcolorder(all_results, c("Class", "insect", "wellbeing", value_cols))

# If you want to reshape to a more analysis-friendly format
# Convert from wide to long format
all_results_long <- melt(all_results, 
                         id.vars = c("Class", "insect", "wellbeing"),
                         variable.name = "parameter",
                         value.name = "value")

# Export both formats
fwrite(all_results, 
       file = here("CEOutput/Main/LCM", "D2_Truncated_LC_3C_MXL_NoDR_V3_SimulatedMeans_Wide.csv"), 
       sep = ",", 
       quote = FALSE)


fwrite(all_results_long, 
       file = here("CEOutput/Main/LCM", "D2_Truncated_LC_3C_MXL_NoDR_V3_SimulatedMeans_Long.csv"), 
       sep = ",", 
       quote = FALSE)





# *************************************************************
#### Section X: Plot setup  ####
# *************************************************************


TextSize <- 12


TextSetup <- element_text(size = TextSize,
                          colour = "black",
                          family = "serif")

# custom_colors <- RColorBrewer::brewer.pal(n = 9, name = "Purples")[c(3, 6, 9)]
custom_colors <- RColorBrewer::brewer.pal(9, "Blues")[c(4, 6, 8)]


Test <-
  here("CEOutput/Main/LCM",
       "D2_Truncated_LC_3C_MXL_NoDR_V3_SimulatedMeans_Wide.csv") %>% fread() %>% data.frame()




TestTable <- Test %>%
  dplyr::filter(wellbeing == 0) %>%
  pivot_longer(cols = 4:9) %>%
  tidyr::separate(
    name,
    into = c("attribute", "level"),
    sep = "_"
  ) %>%
  summarise(.by = c(attribute, level, insect, Class),
            Mean = mean(value, 0.025))


TestTable %>% 
  pivot_wider(names_from = insect, values_from = Mean) %>% 
  arrange(desc(attribute), level, Class) %>% 
  dplyr::select(attribute, level, Class, Bee, Beetle, Wasp) %>% 
  dplyr::mutate(
    Bee = sprintf("%.2f", Bee),
    Beetle = sprintf("%.2f", Beetle),
    Wasp = sprintf("%.2f", Wasp)
  )









TestPlot <- Test %>% 
  dplyr::filter(wellbeing == 0) %>% 
  pivot_longer(cols = 4:9) %>% 
  tidyr::separate(
    name,
    into = c("attribute", "level"),
    sep = "_"
  ) %>% 
  summarise(.by = c(attribute, level, insect, Class),
            y0 = quantile(value, 0.025),
            y25 = quantile(value, 0.25),
            y50 = quantile(value, 0.50),
            y75 = quantile(value, 0.75),
            y100 = quantile(value, 0.975)) %>% 
  mutate(
    attribute = factor(attribute, levels = c("Encounter", "Existence", "Bequest")),
    level = factor(level, levels = c("Medium", "High")),
    Class = case_when(Class == 1 ~ "Class1",
                      Class == 2 ~ "Class2",
                      Class == 3 ~ "Class3"),
    facet_order = factor(paste0(Class, ": ", level),
                         levels = c("Class1: Medium", "Class1: High",
                                    "Class2: Medium", "Class2: High",
                                    "Class3: Medium", "Class3: High")))


D2_Truncated_LC_3C_MXL_NoDR_V3_SimulatedMeans_Boxplot_V1 <- TestPlot %>%  
  ggplot(aes(x = attribute,
             fill = insect)) +
  geom_errorbar(aes(
    ymin = y0,
    ymax = y100),
    width = 0.5,
    position = position_dodge(1)) +
  geom_boxplot(position = position_dodge(1),
               varwidth = 0.5,
               outlier.shape = NA,
               aes(
                 ymin = y0,
                 lower = y25,
                 middle = y50,
                 upper = y75,
                 ymax = y100,
               ),
               stat = "identity"
  ) +
  
  facet_wrap( ~ facet_order,
              ncol = 2,
              nrow = 3,
              scales = "free_x") +
  
  theme_bw() +
  
  ylab("Marginal WTP (GBP) in income tax per household, per annum") +
  
  
  scale_x_discrete(name = "Cultural Ecosystem Service") +
  
  scale_fill_manual(
    name = "Insect",
    values = brewer.pal(9, "Blues")[c(3, 6, 8)],
    
    label = c(
      "Bee",
      "Beetle",
      "Wasp"),
    
    guide = guide_legend(reverse = FALSE)) +
  
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 1.25) +
  geom_vline(xintercept = 1.5, alpha = 0.25) +
  geom_vline(xintercept = 2.5, alpha = 0.25) +
  theme(
    legend.position = "bottom",
    legend.background = element_blank(),
    strip.background.x = element_rect(fill = "white"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    
    axis.text.x = TextSetup,
    axis.text.y = TextSetup,
    axis.title.y = TextSetup,
    axis.title.x = TextSetup,
    legend.text = TextSetup,
    legend.title = TextSetup
    
  ) +
  coord_flip()





ggsave(
  D2_Truncated_LC_3C_MXL_NoDR_V3_SimulatedMeans_Boxplot_V1,
  device = "jpeg",
  filename = here("OtherOutput/Figures",
                  "D2_Truncated_LC_3C_MXL_NoDR_V3_SimulatedMeans_Boxplot_V1.jpg"),
  width = 20,
  height = 25,
  units = "cm",
  dpi = 500
)



