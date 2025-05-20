#### D2: Table 1 Sample Vs Quota ###############
# Script author: Peter King (p.king1@leeds.ac.uk)
# Last Edited: 20/05/2025
# Change log: 
# - Uses "D2_Truncated_LC_3C_MXL_NoDR_V3_estimates.csv"



# ################################################################# #
#### S0: Setup environment and replication information ####
# ################################################################# #

# ################################################################# #
#### Replication Information:

# here() = "K:/WinterAnalysis1307/WP5/WP5P3"
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



# ################################################################# #
#### Libraries:
options(scipen = 90) ## forces R to display numbers with decimals not as scientific notation
# rm(list=ls()) ## Remove everything in the environment


## Libraries that will come in handy later
library(apollo)
library(dplyr)
library(magrittr)
library(ggplot2)
library(ggridges)
library(reshape2)
library(janitor)
library(here)
library(data.table)
library(MASS)
library(tidyverse)
library(tidyr)


# ****************************
# S1: Import survey data: ####
# ****************************


## This is respondent data in long format
database <- here("Data/Main", "database_Step3.csv") %>% fread() %>% data.frame()


## This is respondent data in wide format
Data_Covariates <- here("Data/Main", 
                        "Data_Covariates_Spatial_Step5.csv") %>% fread() %>% data.frame()




# ****************************
# S2: Prepare variables: ####
# ****************************


## It is easier to grab serialSQ from database and append to wide
Data_Covariates$SerialSQ <- database %>%
  distinct(Respondent, SerialSQ) %>%
  .$SerialSQ
database <- here("Data/Main", "database_Step3.csv") %>% fread() %>% data.frame()


# ## Drop weird responses
Data_Covariates <- Data_Covariates %>% dplyr::filter(SerialSQ != 1 &
                                                       Protest_True == 1 &
                                                       CE_ANA_None != 1)

## Rescaling to ~[0,1]
Data_Covariates$Biowell_Bee1_Mean = Data_Covariates$Biowell_Bee1_Mean %>% scale() %>% as.numeric()
Data_Covariates$Biowell_Bee2_Mean = Data_Covariates$Biowell_Bee2_Mean %>% scale() %>% as.numeric()
Data_Covariates$Biowell_Bee3_Mean = Data_Covariates$Biowell_Bee3_Mean %>% scale() %>% as.numeric()
Data_Covariates$Biowell_Beetle1_Mean = Data_Covariates$Biowell_Beetle1_Mean %>% scale() %>% as.numeric()
Data_Covariates$Biowell_Beetle2_Mean = Data_Covariates$Biowell_Beetle2_Mean %>% scale() %>% as.numeric()
Data_Covariates$Biowell_Beetle3_Mean = Data_Covariates$Biowell_Beetle3_Mean %>% scale() %>% as.numeric()
Data_Covariates$Biowell_Wasp1_Mean = Data_Covariates$Biowell_Wasp1_Mean %>% scale() %>% as.numeric()
Data_Covariates$Biowell_Wasp2_Mean = Data_Covariates$Biowell_Wasp2_Mean %>% scale() %>% as.numeric()
Data_Covariates$Biowell_Wasp3_Mean = Data_Covariates$Biowell_Wasp3_Mean %>% scale() %>% as.numeric()


## Recode for ease of interpretation
Data_Covariates$DiscountRate_Scaled_Recoded <- Data_Covariates$DiscountRate %>% 
  as.numeric() %>% 
  scale() %>% 
  as.numeric()


Data_Covariates$IncomeMidpoints_PlusMissing_Recoded <- ifelse(Data_Covariates$IncomeMidpoints_PlusMissing < 500, 
                                                              250, 
                                                              Data_Covariates$IncomeMidpoints_PlusMissing) %>% 
  as.numeric() %>% scale() %>% as.numeric()



Data_Covariates$Q3Country_England <- ifelse( Data_Covariates$Q3Country == 1, 
                                             1, 
                                             0)


Data_Covariates$Biowell_Debrief_BeeCertain <- Data_Covariates$Biowell_Debrief_BeeCertain %>% 
  scale() %>% 
  as.numeric()

Data_Covariates$Biowell_Debrief_BeetleCertain <- Data_Covariates$Biowell_Debrief_BeetlesCertain %>% 
  scale() %>% 
  as.numeric()

Data_Covariates$Biowell_Debrief_WaspCertain <- Data_Covariates$Biowell_Debrief_WaspCertain %>% 
  scale() %>% 
  as.numeric()

Data_Covariates$CE_Debrief_Certain_scaled <- Data_Covariates$CE_Debrief_Certain %>% scale() %>% as.numeric()
Data_Covariates$CE_Debrief_Confident_scaled <- Data_Covariates$CE_Debrief_Confident %>% scale() %>% as.numeric()

## Flag people who chose £0 over £200
Data_Covariates$AlwaysZero <- ifelse(Data_Covariates$QDiscounting_7 == 2, 1, 0)





# ****************************
# S3: Import model data: ####
# ****************************


Estimates <- here("CEOutput/Main/LCM",
                  "D2_Truncated_LC_3C_MXL_NoDR_V3_estimates.csv") %>%
  fread() %>% 
  data.frame()




D2_Truncated_LC_3C_MXL_NoDR_V1_3C_UCWTP <- here("CEOutput/Main/LCM",
                                                "D2_Truncated_LC_3C_MXL_NoDR_V3_model_PiValues.rds") %>% readRDS()


Model <-
  here("CEOutput/Main/LCM", 
       "D2_Truncated_LC_3C_MXL_NoDR_V3_model.rds") %>% 
  readRDS() ## Enter model of interest RDS here


Label_Class1 <- Model$componentReport$choice$param[4][1] %>% 
  str_remove_all("Class\\d+") %>% 
  trimws() %>% 
  as.numeric() %>% 
  multiply_by(100) %>% 
  paste0(., "%")

Label_Class2 <- Model$componentReport$choice$param[5][1] %>% 
  str_remove_all("Class\\d+") %>% 
  trimws() %>% 
  as.numeric() %>% 
  multiply_by(100) %>% 
  paste0(., "%")

Label_Class3 <- Model$componentReport$choice$param[6][1] %>% 
  str_remove_all("Class\\d+") %>% 
  trimws() %>% 
  as.numeric() %>% 
  multiply_by(100) %>% 
  paste0(., "%")



# *************************************************************************
#### S4: Function setup ####
# *************************************************************************



## So this code outputs a table of estimate,  p.v stars and s.e in brackets ##
### To make it easy,  just change the model name here and the code will output the table for your model:
ModelOutputs <- function(Estimates) {
  data.frame("Variable" =  Estimates$V1,
             "Estimate" =  paste(
               ifelse(
                 Estimates$Rob.p.val.0. < 0.01,
                 paste0(Estimates$Estimate %>% round(2) %>% sprintf("%.2f", .),  "***"),
                 ifelse(
                   Estimates$Rob.p.val.0. < 0.05,
                   paste0(Estimates$Estimate %>% round(2) %>% sprintf("%.2f", .),  "**"),
                   ifelse(
                     Estimates$Rob.p.val.0. < 0.1,
                     paste0(Estimates$Estimate %>% round(2) %>% sprintf("%.2f", .),  "*"),
                     paste0(Estimates$Estimate %>% round(2) %>% sprintf("%.2f", .)))
                 )),
               paste0("(", Estimates$Rob.std.err %>% round(2) %>% sprintf("%.2f", .), ")")
             ))
}


## New function to take model and report model stats
Diagnostics <- function(Model) {
  rbind(
    "N" = Model$nIndivs,
    "AIC" = Model$AIC %>% round(3) %>% sprintf("%.3f", .),
    "Adj.R2" = Model$adjRho2_C %>% round(2) %>% sprintf("%.3f", .),
    "LogLik" = Model$LLout %>% as.numeric() %>% round(3) %>% sprintf("%.3f", .)
  )
}


Test <- ModelOutputs(Estimates) %>% data.frame()



# *************************************************************************
#### S5: Class allocation ####
# *************************************************************************


ClassMembership <- apply(do.call(cbind, D2_Truncated_LC_3C_MXL_NoDR_V1_3C_UCWTP), 1, which.max)

Data_Covariates$ClassMembership <- ClassMembership


TableOutput_ClassAllocation_A <- Test %>%
  dplyr::filter(str_starts(Variable, "ClassAllocation_")) %>% 
  mutate(Variable = gsub(x = Variable, pattern = "ClassAllocation_", replacement = "")) %>%
  mutate(
    Class = str_extract(Variable, "^C[0-9]+"),
    Class = as.numeric(str_replace(Class, "C", "")),
    Variable = str_replace(Variable, "^C[0-9]+_", "")
  ) %>% 
  pivot_wider(names_from = Class, values_from = Estimate)



## Find in files this one-way tabyl n (%)
database %>%
  tabyl(AgeGroup) %>%
  adorn_totals("row") %>%
  adorn_pct_formatting(digits = 2, rounding = "half up", affix_sign = TRUE) %>%
  mutate(Output = paste0(AgeGroup, ": ", n, " (", percent, ")")) %>% 
  dplyr::select(Output) %>% 
  write.csv(quote = FALSE, row.names = FALSE)


Summariser <- function(Variable) {
  paste0(
    Variable %>% mean() %>% sprintf("%.2f", .),
    " (",
    Variable %>% sd() %>% sprintf("%.2f", .),
    ")"
  )
}

# TableOutput_ClassAllocation_Variables <-
# Data_Covariates %>%
#   dplyr::group_by(ClassMembership) %>%
#   summarise(
#     "Q1Age" = Q1Age %>% Summariser(),
#     "IncomeMidpoints_PlusMissing_Recoded" = IncomeMidpoints_PlusMissing_Recoded %>% Summariser(),
#     "Female_dummy" = Female_dummy %>% Summariser(),
#     "Q3Country_England" = Q3Country_England %>% Summariser(),
#     "CE_Debrief_Certain_scaled" = CE_Debrief_Certain_scaled %>% Summariser(),
#     "CE_Debrief_Confident_scaled" = CE_Debrief_Confident_scaled %>% Summariser(),
#     "Q43_Citizen" = Q43_Citizen %>% Summariser(),
#     "AlwaysZero" = AlwaysZero %>% Summariser()
#   ) %>% 
#   pivot_longer(cols = 2:9) %>% 
#   pivot_wider(names_from = ClassMembership, values_from = value) 
# 
# TableOutput_ClassAllocation_Variables %>% 
#   write.csv(quote = FALSE, row.names = FALSE)



# First get your class summary statistics
class_stats <- Data_Covariates %>%
  dplyr::group_by(ClassMembership) %>%
  dplyr::summarise(
    "Q1Age" = Q1Age %>% Summariser(),
    "Income" = IncomeMidpoints_PlusMissing_Recoded %>% Summariser(),
    "Female_dummy" = Female_dummy %>% Summariser(),
    "Q3Country_England" = Q3Country_England %>% Summariser(),
    "CE_Debrief_Certain_scaled" = CE_Debrief_Certain_scaled %>% Summariser(),
    "CE_Debrief_Confident_scaled" = CE_Debrief_Confident_scaled %>% Summariser(),
    "Q43_Citizen" = Q43_Citizen %>% Summariser(),
    "AlwaysZero" = AlwaysZero %>% Summariser()
  ) %>%
  pivot_longer(cols = -ClassMembership, names_to = "Variable") %>%
  mutate(summary_stat = paste0("Class ", ClassMembership, ": ", value)) %>%
  dplyr::select(-ClassMembership, -value) %>%
  dplyr::group_by(Variable) %>%
  dplyr::summarise(Class_summaries = paste(summary_stat, collapse = "\n"))

# Match variable names to align with model_estimates
class_stats <- class_stats %>%
  mutate(Variable = case_when(
    Variable == "Q1Age" ~ "Q1Age",
    Variable == "Income" ~ "Income",
    Variable == "Female_dummy" ~ "Female_dummy",
    Variable == "Q3Country_England" ~ "Q3Country_England",
    Variable == "CE_Debrief_Certain_scaled" ~ "CE_Debrief_Certain_scaled",
    Variable == "CE_Debrief_Confident_scaled" ~ "CE_Debrief_Confident_scaled",
    Variable == "Q43_Citizen" ~ "Q43_Citizen",
    Variable == "AlwaysZero" ~ "AlwaysZero",
    TRUE ~ Variable
  ))

# Combine with your existing model estimates
final_table <- Test %>%
  dplyr::filter(str_starts(Variable, "ClassAllocation_")) %>% 
  mutate(Variable = gsub(x = Variable, pattern = "ClassAllocation_", replacement = "")) %>%
  mutate(
    Class = str_extract(Variable, "^C[0-9]+"),
    Class = as.numeric(str_replace(Class, "C", "")),
    Variable = str_replace(Variable, "^C[0-9]+_", "")
  ) %>% 
  pivot_wider(names_from = Class, values_from = Estimate) %>%
  left_join(class_stats, by = "Variable") %>%
  # Add human-readable variable names
  mutate(Variable_Label = case_when(
    Variable == "Q1Age" ~ "Age: years of age",
    Variable == "Income" ~ "Income: mean centred monthly gross income",
    Variable == "Female_dummy" ~ "Female: female (1), any other (0)",
    Variable == "Q3Country_England" ~ "England: living in England (1) vs living in Scotland or Wales (0)",
    Variable == "CE_Debrief_Certain_scaled" ~ "Certainty: \"I am certain about the answers I gave\"",
    Variable == "CE_Debrief_Confident_scaled" ~ "Consequentiality: \"I am confident that my choices in this survey will be considered in decision making\"",
    Variable == "Q43_Citizen" ~ "Citizen science: involved (0) or not involved (1)",
    Variable == "AlwaysZero" ~ "Discounting: chose £0 (1) over £200 (0)",
    TRUE ~ Variable
  )) %>%
  dplyr::select(Variable_Label, Class_summaries, `1`, `2`)

# Rename columns for clarity
names(final_table) <- c("Variable", "Class Summaries", "Class 1 (Pro-insect)", "Class 2 (Insect-averse)")

# *************************************************************************
#### S6: Export ####
# *************************************************************************


TableOutput_ClassAllocation  %>% write.csv(quote = FALSE, row.names = FALSE)
TableOutput_ClassAllocation %>% fwrite(sep = ",", here("CEOutput/Main/LCM", "D2_Truncated_LC_3C_MXL_NoDR_V1_ClassAllocation.txt"))



#### End of script
# *************************************************************************