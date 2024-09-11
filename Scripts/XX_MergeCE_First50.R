#### D2: First pilot cleaning ###############
# Function: To clean and merge the first 50 responses 
# Author: Dr Peter King (p.king1@leeds.ac.uk)
# Last Edited: 10/09/2024
# Change/s:



# **********************************************************************************
#### Section 0: Setting up ####
## NOTES: This is just importing packages.
# **********************************************************************************


## Packages to install
# install.packages("haven")

## Any dcchoice problems run this first to rule out:
# BiocManager::install("Icens")


## Packages to library
library(haven)
library(here)
library(magrittr)
library(data.table)
library(tidyverse)
library(readxl)
library(DCchoice)



# **********************************************************************************
#### Section 1: Import Data  ####
## NOTES: This is in xlsx
# **********************************************************************************


## Respondent data
Data_Covariates <- here("Data/Pilot1/",
                        "Data_Covariates_Step1.csv") %>% 
  fread() %>% 
  data.frame()


## DCE data
Data_CE <-
  here("Data/Pilot1/", "DRUID_pilot_1_DCE_d2_test2_2024-09-10.xlsx") %>% 
  readxl::read_xlsx(sheet = "Data") %>% 
  data.frame()

Data_CE_backup <- Data_CE


# **********************************************************************************
#### Section 2: Merge  ####
# **********************************************************************************


colnames(Data_CE) <- c(
  "RID",
  "DESIGN_ROW",
  "Task",
  "SEQ",
  "Choice",
  
  "Insect_PlanA",
  "Encounter_PlanA",
  "Existence_PlanA",
  "Bequest_PlanA",
  "Tax_PlanA",
  
  "Insect_PlanB",
  "Encounter_PlanB",
  "Existence_PlanB",
  "Bequest_PlanB",
  "Tax_PlanB",
  
  "Insect_PlanC",
  "Encounter_PlanC",
  "Existence_PlanC",
  "Bequest_PlanC",
  "Tax_PlanC"
)



## Reshape DCE:
DCE_Wider <-
  Data_CE %>% pivot_wider(
    id_cols = RID,
    names_from = Task,
    values_from = c(
      "Choice",
      "Insect_PlanA",
      "Encounter_PlanA",
      "Existence_PlanA",
      "Bequest_PlanA",
      "Tax_PlanA",
      
      "Insect_PlanB",
      "Encounter_PlanB",
      "Existence_PlanB",
      "Bequest_PlanB",
      "Tax_PlanB",
      
      "Insect_PlanC",
      "Encounter_PlanC",
      "Existence_PlanC",
      "Bequest_PlanC",
      "Tax_PlanC"
    )
  ) %>% data.frame()

## Combine data
Data_Wide <- cbind(Data_Covariates,
                          DCE_Wider)

## Reshape for apollo
database <- Data_Covariates %>% left_join(Data_CE, by = "RID") 



## APOLLO always needs these availability indicators in case any alternative is not available.
database$av_PlanA <- rep(1, nrow(database)) ## Alternative A: Alt1
database$av_PlanB <- rep(1, nrow(database)) ## Alternative B: Alt2
database$av_PlanC <- rep(1, nrow(database)) ## Alternative C: Status Quo


## Add useful variables:
database$ID <-
  seq.int(nrow(database)) # Unique identifier for each respondent and choice (Uniques: 30,063)
database$Respondent <- rep(1:nrow(Data_Covariates),
                           each = database$Task %>% n_distinct()) # unique identifier for each respondent (Uniques: 3407)


# INSECT *****************************************************************
## Recode Attribute Levels:
database$Insect_PlanA_Words <- dplyr::recode(database$Insect_PlanA,
                           '1' = "Wasps",
                           '2' = "Bees",
                           '3' = "Beetles")


database$Insect_PlanB_Words <- dplyr::recode(database$Insect_PlanB,
                                             '1' = "Wasps",
                                             '2' = "Bees",
                                             '3' = "Beetles")



database$Insect_PlanC_Words <- dplyr::recode(database$Insect_PlanC,
                                             '1' = "Wasps",
                                             '2' = "Bees",
                                             '3' = "Beetles")


# Encountering *****************************************************************
database$Encounter_PlanA_Values <- dplyr::recode(database$Encounter_PlanA,
                                             '1' = 15,
                                             '2' = 30)


database$Encounter_PlanB_Values <- dplyr::recode(database$Encounter_PlanB,
                                                 '1' = 15,
                                                 '2' = 30)

database$Encounter_PlanC_Values <- dplyr::recode(database$Encounter_PlanC,
                                                 '1' = 0)



# Existence *****************************************************************
database$Existence_PlanA_Values <- dplyr::recode(database$Existence_PlanA,
                                                 '1' = 15,
                                                 '2' = 30)


database$Existence_PlanB_Values <- dplyr::recode(database$Existence_PlanB,
                                                 '1' = 15,
                                                 '2' = 30)

database$Existence_PlanC_Values <- dplyr::recode(database$Existence_PlanC,
                                                 '1' = 0)


# Bequesting *****************************************************************
database$Bequest_PlanA_Values <- dplyr::recode(database$Bequest_PlanA,
                                               '1' = 15,
                                               '2' = 30)


database$Bequest_PlanB_Values <- dplyr::recode(database$Bequest_PlanB,
                                               '1' = 15,
                                               '2' = 30)

database$Bequest_PlanC_Values <- dplyr::recode(database$Bequest_PlanC,
                                               '1' = 0)




# TAX *****************************************************************
database$Tax_PlanA_Values <- dplyr::recode(database$Tax_PlanA,
                                           '1' = 2.50,
                                           '2' = 5.00,
                                           '3' = 10.00,
                                           '4' = 20.00,
                                           '5' = 40.00,
                                           '6' = 80.00)
database$Tax_PlanB_Values <- dplyr::recode(database$Tax_PlanB,
                                           '1' = 2.50,
                                           '2' = 5.00,
                                           '3' = 10.00,
                                           '4' = 20.00,
                                           '5' = 40.00,
                                           '6' = 80.00)
database$Tax_PlanC_Values <- dplyr::recode(database$Tax_PlanC,
                                           '1' = 0)

# **********************************************************************************
#### Section X: Export ####
# **********************************************************************************


## Exporting as CSV for ease
## and using fwrite() is much faster than write.csv()
database %>% 
  fwrite(sep = ",", 
         here("Data/Pilot1/", 
              "database_Step2.csv"))

