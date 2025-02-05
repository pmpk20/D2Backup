#### D2: Sliders to factors ####
## Function: Factor analysis of the slider questions
## Author: Dr Peter King (p.king1@leeds.ac.uk)
## Last change: 30/10/2024
## Notes: 
# - Using vss() and fa.parallel()
# - adding database update


# **********************************************************************************
#### Section 0: Libraries ####
# **********************************************************************************


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
library(psych)
library(GPArotation)

# **********************************************************************************
#### Section 1: Data import and isolate ####
# **********************************************************************************


Data_Covariates <- here("Data/Main", "Data_Covariates_Spatial_Step4.csv") %>% fread() %>% data.frame()
database <- here("Data/Main", "database_Step2.csv") %>% fread() %>% data.frame()

## Subset the insects data to only have sliders responses
Sliders <- Data_Covariates[c(122:133)] %>% data.frame() 


# **********************************************************************************
#### Section 2: VSS factors ####
# **********************************************************************************



# VSS complexity 1 achieves a maximimum of 0.67  with  2  factors
vss(x = Sliders, rotate = "varimax")


Factors_VSS <- fa(r = Sliders, nfactors = 2, rotate = "none", fm = "pa") ## Re-estimate with fewer factors

## These are the factors you can cbind() to the data and include in models
Data_Covariates_WithVSSFactors <- cbind(Data_Covariates, 
                         "Sliders_VSS_PA1" = Factors_VSS$scores[, 1],
                         "Sliders_VSS_PA2" = Factors_VSS$scores[, 2]) 



# **********************************************************************************
#### Section 3: FA factors ####
# **********************************************************************************



## Parallel analysis suggests that the number of factors =  4  and the number of components =  3 
fa.parallel(x = Sliders, fm = "pa")


Factors_FA <- fa(r = Sliders, nfactors = 4, rotate = "none", fm = "pa") ## Re-estimate with fewer factors

## These are the factors you can cbind() to the data and include in models
Data_Covariates_WithAllFactors <- cbind(Data_Covariates_WithVSSFactors, 
                                     "Sliders_FA_PA1" = Factors_FA$scores[, 1],
                                     "Sliders_FA_PA2" = Factors_FA$scores[, 2],
                                     "Sliders_FA_PA3" = Factors_FA$scores[, 3],
                                     "Sliders_FA_PA4" = Factors_FA$scores[, 4]) 


# **********************************************************************************
#### Section 4: add to database ####
# **********************************************************************************


database$Sliders_VSS_PA1 <- Data_Covariates_WithAllFactors$Sliders_VSS_PA1 %>% rep(each = 9)
database$Sliders_VSS_PA2 <- Data_Covariates_WithAllFactors$Sliders_VSS_PA2 %>% rep(each = 9)
database$Sliders_FA_PA1 <- Data_Covariates_WithAllFactors$Sliders_FA_PA1 %>% rep(each = 9)
database$Sliders_FA_PA2 <- Data_Covariates_WithAllFactors$Sliders_FA_PA2 %>% rep(each = 9)
database$Sliders_FA_PA3 <- Data_Covariates_WithAllFactors$Sliders_FA_PA3 %>% rep(each = 9)
database$Sliders_FA_PA4 <- Data_Covariates_WithAllFactors$Sliders_FA_PA4 %>% rep(each = 9)


# **********************************************************************************
#### Section X: Export ####
# **********************************************************************************


## Exporting as CSV for ease
Data_Covariates_WithAllFactors %>% 
  fwrite(sep = ",", 
         here("Data/Main", 
              "Data_Covariates_Spatial_Step5.csv"))


database %>% 
  fwrite(sep = ",", 
         here("Data/Main", 
              "database_Step3.csv"))


# END OF SCRIPT **********************************************************************************