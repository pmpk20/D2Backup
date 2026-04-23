#### D2: Discounting Qs  ###############
# Script author: Peter King (p.king1@leeds.ac.uk)
# Last Edited: 31/10/2024.
# COMMENTS: Summarising discount questions
# - Updated with resampled data
# - removed redundant libraries


# **********************************************************************************
#### Section 0: Setup environment and replication information ####
# **********************************************************************************


# **********************************************************************************
#### Replication Information:

# R version 4.4.1 (2024-06-14 ucrt)
# Platform: x86_64-w64-mingw32/x64
# Running under: Windows 11 x64 (build 22631)
# Matrix products: default
# locale:
#   [1] LC_COLLATE=English_United Kingdom.utf8 
# [2] LC_CTYPE=English_United Kingdom.utf8   
# [3] LC_MONETARY=English_United Kingdom.utf8
# [4] LC_NUMERIC=C                           
# [5] LC_TIME=English_United Kingdom.utf8    
# time zone: Europe/London
# tzcode source: internal
# attached base packages:
#   [1] stats     graphics  grDevices utils     datasets  methods   base     
# other attached packages:
#   [1] RColorBrewer_1.1-3 ggdist_3.3.2       ggridges_0.5.6    
# [4] matrixStats_1.3.0  Rfast_2.1.0        RcppParallel_5.1.7
# [7] RcppZiggurat_0.1.6 Rcpp_1.0.12        data.table_1.15.4 
# [10] here_1.0.1         mded_0.1-2         apollo_0.3.3      
# [13] magrittr_2.0.3     lubridate_1.9.3    forcats_1.0.0     
# [16] stringr_1.5.1      dplyr_1.1.4        purrr_1.0.2       
# [19] readr_2.1.5        tidyr_1.3.1        tibble_3.2.1      
# [22] ggplot2_3.5.1      tidyverse_2.0.0   
# loaded via a namespace (and not attached):
#   [1] gtable_0.3.5         lattice_0.22-6       tzdb_0.4.0          
# [4] numDeriv_2016.8-1.1  vctrs_0.6.5          tools_4.4.1         
# [7] generics_0.1.3       parallel_4.4.1       sandwich_3.1-0      
# [10] fansi_1.0.6          pkgconfig_2.0.3      Matrix_1.7-0        
# [13] distributional_0.4.0 lifecycle_1.0.4      truncnorm_1.0-9     
# [16] compiler_4.4.1       maxLik_1.5-2.1       MatrixModels_0.5-3  
# [19] mcmc_0.9-8           munsell_0.5.1        mnormt_2.1.1        
# [22] SparseM_1.83         RSGHB_1.2.2          quantreg_5.98       
# [25] bgw_0.1.3            Rsolnp_1.16          pillar_1.9.0        
# [28] MASS_7.3-61          randtoolbox_2.0.4    tidyselect_1.2.1    
# [31] digest_0.6.35        mvtnorm_1.2-5        stringi_1.8.4       
# [34] splines_4.4.1        miscTools_0.6-28     rprojroot_2.0.4     
# [37] grid_4.4.1           colorspace_2.1-0     cli_3.6.3           
# [40] survival_3.7-0       utf8_1.2.4           withr_3.0.0         
# [43] scales_1.3.0         timechange_0.3.0     rngWELL_0.10-9      
# [46] zoo_1.8-12           hms_1.1.3            coda_0.19-4.1       
# [49] rlang_1.1.4          MCMCpack_1.7-0       glue_1.7.0          
# [52] rstudioapi_0.16.0    R6_2.5.1      


# **********************************************************************************
#### Libraries:
options(scipen=90)
library(tidyverse)
library(data.table)
library(here)
library(magrittr)
library(janitor)



# **********************************************************************************
#### Section 1: Import ####
# **********************************************************************************


# Using fread() from data.table as significantly faster than read.csv()
# Store data in Data subfolder so referencing it using here() package
Data <- here("Data/Main/", 
             "Data_Covariates_Step1.csv") %>% 
  fread() %>% 
  data.frame()

# **********************************************************************************
#### Section 2: Function setup ####
# **********************************************************************************
# Define the amounts for 'today' in each task
payment_today <- c(190,
                   150,
                   100,
                   50,
                   25,
                   10,
                   0)

# Define the fixed future payment in one month
payment_future <- 200

# Time delay for the future payment (1 month)
time_delay <- 12

# Modified function to handle the specific amounts for today
calculate_discount_rate <- function(payment_today, payment_future, choice, time_delay) {
  if (choice == 1) {
    # Calculate the monthly discount rate
    discount_rate_monthly <- (payment_future / payment_today)^(1 / time_delay) - 1
    
    # Convert monthly discount rate to annual discount rate
    discount_rate_annual <- ((1 + discount_rate_monthly)^12 - 1) %>% 
      multiply_by(100) %>% 
      round(2)
    
  } else if (choice == 2) {
    # If respondent always chose the future payment, assume a discount rate of 0
    discount_rate_annual <- 0
  } else {
    stop("Invalid choice. Must be 1 (today) or 2 (one month).")
  }
  
  return(discount_rate_annual)
}


# **********************************************************************************
#### Section 3: Construct data ####
# **********************************************************************************


# Create a new column for the discount rate based on the first switch to 2
Data_WithDiscounting <- Data %>%
  rowwise() %>%
  mutate(
    # Find the index of the first '2' in the QDiscounting_1 to QDiscounting_10 columns
    first_switch_task = ifelse(any(c_across(starts_with("QDiscounting_")) == 2),
                               which(c_across(starts_with("QDiscounting_")) == 2)[1],
                               NA),  # NA if no '2' is found (always chose '1')
    
    # Determine if the respondent always chose 2 (delayed payment) across all tasks
    always_chose_oneMonth = all(c_across(starts_with("QDiscounting_")) == 1),
    
    always_chose_oneYear = all(c_across(starts_with("QDiscounting_")) == 2),
    
    # Use the corresponding payment_today value to calculate the discount rate, or set 0 if no switch
    DiscountRate = case_when(
      always_chose_oneYear ~ 0,  # Always chose the delayed payment, zero discount rate
      is.na(first_switch_task) ~ 1000,  # Always chose today (no switch), extremely high discount rate
      TRUE ~ calculate_discount_rate(payment_today[first_switch_task], payment_future, 1, time_delay)  # Regular discount rate
    ),
    
    # Replace infinite values with 0, although this shouldn't happen anymore
    DiscountRate = ifelse(is.infinite(DiscountRate), 0, DiscountRate)
  ) %>%
  
  ungroup() %>%
  
  mutate(
    DiscountRate_TextCategory = case_when(
      is.na(DiscountRate) ~ NA_character_,
      DiscountRate == 0 ~ "AlwaysTomorrow",    
      DiscountRate == 1000 ~ "AlwaysToday",
      DiscountRate < 100 ~ "Normal",
      DiscountRate >= 100 ~ "High"
    ),
    
    # Scale the DiscountRate to the range [0, 1] using only finite, non-extreme values
    DiscountRate_Scaled = scales::rescale(DiscountRate, to = c(0, 1), na.rm = TRUE)
  )


# **********************************************************************************
#### Section 4: Export ####
# **********************************************************************************


## Exporting as CSV for ease
## and using fwrite() is much faster than write.csv()
Data_WithDiscounting %>% 
  fwrite(sep = ",", 
         here("Data/Main/", 
              "Data_Covariates_Step2.csv"))

