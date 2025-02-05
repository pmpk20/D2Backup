#### D2: First pilot cleaning ###############
# Function: To clean and merge the main 1651 responses 
# Author: Dr Peter King (p.king1@leeds.ac.uk)
# Last Edited: 31/10/2024
# Change/s:
# - Adding code to categorise protest responses
# - adding misc fixes from modelling
# - adding biowell groups
# - changing to resampled respondents


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
library(janitor)


# **********************************************************************************
#### Section 1: Import Data  ####
## NOTES: This is in xlsx
# **********************************************************************************


Data_Covariates <-
  here("Data/Main", "DRUID_resampling_covariates_anonymised_2024_10_31.xlsx") %>% 
  readxl::read_xlsx(sheet = "Data") %>% 
  data.frame()


Data_Timers <-
  here("Data/Main", "DRUID_resampling_timestamps_2024-10-31.xlsx") %>% 
  readxl::read_xlsx(sheet = "Data") %>% 
  data.frame()


Data_CE <-
  here("Data/Main", "DRUID_resampling_DCE_d2_test2_2024-10-31.xlsx") %>% 
  readxl::read_xlsx(sheet = "Data") %>% 
  data.frame()



# **********************************************************************************
#### Section 2A: Identify survey speed/slow ####
### Recode as fail (0) or pass (10)
# **********************************************************************************


## Recode and rename
Data_Covariates$SurveyCompletionTime <-
  Data_Covariates$DURATION %>% as.numeric()


## Arbitrary, but OECD-based, survey thresholds
Data_Covariates <- 
  Data_Covariates %>% 
  dplyr::mutate("Speeders_Survey_Threshold" = 
                  median(SurveyCompletionTime) %>% multiply_by(0.48),
                "Slowers_Survey_Threshold" = 
                  median(SurveyCompletionTime) %>% multiply_by(1.48),
                
                
                "Speeders_Survey_TestDummy" = ifelse(
                  SurveyCompletionTime <= Speeders_Survey_Threshold, 0, 1),
                
                "Slowers_Survey_TestDummy" = ifelse(
                  SurveyCompletionTime >= Slowers_Survey_Threshold, 0, 1)
  )


## Output survey speeder data
Data_Covariates %>%
  summarise(
    Threshold = first(Speeders_Survey_Threshold),
    
    N_Fail = paste0(
      sum(Speeders_Survey_TestDummy == 0),
      " (", mean(Speeders_Survey_TestDummy == 0) %>% 
        round(2) %>% 
        multiply_by(100), "%)"),
    
    N_Pass = paste0(
      sum(Speeders_Survey_TestDummy == 1),
      " (", mean(Speeders_Survey_TestDummy == 1) %>% 
        round(2) %>% 
        multiply_by(100), "%)")) %>% 
  write.csv(quote = FALSE)



# **********************************************************************************
#### Section 2B: Identify valuation speed/slow ####
# ## Recode as fail (0) or pass (10)
# **********************************************************************************


## Append and name 
Data_Covariates$Valuation_Start <- Data_Timers$PAGE_DISPLAY_13 %>% as.numeric()
Data_Covariates$Valuation_Finish <- Data_Timers$PAGE_SUBMIT_33 %>% as.numeric()
Data_Covariates$Valuation_CompletionTime <-  Data_Covariates$Valuation_Finish - Data_Covariates$Valuation_Start

Data_Covariates <- 
  Data_Covariates %>% 
  dplyr::mutate("Speeders_Valuation_Threshold" = 
                  median(Valuation_CompletionTime) %>% multiply_by(0.48),
                "Slowers_Valuation_Threshold" = 
                  median(Valuation_CompletionTime) %>% multiply_by(1.48),
                
                
                "Speeders_Valuation_TestDummy" = ifelse(
                  Valuation_CompletionTime <= Speeders_Valuation_Threshold, 0, 1),
                
                "Slowers_Valuation_TestDummy" = ifelse(
                  Valuation_CompletionTime >= Slowers_Valuation_Threshold, 0, 1)
  )


## Output survey speeder data
Data_Covariates %>%
  summarise(
    Threshold = first(Speeders_Valuation_Threshold),
    
    N_Fail = paste0(
      sum(Speeders_Valuation_TestDummy == 0),
      " (", mean(Speeders_Valuation_TestDummy == 0) %>% 
        round(2) %>% 
        multiply_by(100), "%)"),
    
    N_Pass = paste0(
      sum(Speeders_Valuation_TestDummy == 1),
      " (", mean(Speeders_Valuation_TestDummy == 1) %>% 
        round(2) %>% 
        multiply_by(100), "%)")) %>% 
  write.csv(quote = FALSE)




# **********************************************************************************
#### Sociodemographics: Recode class ####
# **********************************************************************************


## Dummy for ABC1 pre-determined
Data_Covariates$ClassDummy <-
  ifelse(
    Data_Covariates$Q6Occupation %in% c(
      6, 7, 8, 9, 10),
    1, ## ABC1
    0 ## C2DE
  )

Data_Covariates$EmployDummy_Retired <-
  ifelse(
    Data_Covariates$Q6Occupation  == 3,
    1, ## Retired
    0 ## Other
  )


Data_Covariates$EmployDummy_Employed <-
  ifelse(
    Data_Covariates$Q6Occupation > 4,
    1, ## Any other level
    0 ## Homemaker/housewife or househusband, Student/Full time education, Retired, Unemployed/on benefit
  )

# ABC1:
# "Senior management",
# "Office/clerical/administration",
# "Crafts/tradesperson/skilled worker",
# "Middle management",
# "Professional"

# **********************************************************************************
#### Sociodemographics: Recode income and age  ####
# **********************************************************************************


## Recode income using midpoints as per OECD
## And categorising agegroups
Data_Covariates <- Data_Covariates %>%
  dplyr::mutate(
    IncomeMidpoints = dplyr::case_when(
      is.na(Q7Income) ~ NA,
      Q7Income %in% "1" ~	(0.5 * 0) + (0.5 * 1000),
      Q7Income %in% "2" ~	(0.5 * 1001) + (0.5 * 1500),
      Q7Income %in% "3" ~	(0.5 * 1501) + (0.5 * 2000),
      Q7Income %in% "4" ~	(0.5 * 2001) + (0.5 * 2500),
      Q7Income %in% "5" ~	(0.5 * 2501) + (0.5 * 3000),
      Q7Income %in% "6" ~	(0.5 * 3001) + (0.5 * 3500),
      Q7Income %in% "7" ~	(0.5 * 3501) + (0.5 * 4000),
      Q7Income %in% "8" ~	(0.5 * 4001) + (0.5 * 6000),
      Q7Income %in% "9" ~ 6001 , # "£6,001 or more" ~ 
      Q7Income %in% "10" ~ NA # "Prefer not to say." ~
    ),
    
    Income_MissingDummy  = dplyr::case_when( ## missing data or "prefer not to say"
      is.na(Q7Income) ~ 1,
      Q7Income == 10 ~ 1,
      Q7Income < 10 ~	0,
    ),
    
    AgeGroup = dplyr::case_when(
      is.na(Q1Age) ~ NA,
      Q1Age < 30 ~	"18-29",
      Q1Age <= 44 ~	"30-44",
      Q1Age <= 60 ~	"45-60",
      Q1Age > 60 ~	"60+"),
    
    Dummy_HighEducation = dplyr::case_when(
      is.na(Q48_Education ) ~ NA,
      Q48_Education  < 5 ~	0,
      Q48_Education < 7 ~	1,
      Q48_Education == 7 ~	0),
  )


Data_Covariates$Age_1829Dummy <-
  ifelse(Data_Covariates$AgeGroup %in% c("18-29"),
         1,
         0) %>% as.numeric()

Data_Covariates$Age_3044Dummy <-
  ifelse(Data_Covariates$AgeGroup %in% c("30-44"),
         1,
         0) %>% as.numeric()

Data_Covariates$Age_4560Dummy <-
  ifelse(Data_Covariates$AgeGroup %in% c("45-60"),
         1,
         0) %>% as.numeric()

Data_Covariates$Age_60Dummy <-
  ifelse(Data_Covariates$AgeGroup %in% c("60+"),
         1,
         0) %>% as.numeric()


Data_Covariates$AgeGroup_Numeric <- dplyr::recode(Data_Covariates$AgeGroup,
                                                  '18-29' = 23.50,
                                                  '30-44' = 37,
                                                  '45-60' = 52.5,
                                                  '60+' = 60)     

# **********************************************************************************
#### Sociodemographics: Missing income ####
## So here I am using the OECD missing income prediction approach
## it is imperfect
# **********************************************************************************


Model_PredictMissingIncome <- lm(formula = log(Q7Income) ~
                                   AgeGroup +
                                   Dummy_HighEducation +
                                   EmployDummy_Retired +
                                   EmployDummy_Employed,
                                 data = Data_Covariates)


Model_PredictMissingIncome_Predictions <-  predict(Model_PredictMissingIncome, 
                                                   newdata = Data_Covariates[Data_Covariates$Income_MissingDummy == 1, ])

Data_Covariates$IncomeMidpoints_PlusMissing <- ifelse(Data_Covariates$Income_MissingDummy == 1, ## If missing
                                                      Model_PredictMissingIncome_Predictions %>% exp(), ## Add raw predicted income
                                                      Data_Covariates$IncomeMidpoints) ## if not use reported income



## 0 if below or equal sample median, 1 if above
Data_Covariates$Income_Dummy <-
  ifelse(
    Data_Covariates$IncomeMidpoints_PlusMissing <= median(Data_Covariates$IncomeMidpoints_PlusMissing),
    0,
    1
  )





# **********************************************************************************
#### Sociodemographics: Summarise ####
# **********************************************************************************


## 1 for any white, 0 for all else
Data_Covariates$Q5Ethnicity_Dummy <- ifelse(Data_Covariates$Q5Ethnicity  %in% c(1, 2, 3, 4), 1, 0)


# List of the columns (questions) you want to analyze
SD_Questions <- c("AgeGroup_Numeric",
                  "Q2Gender",                         
                  "Q3Country",                        
                  "Q4Urban",                          
                  "Q5Ethnicity",
                  "Q5Ethnicity_Dummy",
                  "Q6Occupation",
                  "ClassDummy",
                  "Q7Income")

# "Q1Age",                            
# "Q8Postcode",            


# Pivot longer to handle multiple questions at once
SD_Output <- Data_Covariates %>%
  pivot_longer(cols = all_of(SD_Questions), 
               names_to = "Question", 
               values_to = "Response") %>%
  group_by(Question, Response) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  group_by(Question) %>%
  dplyr::mutate(
    Total = sum(n),
    Percentage = n / Total,
    Result = sprintf("%d (%s)", n, scales::percent(Percentage, accuracy = 0.1))
  ) %>%
  dplyr::select(Question, Response, Result) 


SD_Output %>% write.csv(quote = FALSE)



# **********************************************************************************
#### Question 11 ####
# **********************************************************************************



Q11_A <- Data_Covariates %>%
  dplyr::count(Q11OpinionOnInsectPopulations_1) %>%
  dplyr::mutate(
    Question = "I believe that insect populations have declined in Great Britain",
    Total = sum(n),
    Percentage = n / Total,
    Result = sprintf("%d (%s)", n, scales::percent(Percentage, accuracy = 0.1))
  ) %>%
  dplyr::select(-n, -Total, -Percentage) 


Q11_B <- Data_Covariates %>%
  dplyr::count(Q11OpinionOnInsectPopulations_2) %>%
  dplyr::mutate(
    Question = "I believe that insect populations will decline in the future in Great Britain",
    Total = sum(n),
    Percentage = n / Total,
    Result = sprintf("%d (%s)", n, scales::percent(Percentage, accuracy = 0.1))
  ) %>%
  dplyr::select(-n, -Total, -Percentage)


bind_rows(
  Q11_A,
  Q11_B) %>% 
  write.csv(quote = FALSE)




# **********************************************************************************
#### Question 12 ####
# **********************************************************************************



Data_Covariates <- Data_Covariates %>%
  dplyr::mutate(
    Q12InsectIDQuiz_DV_1_Dummy = ifelse(str_detect(Q12InsectIDQuiz_DV_1, ": correct"), 1, 0),
    Q12InsectIDQuiz_DV_2_Dummy = ifelse(str_detect(Q12InsectIDQuiz_DV_2, ": correct"), 1, 0),
    Q12InsectIDQuiz_DV_3_Dummy = ifelse(str_detect(Q12InsectIDQuiz_DV_3, ": correct"), 1, 0),
    Q12InsectIDQuiz_DV_4_Dummy = ifelse(str_detect(Q12InsectIDQuiz_DV_4, ": correct"), 1, 0),
    Q12InsectIDQuiz_DV_5_Dummy = ifelse(str_detect(Q12InsectIDQuiz_DV_5, ": correct"), 1, 0),
    Q12InsectIDQuiz_DV_6_Dummy = ifelse(str_detect(Q12InsectIDQuiz_DV_6, ": correct"), 1, 0),
    Q12InsectIDQuiz_Score = 
      (Q12InsectIDQuiz_DV_1_Dummy +
         Q12InsectIDQuiz_DV_2_Dummy +
         Q12InsectIDQuiz_DV_3_Dummy +
         Q12InsectIDQuiz_DV_4_Dummy +
         Q12InsectIDQuiz_DV_5_Dummy +
         Q12InsectIDQuiz_DV_6_Dummy),
    Q12InsectIDQuiz_DV_6_ScoreScaled = 
      (100/6*Q12InsectIDQuiz_Score) %>% 
      round(2)
  )


InsectQuizScore <- Data_Covariates %>%
  dplyr::count(Q12InsectIDQuiz_Score) %>%
  dplyr::mutate(
    Total = sum(n),
    Percentage = n / Total,
    Result = sprintf("%d (%s)", n, scales::percent(Percentage, accuracy = 0.1))
  ) %>%
  dplyr::select(Result) 


## Watch out the row-numbers are 1 higher than the count
InsectQuizScore %>% 
  write.csv(quote = FALSE)




# **********************************************************************************
#### Question 12: Followup ####
# **********************************************************************************



Q12_1 <- Data_Covariates %>%
  dplyr::count(Q12InsectIDQuizFollowUp_1) %>%
  dplyr::mutate(
    Question = "I did not know that the same type of insect could look so different",
    Total = sum(n),
    Percentage = n / Total,
    Result = sprintf("%d (%s)", n, scales::percent(Percentage, accuracy = 0.1))
  ) %>%
  dplyr::select(-n, -Total, -Percentage) 


Q12_2 <- Data_Covariates %>%
  dplyr::count(Q12InsectIDQuizFollowUp_2) %>%
  dplyr::mutate(
    Question = "I have seen most of the types of insects pictured before",
    Total = sum(n),
    Percentage = n / Total,
    Result = sprintf("%d (%s)", n, scales::percent(Percentage, accuracy = 0.1))
  ) %>%
  dplyr::select(-n, -Total, -Percentage)



bind_rows(
  Q12_1,
  Q12_2) %>% 
  write.csv(quote = FALSE)





# **********************************************************************************
#### CE Intro Checks ####
# **********************************************************************************


## As an attention check these were reverse in the survey
## So I'm reversing them here again back to agree (1) -> disagree (5)
# Data_Covariates$CE_TasksConsequentiality_1_Scaled <- (Data_Covariates$CE_TasksConsequentiality_1)
# Data_Covariates$CE_TasksConsequentiality_2_Scaled <- (Data_Covariates$CE_TasksConsequentiality_2)

# List of the columns (questions) you want to analyze
CE_Check_Questions <- c("CE_Task_Insect_Check_1", 
                        "CE_Task_Plan_Check_1",
                        "CE_Task_A1_Check_1",
                        "CE_Task_A2_Check_1",
                        "CE_Task_A3_Check_1",
                        "CE_Task_A4_Check_1",
                        "CE_Task_A4_Check_1",
                        "CE_TasksConsequentiality_1",
                        "CE_TasksConsequentiality_2")



# Pivot longer to handle multiple questions at once
CE_Check_Output <- Data_Covariates %>%
  pivot_longer(cols = all_of(CE_Check_Questions), 
               names_to = "Question", 
               values_to = "Response") %>%
  group_by(Question, Response) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  group_by(Question) %>%
  dplyr::mutate(
    Total = sum(n),
    Percentage = n / Total,
    Result = sprintf("%d (%s)", n, scales::percent(Percentage, accuracy = 0.1))
  ) %>%
  dplyr::select(Question, Response, Result) 


CE_Check_Output %>% write.csv(quote = FALSE)




# **********************************************************************************
#### Bid vector ####
# **********************************************************************************

# 
# ## merge
# Data_Covariates$QX_BidVector_All <- dplyr::coalesce(
#   Data_Covariates$QX_BidVector_Bees,
#   Data_Covariates$QX_BidVector_Beetles,
#   Data_Covariates$QX_BidVector_Wasps
# )
# 
# 
# 
# ## Set this summariser up 
# summary_function <- function(data, column) {
#   Data_Covariates %>%
#     summarise(across({{column}}, 
#                      list(Obs = ~round(sum(.x > -1, na.rm = TRUE), 3),
#                           Median = ~round(median(.x, na.rm = TRUE), 3), 
#                           Mean = ~round(mean(.x, na.rm = TRUE), 3), 
#                           "1st" = ~round(quantile(.x, c(0.25), na.rm = TRUE), 3),
#                           "3rd" = ~round(quantile(.x, c(0.75), na.rm = TRUE), 3),
#                           Min = ~round(min(.x, na.rm = TRUE), 3),
#                           Max = ~round(max(.x, na.rm = TRUE), 3)),
#                      .names = "{.fn}"), 
#               .groups = "drop")
# }
# 
# 
# 
# BidVector_Matrix <- rbind(
#   "Bees" = summary_function(data = Data_Covariates, column = QX_BidVector_Bees),
#   "Beetles" = summary_function(data = Data_Covariates, column = QX_BidVector_Beetles),
#   "Wasps" = summary_function(data = Data_Covariates, column = QX_BidVector_Wasps),
#   "All" = summary_function(data = Data_Covariates, column = QX_BidVector_All)
# )
# 
# 
# BidVector_Matrix %>% write.csv(quote = FALSE)
# 
# 
# 
# Data_Covariates$QX_BidVector_Bees %>% table() %>% write.csv(quote = FALSE)
# Data_Covariates$QX_BidVector_Beetles %>% table() %>% write.csv(quote = FALSE)
# Data_Covariates$QX_BidVector_Wasps %>% table() %>% write.csv(quote = FALSE)
# 
# 



# **********************************************************************************
#### CE debrief ####
# **********************************************************************************


## This is outputted for the reports
rbind(
  "CE debrief Certain" = Data_Covariates$CE_Debrief_Certain %>% summary(),
  "CE debrief Confident" = Data_Covariates$CE_Debrief_Confident %>% summary()
)



# **********************************************************************************
#### CE ANA ####
# **********************************************************************************


## Output survey speeder data
CE_ANA_Table <- Data_Covariates %>%
  summarise(
    Considered_Insect  = paste0(
      sum(CE_ANA_1  == 1),
      " (", mean(CE_ANA_1  == 1) %>% 
        round(2) %>% 
        multiply_by(100), "%)"),
    
    Considered_Encounter  = paste0(
      sum(CE_ANA_2  == 1),
      " (", mean(CE_ANA_2  == 1) %>% 
        round(2) %>% 
        multiply_by(100), "%)"),
    
    Considered_Existence  = paste0(
      sum(CE_ANA_3  == 1),
      " (", mean(CE_ANA_3  == 1) %>% 
        round(2) %>% 
        multiply_by(100), "%)"),
    
    Considered_Bequest  = paste0(
      sum(CE_ANA_4  == 1),
      " (", mean(CE_ANA_4  == 1) %>% 
        round(2) %>% 
        multiply_by(100), "%)"),
    
    Considered_Tax  = paste0(
      sum(CE_ANA_5  == 1),
      " (", mean(CE_ANA_5  == 1) %>% 
        round(2) %>% 
        multiply_by(100), "%)")) 


CE_ANA_Table %>% write.csv(quote = FALSE)



Data_Covariates$CE_ANA_None <-
  ifelse(
    Data_Covariates$CE_ANA_1 == 2 &
      Data_Covariates$CE_ANA_2 == 2 &
      Data_Covariates$CE_ANA_3 == 2 &
      Data_Covariates$CE_ANA_4 == 2 &
      Data_Covariates$CE_ANA_5 == 2,
    1,
    0
  )

Data_Covariates$CE_ANA_All <-
  ifelse(
    Data_Covariates$CE_ANA_1 == 1 &
      Data_Covariates$CE_ANA_2 == 1 &
      Data_Covariates$CE_ANA_3 == 1 &
      Data_Covariates$CE_ANA_4 == 1 &
      Data_Covariates$CE_ANA_5 == 1,
    1,
    0
  )



# **********************************************************************************
#### Sliders on perceptions ####
# **********************************************************************************

rbind(
  "Beetles_Encounter" = Data_Covariates$Sliding_Beetles_Encounter %>% summary(),
  "Beetles_Ecology" = Data_Covariates$Sliding_Beetles_Ecology %>% summary(),
  "Beetles_Conserved" = Data_Covariates$Sliding_Beetles_Conserved %>% summary(),
  "Beetles_Important" = Data_Covariates$Sliding_Beetles_Important %>% summary(),
  
  "Bees_Encounter" = Data_Covariates$Sliding_Bees_Encounter %>% summary(),
  "Bees_Ecology" = Data_Covariates$Sliding_Bees_Ecology %>% summary(),
  "Bees_Conserved" = Data_Covariates$Sliding_Bees_Conserved %>% summary(),
  "Bees_Important" = Data_Covariates$Sliding_Bees_Important %>% summary(),
  
  
  "Wasps_Encounter" = Data_Covariates$Sliding_Wasps_Encounter %>% summary(),
  "Wasps_Ecology" = Data_Covariates$Sliding_Wasps_Ecology %>% summary(),
  "Wasps_Conserved" = Data_Covariates$Sliding_Wasps_Conserved %>% summary(),
  "Wasps_Important" = Data_Covariates$Sliding_Wasps_Important %>% summary()
) %>% write.csv(quote = FALSE)


# I encounter frequently
# I have a good understanding of the ecological roles that beetles play
# should be conserved for future generations  
# important even if no-one encounters them




# **********************************************************************************
#### Visit frequency ####
# **********************************************************************************

rbind(
  "Child" = Data_Covariates$Visit_Child %>% summary(),
  "Teen" = Data_Covariates$Visit_Teen %>% summary(),
  "Adult" = Data_Covariates$Visit_Adult %>% summary()
) %>% write.csv(quote = FALSE)




# List of the columns (questions) you want to analyze
Visit_Questions <- c("Q41_FreqOfVisitGreenSpace_1", 
                     "Q41_FreqOfVisitBlueSpace_1",
                     "Q41_WalkTimeToGreenSpace",
                     "Q41_WalkTimeToBlueSpace")



# Pivot longer to handle multiple questions at once
Visit_Output <- Data_Covariates %>%
  pivot_longer(cols = all_of(Visit_Questions), 
               names_to = "Question", 
               values_to = "Response") %>%
  group_by(Question, Response) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  group_by(Question) %>%
  dplyr::mutate(
    Total = sum(n),
    Percentage = n / Total,
    Result = sprintf("%d (%s)", n, scales::percent(Percentage, accuracy = 0.1))
  ) %>%
  dplyr::select(Question, Response, Result) 



Visit_Output %>% write.csv(quote = FALSE)


# **********************************************************************************
#### Debriefing ####
# **********************************************************************************


# List of the columns (questions) you want to analyze
Misc1_Questions <- c("Q42_Charity", 
                     "Q43_Citizen",
                     "Q44_Sight",
                     "Q45_Garden",
                     "Q46_Farm",
                     "Q47_Fish")



# Pivot longer to handle multiple questions at once
Misc1_Output <- Data_Covariates %>%
  pivot_longer(cols = all_of(Misc1_Questions), 
               names_to = "Question", 
               values_to = "Response") %>%
  group_by(Question, Response) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  group_by(Question) %>%
  dplyr::mutate(
    Total = sum(n),
    Percentage = n / Total,
    Result = sprintf("%d (%s)", n, scales::percent(Percentage, accuracy = 0.1))
  ) %>%
  dplyr::select(Question, Response, Result) 


Misc1_Output %>% 
  pivot_wider(names_from = Response, values_from = Result) %>% 
  write.csv(quote = FALSE) %>% write.csv(quote = FALSE)


# **********************************************************************************
#### Education ####
# **********************************************************************************



Data_Covariates %>%
  pivot_longer(cols = Q48_Education,
               names_to = "Question",
               values_to = "Response") %>%
  group_by(Question, Response) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  group_by(Question) %>%
  dplyr::mutate(
    Total = sum(n),
    Percentage = n / Total,
    Result = sprintf("%d (%s)", n, scales::percent(Percentage, accuracy = 0.1))
  ) %>%
  dplyr::select(Question, Response, Result) %>% 
  write.csv(quote = FALSE)


# **********************************************************************************
#### Discounting ####
# **********************************************************************************


# List of the columns (questions) you want to analyze
Discounting_Questions <- c("QDiscounting_1", 
                           "QDiscounting_2",
                           "QDiscounting_3",
                           "QDiscounting_4",
                           "QDiscounting_5",
                           "QDiscounting_6",
                           "QDiscounting_7")


# Pivot longer to handle multiple questions at once
Discounting_Output <- Data_Covariates %>%
  pivot_longer(cols = all_of(Discounting_Questions), 
               names_to = "Question", 
               values_to = "Response") %>%
  group_by(Question, Response) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  group_by(Question) %>%
  dplyr::mutate(
    Total = sum(n),
    Percentage = n / Total,
    Result = sprintf("%d (%s)", n, scales::percent(Percentage, accuracy = 0.1))
  ) %>%
  dplyr::select(Question, Response, Result) %>% 
  pivot_wider(names_from = Response, values_from = Result)

Discounting_Output %>% write.csv(quote = FALSE)


# **********************************************************************************
#### Text responses at the end ####
# **********************************************************************************


table(Data_Covariates$FurtherComments) %>% write.csv(quote = FALSE)

FurtherComments_tidied <- Data_Covariates %>%
  dplyr::select(FurtherComments) %>%
  pivot_longer(cols = everything(), values_to = "word") %>%
  mutate(word = tolower(word)) %>%
  mutate(
    word = dplyr::recode(
      word,
      "no" = "none",
      "no comments" = "none",
      "no comment" = "none",
      "no thanks" = "none",
      "no additional comments" = "none",
      "no further comments" = "none",
      "nothing" = "none",
      "nothing to add" = "none",
      "nothing to add." = "none",
      "non" = "none",
      "n/a" = "none",
      "na" = "none",
      "nil" = "none",
      "no issues" = "none",
      "thank you" = "thanks",
      "i have no additional comments" = "none",
      "very interesting" = "very interesting survey",
      "good" = "good survey",
      "a good survey" = "good survey",
      "great" = "great survey",
      "the survey was great" = "great survey",
      "interesting" = "interesting survey",
      "an interesting survey" = "interesting survey",
      "a very interesting surveyy" = "very interesting survey"
    )
  )

FurtherComments_tidied %>%
  group_by(word) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) 


# **********************************************************************************
#### Section Y: Protests ####
# **********************************************************************************


Data_Covariates$Protest_Labelled <-
  dplyr::recode(
    Data_Covariates$Q29CEProtest_Statements,
    '1' = "I cannot afford to pay anything for this plan.",
    '2' = "I am not willing to pay anything for this plan.",
    '3' = "The proposed plan is not worth the cost.",
    '4' = "I do not believe that we should have to pay for this plan, it is others’ responsibility.",
    '5' = "I would prefer there to be no change in the plan.",
    '6' = "I do not like these insects.",
    '7' = "I did not understand the tasks.",
    '8' = "I did not believe that making a choice would make a difference.",
    '9' = "I do not believe the plan would work.",
    '10' = "Other."
  )


Data_Covariates$Protest_True <- ifelse(!Data_Covariates$Q29CEProtest_Statements %in% c(1, 3), 
       1,
       0)


# **********************************************************************************
#### Biowell ####
# **********************************************************************************



## 100 - scores to centre correctly
BioWell <- (100 - Data_Covariates[c(74:118)])


BioWellScores <- bind_cols(
  "Biowell_Beetle1_Mean" = BioWell %>% dplyr::select(starts_with("Biowell_Beetle1")) %>% rowMeans(),
  "Biowell_Beetle2_Mean" = BioWell %>% dplyr::select(starts_with("Biowell_Beetle2")) %>% rowMeans(),
  "Biowell_Beetle3_Mean" = BioWell %>% dplyr::select(starts_with("Biowell_Beetle3")) %>% rowMeans(),
  "Biowell_Bee1_Mean" = BioWell %>% dplyr::select(starts_with("Biowell_Bee1")) %>% rowMeans(),
  "Biowell_Bee2_Mean" = BioWell %>% dplyr::select(starts_with("Biowell_Bee2")) %>% rowMeans(),
  "Biowell_Bee3_Mean" = BioWell %>% dplyr::select(starts_with("Biowell_Bee3")) %>% rowMeans(),
  "Biowell_Wasp1_Mean" = BioWell %>% dplyr::select(starts_with("Biowell_Wasp1")) %>% rowMeans(),
  "Biowell_Wasp2_Mean" = BioWell %>% dplyr::select(starts_with("Biowell_Wasp2")) %>% rowMeans(),
  "Biowell_Wasp3_Mean" = BioWell %>% dplyr::select(starts_with("Biowell_Wasp3")) %>% rowMeans()
)


## Isolate row and respondent scores here:
Overall <- BioWellScores %>% data.frame() %>% rowSums() %>% divide_by(9)



# BioWell_Everything <-cbind(BioWell, BioWellScores, "Overall" = rowSums(BioWellScores)/17)
Data_Covariates <- bind_cols(Data_Covariates,
                        BioWellScores,
                        "Overall" = Overall)


# **********************************************************************************
#### Section X: Misc ####
## All misc fixes from other scripts stored here
# **********************************************************************************


## RID from external
# Excludes <- c(830, 856, 870, 1311, 1611, 2180, 2217)

## Drop here
# Data_Covariates$ExcludedRID <- ifelse(Data_Covariates$RID %in% Excludes, 1, 0)


Data_Covariates$Age_Dummy <- ifelse(Data_Covariates$Q1Age < median(Data_Covariates$Q1Age), 0, 1)


Data_Covariates$Female_dummy <- ifelse(Data_Covariates$Q2Gender == 1, 1, 0)

Data_Covariates$Q3Country_Wales <- ifelse(Data_Covariates$Q3Country == 3, 1, 0)
Data_Covariates$Q3Country_Scotland <- ifelse(Data_Covariates$Q3Country == 2, 1, 0)

Data_Covariates$Q5Ethnicity_Dummy <- ifelse(Data_Covariates$Q5Ethnicity  %in% c(1, 2, 3, 4), 1, 0)


Data_Covariates$Q4Urban <- (Data_Covariates$Q4Urban - 1) 
Data_Covariates$Q42_Charity <- (Data_Covariates$Q42_Charity - 1) 
Data_Covariates$Q43_Citizen <- (Data_Covariates$Q43_Citizen - 1) 
Data_Covariates$Q44_Sight <- (Data_Covariates$Q44_Sight - 1) 
Data_Covariates$Q45_Garden <- (Data_Covariates$Q45_Garden - 1) 
Data_Covariates$Q46_Farm <- (Data_Covariates$Q46_Farm - 1) 
Data_Covariates$Q47_Fish <- (Data_Covariates$Q47_Fish - 1) 



## Recode so 1 equals speeders
Data_Covariates$Dummy_Speeders <- (1 - Data_Covariates$Speeders_Valuation_TestDummy)

## Dummy for never visiting green spaces
Data_Covariates$Visit_Green_Never <- ifelse(Data_Covariates$Q41_FreqOfVisitGreenSpace_1 == 1, 1, 0 )

## Dummy for never visiting blue spaces
Data_Covariates$Visit_Blue_Never <- ifelse(Data_Covariates$Q41_FreqOfVisitBlueSpace_1 == 1, 1, 0 )

## Dummy for always visiting green spaces
Data_Covariates$Visit_Green_Always <- ifelse(Data_Covariates$Q41_FreqOfVisitGreenSpace_1 == 7, 1, 0 )

## Dummy for always visiting blue spaces
Data_Covariates$Visit_Blue_Always <- ifelse(Data_Covariates$Q41_FreqOfVisitBlueSpace_1 == 7, 1, 0 )



# **********************************************************************************
#### Section Y: Export ####
# **********************************************************************************


## Exporting as CSV for ease
## and using fwrite() is much faster than write.csv()
Data_Covariates %>% 
  fwrite(sep = ",", 
         here("Data/Main", 
              "Data_Covariates_Step1.csv"))



