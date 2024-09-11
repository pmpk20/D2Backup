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


Data_Covariates <-
  here("Data/Pilot1/", "D2_Pilot1_Covariates.xlsx") %>% 
  readxl::read_xlsx(sheet = "Data") %>% 
  data.frame()


Data_Timers <-
  here("Data/Pilot1/", "DRUID_pilot_1_timestamps_2024-09-10.xlsx") %>% 
  readxl::read_xlsx(sheet = "Data") %>% 
  data.frame()


Data_CE <-
  here("Data/Pilot1/", "DRUID_pilot_1_DCE_d2_test2_2024-09-10.xlsx") %>% 
  readxl::read_xlsx(sheet = "Data") %>% 
  data.frame()



# **********************************************************************************
#### Section 2A: Identify survey speed/slow ####
### Recode as fail (0) or pass (10)
# **********************************************************************************


Data_Covariates$SurveyCompletionTime <- Data_Covariates$DURATION %>% as.numeric()


Data_Covariates <- 
  Data_Covariates %>% 
  mutate("Speeders_Survey_Threshold" = 
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


Data_Covariates$Valuation_Start <- Data_Timers$PAGE_DISPLAY_13 %>% as.numeric()
Data_Covariates$Valuation_Finish <- Data_Timers$PAGE_SUBMIT_33 %>% as.numeric()
Data_Covariates$Valuation_CompletionTime <-  Data_Covariates$Valuation_Finish - Data_Covariates$Valuation_Start

Data_Covariates <- 
  Data_Covariates %>% 
  mutate("Speeders_Valuation_Threshold" = 
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
#### Section 3A: Insect qual data Q9  ####
# **********************************************************************************



QualData_Q9InsectWords <- cbind(
  Data_Covariates$Q9Insect_Word1,
  Data_Covariates$Q9Insect_Word2,
  Data_Covariates$Q9Insect_Word3
)

# Convert all entries to lowercase
QualData_Q9InsectWords_clean <- tolower(QualData_Q9InsectWords)

# Correct common misspellings or variations using gsub
QualData_Q9InsectWords_clean <- gsub("bettle", "beetle", QualData_Q9InsectWords_clean)
QualData_Q9InsectWords_clean <- gsub("moskit", "mosquito", QualData_Q9InsectWords_clean)
QualData_Q9InsectWords_clean <- gsub("gly", "fly", QualData_Q9InsectWords_clean)
QualData_Q9InsectWords_clean <- gsub("annpoying", "annoying", QualData_Q9InsectWords_clean)
QualData_Q9InsectWords_clean <- gsub("ants", "ant", QualData_Q9InsectWords_clean)
QualData_Q9InsectWords_clean <- gsub("bees", "bee", QualData_Q9InsectWords_clean)
QualData_Q9InsectWords_clean <- gsub("bugs", "bug", QualData_Q9InsectWords_clean)
QualData_Q9InsectWords_clean <- gsub("butterflies", "butterfly", QualData_Q9InsectWords_clean)
QualData_Q9InsectWords_clean <- gsub("spiders", "spider", QualData_Q9InsectWords_clean)
QualData_Q9InsectWords_clean <- gsub("wasps", "wasp", QualData_Q9InsectWords_clean)



# Create a frequency table of the cleaned data
QualData_Q9InsectWords_clean_Freq <- table(QualData_Q9InsectWords_clean)

# Sort the table by frequency (descending order)
QualData_Q9InsectWords_clean_Freq_Sorted <- sort(QualData_Q9InsectWords_clean_Freq, 
                                                 decreasing = TRUE)

# Print the table
QualData_Q9InsectWords_clean_Freq_Sorted %>% write.csv(quote = FALSE)





# **********************************************************************************
#### Section 3A: Insect qual data Q9  ####
# **********************************************************************************


QualData_Q10InsectWords <- cbind(
  Data_Covariates$Q10Insect_Species1,
  Data_Covariates$Q10Insect_Species2,
  Data_Covariates$Q10Insect_Species3
)

QualData_Q10InsectWords




# Convert all entries to lowercase
QualData_Q10InsectWords_clean <- tolower(QualData_Q10InsectWords)

# Correct common misspellings or variations using gsub
QualData_Q10InsectWords_clean <- gsub("ants", "ant", QualData_Q10InsectWords_clean)
QualData_Q10InsectWords_clean <- gsub("aunts", "ant", QualData_Q10InsectWords_clean)
QualData_Q10InsectWords_clean <- gsub("bees", "bee", QualData_Q10InsectWords_clean)
QualData_Q10InsectWords_clean <- gsub("butterflie", "butterfly", QualData_Q10InsectWords_clean)
QualData_Q10InsectWords_clean <- gsub("ear wig", "earwig", QualData_Q10InsectWords_clean)
QualData_Q10InsectWords_clean <- gsub("lady bird", "ladybird", QualData_Q10InsectWords_clean)
QualData_Q10InsectWords_clean <- gsub("lady bug", "ladybird", QualData_Q10InsectWords_clean)
QualData_Q10InsectWords_clean <- gsub("mosca", "mosquito", QualData_Q10InsectWords_clean)
QualData_Q10InsectWords_clean <- gsub("moskit", "mosquito", QualData_Q10InsectWords_clean)
QualData_Q10InsectWords_clean <- gsub("wasps", "wasp", QualData_Q10InsectWords_clean)
QualData_Q10InsectWords_clean <- gsub("worms", "worm", QualData_Q10InsectWords_clean)



# Create a frequency table of the cleaned data
QualData_Q10InsectWords_clean_Freq <- table(QualData_Q10InsectWords_clean)

# Sort the table by frequency (descending order)
QualData_Q10InsectWords_clean_Freq_Sorted <- sort(QualData_Q10InsectWords_clean_Freq, 
                                                 decreasing = TRUE)

# Print the table
QualData_Q10InsectWords_clean_Freq_Sorted %>% write.csv(quote = FALSE)



# **********************************************************************************
#### Sociodemographics: Recode class ####
# **********************************************************************************

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


# **********************************************************************************
#### Sociodemographics: Summarise ####
# **********************************************************************************


# List of the columns (questions) you want to analyze
SD_Questions <- c("AgeGroup_Numeric",
                  "Q2Gender",                         
                  "Q3Country",                        
                  "Q4Urban",                          
                  "Q5Ethnicity",                        
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
  mutate(
    Total = sum(n),
    Percentage = n / Total,
    Result = sprintf("%d (%s)", n, scales::percent(Percentage, accuracy = 0.1))
  ) %>%
  select(Question, Response, Result) 

SD_Output %>% write.csv(quote = FALSE)

# **********************************************************************************
#### Question 11 ####
# **********************************************************************************



Q11_A <- Data_Covariates %>%
  count(Q11OpinionOnInsectPopulations_1) %>%
  mutate(
    Question = "I believe that insect populations have declined in Great Britain",
    Total = sum(n),
    Percentage = n / Total,
    Result = sprintf("%d (%s)", n, scales::percent(Percentage, accuracy = 0.1))
  ) %>%
  select(-n, -Total, -Percentage) 


Q11_B <- Data_Covariates %>%
  count(Q11OpinionOnInsectPopulations_2) %>%
  mutate(
    Question = "I believe that insect populations will decline in the future in Great Britain",
    Total = sum(n),
    Percentage = n / Total,
    Result = sprintf("%d (%s)", n, scales::percent(Percentage, accuracy = 0.1))
  ) %>%
  select(-n, -Total, -Percentage)


bind_rows(
  Q11_A,
  Q11_B) %>% 
  write.csv(quote = FALSE)




# **********************************************************************************
#### Question 12 ####
# **********************************************************************************



Data_Covariates <- Data_Covariates %>%
  mutate(
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
  count(Q12InsectIDQuiz_Score) %>%
  mutate(
    Total = sum(n),
    Percentage = n / Total,
    Result = sprintf("%d (%s)", n, scales::percent(Percentage, accuracy = 0.1))
  ) %>%
  dplyr::select(Result) 

InsectQuizScore %>% 
  write.csv(quote = FALSE)




# **********************************************************************************
#### Question 12: Followup ####
# **********************************************************************************



Q12_1 <- Data_Covariates %>%
  count(Q12InsectIDQuizFollowUp_1) %>%
  mutate(
    Question = "I did not know that the same type of insect could look so different",
    Total = sum(n),
    Percentage = n / Total,
    Result = sprintf("%d (%s)", n, scales::percent(Percentage, accuracy = 0.1))
  ) %>%
  select(-n, -Total, -Percentage) 


Q12_2 <- Data_Covariates %>%
  count(Q12InsectIDQuizFollowUp_2) %>%
  mutate(
    Question = "I have seen most of the types of insects pictured before",
    Total = sum(n),
    Percentage = n / Total,
    Result = sprintf("%d (%s)", n, scales::percent(Percentage, accuracy = 0.1))
  ) %>%
  select(-n, -Total, -Percentage)



bind_rows(
  Q12_1,
  Q12_2) %>% 
  write.csv(quote = FALSE)





# **********************************************************************************
#### CE Intro Checks ####
# **********************************************************************************


## As an attention check these were reverse in the survey
## So I'm reversing them here again back to agree (1) -> disagree (5)
Data_Covariates$CE_TasksConsequentiality_1_Scaled <- (5 - Data_Covariates$CE_TasksConsequentiality_1)
Data_Covariates$CE_TasksConsequentiality_2_Scaled <- (5 - Data_Covariates$CE_TasksConsequentiality_2)

# List of the columns (questions) you want to analyze
CE_Check_Questions <- c("CE_Task_Insect_Check_1", 
               "CE_Task_Plan_Check_1",
               "CE_Task_A1_Check_1",
               "CE_Task_A2_Check_1",
               "CE_Task_A3_Check_1",
               "CE_Task_A4_Check_1",
               "CE_Task_A4_Check_1",
               "CE_TasksConsequentiality_1_Scaled",
               "CE_TasksConsequentiality_2_Scaled")



# Pivot longer to handle multiple questions at once
CE_Check_Output <- Data_Covariates %>%
  pivot_longer(cols = all_of(CE_Check_Questions), 
               names_to = "Question", 
               values_to = "Response") %>%
  group_by(Question, Response) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  group_by(Question) %>%
  mutate(
    Total = sum(n),
    Percentage = n / Total,
    Result = sprintf("%d (%s)", n, scales::percent(Percentage, accuracy = 0.1))
  ) %>%
  select(Question, Response, Result) 


CE_Check_Output %>% write.csv(quote = FALSE)




# **********************************************************************************
#### Bid vector ####
# **********************************************************************************


## merge
Data_Covariates$QX_BidVector_All <- dplyr::coalesce(
  Data_Covariates$QX_BidVector_Bees,
  Data_Covariates$QX_BidVector_Beetles,
  Data_Covariates$QX_BidVector_Wasps
)



## Set this summariser up 
summary_function <- function(data, column) {
  Data_Covariates %>%
    summarise(across({{column}}, 
                     list(Obs = ~round(sum(.x > -1, na.rm = TRUE), 3),
                          Median = ~round(median(.x, na.rm = TRUE), 3), 
                          Mean = ~round(mean(.x, na.rm = TRUE), 3), 
                          "1st" = ~round(quantile(.x, c(0.25), na.rm = TRUE), 3),
                          "3rd" = ~round(quantile(.x, c(0.75), na.rm = TRUE), 3),
                          Min = ~round(min(.x, na.rm = TRUE), 3),
                          Max = ~round(max(.x, na.rm = TRUE), 3)),
                     .names = "{.fn}"), 
              .groups = "drop")
}



BidVector_Matrix <- rbind(
  "Bees" = summary_function(data = Data_Covariates, column = QX_BidVector_Bees),
  "Beetles" = summary_function(data = Data_Covariates, column = QX_BidVector_Beetles),
  "Wasps" = summary_function(data = Data_Covariates, column = QX_BidVector_Wasps),
  "All" = summary_function(data = Data_Covariates, column = QX_BidVector_All)
)


BidVector_Matrix %>% write.csv(quote = FALSE)



Data_Covariates$QX_BidVector_Bees %>% table() %>% write.csv(quote = FALSE)
Data_Covariates$QX_BidVector_Beetles %>% table() %>% write.csv(quote = FALSE)
Data_Covariates$QX_BidVector_Wasps %>% table() %>% write.csv(quote = FALSE)





# **********************************************************************************
#### CE debrief ####
# **********************************************************************************

rbind(
  "CE debrief Easy" = Data_Covariates$CE_Debrief_Easy %>% summary(),
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
  mutate(
    Total = sum(n),
    Percentage = n / Total,
    Result = sprintf("%d (%s)", n, scales::percent(Percentage, accuracy = 0.1))
  ) %>%
  select(Question, Response, Result) 






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
  mutate(
    Total = sum(n),
    Percentage = n / Total,
    Result = sprintf("%d (%s)", n, scales::percent(Percentage, accuracy = 0.1))
  ) %>%
  select(Question, Response, Result) 


Misc1_Output %>% write.csv(quote = FALSE)


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
  mutate(
    Total = sum(n),
    Percentage = n / Total,
    Result = sprintf("%d (%s)", n, scales::percent(Percentage, accuracy = 0.1))
  ) %>%
  select(Question, Response, Result) %>% write.csv(quote = FALSE)


# **********************************************************************************
#### Pretest ####
# **********************************************************************************


Data_Covariates %>%
  pivot_longer(cols = Pretest_SliderOrLikert, 
               names_to = "Question", 
               values_to = "Response") %>%
  group_by(Question, Response) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  group_by(Question) %>%
  mutate(
    Total = sum(n),
    Percentage = n / Total,
    Result = sprintf("%d (%s)", n, scales::percent(Percentage, accuracy = 0.1))
  ) %>%
  select(Question, Response, Result) %>% write.csv(quote = FALSE)


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
                     "QDiscounting_7",
                     "QDiscounting_8",
                     "QDiscounting_9",
                     "QDiscounting_10")


# Pivot longer to handle multiple questions at once
Discounting_Output <- Data_Covariates %>%
  pivot_longer(cols = all_of(Discounting_Questions), 
               names_to = "Question", 
               values_to = "Response") %>%
  group_by(Question, Response) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  group_by(Question) %>%
  mutate(
    Total = sum(n),
    Percentage = n / Total,
    Result = sprintf("%d (%s)", n, scales::percent(Percentage, accuracy = 0.1))
  ) %>%
  select(Question, Response, Result) 

Discounting_Output%>% write.csv(quote = FALSE)


# **********************************************************************************
#### Text responses at the end ####
# **********************************************************************************


table(Data_Covariates$Pretest_AnyDifficultQuestions) %>% write.csv(quote = FALSE)



table(Data_Covariates$FurtherComments) %>% write.csv(quote = FALSE)




# **********************************************************************************
#### Section X: Export ####
# **********************************************************************************


## Exporting as CSV for ease
## and using fwrite() is much faster than write.csv()
Data_Covariates %>% 
  fwrite(sep = ",", 
         here("Data/Pilot1/", 
              "Data_Covariates_Step1.csv"))



