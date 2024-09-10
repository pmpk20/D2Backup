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
  readxl::read_xlsx(col_types = "text", sheet = "Data") %>% 
  data.frame()


Data_Timers <-
  here("Data/Pilot1/", "DRUID_pilot_1_timestamps_2024-09-10.xlsx") %>% 
  readxl::read_xlsx(col_types = "text", sheet = "Data") %>% 
  data.frame()


Data_CE <-
  here("Data/Pilot1/", "DRUID_pilot_1_DCE_d2_test2_2024-09-10.xlsx") %>% 
  readxl::read_xlsx(col_types = "text", sheet = "Data") %>% 
  data.frame()




# **********************************************************************************
#### Section 1: Identify prob test failures ####
# **********************************************************************************


## Recategorise so that pass  = 1
USData_Client_Xlsx$ProbabilityTest_Numeric <- ifelse(
  USData_Client_Xlsx$T1 == 
    "The risk of getting a cold is higher in group A", 
  0 , 
  1) 

USData_Client_Xlsx$ProbabilityTest_Categorical <- ifelse(
  USData_Client_Xlsx$T1 == 
    "The risk of getting a cold is higher in group A", 
  "Fail", 
  "Pass") 



# **********************************************************************************
#### Section 2: Identify speeders ####
## First survey speeders then valuation section
# **********************************************************************************


## First append the country from the main data to the timer data
USData_Timers$Country <- USData_Client_Xlsx$Country


USData_Client_Xlsx <- USData_Client_Xlsx %>% 
  dplyr::filter(Country == "United States")


USData_Timers <- USData_Timers %>% 
  dplyr::filter(Country == "United States")

# **********************************************************************************
#### SURVEY SPEEDERS

## Calculate each persons time
## by getting each rows sum of time in seconds
USData_Timers$Speeders_Survey_CompletionTime <-
  USData_Timers[2:92] %>% 
  rowSums(na.rm = TRUE) 


## Now calculate and append to data the 
## country specific threshold using 48% of median (for speeding)
## of 148% for too slow
USData_Timers <- 
  USData_Timers %>% 
  group_by(Country) %>% 
  mutate("Speeders_Survey_CountryThreshold" = 
           median(Speeders_Survey_CompletionTime) %>% multiply_by(0.48),
         "Slowers_Survey_CountryThreshold" = 
           median(Speeders_Survey_CompletionTime) %>% multiply_by(1.48)
  )


## Recode as fail (0) or pass (10)
USData_Timers$Speeders_Survey_TestDummy <- ifelse(
  USData_Timers$Speeders_Survey_CompletionTime <=
    USData_Timers$Speeders_Survey_CountryThreshold, 0, 1)


## Test people going too slow
USData_Timers$Slowers_Survey_TestDummy <- ifelse(
  USData_Timers$Speeders_Survey_CompletionTime >=
    USData_Timers$Slowers_Survey_CountryThreshold, 0, 1)


## Output survey speeder data
USData_Timers %>%
  group_by(Country) %>%
  summarise(
    Threshold = first(Speeders_Survey_CountryThreshold),
    
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
#### VALUATION SPEEDERS


## Valuation section
USData_Timers$Speeders_Valuation_CompletionTime <-
  USData_Timers[c("Q1", "Q2", "Q3", "Q4", "Q5", "Q6a", "Q6b", "Q6c", "Q7")] %>%
  rowSums(na.rm = TRUE) 


## Categorise valuation section threshold
USData_Timers <- USData_Timers %>% 
  group_by(Country) %>% 
  mutate("Speeders_Valuation_CountryThreshold" = 
           median(Speeders_Valuation_CompletionTime) %>% multiply_by(0.48),
         "Slowers_Valuation_CountryThreshold" = 
           median(Speeders_Valuation_CompletionTime) %>% multiply_by(1.48))


## Recode as fail (0) or pass (10)
USData_Timers$Speeders_Valuation_TestDummy <- ifelse(
  USData_Timers$Speeders_Valuation_CompletionTime <=
    USData_Timers$Speeders_Valuation_CountryThreshold, 0, 1)


USData_Timers$Slowers_Valuation_TestDummy <- ifelse(
  USData_Timers$Speeders_Valuation_CompletionTime >=
    USData_Timers$Slowers_Valuation_CountryThreshold, 0, 1)



USData_Timers %>%
  group_by(Country) %>%
  summarise(
    Threshold = first(Speeders_Valuation_CountryThreshold),
    
    N_Fail = paste0(
      sum(Speeders_Valuation_TestDummy == 0),
      " (", mean(Speeders_Valuation_TestDummy == 0) %>% 
        round(2) %>% 
        multiply_by(100), "%)"),
    
    N_Pass = paste0(
      sum(Speeders_Valuation_TestDummy == 1),
      " (", mean(Speeders_Valuation_TestDummy == 1) %>% 
        round(2) %>% 
        multiply_by(100), "%)")
  ) %>% 
  write.csv(quote = FALSE)



USData_Timers %>% 
  fwrite(sep = ",", 
         here("Data/22-029424-01_SWACHE_Hypertension_Main_EL_PT_FI_US_client",
              "USData_Main_Timers_Cleaned.csv"))

# 
# ## Add timer data to main dataset
# USData_Client_Xlsx <- cbind(
#   USData_Client_Xlsx,
#   USData_Timers[, 94:103]
# )


# USData_Timers$ProbabilityTest_Numeric <- USData_Client_Xlsx$ProbabilityTest_Numeric
# USData_Timers$DBDC <- USData_Client_Xlsx$DBDC
# 
# 
# 
# 
# summary_function <- function(data, column) {
#   data %>%
#     group_by(Country) %>%
#     summarise(across({{column}}, 
#                      list(Min = min, 
#                           `1st Qu.` = ~quantile(.x, 0.25, na.rm = TRUE), 
#                           Median = ~round(median(.x, na.rm = TRUE), 3), 
#                           Mean = ~round(mean(.x, na.rm = TRUE), 3), 
#                           `3rd Qu.` = ~quantile(.x, 0.75, na.rm = TRUE), 
#                           Max = max,
#                           SD = ~round(sd(.x, na.rm = TRUE), 3)), # Correct rounding within the list
#                      .names = "{.fn}"), 
#               .groups = "drop")
# }
# 
# rbind(
#   "Survey time",
#   summary_function(USData_Timers, Speeders_Survey_CompletionTime),
#   "Valuation time",
#   summary_function(USData_Timers, Speeders_Valuation_CompletionTime)
# ) %>% write.csv(quote = FALSE)










# ## Subset by prob test
# summary_function <- function(data, column) {
#   data %>%
#     filter(ProbabilityTest_Numeric == 1) %>% 
#     group_by(Country) %>%
#     summarise(across({{column}}, 
#                      list(Min = min, 
#                           `1st Qu.` = ~quantile(.x, 0.25, na.rm = TRUE), 
#                           Median = ~round(median(.x, na.rm = TRUE), 3), 
#                           Mean = ~round(mean(.x, na.rm = TRUE), 3), 
#                           `3rd Qu.` = ~quantile(.x, 0.75, na.rm = TRUE), 
#                           Max = max,
#                           SD = ~round(sd(.x, na.rm = TRUE), 3)), # Correct rounding within the list
#                      .names = "{.fn}"), 
#               .groups = "drop")
# }
# 
# rbind(
#   "ProbTest0",
#   summary_function(USData_Timers[USData_Timers$ProbabilityTest_Numeric == 0,], Speeders_Survey_CompletionTime),
#   summary_function(USData_Timers[USData_Timers$ProbabilityTest_Numeric == 0,], Speeders_Valuation_CompletionTime),
#   "ProbTest1",
#   summary_function(USData_Timers[USData_Timers$ProbabilityTest_Numeric == 1,], Speeders_Survey_CompletionTime),
#   summary_function(USData_Timers[USData_Timers$ProbabilityTest_Numeric == 1,], Speeders_Valuation_CompletionTime)
# ) %>% write.csv(quote = FALSE)
# 
# 
# 
# Test <- c("YY",
#           "NY",
#           "NN",
#           "YN")

# 
# rbind(
#   "YY",
#   summary_function(USData_Timers[USData_Timers$DBDC == "YY",], Speeders_Survey_CompletionTime),
#   summary_function(USData_Timers[USData_Timers$DBDC == "YY",], Speeders_Valuation_CompletionTime),
#   "YN",
#   summary_function(USData_Timers[USData_Timers$DBDC == "YN",], Speeders_Survey_CompletionTime),
#   summary_function(USData_Timers[USData_Timers$DBDC == "YN",], Speeders_Valuation_CompletionTime),
#   "NY",
#   summary_function(USData_Timers[USData_Timers$DBDC == "NY",], Speeders_Survey_CompletionTime),
#   summary_function(USData_Timers[USData_Timers$DBDC == "NY",], Speeders_Valuation_CompletionTime),
#   "NN",
#   summary_function(USData_Timers[USData_Timers$DBDC == "NN",], Speeders_Survey_CompletionTime),
#   summary_function(USData_Timers[USData_Timers$DBDC == "NN",], Speeders_Valuation_CompletionTime)
# ) %>% write.csv(quote = FALSE)
# 




# **********************************************************************************
#### Section 2B: Count and proportion by two groups  ####
# **********************************************************************************

USData_Client_Xlsx %>%
  count(Country, T1) %>%
  group_by(Country) %>%
  mutate(
    Total = sum(n),
    Data = sprintf("%d (%s)", n, percent(n / Total, accuracy = 0.1))
  ) %>%
  select(-n, -Total) %>%
  pivot_wider(names_from = T1, values_from = Data) %>%
  ungroup() %>% 
  write.csv(quote = FALSE)




# **********************************************************************************
#### Section 3: DBDC Matrix ####
# **********************************************************************************


# Recoding -------------------------------------------------------------
USData_Client_Xlsx$Question1 <- 
  ifelse(
    USData_Client_Xlsx$Q1 == "Current risk", 
    0, ## Vote no
    1) ## Vote yes

USData_Client_Xlsx$Q2_Missing <- ifelse(USData_Client_Xlsx$Q2 %>% is.na(), 
                                        NA, ## is.na = true so code as na
                                        USData_Client_Xlsx$Q2) ## put answer if not na

USData_Client_Xlsx$Q3_Missing <- ifelse(USData_Client_Xlsx$Q3 %>% is.na(), 
                                        NA, 
                                        USData_Client_Xlsx$Q3)


USData_Client_Xlsx$FollowUp <- coalesce(USData_Client_Xlsx$Q2_Missing, USData_Client_Xlsx$Q3_Missing)
USData_Client_Xlsx$FollowUp_Numeric <- ifelse(USData_Client_Xlsx$FollowUp == "Current risk", 
                                              0, 
                                              1)



## Remove currency signs and convert to numeric
USData_Client_Xlsx$Bid1_Numeric <-
  USData_Client_Xlsx$BID1 %>% 
  gsub(x = ., 
       pattern = "\\$ ", 
       replacement = "") %>% 
  gsub(x = ., 
       pattern = ",", 
       replacement = "") %>% 
  as.numeric()


## DBDC second bound bid level
USData_Client_Xlsx$FollowUp_Bid <-
  ifelse(USData_Client_Xlsx$Q2_Missing %>% is.na(), 
         USData_Client_Xlsx$BID3, 
         USData_Client_Xlsx$BID2) %>% 
  gsub(pattern = "\\$ ", replacement = "") %>% 
  gsub(pattern = ",", replacement = "") %>% 
  as.numeric()


## Originally character for reasons
USData_Client_Xlsx$BASE_Numeric <- USData_Client_Xlsx$BASE %>% as.numeric()
USData_Client_Xlsx$RED_Numeric <- USData_Client_Xlsx$RED %>% as.numeric()


USData_Client_Xlsx$DBDC <- ifelse(
  (USData_Client_Xlsx$Question1 == 0) & (USData_Client_Xlsx$FollowUp_Numeric == 0), 
  "NN", 
  ifelse(
    (USData_Client_Xlsx$Question1 == 0) & (USData_Client_Xlsx$FollowUp_Numeric == 1), 
    "NY",
    ifelse(
      (USData_Client_Xlsx$Question1 == 1) & (USData_Client_Xlsx$FollowUp_Numeric == 0), 
      "YN",
      ifelse(
        (USData_Client_Xlsx$Question1 == 1) & (USData_Client_Xlsx$FollowUp_Numeric == 1), 
        "YY", 
        1)))) 


# Create summaries  -------------------------------------------------------------


## DBDC matrix by country
USData_Client_Xlsx %>% select("Country", "DBDC") %>% table(.)

## Output matrix by country
USData_Client_Xlsx %>%
  count(Country, DBDC) %>%
  group_by(Country) %>%
  mutate(
    Total = sum(n),
    Percentage = n / Total,
    Result = sprintf("%d (%s)", n, percent(Percentage, accuracy = 0.1))
  ) %>%
  select(-n, -Total, -Percentage) %>%
  pivot_wider(names_from = DBDC, values_from = Result) %>%
  ungroup() %>% 
  write.csv(quote = FALSE)

## tabulate responses by bid level
rbind(
  cbind(table(USData_Client_Xlsx$Q1[USData_Client_Xlsx$BASE_Numeric == 30], 
              USData_Client_Xlsx$Bid1_Numeric[USData_Client_Xlsx$BASE_Numeric == 30]), 0),
  cbind(table(USData_Client_Xlsx$Q1[USData_Client_Xlsx$BASE_Numeric != 30], 
              USData_Client_Xlsx$Bid1_Numeric[USData_Client_Xlsx$BASE_Numeric != 30]), 0)) %>% 
  write.csv(quote = FALSE)


## Answer question 3
table(USData_Client_Xlsx$Bid1_Numeric, USData_Client_Xlsx$BASE) %>% t() %>% write.csv(quote = FALSE)




paste0(USData_Client_Xlsx$DBDC %>% table() %>% names(),
       ": ", 
       USData_Client_Xlsx$DBDC %>% table(),
       " (",
       USData_Client_Xlsx$DBDC %>% table() %>% prop.table() %>% multiply_by(100) %>% round(2),
       "%)"
)




USData_Client_Xlsx %>% 
  fwrite(sep = ",", 
         here("Data/SWACHE_July2024Latest/", 
              "USData_Main_Data_Cleaned.csv"))



# **********************************************************************************
#### Section 5: Debriefing questions  ####
# **********************************************************************************



# Did you understand that your risk of developing high blood pressure would return to its current level after 10 years even if you chose to pay to reduce your risk over 10 years?
USData_Client_Xlsx$HBP_10Yr <- ifelse(
  USData_Client_Xlsx$H3 == "Yes",
  1, 0)


# When you choose between the current and reduced risk, were you thinking you could just adopt a healthier lifestyle to reduce your risk instead of paying?
USData_Client_Xlsx$HBP_Healthier <- ifelse(
  USData_Client_Xlsx$H4 == "Yes",
  1, 0)


# Please consider the statement: “I would pay almost anything to improve my health even by a small amount”. Do you…
USData_Client_Xlsx$HBP_PayAnything <- ifelse(
  USData_Client_Xlsx$H5 == "…agree",
  1, 0)




# How much do you agree or disagree with the following statements? - My survey responses reflect what I would have done in real life.
USData_Client_Xlsx$Response_RealLife <- dplyr::recode(
  USData_Client_Xlsx$A1_1,
  "Strongly agree" = 4,
  "Somewhat agree" = 3,
  "Neither agree nor disagree" = 2,
  "Somewhat disagree"          = 1,
  "Strongly disagree"  = 0
)


# How much do you agree or disagree with the following statements? - The survey provided me with enough information to make informed choices.
USData_Client_Xlsx$Response_Information <- dplyr::recode(
  USData_Client_Xlsx$A1_2,
  "Strongly agree" = 4,
  "Somewhat agree" = 3,
  "Neither agree nor disagree" = 2,
  "Somewhat disagree"          = 1,
  "Strongly disagree"  = 0
)


# How much do you agree or disagree with the following statements? - I find the description of high blood pressure provided in this survey to be accurate
USData_Client_Xlsx$Response_Accurate <- dplyr::recode(
  USData_Client_Xlsx$A1_3,
  "Strongly agree" = 4,
  "Somewhat agree" = 3,
  "Neither agree nor disagree" = 2,
  "Somewhat disagree"          = 1,
  "Strongly disagree"  = 0
)


# How confident are you that the information that has been provided in this survey is correct?
USData_Client_Xlsx$Response_Confident <- dplyr::recode(
  USData_Client_Xlsx$A2,
  "Very confident" = 4,
  "Somewhat confident"                 = 3,
  "Neither doubtful nor confident"  = 2,
  "Somewhat doubtful" = 1,
  "Very doubtful"   = 0
)


# How would you describe your knowledge of high blood pressure before having taken this survey?
USData_Client_Xlsx$Response_HBP_Before <- dplyr::recode(
  USData_Client_Xlsx$A3,
  "Excellent" = 4,
  "Good" = 3,
  "Fair" = 2,
  "Poor" = 1,
  "Very poor" = 0
)

# How would you describe your knowledge of high blood pressure after having taken this survey?# How would you describe your knowledge of high blood pressure after having taken this survey?
USData_Client_Xlsx$Response_HBP_After <- dplyr::recode(
  USData_Client_Xlsx$A4,
  "Excellent" = 4,
  "Good" = 3,
  "Fair" = 2,
  "Poor" = 1,
  "Very poor" = 0
)





# A1_1
# A1_2
# A1_3
# A2
# A3
# A4
# H3
# H4
# H5




# **********************************************************************************
#### Section 4A: Plot general  ####
# **********************************************************************************


TextSize <- 12
TextType <- element_text(size = TextSize,
                         colour = "black",
                         family = "serif")



Fig1 <- USData_Client_Xlsx %>% 
  ggplot(
    aes(x = Bid1_Numeric, y = Question1)) + 
  geom_smooth(method = "glm",
              method.args = list(family = "binomial")) + 
  scale_y_continuous(labels = c("Current\nrisk", 
                                "Risk\nreduction"), 
                     breaks = c(0, 1), 
                     limits = seq(0, 1, 1)) + 
  theme_bw() + 
  facet_wrap(~ paste0(Country, " - ", BASE),
             nrow = 5, 
             ncol = 2) +
  xlab("Bid level") + 
  ylab("") +
  theme(
    legend.position = "bottom",
    legend.background = element_blank(),
    strip.background = element_rect(fill = "white"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    
    axis.text.x = TextType,
    axis.text.y = TextType,
    axis.title.y = TextType,
    axis.title.x = TextType,
    legend.text = TextType,
    legend.title = TextType
  )






ggsave(
  Fig1,
  device = "png",
  filename = paste0(here(), "/Output/Figures/", "Fig1_ScopeAnalysis_Pilot_Main_Check.png"),
  width = 20,
  height = 15,
  units = "cm",
  dpi = 500
)


# **********************************************************************************
#### Section 4B: Plot improved  ####
# **********************************************************************************


Fig2 <- USData_Client_Xlsx %>%
  mutate(BASE = as.factor(BASE)) %>%  # Ensure BASE is treated as a factor
  ggplot(aes(x = Bid1_Numeric, y = Question1, fill = BASE)) +
  geom_ribbon(stat='smooth', 
              method = "glm", 
              method.args = list(family = "binomial"), 
              se = TRUE, 
              alpha=0.25,  colour = NA) +
  geom_line(stat='smooth',
            method = "glm",
            method.args = list(family = "binomial"), 
            alpha = 1, linewidth = 1.5, 
            aes(group = BASE, colour = BASE)) +
  scale_y_continuous(
    labels = c("Current\nrisk", "Risk\nreduction"),
    breaks = c(0, 1),
    limits = seq(0, 1, 1)
  ) +
  facet_grid(~ProbabilityTest_Numeric + RR) +
  facet_grid( ~ ProbabilityTest_Numeric + RR, 
              labeller = as_labeller(c('0' = "Prob Test: Fail", 
                                       '1' = "Prob Test: Pass",
                                       '10' = "RR: 10",
                                       '20' = "RR: 20"))) +
  theme_bw() +
  scale_colour_manual(values = c("black", "blue")) +
  scale_fill_manual(values = c("black", "blue")) +
  coord_cartesian(xlim = c(0, 1000)) +
  xlab("Bid level") + 
  ylab("") +
  theme(
    legend.position = "bottom",
    legend.background = element_blank(),
    strip.background = element_rect(fill = "white"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    
    axis.text.x = TextType,
    axis.text.y = TextType,
    axis.title.y = TextType,
    axis.title.x = TextType,
    legend.text = TextType,
    legend.title = TextType
  )





ggsave(
  Fig2,
  device = "png",
  filename = paste0(here(), "/Output/Figures/", "Fig1_ScopeAnalysis_Main2Check.png"),
  width = 20,
  height = 15,
  units = "cm",
  dpi = 500
)









TextFormat <- element_text(size = 10,
                           colour = "black",
                           family = "sans")


# **********************************************************************************
#### A2  ####
# **********************************************************************************

USData_Client_Xlsx %>% 
  pivot_longer(cols = A2, 
               names_to = "Question", 
               values_to = "Response") %>%
  count(Country, Question, Response) %>% 
  group_by(Country, Question) %>% 
  mutate(Percentage = n / sum(n),
         Label = sprintf("%d (%s)", n, scales::percent(Percentage, accuracy = 0.1))) %>%
  ungroup() %>% 
  mutate(Response = factor(Response, levels = 
                             c("Very confident",
                               "Somewhat confident",
                               "Neither doubtful nor confident",
                               "Somewhat doubtful",
                               "Very doubtful"))) %>% 
  dplyr::select(-n, -Percentage) %>% 
  pivot_wider(names_from = Response, values_from = Label) 


# **********************************************************************************
#### A3  ####
# **********************************************************************************



USData_Client_Xlsx %>% 
  pivot_longer(cols = A3, 
               names_to = "Question", 
               values_to = "Response") %>%
  count(Country, Question, Response) %>% 
  group_by(Country, Question) %>% 
  mutate(Percentage = n / sum(n),
         Label = sprintf("%d (%s)", n, scales::percent(Percentage, accuracy = 0.1))) %>%
  ungroup() %>% 
  mutate(Response = factor(Response, levels = 
                             c(  "Excellent" ,
                                 "Good",
                                 "Fair",
                                 "Poor",
                                 "Very poor"))) %>% 
  dplyr::select(-n, -Percentage) %>% 
  pivot_wider(names_from = Response, values_from = Label) %>% write.csv(quote = FALSE)




# **********************************************************************************
#### A4  ####
# **********************************************************************************



USData_Client_Xlsx %>% 
  pivot_longer(cols = A4, 
               names_to = "Question", 
               values_to = "Response") %>%
  count(Country, Question, Response) %>% 
  group_by(Country, Question) %>% 
  mutate(Percentage = n / sum(n),
         Label = sprintf("%d (%s)", n, scales::percent(Percentage, accuracy = 0.1))) %>%
  ungroup() %>% 
  mutate(Response = factor(Response, levels = 
                             c(  "Excellent" ,
                                 "Good",
                                 "Fair",
                                 "Poor",
                                 "Very poor"))) %>% 
  dplyr::select(-n, -Percentage) %>% 
  pivot_wider(names_from = Response, values_from = Label) %>% write.csv(quote = FALSE)





USData_Client_Xlsx %>% 
  pivot_longer(cols = H3:H4, 
               names_to = "Question", 
               values_to = "Response") %>%
  count(Country, Question, Response) %>% 
  group_by(Country, Question) %>% 
  mutate(Percentage = n / sum(n),
         Label = sprintf("%d (%s)", n, scales::percent(Percentage, accuracy = 0.1))) %>%
  ungroup() %>%
  dplyr::select(-n, -Percentage) %>% 
  pivot_wider(names_from = Response, values_from = Label) %>% write.csv(quote = FALSE)





USData_Client_Xlsx %>%
  pivot_longer(cols = H5,
               names_to = "Question",
               values_to = "Response") %>%
  count(Country, Question, Response) %>%
  group_by(Country, Question) %>%
  mutate(Percentage = n / sum(n),
         Label = sprintf("%d (%s)", n, scales::percent(Percentage, accuracy = 0.1))) %>%
  ungroup() %>%
  dplyr::select(-n,-Percentage) %>%
  pivot_wider(names_from = Response, values_from = Label) %>% write.csv(quote = FALSE)



# **********************************************************************************
#### Section 2: Experimental  ####
# **********************************************************************************

USData_Client_Xlsx %>% 
  pivot_longer(cols = starts_with("A1_"), names_to = "Question", values_to = "Response") %>%
  count(Country, Question, Response) %>% 
  group_by(Country, Question) %>% 
  mutate(Percentage = n / sum(n),
         Label = sprintf("%d (%s)", n, scales::percent(Percentage, accuracy = 0.1))) %>%
  ungroup() %>% 
  mutate(Response = factor(Response, levels = c("Strongly disagree", "Somewhat disagree",  "Neither agree nor disagree", "Somewhat agree", "Strongly agree"))) %>% 
  dplyr::select(-n, -Percentage) %>% 
  pivot_wider(names_from = Response, values_from = Label) %>% 
  write.csv(quote = FALSE)



Test_A <- USData_Client_Xlsx %>% 
  pivot_longer(cols = starts_with("A1_"), names_to = "Question", values_to = "Response") %>%
  count(Country, Question, Response) %>% 
  group_by(Country, Question) %>% 
  mutate(Percentage = n / sum(n),
         Label = sprintf("%d (%s)", n, scales::percent(Percentage, accuracy = 0.1))) %>%
  ungroup() %>% 
  mutate(Response = factor(Response, levels = c("Strongly disagree", "Somewhat disagree",  "Neither agree nor disagree", "Somewhat agree", "Strongly agree"))) %>% 
  ggplot(aes(x = Country, y = n, fill = Response)) + 
  geom_bar(stat = "identity", position = "dodge") +
  labs(y = "Count", title = "Responses by Country and Question", fill = "Response") +
  scale_fill_brewer(palette = "Blues") +
  theme_bw() +
  facet_wrap(~ Question, 
             labeller = as_labeller(c("A1_1" = "A1_1: Real life", 
                                      "A1_2" = "A1_2: Informed",
                                      "A1_3" = "A1_3: Accurate"))) +
  guides(fill = guide_legend(nrow = 3)) +
  theme(legend.position = "bottom",
        legend.background=element_blank(),
        legend.box.background = element_rect(colour="black"),
        axis.text.x = TextFormat, 
        axis.text.y = TextFormat,
        text = TextFormat,
        legend.text = TextFormat)





ggsave(
  Test_A,
  device = "png",
  filename = paste0(here(), 
                    "/Output/Figures/", 
                    "Maincheck_Test_A.png"),
  width = 20,
  height = 15,
  units = "cm",
  dpi = 500
)












Test_B <- USData_Client_Xlsx %>% 
  pivot_longer(cols = c(A3, A4), names_to = "Question", values_to = "Response") %>%
  count(Country, Question, Response) %>% 
  group_by(Country, Question) %>% 
  mutate(Percentage = n / sum(n),
         Label = sprintf("%d (%s)", n, scales::percent(Percentage, accuracy = 0.1))) %>%
  ungroup() %>% 
  mutate(Response = factor(Response, levels = c(  "Excellent" ,
                                                  "Good",
                                                  "Fair",
                                                  "Poor",
                                                  "Very poor"))) %>% 
  ggplot(aes(x = Question, y = n, fill = Response)) + 
  geom_bar(stat = "identity", position = "dodge") +
  labs(y = "Count", title = "Responses by Country and Question", fill = "Response") +
  scale_x_discrete(labels = c("Before", "After"))+
  scale_fill_brewer(palette = "Blues", direction = -1) +
  theme_bw() +
  facet_wrap(~ Country) +
  guides(fill = guide_legend(nrow = 3)) +
  theme(legend.position = "bottom",
        legend.background=element_blank(),
        legend.box.background = element_rect(colour="black"),
        axis.text.x = TextFormat, 
        axis.text.y = TextFormat,
        text = TextFormat,
        legend.text = TextFormat)



ggsave(
  Test_B,
  device = "png",
  filename = paste0(here(), 
                    "/Output/Figures/", 
                    "Maincheck_Test_B.png"),
  width = 20,
  height = 15,
  units = "cm",
  dpi = 500
)