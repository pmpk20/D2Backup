#### D2: CE Descriptives  ###############
# Script author: Peter King (p.king1@leeds.ac.uk)
# Last Edited: 31/10/2024.
# COMMENTS: Plotting choice frequency
# - now with resampled

# **********************************************************************************
#### Section 0: Setup environment and replication information ####
# **********************************************************************************


# **********************************************************************************
#### Replication Information:

# here() = "K:/WinterAnalysis1307/WP5/WP5P3"
# R version 4.2.0 (2022-04-22 ucrt)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows 10 x64 (build 19044)
# Matrix products: default
#   [1] LC_COLLATE=English_United Kingdom.utf8  LC_CTYPE=English_United Kingdom.utf8   
# [3] LC_MONETARY=English_United Kingdom.utf8 LC_NUMERIC=C                           
# [5] LC_TIME=English_United Kingdom.utf8    
#   [1] matrixStats_0.62.0 Rfast_2.0.6        RcppZiggurat_0.1.6 Rcpp_1.0.8.3       data.table_1.14.2 
# [6] mded_0.1-2         magrittr_2.0.3     forcats_0.5.1      stringr_1.4.0      dplyr_1.0.9       
# [11] purrr_0.3.4        readr_2.1.2        tidyr_1.2.0        tibble_3.1.7       ggplot2_3.3.6     
# [16] tidyverse_1.3.1    apollo_0.2.7       here_1.0.1        


# **********************************************************************************
#### Libraries:
options(scipen=90)
library(apollo)
library(tidyverse)
library(dplyr)
library(magrittr)
library(mded)
library(here)
library(data.table)
library(Rfast)
library(matrixStats)
library(ggplot2)
library(ggridges)
library(ggdist)
library(RColorBrewer)


# **********************************************************************************
#### Section 1: Import ####
# **********************************************************************************


# Using fread() from data.table as significantly faster than read.csv()
# Store data in Data subfolder so referencing it using here() package
database <- here("Data/Main", "database_Step3.csv") %>% fread() %>% data.frame()


## Recode to just Plan C or not
database$Choice_SQ <- ifelse(database$Choice == 3, 1, 0)


# **********************************************************************************
#### Section 2: Rearrange data ####
# **********************************************************************************


## Helps scale values later
Type_totals <- tibble(
  Insect_PlanC_Words = c("Bees", "Beetles", "Wasps"),
  Total = c(4953, 4953, 4953)
)




Test <- database %>%
  dplyr::select(Choice_SQ, Insect_PlanC_Words) %>%
  pivot_longer(cols = starts_with("Choice_SQ"),
               names_to = "Choice_SQ",
               values_to = "value") %>%
  group_by(Choice_SQ, value, Insect_PlanC_Words) %>%
  summarise(Freq = n(), .groups = "drop") %>%
  left_join(Type_totals, by = "Insect_PlanC_Words") %>%
  mutate(
    YMIN = Freq - 1.96 * sqrt(Freq),
    Y25 = Freq - (1.96/2) * sqrt(Freq),
    Y75 = Freq + (1.96/2) * sqrt(Freq),
    YMAX = Freq + 1.96 * sqrt(Freq),
    Percentage = (100/Total * Freq)/100,
    YMIN = (100/Total * YMIN)/100,
    YMAX = (100/Total * YMAX)/100,
    Y25 = (100/Total * Y25)/100,
    Y75 = (100/Total * Y75)/100,) %>%
  group_by(value, Insect_PlanC_Words) %>%
  summarise(
    Freq = sum(Freq),
    YMIN = sum(YMIN),
    YMAX = sum(YMAX),
    Y25 = sum(Y25),
    Y75 = sum(Y75),
    Percentage = sum(Percentage))



# ****************************************************************
#### Section 3A: Plot Setup ####
# ****************************************************************

TextSize <- 12

TextSetup <- element_text(size = TextSize,
                          colour = "black",
                          family = "serif")


# ****************************************************************
#### Section 3B: Create Plot ####
# ****************************************************************


Figure2_ScaleVariance <- Test %>% 
  ggplot(aes(x = value %>% as.factor(), 
             y = Percentage,
             fill = Insect_PlanC_Words %>% as.factor())) +
  geom_errorbar(aes(ymin = YMIN, 
                    ymax = YMAX),
                width = 0.25,
                position = position_dodge(width = 0.8)
  ) +
  geom_boxplot(
    aes(
      ymin = YMIN,
      lower = Y25,
      middle = Percentage,
      upper = Y75,
      ymax = YMAX
    ),
    varwidth = 0.5,
    width = 0.8,
    outlier.shape = NA,
    stat = "identity"
  ) +
  
  theme_bw() +
  
  facet_wrap( ~ value, 
              labeller = as_labeller(c("1" = "Plan C (SQ)",
                                       "0" = "Plan A or B")),
              scales = "free_y") +
  
  scale_y_continuous(name = "Percentage of Choices",
                     labels = scales::percent,
                     breaks = seq.int(from = 0, to = 1, by = 0.025)) +
  
  scale_x_discrete(name = "Choice",
                   labels = NULL) +
  
  scale_fill_manual(
    name = "Insect",
    values = RColorBrewer::brewer.pal(9, "Blues")[c(4, 6, 8)],
    label = c(
      "Bees",
      "Beetles",
      "Wasps"
    ),
    guide = guide_legend(reverse = FALSE)
  ) +
  
  theme(
    legend.position = "bottom",
    legend.text = TextSetup,
    legend.title = TextSetup,
    axis.text.x = TextSetup,
    axis.text.y = TextSetup,
    axis.title.y = TextSetup,
    strip.background = element_rect(fill = "white"),
    strip.text = TextSetup,
    legend.background = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank()
  )



# ****************************************************************
#### Section 4: Export Plot ####
# ****************************************************************


ggsave(
  Figure2_ScaleVariance,
  device = "jpeg",
  filename = here("OtherOutput/Figures",
                  "FigureX_ScaleVariance.jpeg"),
  width = 20,
  height = 15,
  units = "cm",
  dpi = 500
)


