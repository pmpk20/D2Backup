#### D2: Figure 3  ###############
# AUTHOR: Peter King (p.king1@leeds.ac.uk)
# LAST CHANGE: 20/05/2025
# FUNCTION: To make Figure 3

# *************************************************************
#### Section 0: Setup librarires including CMDLR ####
# *************************************************************


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


# install.packages("data.table",repos="http://cran.us.r-project.org")
options(scipen = 90)
library(tidyverse)
library(dplyr)
library(magrittr)
library(mded)
library(here)
library(data.table)
library(stats)
library(coin)
library(mvtnorm)
library(apollo)
# library(cmdlr)



# *************************************************************
#### Section 1: Import data ####
# *************************************************************




all_results <- here("CEOutput/Main/LCM", "D2_Truncated_LC_3C_MXL_NoDR_V3_SimulatedMeans_Wide.csv") %>% 
  fread() %>% data.frame()


all_results <- here("CEOutput/Main/LCM", "D2_Truncated_LC_3C_MXL_NoDR_V3_SimulatedMeans_Wide.csv") %>% 
  fread() %>% data.frame()



# *************************************************************
#### Section 2: Plot setup  ####
# *************************************************************


Classes <- c(1, 2, 3)
Insects <- c("Beetle", "Bee", "Wasp")
# Define text setup (missing from your code snippet)
TextSetup <- element_text(size = 10, face = "plain")

TextSize <- 18


TextSetup <- element_text(size = TextSize,
                          colour = "black",
                          face = "plain",
                          family = "serif")

wellbeing_levels <- c(-2, -1, 0, 1, 2) 



# custom_colors <- RColorBrewer::brewer.pal(n = 9, name = "Purples")[c(3, 6, 9)]
custom_colors <- c(
  RColorBrewer::brewer.pal(9, "Blues")[c(6)],
  RColorBrewer::brewer.pal(9, "Reds")[c(6)]
)




# *************************************************************
#### Section 2B: Reshape data ####
# *************************************************************


# Reshape data from wide to long format for plotting
all_results_long <- all_results %>%
  pivot_longer(
    cols = c(Encounter_Medium, Encounter_High, Existence_Medium, Existence_High, 
             Bequest_Medium, Bequest_High),
    names_to = "attribute_level",
    values_to = "wtp"
  ) %>%
  # Split attribute_level into attribute and level
  separate(attribute_level, into = c("attribute", "level"), sep = "_") %>%
  # Convert wellbeing to factor with proper levels
  mutate(wellbeing = factor(wellbeing, levels = wellbeing_levels),
         insect = factor(insect),
         Class = factor(Class),
         level = case_when(
           level == "Medium" ~ "Small",
           level == "High" ~ "Large"
         ))


# *************************************************************
#### Section 3: Export plot  ####
# *************************************************************


D2_Truncated_LC_3C_MXL_NoDR_V3_SimulatedMeans_Wellbeing_Points_V1 <-
  
  all_results_long %>%
  dplyr::filter(wellbeing %in% c(-2, 0, 2)) %>% 
  group_by(Class, insect, attribute, wellbeing, level) %>%
  summarise(
    mean_wtp = mean(wtp),
    sd_wtp = sd(wtp),
    # Create artificial range for visualization
    lower = mean_wtp - sd_wtp,
    upper = mean_wtp + sd_wtp,
    .groups = "drop"
  ) %>% 
  mutate(Class = case_when(
    Class == 1 ~ "Class 1 (Pro-insect)",
    Class == 2 ~ "Class 2 (Insect-averse)",
    Class == 3 ~ "Class 3 (Ambivalent)"
  )) %>% 
  
  ggplot(
    aes(x = wellbeing, 
        y = mean_wtp, 
        colour = level, 
        fill = level, 
        group = level)) +
  
  geom_ribbon(aes(ymin = lower, ymax = upper), colour = NA, alpha = 0.25) +
  
  geom_line(linewidth = 0.8) +
  
  geom_point(size = 2) +
  
  facet_grid(Class ~ insect + attribute,
             scales = "free_y") +
  
  labs(
    x = "Wellbeing Level",
    y = "Willingness to Pay (WTP)",
    # title = "WTP by Wellbeing Level, Attribute, Insect Type and Class",
    colour = "Level",
    fill = "Level"
  ) +
  
  scale_colour_manual(values = c(
    RColorBrewer::brewer.pal(9, "Blues")[c(5, 9)]
  ) %>% rev()) +
  
  theme_bw() +
  
  geom_hline(yintercept = 0, alpha = 0.5, linetype = "dashed") +
  geom_vline(xintercept = 2, alpha = 0.5, linetype = "dashed") +
  
  theme(
    legend.position = "bottom",
    strip.background = element_rect(fill = "white"),
    strip.text.x = element_text(
      size = TextSize,
      colour = "black",
      family = "serif",
      face = "bold"
    ),
    strip.text.y = element_text(
      size = TextSize,
      colour = "black",
      family = "serif",
      face = "bold"
    ),
    axis.text.x = element_text(
      size = TextSize,
      colour = "black",
      family = "serif",
      face = "plain",
      # angle = 35,
      hjust = 1
    ),
    
    legend.background = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    
    axis.text.y = TextSetup,
    axis.title.y = TextSetup,
    axis.title.x = TextSetup,
    legend.text = TextSetup,
    legend.title = TextSetup
    
  ) 


# *************************************************************
#### Section 4: Export Plot ####
# *************************************************************


ggsave(
  D2_Truncated_LC_3C_MXL_NoDR_V3_SimulatedMeans_Wellbeing_Points_V1,
  device = "jpeg",
  filename = here("OtherOutput/Figures",
                  "D2_Truncated_LC_3C_MXL_NoDR_V3_SimulatedMeans_Wellbeing_Points_V1.jpg"),
  width = 30,
  height = 25,
  units = "cm",
  dpi = 500
)


#### End of script
# *************************************************************************