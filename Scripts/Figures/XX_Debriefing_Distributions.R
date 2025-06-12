#### D2: Debriefing Descriptives  ###############
# Script author: Peter King (p.king1@leeds.ac.uk)
# Last Edited: 31/10/2024.
# COMMENTS: Plotting and summarising biowell
# - updated with resampled

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
Data <-
  here("Data/Main", "Data_Covariates_Step3.csv") %>% fread() %>% data.frame()


## 100 - scores to centre correctly
PlotData <- cbind(
  "CE_Debrief_Certain" = Data$CE_Debrief_Certain,
  "CE_Debrief_Confident" = Data$CE_Debrief_Confident
) %>% data.frame() %>% pivot_longer(cols = 1:2)


# **********************************************************************************
#### Section 2: Setup plot ####
# **********************************************************************************


# Custom mapping for y-axis labels
# prefix_labels <- c(
#   "Wasp3" = "Wasp:\nExist in the future",
#   "Wasp2" = "Wasp:\nExist now",
#   "Wasp1" = "Wasp:\nEncountering",
#   "Beetle3" = "Beetle:\nExist in the future",
#   "Beetle2" = "Beetle:\nExist now",
#   "Beetle1" = "Beetle:\nEncountering",
#   "Bee3" = "Bee:\nExist in the future",
#   "Bee2" = "Bee:\nExist now",
#   "Bee1" = "Bee:\nEncountering"
# )
# 
# Custom colors for different insects
# custom_colors <- c(
#   "Bee1" = "#1f78b4", "Bee2" = "#1f78b4", "Bee3" = "#1f78b4",  # Shades of blue for Bees
#   "Beetle1" = "#e31a1c", "Beetle2" = "#e31a1c", "Beetle3" = "#e31a1c",  # Shades of red for Beetles
#   "Wasp1" = "#33a02c", "Wasp2" = "#33a02c", "Wasp3" = "#33a02c"   # Shades of green for Wasps
# )
# 
# 
# custom_colors <- c(
#   RColorBrewer::brewer.pal(n = 3, name = "Blues"),
#   RColorBrewer::brewer.pal(n = 3, name = "Reds"),
#   RColorBrewer::brewer.pal(n = 3, name = "Greens"))



# **********************************************************************************
#### Section 3: Plot ####
# **********************************************************************************




# Plotting
FX <- PlotData %>%
  ggplot(aes(
    x = value,
    y = name,
    fill = name,
    group = name
  )) +
  ggdist::stat_halfeye(outline_bars = TRUE,
                       aes(point_size = 1.5,
                           slab_colour = "black")) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    legend.background = element_blank(),
    legend.box.background = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank()
  )



# **********************************************************************************
#### Section 3: Plot ####
# **********************************************************************************



# Date <- gsub(pattern = "-",replacement = "_",Sys.Date())
## Save output in highest DPI
ggsave(
  FX,
  device = "jpeg",
  filename = here("OtherOutput/Figures", "CE_Debrief_Distributions_Main.jpg"),
  width = 20,
  height = 25,
  units = "cm",
  dpi = 500
)


# **********************************************************************************
#### Section Y: Biowell by discount rates ####
# **********************************************************************************

# 
# 
# cbind(
#   BioWell,
#   Data$DiscountRate, 
#   Data$DiscountRate_Scaled,
#   Data$DiscountRate_Transform
# ) %>% data.frame() %>% 
#   pivot_longer(cols = 1:45, names_to = "Question", values_to = "Response") %>%   mutate(Prefix = sub("_Item.*", "", Question),
#                                                                                         Prefix = sub("Biowell_*", "", Prefix)) %>% 
#   ggplot(aes(
#     x = Response,
#     y = Prefix,
#     group = Prefix,
#     fill = Prefix
#   )) +
#   ggdist::stat_histinterval(outline_bars = TRUE, 
#                             aes(
#                               point_size = 1.5, 
#                               slab_colour = "black"
#                             )) +
#   
#   # Custom x-axis labels
#   scale_x_continuous(
#     name = "Wellbeing",
#     limits = c(0, 100),
#     breaks = seq(0, 100, 10),
#     labels = c("0\n(Low)", seq(10, 90, 10), "100\n(High)")
#   ) +
#   
#   # Custom y-axis labels
#   scale_y_discrete(name = "Variable", 
#                    labels = prefix_labels) +
#   
#   # Adding vertical line at 50
#   geom_vline(xintercept = 50, 
#              alpha = 0.5) +
#   
#   theme_bw() +
#   
#   facet_grid(~Data.DiscountRate_Transform) +
#   
#   # Custom fill colors
#   scale_fill_manual(
#     name = "Wellbeing scores", 
#     values = custom_colors,
#     labels = prefix_labels,
#     guide = guide_legend(nrow = 3, 
#                          ncol = 3, 
#                          reverse = TRUE)
#   ) +
#   
#   scale_point_size_continuous(guide = NULL) +
#   scale_slab_colour_discrete(guide = NULL) +
#   # Remove black rectangle around legend and adjust layout
#   theme(
#     legend.position = "bottom",
#     legend.background = element_blank(),
#     legend.box.background = element_blank(),
#     panel.grid.major.x = element_blank(),
#     panel.grid.minor.x = element_blank(),
#     panel.grid.major.y = element_blank()
#   )



