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



## Group here
PlotData <- cbind(
  "Bees_Conserved" = Data$Sliding_Bees_Conserved,
  "Bees_Encounter" = Data$Sliding_Bees_Encounter,
  "Bees_Ecology" = Data$Sliding_Bees_Ecology,
  "Bees_Important" = Data$Sliding_Bees_Important,
  
  "Beetles_Conserved" = Data$Sliding_Beetles_Conserved,
  "Beetles_Encounter" = Data$Sliding_Beetles_Encounter,
  "Beetles_Ecology" = Data$Sliding_Beetles_Ecology,
  "Beetles_Important" = Data$Sliding_Beetles_Important,
  
  "Wasps_Conserved" = Data$Sliding_Wasps_Conserved,
  "Wasps_Encounter" = Data$Sliding_Wasps_Encounter,
  "Wasps_Ecology" = Data$Sliding_Wasps_Ecology,
  "Wasps_Important" = Data$Sliding_Wasps_Important
) %>%
  data.frame() %>%
  pivot_longer(cols = everything(),
               names_to = "name",
               values_to = "value") %>%
  mutate(Insect = sub("_.*", "", name),
         # Extract text before the first underscore
         Statement = sub(".*_", "", name))       # Extract text after the first underscore)



# **********************************************************************************
#### Section 2: Setup plot ####
# **********************************************************************************


# Custom mapping for y-axis labels
prefix_labels <- c(
  "Bees_Conserved" = "Bees\nShould be conserved",
  "Bees_Encounter" = "Bees\nEncounter frequently",
  "Bees_Ecology" = "Bees\nUnderstand ecological roles",
  "Bees_Important" = "Bees\nNot important to conserve",
  "Beetles_Conserved" = "Beetles\nShould be conserved",
  "Beetles_Encounter" = "Beetles\nEncounter frequently",
  "Beetles_Ecology" = "Beetles\nUnderstand ecological roles",
  "Beetles_Important" = "Beetles\nNot important to conserve",
  "Wasps_Conserved" = "Wasps\nShould be conserved",
  "Wasps_Encounter" = "Wasps\nEncounter frequently",
  "Wasps_Ecology" = "Wasps\nUnderstand ecological roles",
  "Wasps_Important" = "Wasps\nNot important to conserve"
)


## Specify per-insect here
custom_colors <- c(
  RColorBrewer::brewer.pal(n = 4, name = "Purples"),
  RColorBrewer::brewer.pal(n = 4, name = "Greys"),
  RColorBrewer::brewer.pal(n = 4, name = "Reds"))


TextSize <- 12


TextSetup <- element_text(size = TextSize,
                          colour = "black",
                          family = "serif")


# **********************************************************************************
#### Section 3: Plot ####
# **********************************************************************************


# Plotting
FX <-  PlotData %>%
  ggplot(aes(
    x = value,
    y = Statement,
    group = name,
    fill = name
  )) +
  ggdist::stat_histinterval(outline_bars = TRUE,
                            position = "dodge",
                            slab_colour = "black",
                            aes(
                              point_size = 1.5
                            )) +
  
  # Custom x-axis labels
  scale_x_continuous(
    name = "Response\n(Strongly disagree - strongly agree)",
    limits = c(0, 100),
    breaks = seq(0, 100, 10),
    labels = seq(0, 100, 10)
  ) +
  
  # Custom y-axis labels
  scale_y_discrete(name = "Variable", 
                   labels = PlotData$Statement %>% unique()) +
  
  # Adding vertical line at 50
  geom_vline(xintercept = 50, 
             alpha = 0.5) +
  
  theme_bw() + 
  
  facet_grid( ~ Insect) +
  
  # Custom fill colors
  scale_fill_manual(
    name = "Response", 
    values = c(
      RColorBrewer::brewer.pal(n = 4, name = "Purples"),
      RColorBrewer::brewer.pal(n = 4, name = "Greys"),
      RColorBrewer::brewer.pal(n = 4, name = "Reds")),
    labels = prefix_labels,
    guide = guide_legend(nrow = 4, 
                         ncol = 3, 
                         reverse = TRUE)
  ) +
  
  scale_point_size_continuous(guide = NULL) +
  scale_slab_colour_discrete(guide = NULL) +
  # Remove black rectangle around legend and adjust layout
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

# **********************************************************************************
#### Section 3: Plot ####
# **********************************************************************************



# Date <- gsub(pattern = "-",replacement = "_",Sys.Date())
## Save output in highest DPI
ggsave(
  FX,
  device = "jpeg",
  filename = here("OtherOutput/Figures", "Sliders_Distributions_Main.jpg"),
  width = 20,
  height = 25,
  units = "cm",
  dpi = 500
)


# **********************************************************************************
#### Section Y: Sliders but facet and Y axis swapped ####
# **********************************************************************************

# PlotData %>%
#   ggplot(aes(
#     x = value,
#     y = Insect,
#     group = name,
#     fill = name
#   )) +
#   ggdist::stat_histinterval(outline_bars = TRUE,
#                             position = "dodge",
#                             slab_colour = "black",
#                             aes(
#                               point_size = 1.5
#                             )) +
#   
#   # Custom x-axis labels
#   scale_x_continuous(
#     name = "Response\n(Strongly disagree - strongly agree)",
#     limits = c(0, 100),
#     breaks = seq(0, 100, 10),
#     labels = seq(0, 100, 10)
#   ) +
#   
#   # Custom y-axis labels
#   scale_y_discrete(name = "Variable", 
#                    labels = PlotData$Insect %>% unique()) +
#   
#   # Adding vertical line at 50
#   geom_vline(xintercept = 50, 
#              alpha = 0.5) +
#   
#   theme_bw() + 
#   
#   facet_grid( ~ Statement) +
#   
#   # Custom fill colors
#   scale_fill_manual(
#     name = "Response", 
#     values = c(
#       RColorBrewer::brewer.pal(n = 4, name = "Purples"),
#       RColorBrewer::brewer.pal(n = 4, name = "Greys"),
#       RColorBrewer::brewer.pal(n = 4, name = "Reds")),
#     labels = prefix_labels,
#     guide = guide_legend(nrow = 4, 
#                          ncol = 3, 
#                          reverse = TRUE)
#   ) +
#   
#   scale_point_size_continuous(guide = NULL) +
#   scale_slab_colour_discrete(guide = NULL) +
#   # Remove black rectangle around legend and adjust layout
#   theme(
#     legend.position = "bottom",
#     legend.text = TextSetup,
#     legend.title = TextSetup,
#     axis.text.x = TextSetup,
#     axis.text.y = TextSetup,
#     axis.title.y = TextSetup,
#     strip.background = element_rect(fill = "white"),
#     strip.text = TextSetup,
#     legend.background = element_blank(),
#     panel.grid.major.x = element_blank(),
#     panel.grid.minor.x = element_blank(),
#     panel.grid.major.y = element_blank()
#   )
