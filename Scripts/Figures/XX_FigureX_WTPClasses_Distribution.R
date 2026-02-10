#### D2: Plot simulated 3class WTP  ###############
# Script author: Peter King (p.king1@leeds.ac.uk)
# Last Edited: 30/01/20026
# COMMENTS: Plotting and summarising WTP
# - updated with weighted


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



Test <-
  here("CEOutput/Main/LCM",
       "D2_Truncated_LC_3C_MXL_NoDR_V3_SimulatedMeans_Wide.csv") %>% fread() %>% data.frame()



# **********************************************************************************
#### Section 2: Reshape ####
# **********************************************************************************




# Modified Section 2: Reshape - need to keep level as a variable
TestPlot <- Test %>% 
  dplyr::filter(wellbeing == 0) %>% 
  pivot_longer(cols = 4:9) %>% 
  tidyr::separate(
    name,
    into = c("attribute", "level"),
    sep = "_"
  ) %>% 
  summarise(.by = c(attribute, level, insect, Class),
            y0 = quantile(value, 0.025),
            y25 = quantile(value, 0.25),
            y50 = quantile(value, 0.50),
            y75 = quantile(value, 0.75),
            y100 = quantile(value, 0.975),
            mean = mean(value)) %>% 
  mutate(
    attribute = factor(attribute, levels = c("Encounter", "Existence", "Bequest")),
    level = case_when(
      level == "Medium" ~ "Small",
      level == "High" ~ "Large"
    ),
    level = factor(level, levels = c("Small", "Large")),
    Class = case_when(Class == 1 ~ "Class1",
                      Class == 2 ~ "Class2",
                      Class == 3 ~ "Class3"))

# Section 3: Add weighted WTP - keep level in the process
class1_data <- TestPlot %>% filter(Class == "Class1")
class2_data <- TestPlot %>% filter(Class == "Class2")
class3_data <- TestPlot %>% filter(Class == "Class3")

weights <- c(Class1 = 0.4233, Class2 = 0.2494, Class3 = 0.3273)

combos <- TestPlot %>% 
  dplyr::select(attribute, level, insect) %>% 
  distinct()

class4_rows <- lapply(1:nrow(combos), function(i) {
  attr <- combos$attribute[i]
  lvl <- combos$level[i]
  ins <- combos$insect[i]
  
  c1_row <- class1_data %>% filter(attribute == attr, level == lvl, insect == ins)
  c2_row <- class2_data %>% filter(attribute == attr, level == lvl, insect == ins)
  c3_row <- class3_data %>% filter(attribute == attr, level == lvl, insect == ins)
  
  y_cols <- c("y0", "y25", "y50", "y75", "y100", "mean")
  weighted_vals <- sapply(y_cols, function(col) {
    weighted.mean(c(c1_row[[col]], c2_row[[col]], c3_row[[col]]), 
                  w = weights)
  })
  
  new_row <- tibble(
    attribute = attr,
    level = lvl,
    insect = ins,
    Class = "Class4",
    y0 = weighted_vals["y0"],
    y25 = weighted_vals["y25"],
    y50 = weighted_vals["y50"],
    y75 = weighted_vals["y75"],
    y100 = weighted_vals["y100"],
    mean = weighted_vals["mean"]
  )
  
  return(new_row)
}) %>% bind_rows()

TestPlot_with_class4 <- bind_rows(TestPlot, class4_rows)

# Create class labels
TestPlot_with_class4 <- TestPlot_with_class4 %>%
  mutate(
    Class = factor(Class, 
                   levels = c("Class1", "Class2", "Class3", "Class4"),
                   labels = c("Class 1 (Pro-insect)",
                              "Class 2 (Insect-averse)", 
                              "Class 3 (Ambivalent)",
                              "All classes weighted"))
  )




# **********************************************************************************
#### Section 4: Setup plot ####
# **********************************************************************************


TextSize <- 16


TextSetup <- element_text(size = TextSize,
                          colour = "black",
                          family = "serif")

# custom_colors <- RColorBrewer::brewer.pal(n = 9, name = "Purples")[c(3, 6, 9)]
custom_colors <- RColorBrewer::brewer.pal(9, "Blues")[c(4, 6, 8)]

library(ggh4x)  # You'll need this package


# **********************************************************************************
#### Section 5:  plot ####
# **********************************************************************************


plot_data <- Test %>%
  filter(wellbeing == 0) %>%
  pivot_longer(cols = 4:9) %>%
  separate(name, into = c("attribute", "level"), sep = "_") %>%
  mutate(
    attribute = factor(attribute, levels = c("Encounter", "Existence", "Bequest")),
    level = case_when(
      level == "Medium" ~ "Small",
      level == "High" ~ "Large"
    ),
    level = factor(level, levels = c("Small", "Large")),
    Class = factor(Class,
                   levels = c(1, 2, 3),
                   labels = c("Class 1 (Pro-insect)",
                              "Class 2 (Insect-averse)", 
                              "Class 3 (Ambivalent)"))
  )




D2_FigureX_WTPClasses_Distribution <- 
  
  plot_data %>%
  ggplot(aes(
    y = attribute %>% rev(),
    x = value,
    fill = level
  )) +
  stat_histinterval(
    position = position_dodge(width = 0.9),
    point_color = "gray75",
    point_fill = "black",
    .width = c(0.5, 0.95),
    outline_bars = TRUE,
    slab_colour = "black",
    slab_linewidth = 0.5,
    normalize = "panels",
    breaks = "Sturges",
    point_interval = "mean_qi"
  ) +
  facet_grid(insect ~ Class,
             scales = "free_x",
             labeller = labeller(Class = label_wrap_gen(width = 15))) +
  facetted_pos_scales(x = list(
    scale_x_continuous(limits = c(-50, 125)),
    # Class 1
    scale_x_continuous(limits = c(-200, 25)),
    # Class 2
    scale_x_continuous(limits = c(-50, 50))     # Class 3
  )) +
  
  theme_bw() +
  
  xlab("Marginal WTP (GBP) in income tax\nper household, per month") +
  
  ylab("Cultural Ecosystem Service") +
  
  scale_fill_manual(
    name = "Level",
    
    values = rev(brewer.pal(9, "Blues")[c(9, 5)]),
    
    guide = guide_legend(reverse = FALSE)
  ) +
  
  geom_vline(xintercept = 0, alpha = 0.5) +
  
  geom_hline(yintercept = 1.5, alpha = 0.5) +
  
  geom_hline(yintercept = 2.5, alpha = 0.5) +
  
  scale_y_discrete(labels = plot_data$attribute %>% unique() %>% rev()) +
  
  theme(
    legend.position = "bottom",
    
    legend.background = element_blank(),
    
    strip.background = element_rect(fill = "white"),
    
    strip.text = element_text(
      size = TextSize,
      colour = "black",
      
      family = "serif",
      face = "bold"
    ),
    
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    
    axis.text = TextSetup,
    
    axis.title = TextSetup,
    
    legend.text = TextSetup,
    
    legend.title = TextSetup
    
  )





# **********************************************************************************
#### Section X: Export plot ####
# **********************************************************************************


# Date <- gsub(pattern = "-",replacement = "_",Sys.Date())
## Save output in highest DPI
## Find Figure 2 here
ggsave(
  D2_FigureX_WTPClasses_Distribution,
  device = "jpeg",
  filename = here("OtherOutput/Figures", "D2_FigureX_WTPClasses_Distribution.jpg"),
  width = 30,
  height = 25,
  units = "cm",
  dpi = 500
)

