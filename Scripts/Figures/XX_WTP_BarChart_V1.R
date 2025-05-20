#### D2: Plot simulated 3class WTP  ###############
# Script author: Peter King (p.king1@leeds.ac.uk)
# Last Edited: 19/03/20025
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
    level = factor(level, levels = c("Medium", "High")),
    Class = case_when(Class == 1 ~ "Class1",
                      Class == 2 ~ "Class2",
                      Class == 3 ~ "Class3"),
    facet_order = factor(paste0(Class, ": ", level),
                         levels = c("Class1: Medium", "Class1: High",
                                    "Class2: Medium", "Class2: High",
                                    "Class3: Medium", "Class3: High")))


# **********************************************************************************
#### Section 3: Add weighted WTP ####
# **********************************************************************************


# Split data by Class
class1_data <- TestPlot %>% filter(Class == "Class1")
class2_data <- TestPlot %>% filter(Class == "Class2")
class3_data <- TestPlot %>% filter(Class == "Class3")

# Define weights
weights <- c(Class1 = 0.4233, Class2 = 0.2494, Class3 = 0.3273)

# Get unique combinations of attribute, level, insect
combos <- TestPlot %>% 
  dplyr::select(attribute, level, insect) %>% 
  distinct()

# Calculate weighted sums for each combination
class4_rows <- lapply(1:nrow(combos), function(i) {
  attr <- combos$attribute[i]
  lvl <- combos$level[i]
  ins <- combos$insect[i]
  
  # Get matching rows from each class
  c1_row <- class1_data %>% filter(attribute == attr, level == lvl, insect == ins)
  c2_row <- class2_data %>% filter(attribute == attr, level == lvl, insect == ins)
  c3_row <- class3_data %>% filter(attribute == attr, level == lvl, insect == ins)
  
  # Calculate weighted values for each y column
  y_cols <- c("y0", "y25", "y50", "y75", "y100", "mean")
  weighted_vals <- sapply(y_cols, function(col) {
    weighted.mean(c(c1_row[[col]], c2_row[[col]], c3_row[[col]]), 
                  w = weights)
  })
  
  # Create new row
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
    mean = weighted_vals["mean"],
    facet_order = paste0("Class4: ", lvl)
  )
  
  return(new_row)
}) %>% bind_rows()

# Combine with original data
TestPlot_with_class4 <- bind_rows(TestPlot, class4_rows)


# Modify facet labels for Class4
class4_rows <- class4_rows %>%
  mutate(facet_order = case_when(
    level == "Medium" ~ "Weighted WTP: Medium",
    level == "High" ~ "Weighted WTP: High",
    TRUE ~ facet_order
  ))

# Combine with original data
TestPlot_with_class4 <- bind_rows(TestPlot, class4_rows)

# For plotting with medium levels on left and high on right
# Create a new factor for facet_order with custom ordering
TestPlot_with_class4 <- TestPlot_with_class4 %>%
  mutate(facet_order = factor(facet_order, levels = c(
    "Class1: Medium", "Class1: High",
    "Class2: Medium", "Class2: High",
    "Class3: Medium", "Class3: High",
    "Weighted WTP: Medium", "Weighted WTP: High"
  )))


# Update facet labels with new descriptive names
TestPlot_with_class4 <- TestPlot_with_class4 %>%
  mutate(facet_order = factor(as.character(facet_order), 
                              levels = c(
                                "Class1: Medium", "Class1: High",
                                "Class2: Medium", "Class2: High",
                                "Class3: Medium", "Class3: High",
                                "Weighted WTP: Medium", "Weighted WTP: High"
                              ),
                              labels = c(
                                "Class 1 (Pro-insect): small increase",
                                "Class 1 (Pro-insect): large increase",
                                "Class 2 (Insect-averse): small increase",
                                "Class 2 (Insect-averse): large increase",
                                "Class 3 (Ambivalent): small increase",
                                "Class 3 (Ambivalent): large increase",
                                "All classes weighted: small increase",
                                "All classes weighted: large increase"
                              )))

# Verify the new labels
TestPlot_with_class4$facet_order %>% tabyl()



## Export summayr table here
TestPlot_with_class4 %>% 
  dplyr::select(attribute, level, insect, Class, mean) %>% 
  pivot_wider(names_from = insect, values_from = mean) %>% 
  arrange(attribute, desc(level), Class) %>% 
  dplyr::mutate(
    Bee = sprintf("%.2f", Bee),
    Beetle = sprintf("%.2f", Beetle),
    Wasp = sprintf("%.2f", Wasp)
  ) %>% 
  write.csv(quote = FALSE, row.names = FALSE)




# **********************************************************************************
#### Section 4: Setup plot ####
# **********************************************************************************


TextSize <- 16


TextSetup <- element_text(size = TextSize,
                          colour = "black",
                          family = "serif")

# custom_colors <- RColorBrewer::brewer.pal(n = 9, name = "Purples")[c(3, 6, 9)]
custom_colors <- RColorBrewer::brewer.pal(9, "Blues")[c(4, 6, 8)]




# **********************************************************************************
#### Section 5: Make plot ####
# **********************************************************************************



# Plot with free_y instead of free_x, and without coord_flip
D2_Truncated_LC_3C_MXL_NoDR_V1_SimulatedMeans_barcharrt_V1 <- 
  TestPlot_with_class4 %>% 
  ggplot(
    aes(x = attribute, 
        y = y50, 
        fill = insect)) +

  geom_bar(stat = "identity", 
           position = "dodge",
           colour = "black",
           linewidth = 0.1) +
  geom_errorbar(aes(ymin = y25, 
                    ymax = y75), 
                position = position_dodge(width = 0.9), 
                width = 0.25) +
  
  #   geom_errorbar(aes(
  #   ymin = y0,
  #   ymax = y100),
  #   width = 0.5,
  #   position = position_dodge(1)) +
  # geom_boxplot(position = position_dodge(1),
  #              varwidth = 0.5,
  #              outlier.shape = NA,
  #              aes(
  #                ymin = y0,
  #                lower = y25,
  #                middle = y50,
  #                upper = y75,
  #                ymax = y100,
  #              ),
  #              stat = "identity"
  # ) +
  facet_wrap(~ facet_order, 
             ncol = 2, 
             scales = "free_y") +
  
  theme_bw() +
  
  ylab("Marginal WTP (GBP) in income tax\nper household, per annum") +
  
  
  scale_x_discrete(name = "Cultural Ecosystem Service") +
  
  scale_fill_manual(
    name = "Insect",
    values = brewer.pal(9, "Blues")[c(3, 6, 8)],
    
    label = c(
      "Bee",
      "Beetle",
      "Wasp"),
    
    guide = guide_legend(reverse = FALSE)) +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 1.25) +
  geom_vline(xintercept = 1.5, alpha = 0.25) +
  geom_vline(xintercept = 2.5, alpha = 0.25) +
  theme(
    legend.position = "bottom",
    legend.background = element_blank(),
    strip.background.x = element_rect(fill = "white"),
    strip.text.x = TextSetup,
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    
    axis.text.x = TextSetup,
    axis.text.y = TextSetup,
    axis.title.y = TextSetup,
    axis.title.x = TextSetup,
    legend.text = TextSetup,
    legend.title = TextSetup
    
  )



# **********************************************************************************
#### Section 6: Export plot ####
# **********************************************************************************


# Date <- gsub(pattern = "-",replacement = "_",Sys.Date())
## Save output in highest DPI
ggsave(
  D2_Truncated_LC_3C_MXL_NoDR_V1_SimulatedMeans_barcharrt_V1,
  device = "jpeg",
  filename = here("OtherOutput/Figures", "D2_Truncated_LC_3C_MXL_NoDR_V1_SimulatedMeans_barcharrt_V1.jpg"),
  width = 30,
  height = 25,
  units = "cm",
  dpi = 500
)


