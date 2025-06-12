#### Meadows: Table X  ###############
# AUTHOR: Peter King (p.king1@leeds.ac.uk)
# LAST CHANGE: 22/02/2025
# FUNCTION: To simulate means
# THANKS: Thanks RCSD!
## WARNING VERY TIME INTENSIVE

# *************************************************************
#### Section 0: Setup librarires including CMDLR ####
# *************************************************************

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



Model <- here("CEOutput/Main/LCM", 
              "D2_Truncated_LC_3C_MXL_NoDR_V1_model.rds") %>% readRDS()


# *************************************************************
#### Section 2: Define simulator function ####
# *************************************************************

Simulator <- function(Model, 
                      Class,
                      insect,
                      wellbeing) {
  
  # Initialize simulation parameters
  # N_Reps_KrinskyRobb <- 100000
  # N_Reps_Draws <- 1000000
  N_Reps_KrinskyRobb <- 100
  N_Reps_Draws <- 100
  
  # Define attributes and levels
  names <- c(
    "Encounter_Medium",
    "Encounter_High",
    "Existence_Medium",
    "Existence_High",
    "Bequest_Medium",
    "Bequest_High"
  )
  
  # Initialize matrices for WTP and random draws
  wtp_matrices <- lapply(names, function(x) matrix(0, nrow = N_Reps_KrinskyRobb, ncol = 1))
  
  # Initialize draws for both base and interaction terms
  all_params <- c(names, paste0("Int_", 
                                rep(names, each=2), 
                                "_", 
                                rep(c("Bee", "Wasp"), 
                                    times=length(names))))
  draw_matrices <- lapply(all_params, function(x) rnorm(N_Reps_Draws, 0, 1))
  
  # draw_matrices <- lapply(names, function(x) rnorm(N_Reps_Draws, 0, 1))
  names(wtp_matrices) <- names
  names(draw_matrices) <- all_params
  
  # Extract model parameters
  NParams <- length(Model$estimate)
  mean_v <- Model$estimate[1:NParams]
  covar <- as.matrix(Model$robvarcov)
  
  # Draw from multivariate normal distribution
  model_draws <- mvtnorm::rmvnorm(n = N_Reps_KrinskyRobb,
                                  mean = mean_v,
                                  sigma = covar)
  model_draws <- as.matrix(model_draws)
  
  # Parameter name mapping
  # param_mapping <- list(
  #   "Appearance_W" = "Appearance_W",
  #   "Appearance_WYB" = "Appearance_WYB",
  #   "Appearance_WYBPR" = "Appearance_WYBPR",
  #   "Native_Decrease" = "NativeDecrease",
  #   "Native_Increase" = "NativeIncrease",
  #   "Plants_5" = "Plants5",
  #   "Plants_10" = "Plants10",
  #   "PollinatorQuality_Medium" = "PollinatorQualityMedium",
  #   "PollinatorQuality_High" = "PollinatorQualityHigh"
  # )
  # 
  wtp_results <- vector("list", N_Reps_KrinskyRobb)
  
  # Main simulation loop
  for (i in 1:N_Reps_KrinskyRobb) {
    wtp_results[[i]] <- vector("list", length(names))
    names(wtp_results[[i]]) <- names
    
    for (param in names) {

      # Base parameter names
      mu_name <- paste0("mu_", param, "_Class", Class)
      sig_name <- paste0("sig_", param, "_Class", Class)
      
      Int_name_Insect <- ifelse(insect == "Beetle", 
                                "Int_LV_Beetle_", ## Default to beetle
                                paste0("Int_LV_", insect, "_")) ## else append insect name
      Int_name <- paste0(Int_name_Insect, param, "_Class", Class)
      
  
      # First calculate base WTP (beetle as reference)
      base_wtp <- model_draws[i, mu_name] + 
        model_draws[i, sig_name] * draw_matrices[[param]] +
        model_draws[i, Int_name] * wellbeing  # Biowell placeholder
      
      # Now adjust for insect-specific effects (if not beetle)
      if (insect == "Bee" || insect == "Wasp") {
        # Get interaction parameter names
        int_mu_name <- paste0("mu_Int_", param, "_", insect, "_Class", Class)
        int_sig_name <- paste0("sig_Int_", param, "_", insect, "_Class", Class)
        
        # Add interaction effect
        insect_effect <- model_draws[i, int_mu_name] + 
          model_draws[i, int_sig_name] * draw_matrices[[paste0("Int_", param, "_", insect)]]
        
        # Total WTP is base + interaction
        total_wtp <- base_wtp + insect_effect
      } else {
        # For beetle, just use base
        total_wtp <- base_wtp
      }
      
      
      
      # Store mean WTP for this iteration
      wtp_results[[i]][[param]] <- mean(total_wtp)
    }
  }
  
  # Combine results into a data frame
  Output <- rbindlist(wtp_results, use.names = TRUE, fill = TRUE) %>% 
    data.frame()
  
  # Add metadata columns
  Output$insect <- insect

  return(Output)
  
}


# *************************************************************
#### Section 3: Run simulator for all types ####
# *************************************************************



Classes <- c(1, 2, 3)
Insects <- c("Beetle", "Bee", "Wasp")
# cities <- c("C1", "C2")
# Define wellbeing values to simulate
wellbeing_levels <- c(-2, -1, 0, 1, 2)  # Standard deviations from mean

# Get all combinations of classes, insects, and wellbeing levels
param_grid <- expand.grid(
  Class = Classes,
  insect = Insects,
  wellbeing = wellbeing_levels
)

# Apply the Simulator function to each combination
results <- lapply(1:nrow(param_grid), function(i) {
  class <- param_grid$Class[i]
  insect <- param_grid$insect[i]
  wellbeing <- param_grid$wellbeing[i]
  
  # Modify your simulator to accept wellbeing parameter
  sim_result <- Simulator(Model = Model, Class = class, insect = insect, wellbeing = wellbeing)
  
  # Add metadata to identify the combination
  sim_result$Class <- class
  sim_result$insect <- insect
  sim_result$wellbeing <- wellbeing
  
  return(sim_result)
})

# Combine all results into a single data frame
all_results <- do.call(rbind, results)

library(parallel)
results <- mclapply(1:nrow(param_grid), function(i) {...}, mc.cores = detectCores()-1)

# *************************************************************
#### Section 5: Export data ####
# *************************************************************



all_results %>% fwrite(sep = ",",
                       quote = FALSE,
                       paste0(here(
                         "CEOutput/Main/LCM",
                         "D2_Truncated_LC_3C_MXL_NoDR_V2_SimulatedMeans.csv"
                       )))




all_results <- here("CEOutput/Main/LCM", "D2_Truncated_LC_3C_MXL_NoDR_V2_SimulatedMeans_Wide.csv") %>% 
  fread() %>% data.frame()


all_results <- here("CEOutput/Main/LCM", "D2_Truncated_LC_3C_MXL_NoDR_V3_SimulatedMeans_Wide.csv") %>% 
  fread() %>% data.frame()



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



D2_Truncated_LC_3C_MXL_NoDR_V1_SimulatedMeans_Wellbeing_Points_V0 <-
  
  all_results_long %>% 
  mutate(Class = case_when(
    Class == 1 ~ "Class 1 (Pro-insect)",
    Class == 2 ~ "Class 2 (Insect-averse)",
    Class == 3 ~ "Class 3 (Ambivalent)"
  )) %>% 
  
  ggplot(
       aes(
         x = wellbeing,
         y = wtp,
         colour = level,
         group = level
       )) +
  
  stat_summary(fun = mean, geom = "point", size = 2) +
  
  stat_summary(fun = mean, geom = "line") +
  
  stat_summary(fun.data = mean_se,
               geom = "errorbar",
               width = 0.2) +
  
  facet_grid(Class ~ insect + attribute,
             scales = "free_y") +
  
  labs(
    x = "Wellbeing Level",
    y = "Willingness to Pay (WTP)",
    # title = "WTP by Wellbeing Level, Attribute, Insect Type and Class",
    colour = "Level"
  ) +

  scale_colour_manual(values = custom_colors) +
  # scale_colour_manual(values = c("blue", "red")) +
  
  theme_bw() +
  
  geom_hline(yintercept = 0, alpha = 0.5) +
  geom_vline(xintercept = 3, alpha = 0.5) +
  
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
      angle = 55,
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



ggsave(
  D2_Truncated_LC_3C_MXL_NoDR_V1_SimulatedMeans_Wellbeing_Points_V0,
  device = "jpeg",
  filename = here("OtherOutput/Figures",
                  "D2_Truncated_LC_3C_MXL_NoDR_V1_SimulatedMeans_Wellbeing_Points_V0.jpg"),
  width = 30,
  height = 25,
  units = "cm",
  dpi = 500
)


# *************************************************************
#### NEW APPROACH  ####
# *************************************************************


D2_Truncated_LC_3C_MXL_NoDR_V1_SimulatedMeans_Wellbeing_Points_V1 <-
  
  all_results_long %>%
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
  
  geom_line(size = 0.8) +
  
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
  
  geom_hline(yintercept = 0, alpha = 0.5) +
  geom_vline(xintercept = 3, alpha = 0.5) +
  
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
      angle = 55,
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



ggsave(
  D2_Truncated_LC_3C_MXL_NoDR_V1_SimulatedMeans_Wellbeing_Points_V1,
  device = "jpeg",
  filename = here("OtherOutput/Figures",
                  "D2_Truncated_LC_3C_MXL_NoDR_V1_SimulatedMeans_Wellbeing_Points_V1.jpg"),
  width = 30,
  height = 25,
  units = "cm",
  dpi = 500
)























## BOXPLOT VERSION
all_results_long %>% 
  ggplot(aes(x = wellbeing, y = wtp, fill = level)) +
  geom_boxplot(position = position_dodge(width = 0.8), width = 0.7, outliers = FALSE, notch = TRUE) +
  facet_grid(attribute ~ insect + paste0("Class", Class)) +
  labs(
    x = "Wellbeing Level",
    y = "Willingness to Pay (WTP)",
    title = "WTP Distribution by Wellbeing Level, Attribute, Insect Type and Class",
    fill = "Level"
  ) +
  theme_bw() +
  scale_colour_manual(values = c("blue", "red")) +
  geom_hline(yintercept = 0, alpha = 0.5) +
  geom_vline(xintercept = 3, alpha = 0.5) +
  theme(legend.position = "bottom",
        strip.background = element_rect(fill = "white"),
        strip.text = element_text(face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_cartesian(ylim = c(-100, 100))


## PLOTTING JUST WTP 
all_results_long %>%
  filter(wellbeing == 0) %>% 
  ggplot(aes(x = attribute, y = wtp, fill = level)) +
  geom_boxplot(position = position_dodge(width = 0.8), width = 0.7, outliers = FALSE, notch = TRUE) +
  facet_wrap(insect ~ paste0("Class", Class), scales = "free_y") +
  labs(
    x = "Attribute",
    y = "Willingness to Pay (WTP)",
    title = "WTP Distribution for Wellbeing = 0 by Attribute, Insect Type and Class",
    fill = "Level"
  ) +
  theme_bw() +
  scale_fill_manual(values = c("blue", "red")) +
  geom_hline(yintercept = 0, alpha = 0.5) +
  theme(legend.position = "bottom",
        strip.background = element_rect(fill = "white"),
        strip.text = element_text(face = "bold"))







library(tidyverse)
library(RColorBrewer)

# Define text setup (missing from your code snippet)
TextSetup <- element_text(size = 10, face = "plain")

TextSize <- 12


TextSetup <- element_text(size = TextSize,
                          colour = "black",
                          family = "serif")


# Process all_results data to match the structure in your example
PlotData <- all_results %>%
  filter(wellbeing == 0) %>% 
  # First pivot to long format
  pivot_longer(
    cols = c(Encounter_Medium, Encounter_High, Existence_Medium, Existence_High, 
             Bequest_Medium, Bequest_High),
    names_to = "attribute_level",
    values_to = "value"
  ) %>%
  # Split attribute_level into attribute and level
  separate(attribute_level, into = c("attribute", "level"), sep = "_") %>%
  # Calculate quantiles by attribute, level, insect, and Class
  summarise(.by = c(attribute, level, insect, Class),
            y0 = quantile(value, 0.025),
            y25 = quantile(value, 0.25),
            y50 = quantile(value, 0.50),
            y75 = quantile(value, 0.75),
            y100 = quantile(value, 0.975))

# Create plot matching the example style
all_results_plot <- PlotData %>%
  mutate(
    attribute = factor(attribute, levels = c("Encounter", "Existence", "Bequest")),
    level = factor(level, levels = c("Medium", "High")),
    # Assuming Class is numeric in all_results, convert to similar format as example
    Class = factor(paste0("Class", Class), levels = c("Class1", "Class2", "Class3")),
    facet_order = factor(paste0(Class, ": ", level),
                         levels = c("Class1: Medium", "Class1: High",
                                    "Class2: Medium", "Class2: High",
                                    "Class3: Medium", "Class3: High"))
  ) %>%
  ggplot(aes(x = attribute,
             fill = insect)) +
  geom_errorbar(aes(
    ymin = y0,
    ymax = y100),
    width = 0.5,
    position = position_dodge(1)) +
  geom_boxplot(position = position_dodge(1),
               varwidth = 0.5,
               outlier.shape = NA,
               aes(
                 ymin = y0,
                 lower = y25,
                 middle = y50,
                 upper = y75,
                 ymax = y100,
               ),
               stat = "identity"
  ) +
  
  facet_wrap( ~ facet_order,
              ncol = 2,
              nrow = 3,
              scales = "free_x") +
  
  theme_bw() +
  
  ylab("Marginal WTP (GBP) in income tax per household, per annum") +
  
  scale_y_continuous(breaks = seq(-150, 50, 25)) +
  
  scale_x_discrete(name = "Cultural Ecosystem Service") +
  
  scale_fill_manual(
    name = "Insect",
    values = brewer.pal(9, "Blues")[c(3, 6, 8)],
    guide = guide_legend(reverse = FALSE)) +
  
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 1.25) +
  geom_vline(xintercept = 1.5, alpha = 0.25) +
  geom_vline(xintercept = 2.5, alpha = 0.25) +
  theme(
    legend.position = "bottom",
    legend.background = element_blank(),
    
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
    
  ) +
  coord_flip()

# Display the plot
all_results_plot