library(parallel)
library(data.table)
library(here)
library(magrittr)
library(mvtnorm)
library(future)
library(future.apply)

# Set up parallel environment
n_cores <- detectCores() - 1
RNGkind("L'Ecuyer-CMRG")  # For reproducible parallel random numbers
set.seed(123)

# Load model
Model <- here("CEOutput/Main/LCM", "D2_Truncated_LC_3C_MXL_NoDR_V1_model.rds") %>% readRDS()

# Simulator function - optimised
Simulator <- function(Model, Class, insect, wellbeing) {
  # Reduced iterations for testing - increase for production
  N_Reps_KrinskyRobb <- 10000
  N_Reps_Draws <- 10000
  
  # Define attributes and levels
  names <- c("Encounter_Medium", "Encounter_High", "Existence_Medium", 
             "Existence_High", "Bequest_Medium", "Bequest_High")
  
  # Extract model parameters once
  mean_v <- Model$estimate
  covar <- as.matrix(Model$robvarcov)
  
  # Draw from multivariate normal distribution - do this once
  model_draws <- mvtnorm::rmvnorm(n = N_Reps_KrinskyRobb,
                                  mean = mean_v,
                                  sigma = covar)
  
  # Pre-generate all random draws at once
  all_params <- c(names, paste0("Int_", 
                                rep(names, each=2), 
                                "_", 
                                rep(c("Bee", "Wasp"), 
                                    times=length(names))))
  
  # Generate all normal draws at once
  set.seed(123 + Class + match(insect, Insects) + wellbeing*10)  # Different seed for each combo
  all_draws <- matrix(rnorm(N_Reps_Draws * length(all_params)), 
                      nrow = N_Reps_Draws, 
                      ncol = length(all_params))
  colnames(all_draws) <- all_params
  
  # Pre-allocate results matrix
  result_mat <- matrix(NA, nrow = N_Reps_KrinskyRobb, ncol = length(names))
  colnames(result_mat) <- names
  
  # Main simulation loop - vectorised where possible
  for (i in 1:N_Reps_KrinskyRobb) {
    for (j in seq_along(names)) {
      param <- names[j]
      
      # Parameter names
      mu_name <- paste0("mu_", param, "_Class", Class)
      sig_name <- paste0("sig_", param, "_Class", Class)
      
      Int_name_Insect <- ifelse(insect == "Beetle", 
                                "Int_LV_Beetle_", 
                                paste0("Int_LV_", insect, "_"))
      Int_name <- paste0(Int_name_Insect, param, "_Class", Class)
      
      # Calculate base WTP
      base_wtp <- model_draws[i, mu_name] + 
        model_draws[i, sig_name] * all_draws[, param] +
        model_draws[i, Int_name] * wellbeing
      
      # Adjust for insect-specific effects
      if (insect != "Beetle") {
        int_mu_name <- paste0("mu_Int_", param, "_", insect, "_Class", Class)
        int_sig_name <- paste0("sig_Int_", param, "_", insect, "_Class", Class)
        
        insect_effect <- model_draws[i, int_mu_name] + 
          model_draws[i, int_sig_name] * all_draws[, paste0("Int_", param, "_", insect)]
        
        total_wtp <- base_wtp + insect_effect
      } else {
        total_wtp <- base_wtp
      }
      
      # Store mean WTP
      result_mat[i, j] <- mean(total_wtp)
    }
  }
  
  # Convert to data.table directly (avoid data.frame conversion)
  Output <- as.data.table(result_mat)
  Output[, insect := insect]
  
  return(Output)
}

# Define parameter grid
Classes <- c(1, 2, 3)
Insects <- c("Beetle", "Bee", "Wasp")
wellbeing_levels <- c(-2, -1, 0, 1, 2)

param_grid <- expand.grid(
  Class = Classes,
  insect = Insects,
  wellbeing = wellbeing_levels
)

# Setup parallel backend with proper RNG
plan(multisession, workers = detectCores() - 1)

# Add future.seed=TRUE to ensure proper random number generation
results <- future_lapply(1:nrow(param_grid), function(i) {
  class <- param_grid$Class[i]
  insect <- param_grid$insect[i]
  wellbeing <- param_grid$wellbeing[i]
  
  sim_result <- Simulator(Model = Model, Class = class, insect = insect, wellbeing = wellbeing)
  
  # Add metadata
  sim_result[, Class := class]
  sim_result[, wellbeing := wellbeing]
  
  return(sim_result)
}, future.seed = TRUE)  # This is the key addition


# Combine results efficiently
all_results <- rbindlist(results, fill = TRUE)


# Make sure all columns are properly formatted
all_results <- all_results[, lapply(.SD, function(x) if(is.numeric(x)) round(x, 4) else x)] %>% 
  data.frame


# Get column names (excluding the metadata columns)
value_cols <- setdiff(colnames(all_results), c("Class", "insect", "wellbeing"))

# Set column order
setcolorder(all_results, c("Class", "insect", "wellbeing", value_cols))

# If you want to reshape to a more analysis-friendly format
# Convert from wide to long format
all_results_long <- melt(all_results, 
                         id.vars = c("Class", "insect", "wellbeing"),
                         variable.name = "parameter",
                         value.name = "value")

# Export both formats
fwrite(all_results, 
       file = here("CEOutput/Main/LCM", "D2_Truncated_LC_3C_MXL_NoDR_V2_SimulatedMeans_Wide.csv"), 
       sep = ",", 
       quote = FALSE)


fwrite(all_results_long, 
       file = here("CEOutput/Main/LCM", "D2_Truncated_LC_3C_MXL_NoDR_V2_SimulatedMeans_Long.csv"), 
       sep = ",", 
       quote = FALSE)





# *************************************************************
#### Section X: Plot setup  ####
# *************************************************************


TextSize <- 12


TextSetup <- element_text(size = TextSize,
                          colour = "black",
                          family = "serif")

# custom_colors <- RColorBrewer::brewer.pal(n = 9, name = "Purples")[c(3, 6, 9)]
custom_colors <- RColorBrewer::brewer.pal(9, "Blues")[c(4, 6, 8)]


Test <-
  here("CEOutput/Main/LCM",
       "D2_Truncated_LC_3C_MXL_NoDR_V2_SimulatedMeans_Wide.csv") %>% fread() %>% data.frame()



# Weights as given
weights <- c(Class1 = 0.4233, Class2 = 0.2494, Class3 = 0.3273)

Summariser <- function(Variable) {
  paste0(
    Variable %>% mean() %>% sprintf("%.2f", .),
    " (",
    Variable %>% quantile(., c(0.025)) %>% sprintf("%.2f", .),
    " - ",
    Variable %>% quantile(., c(0.975)) %>% sprintf("%.2f", .),
    ")"
  )
}




TestTable <- Test %>%
  dplyr::filter(wellbeing == 0) %>%
  pivot_longer(cols = 4:9) %>%
  tidyr::separate(
    name,
    into = c("attribute", "level"),
    sep = "_"
  ) %>%
  summarise(.by = c(attribute, level, insect, Class),
            Mean = Summariser(value))  %>% 
  pivot_wider(names_from = insect, values_from = Mean) %>% 
  arrange(desc(attribute), level, Class) %>% 
  dplyr::select(attribute, level, Class, Bee, Beetle, Wasp)


# First, extract the numeric values from your Mean column by processing the string format
TestTableExpanded <- Test %>%
  dplyr::filter(wellbeing == 0) %>%
  pivot_longer(cols = 4:9) %>%
  tidyr::separate(
    name,
    into = c("attribute", "level"),
    sep = "_"
  ) %>%
  group_by(attribute, level, insect, Class) %>%
  summarise(
    Mean_value = mean(value),
    CI_lower = quantile(value, 0.025),
    CI_upper = quantile(value, 0.975),
    .groups = "drop"
  )

# Calculate weighted averages for each combination of attribute, level, and insect
WeightedData <- TestTableExpanded %>%
  group_by(attribute, level, insect) %>%
  summarise(
    Mean_value = sum(Mean_value * weights[paste0("Class", Class)]),
    CI_lower = sum(CI_lower * weights[paste0("Class", Class)]),
    CI_upper = sum(CI_upper * weights[paste0("Class", Class)]),
    Class = 4,
    .groups = "drop"
  )

# Format the weighted data in the same format as original
WeightedData <- WeightedData %>%
  mutate(Mean = paste0(
    sprintf("%.2f", Mean_value),
    " (",
    sprintf("%.2f", CI_lower),
    " - ",
    sprintf("%.2f", CI_upper),
    ")"
  ))


# Format the weighted data in the same format as original
TestTableExpanded_Formatted <- TestTableExpanded %>%
  mutate(Mean = paste0(
    sprintf("%.2f", Mean_value),
    " (",
    sprintf("%.2f", CI_lower),
    " - ",
    sprintf("%.2f", CI_upper),
    ")"
  )) %>% 
  dplyr::select(attribute, level, insect, Class, Mean)


# Add the same columns as your original summary
WeightedData <- WeightedData %>%
  dplyr::select(attribute, level, insect, Class, Mean)

# Combine with original data
Combined <- bind_rows(
  TestTableExpanded_Formatted,
  WeightedData
)

# Create your final table with Class4 included
FinalTable <- Combined %>%
  pivot_wider(names_from = insect, values_from = Mean) %>%
  arrange(attribute, level, Class) %>%
  dplyr::select(attribute, level, Class, Bee, Beetle, Wasp)



FinalTable %>% write.csv(quote = FALSE, row.names = FALSE)




TestTable <- Test %>%
  dplyr::filter(wellbeing == 0) %>%
  pivot_longer(cols = 4:9) %>%
  tidyr::separate(
    name,
    into = c("attribute", "level"),
    sep = "_"
  ) %>%
  summarise(.by = c(attribute, level, insect, Class),
            Mean = Summariser(value))


TestTable %>% 
  pivot_wider(names_from = insect, values_from = Mean) %>% 
  arrange(desc(attribute), level, Class) %>% 
  dplyr::select(attribute, level, Class, Bee, Beetle, Wasp) %>% 
  dplyr::mutate(
    Bee = sprintf("%.2f", Bee),
    Beetle = sprintf("%.2f", Beetle),
    Wasp = sprintf("%.2f", Wasp)
    )









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
            y100 = quantile(value, 0.975)) %>% 
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
  
  
D2_Truncated_LC_3C_MXL_NoDR_V1_SimulatedMeans_Boxplot_V1 <- TestPlot %>%  
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
  
  
  
  

ggsave(
  D2_Truncated_LC_3C_MXL_NoDR_V1_SimulatedMeans_Boxplot_V1,
  device = "jpeg",
  filename = here("OtherOutput/Figures",
                  "D2_Truncated_LC_3C_MXL_NoDR_V1_SimulatedMeans_Boxplot_V1.jpg"),
  width = 20,
  height = 25,
  units = "cm",
  dpi = 500
)

  
  
  
