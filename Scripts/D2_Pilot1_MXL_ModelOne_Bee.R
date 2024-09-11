#### D2: Insects Pilot1 ####
## Function: Estimate basic MNL
## Author: Dr Peter King (p.king1@leeds.ac.uk)
## Last change: 11/09/2024
## TODO: Make sure it always outputs exactly to table



# ****************************
# Replication Information: ####
# ****************************



# ****************************
# Setup Environment: ####
# ****************************

## Libraries that will come in handy later
library(apollo)
library(dplyr)
library(magrittr)
library(ggplot2)
library(ggridges)
library(reshape2)
library(mded)
library(here)
library(data.table)

# ****************************
# Import Data: ####
# ****************************


database <- here("Data/Pilot1/",
                 "database_Step2.csv") %>% fread() %>% data.frame()


database <- database[database$Insect_PlanA == 2, ]

## Keep only relevant columns
database <- database[, c(
  "Choice",
  "Respondent",
  "Task",
  "ID",
  "Tax_PlanA",
  "Tax_PlanB",
  "Tax_PlanA_Values",
  "Tax_PlanB_Values",
  "Encounter_PlanA_Values",
  "Encounter_PlanB_Values",
  "Existence_PlanA_Values",
  "Existence_PlanB_Values",
  "Bequest_PlanA_Values",
  "Bequest_PlanB_Values",
  "Insect_PlanA",
  "Insect_PlanB",
  "Insect_PlanC"
)]


## Necessary to get apollo working
apollo_initialise()


# ****************************
# Estimation Basics: ####
# ****************************


## Note 10 cores as I'm using the University of Kent 'Tesla' HPC:
apollo_control = list(
  nCores    = 1,
  mixing    = TRUE,
  modelDescr = "D2_Pilot1_MXL_ModelOne_Bee",
  modelName  = "D2_Pilot1_MXL_ModelOne_Bee", ## Added dates last verified
  indivID    = "Respondent", ## This is the name of a column in the database indicating each unique respondent
  outputDirectory="CEOutput/MNL"
)


## Define parameters starting values:
apollo_beta = c(
  mu_ASC = 0,
  sig_ASC = 0,
  
  mu_Tax    = -3,
  sig_Tax    = 0.1,
  
  mu_Encounter_Large = 0,
  mu_Existence_Large = 0,
  mu_Bequest_Large = 0,
  
  sig_Encounter_Large = 0,
  sig_Existence_Large = 0,
  sig_Bequest_Large = 0
)


## Hold Alternative-Specific Constants for non-status-quo options at zero
apollo_fixed = c()


## Set parameters for generating draws
### Note that draws by attribute (5) or by level (9) a big difference!
apollo_draws = list(
  interDrawsType = "pmc",## Robust to using MLHS or Sobol draws
  interNDraws    = 1000, ## Same results if you use 5000 draws
  interUnifDraws = c(),
  interNormDraws = c(
    "draws_ASC",
    "draws_Tax",
    "draws_Encounter_Large",
    "draws_Existence_Large",
    "draws_Bequest_Large"
  ))


## Create random parameters
### Note lognormal for tax attribute to impose negative signs
apollo_randCoeff = function(apollo_beta, apollo_inputs){
  randcoeff = list()
  randcoeff[["ASC"]] = (mu_ASC + sig_ASC * draws_ASC )
  randcoeff[["b_Tax"]] = -exp(mu_Tax + sig_Tax * draws_Tax )
  randcoeff[["b_Encounter_Large"]] =  (mu_Encounter_Large + sig_Encounter_Large * draws_Encounter_Large )
  randcoeff[["b_Existence_Large"]] =  (mu_Existence_Large + sig_Existence_Large * draws_Existence_Large )
  randcoeff[["b_Bequest_Large"]] =  (mu_Bequest_Large + sig_Bequest_Large * draws_Bequest_Large )
  return(randcoeff)
}



apollo_inputs = apollo_validateInputs() ## Required to check inputs are fine


# ****************************
# Estimation Specification: ####
### Note: Model in WTP-space.
# ****************************

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  P = list()
  
  V = list()
  V[['A']]  = 
    b_Tax  * (Tax_PlanA_Values + 
    b_Encounter_Large * (Encounter_PlanA_Values == 30) + 
    
    b_Existence_Large * (Existence_PlanA_Values == 30) + 
    
    b_Bequest_Large * (Bequest_PlanA_Values == 30))
  
  
  V[['B']]  =  b_Tax  * (Tax_PlanB_Values + 
    b_Encounter_Large * (Encounter_PlanB_Values == 30) + 
    
    b_Existence_Large * (Existence_PlanB_Values == 30) + 
    
    b_Bequest_Large * (Bequest_PlanB_Values == 30))
  
  
  
  V[['C']]  = ASC
  
  mnl_settings = list(
    alternatives = c(A = 1, B = 2, C = 3),
    avail        = list(A = 1, B = 1, C = 1),
    choiceVar    = Choice,
    V            = V
  )
  
  ## Compute probabilities using MNL model
  P[['model']] = apollo_mnl(mnl_settings, functionality)
  
  ## Take product across observation for same individual
  P = apollo_panelProd(P, apollo_inputs, functionality)
  
  ## Average across inter-individual draws
  P = apollo_avgInterDraws(P, apollo_inputs, functionality)
  
  ## Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}


# ****************************
# Model Outputs: ####
# ****************************

#
# ## Actually estimates the model
D2_Pilot1_MXL_ModelOne_Bee = apollo_estimate(apollo_beta, 
                                              apollo_fixed, 
                                              apollo_probabilities, 
                                              apollo_inputs)

# Model output and results here alongside saving information
apollo_modelOutput(D2_Pilot1_MXL_ModelOne_Bee,
                   modelOutput_settings = list(printPVal = TRUE))



apollo_saveOutput(D2_Pilot1_MXL_ModelOne_Bee,
                  saveOutput_settings = list(printPVal=TRUE))
# saveRDS(D2_Pilot1_MXL_ModelOne_Bee, here("CEoutput/ModelOne","D2_Pilot1_MXL_ModelOne_Bee.rds"))

D2_Pilot1_MXL_ModelOne_Bee_Conditionals <- apollo_conditionals(model = D2_Pilot1_MXL_ModelOne_Bee,
                                                               apollo_probabilities, 
                                                               apollo_inputs) %>% 
  data.frame()

# ****************************
# Summarise WTP: ####
# ****************************


#
nsim <- 1000
betas <- D2_Pilot1_MXL_ModelOne_Bee$estimate
vcmat <- D2_Pilot1_MXL_ModelOne_Bee$robvarcov
#
#
draws <- mvrnorm(nsim, mu=betas, Sigma=vcmat)
#
#
sim.wtp <- matrix(0, nrow = nsim, ncol = 8)    # use this matrix to store simulated WTP distributions

for(i in 1:nsim){
  sim.wtp[i,1] <- draws[i,4]
  sim.wtp[i,2] <- draws[i,5]
  sim.wtp[i,3] <- draws[i,6]
  
}

# for(i in 1:nsim){
#   sim.wtp[i,1] <- -draws[i,4] / draws[i,2]
#   sim.wtp[i,2] <- -draws[i,5] / draws[i,2]
#   sim.wtp[i,3] <- -draws[i,6] / draws[i,2]
#   
# }

#
wtp <-
  matrix(0, nrow = 3, ncol = 3) # use this matrix to store WTP means and CIs

for(i in 1:nrow(wtp)){
  wtp[i,1] <- betas[i+3]
  wtp[i, 2:3] <- quantile(sim.wtp[, i], probs = c(0.025, 0.975))
}
round(wtp, digits=2)
WTP <- wtp %>% data.frame()
colnames(WTP) <- c("mean","lb","ub")
WTP$variable <-
  c(
    "b_Encounter_Large",
    "b_Existence_Large",
    "b_Bequest_Large"
  )
#
#
#
PlotData <- WTP %>%
  slice(match(c(
    "b_Encounter_Large",
    "b_Existence_Large",
    "b_Bequest_Large"
  ), variable)) %>%
  mutate(variable = factor(variable,
                           levels = unique(variable)))





TextSetup <- element_text(size = 10,
                          colour = "black",
                          family = "serif")


D2_Pilot1_MXL_ModelOne_Bee_Conditionals %>%
  dplyr::select(c(
    b_Encounter_Large.post.mean,
    b_Existence_Large.post.mean,
    b_Bequest_Large.post.mean,
    ASC.ID)) %>% 
  pivot_longer(cols = 1:3) %>% 
  ggplot(aes(
    x = value,
    y = name,
    fill = name,
    group = name
  )) +
  stat_boxplot(geom = "errorbar",
               width = 0.25,
               position = position_dodge(width = 0.75)) +
  geom_boxplot(outlier.shape = NA) +
  facet_wrap(~name, scales = "free_x") +
  theme_bw() +
  scale_x_continuous(name = "") +
  scale_y_discrete(name = "Attribute and level") +
  theme(
    legend.position = "bottom",
    legend.text = TextSetup,
    legend.background = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    legend.title = TextSetup,
    axis.text.x = TextSetup,
    axis.text.y = TextSetup,
    axis.title.y = TextSetup,
    axis.title.x = TextSetup
  ) 



# *********************************************************************************************************
#### END OF SCRIPT ####
## Next Step: Can we have insect in the model?
# *********************************************************************************************************

# End  -------------------------------------
