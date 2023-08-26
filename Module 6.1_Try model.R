#### Module 6.1: Try model
#### Get PGLS models and variance partition
library(phylolm)
library(rr2)

## Create a function to run PGLS model
Mod.Var <- function(rv){
  ## Prepare formulas for full & reduce models
  fml.full <- formula(paste(rv, '~','ED.median')) 
  fml.reduce <- formula(paste(rv, '~', 1)) 
 
   ##PGLS (lambda) models & get the three types of R2
  r2 <- data.frame(Respond_Variable = rv, Full.model.R2 = NA, Phylogeny.R2 = NA, Predictors.R2 = NA)
  
  # 1) Full model -- to get the full model R2.
  m <- phylolm(fml.full, 
              data = phy_data$data, phy = phy_data$phy, model = 'lambda')
  r2$Full.model.R2 <- R2_lik(m)          
 
   # 2) Remove phylo structure (lambda = 0) -- to get phylogeny R2.
  nop <- data.frame(lambda = 0)
  mP <- phylolm(fml.full,
                 data = phy_data$data, phy = phy_data$phy, model = 'lambda',
                 starting.value = nop, lower.bound = nop$lambda, upper.bound = nop$lambda)
  r2$Phylogeny.R2 <- R2_lik(m, mP)    
  
  # 3) Intercept-only model (no predictors) -- to get predictors R2.
  nov <- data.frame(lambda = m$optpar)
  mV <- phylolm(fml.reduce, 
                 data = phy_data$data, phy = phy_data$phy, model = 'lambda', 
                 starting.value = nov, lower.bound = nov$lambda, upper.bound = nov$lambda) 
  r2$Predictors.R2 <- R2_lik(m, mV)   

  ## save the model results and R2 values.
  model_res <- list(m, r2) # m: Table S2-4.  r2: Table S5.
  return(model_res)
}

# Run the model
Sum.lambda <- Mod.Var('Sum.lambda')
Avg.lambda <- Mod.Var('Avg.lambda')
Latest.lambda <- Mod.Var('Latest.lambda')

# Check the result
summary(Sum.lambda[[1]]) # The lambda is alomost 0...
Sum.lambda[[2]] # the phylogeny R-squared is even lower than the useless predictor.
summary(Avg.lambda[[1]]) # Although lambda is 0.7, but the r-squared is too low.
Avg.lambda[[2]] # Phylogeny only has a R-squared as low as 0.007, so the phylogeny is not important, either.
summary(Latest.lambda[[1]]) # The lambda is alomost 0...
Latest.lambda[[2]] # the phylogeny R-squared is even lower than the useless predictor.
