#### Module 6.2: 50-tree Model
#### Get PGLS models and variance partition
library(ape)
library(caper)
library(phylolm)
library(rr2)

#### Create a function to run the model 50 times using different trees
Mod.Var.50T <- function(rv){
  fml.full <- formula(paste(rv, '~', 'ED.median')) # + Freq only for Sum.lambda
  fml.reduce <- formula(paste(rv, '~', 1))
  model_coef <- data.frame()
  r2 <- data.frame(Full.model.R2 = NA, Phylogeny.R2 = NA, Predictors.R2 = NA)
  for (i in 1:50){
    one_tree <- fifty_tree[[i]]
    tree <- drop.tip(one_tree, setdiff(one_tree$tip.label, data$Binomial))
    phy_data <- comparative.data(phy = tree, data = data, names.col = "Binomial", 
                                 vcv = T, na.omit = T)
    m <- phylolm(fml.full,
                 data = phy_data$data, phy = phy_data$phy, model = 'lambda')
    r2$Full.model.R2 <- R2_lik(m)  
    summary_m<-summary(m)   
    nop <- data.frame(lambda = 0)
    mP <- phylolm(fml.full,
                  data = phy_data$data, phy = phy_data$phy, model = 'lambda',
                  starting.value = nop, lower.bound = nop$lambda, upper.bound = nop$lambda)
    r2$Phylogeny.R2 <- R2_lik(m, mP)    
    nov <- data.frame(lambda = m$optpar)
    mV <- phylolm(fml.reduce, 
                  data = phy_data$data, phy = phy_data$phy, model = 'lambda', 
                  starting.value = nov, lower.bound = nov$lambda, upper.bound = nov$lambda) 
    r2$Predictors.R2 <- R2_lik(m, mV)   
    ########### Now we have a df called r2 which content 3 kinds of r2 and the summary_m
    ##Select the coefficients and r2, then append to the data frame.
    lambda_coef <- cbind(rownames(summary_m$coefficients), summary_m$coefficients, m$optpar, r2)
    model_coef <- rbind(model_coef, lambda_coef) # model_coef is getting longer as the loop going 
  }
  return(model_coef) #It is a df. The first few cols are different coef of the model and then the lambda col, var cols and R2 cols.
}

##Use the function
Sum.lambda <- Mod.Var.50T('Sum.lambda')
Avg.lambda <- Mod.Var.50T('Avg.lambda') # about 2 min for me
Latest.lambda <- Mod.Var.50T('Latest.lambda')

## Clean up the data
library(dplyr)
#### Make a function to do it
Avg.Mod <- function(mv){
  colnames(mv)[1] <- "Variables"
  colnames(mv)[6] <- "lambda"
  avg <- mv %>% group_by(Variables) %>% summarise_all(mean)
  return(avg)
}

Sum.lambda <- Avg.Mod(Sum.lambda) 
Avg.lambda <- Avg.Mod(Avg.lambda) 
Latest.lambda <- Avg.Mod(Latest.lambda) 

##Export results
write.csv(Sum.lambda, "PGLS model result (Sum.lambda with Freq).csv") 
write.csv(Avg.lambda, "PGLS model result (Avg.lambda).csv") 
write.csv(Latest.lambda, "PGLS model result (Latest.lambda).csv") 


