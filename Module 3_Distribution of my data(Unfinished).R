###### Module 3 Distribution of my data
library(caper)
library(phylolm)
library(rr2)

# Create a df with all mammal species and have one col that show if it's my study subject.
study_sub <- data.frame(SP = edge_data$Species)
study_sub$Mine <- 0
for (i in 1:nrow(study_sub)) {
  if (any(grepl(study_sub$SP[i], lpi_data$EDGE.species.name))) {
    study_sub$Mine[i] <- 1
  }
}
sum(study_sub$Mine) #817, good.

#### Create a function to run the logistic model 50 times using different trees
L.Mod.Var.50T <- function(rv){
  fml.reduce <- formula(paste(rv, '~', 1))
  model_50_coef <- data.frame()
  for (i in 1:50){
    one_tree <- fifty_tree[[i]]
    tree <- drop.tip(one_tree, setdiff(one_tree$tip.label, study_sub$SP))
    phy_data <- comparative.data(phy = tree, data = study_sub, names.col = "SP", 
                                 vcv = T, na.omit = T)
    m <- phyloglm(fml.reduce, data = phy_data$data, phy = phy_data$phy, 
                  method = 'logistic_MPLE' ) 
    summary_m <- summary(m)   
    model_1_coef <- cbind(rownames(summary_m$coefficients), summary_m$coefficients, m$alpha)
    model_50_coef <- rbind(model_50_coef, model_1_coef) # model_coef is getting longer as the loop going 
  }
  return(model_50_coef) #It is a df. The first few cols are different coef of the model and then the lambda col, var cols and R2 cols.
}

##Use the function
my_sub <- L.Mod.Var.50T('Mine')
backup <- my_sub
my_sub <- backup
colnames(my_sub)[6] <- "alpha"
my_sub <- my_sub[, -1]
my_sub$Estimate <- as.numeric(my_sub$Estimate)
my_sub$StdErr <- as.numeric(my_sub$StdErr)
my_sub$z.value<- as.numeric(my_sub$z.value)
my_sub$p.value <- as.numeric(my_sub$p.value)
my_sub$alpha <- as.numeric(my_sub$alpha)
average_re <- colMeans(my_sub, na.rm = TRUE)

##Export results
write.csv(average_re, "PGLS model result (Study Subjects).csv") 


