#### Module 6: Preparing data for model

## The needed df "my_data" is made from Module 4.9

# check and adjust the distribution of continuous variables in my data
hist(my_data$ED.median,breaks = 100) # looks left-skewed
my_data$ED.median <- log(my_data$ED.median) # looks too left-skewed, need to log
hist(my_data$Freq,breaks = 100) # looks left-skewed
hist(my_data$Sum.lambda, breaks = 100) # looks close to normal distribution
hist(my_data$Avg.lambda,breaks = 100) # looks close to normal distribution
hist(my_data$Latest.lambda,breaks = 100) # looks close to normal distribution


# Standardisation of continuous variables (only need to do this to predictors when their not the only one predictor)
# my_data$ED.median <- as.vector(scale(my_data$ED.median))
# my_data$Freq <- as.vector(scale(my_data$Freq))

# final check
par(mfrow = c(2,2),mar = c(3,3,2,1))
hist(my_data$Sum.lambda,breaks = 50)
hist(my_data$Avg.lambda,breaks = 50) 
hist(my_data$Latest.lambda,breaks = 50) 
hist(my_data$ED.median,breaks = 50) # all looks fine
par(mfrow = c(1,1),mar = c(3,3,2,1))

# making phy-data
library(ape)
fifty_tree <- sample(phy.block.1000, size=50)
one_tree <- fifty_tree[[1]]
tree <- drop.tip(one_tree, setdiff(one_tree$tip.label, my_data$Binomial)) 
data <- my_data[,c("Binomial",'Order','Family',"ED.median","Sum.lambda"
                   ,"Avg.lambda", "Latest.lambda", "Freq")]
library(caper)
phy_data <- comparative.data(phy = tree, data = data, names.col = "Binomial", 
                             vcv = T, na.omit = T)

# check the number of species
length(phy_data$data$Avg.lambda) #816
