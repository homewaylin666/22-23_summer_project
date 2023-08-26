### Module 8.4: ATED & sATED 50 trees
library(ape)
library(caper)
library(phylolm)
library(rr2)

# Create a loop to compare
ATED50 <- function(trend){
  # Calculate ATPD and sATPD by 50 diff trees
  ATED_fifty_results <- data.frame(matrix(ncol = 50, nrow = length(trend)))
  sATED_fifty_results <- data.frame(matrix(ncol = 50, nrow = length(trend)))
  mean_result <- data.frame(ATED = numeric(length(trend)), sATED = numeric(length(trend)))
  for (j in 1:50){
    one_tree <- fifty_tree[[j]]
    tree <- drop.tip(one_tree, setdiff(one_tree$tip.label, data$Binomial))
    phy_data <- comparative.data(phy = tree, data = data, names.col = "Binomial", 
                                 vcv = T, na.omit = T)
    # Use the functions to calculate
    ATED_fifty_results[,j] <- ATED(trend = trend, tree = phy_data$phy)[,1]
    sATED_fifty_results[,j] <- sATED(trend = trend, tree = phy_data$phy)[,1]
  }
  for (i in 1:length(trend)) {
    mean_result[i,1] <- mean(as.numeric(ATED_fifty_results[i,]))
    mean_result[i,2] <- mean(as.numeric(sATED_fifty_results[i,]))
  }
  rownames(mean_result) <- phy_data$phy$tip.label
  return(mean_result)
}
# try this function using data from Module 8.
ATED50_result <- ATED50(trend = trend)
ATED50_result
