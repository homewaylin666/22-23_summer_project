### Module 0: Basic operation

# Basic start
rm(list=ls())
setwd("/Users/homeway/Desktop/Summer Project/data/")

# Load data
lpi_data <- read.csv("LPI_mammals_2023.csv")
edge_data <- read.csv("EDGE2_all_sp.csv")
edge_data$Species <- gsub(" ", "_", edge_data$Species)

# Load 1000 mammal tree(phy.block.1000) and species list (Species)
load("100_mammal_trees_with_2020_RL.RData")
library(ape)
fifty_tree <- sample(phy.block.1000, size=50)
tree1 <- fifty_tree[[1]]
# n(SP)=6253
