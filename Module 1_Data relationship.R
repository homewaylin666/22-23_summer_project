### Module 1 Data relationship

# I've unified the taxonomy in Module 0.8, so this module is just for checking.

### Check their number of species
# see how many sp in the tree, and how many I have
tree_sp_n <- length(unique(tree1$tip.label)) # 6253
lpi_sp_n <- length(unique(lpi_data$EDGE.species.name)) # 785
paste(round(100*lpi_sp_n/tree_sp_n,2), "%", sep=" ") # it shows I only got 12.55% species

# see how many sp that edge_mammal has
edge_sp_n <- length(unique(edge_data$Species)) # 6253
paste(round(100*edge_sp_n/tree_sp_n,2), "%", sep=" ") # 100%

### Check their relationship by matching them
# Check how many LPI sp are in the tree data
tree_sp <- data.frame(all_sp = tree1$tip.label)
tree_sp$my_sp <- 0
for (i in 1:nrow(tree_sp)) {
  if (any(grepl(tree_sp$all_sp[i], lpi_data$EDGE.species.name))) {
    tree_sp$my_sp[i] <- 1
  }
}
paste(round(100*sum(tree_sp$my_sp)/lpi_sp_n,2), "%", sep=" ")
# 100%, completely matching.

#####################################################################################
# Then, check how many edge sp are in the tree data
# It's not important, because I'll use lpi_data to analyze
# Before that, I need to change one species name, which I found in Module 0.8.
edge_data$Species[edge_data$Species == 'Camelus_ferus'] <- 'Camelus_bactrianus'
tree_sp$edge_sp <- 0
for (i in 1:nrow(tree_sp)) {
  if (any(grepl(tree_sp$all_sp[i], edge_data$Species))) {
    tree_sp$edge_sp[i] <- 1
  }
}
paste(round(100*sum(tree_sp$edge_sp) /edge_sp_n,2), "%", sep=" ")
# 100%, perfect.

