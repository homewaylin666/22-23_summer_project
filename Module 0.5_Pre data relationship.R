###### Module 0.5: Pre data relationship
## I'm trying to figure out the relationship 
## between LPI, EDGE and phylogenetic tree species data first!

### Check their number of species
# see how many sp in the tree, and how many I have
tree_sp_n <- length(unique(tree1$tip.label)) # 6253
lpi_sp_n <- length(unique(lpi_data$Binomial)) # 795
paste(round(100*lpi_sp_n/tree_sp_n,2), "%", sep=" ")
  # it shows I only got 12.71% species

# see how many sp that edge_mammal has
edge_sp_n <- length(unique(edge_data$Species)) # 6253, god
paste(round(100*edge_sp_n/tree_sp_n,2), "%", sep=" ")
  # it's even more than 5897 (100%)

### Check their relationship by matching them
# Check how many LPI sp are in the tree data
tree_sp <- data.frame(all_sp = tree1$tip.label)
tree_sp$my_sp <- 0
for (i in 1:nrow(tree_sp)) {
  if (any(grepl(tree_sp$all_sp[i], lpi_data$Binomial))) {
    tree_sp$my_sp[i] <- 1
  }
}
sum(tree_sp$my_sp)
  #730, it's not 795, indicating they are different taxonomy, so we need to use a crosswalk.

# BTW, check how many edge sp are in the tree data
tree_sp$edge_sp <- 0
for (i in 1:nrow(tree_sp)) {
  if (any(grepl(tree_sp$all_sp[i], edge_data$Species))) {
    tree_sp$edge_sp[i] <- 1
  }
}
sum(tree_sp$edge_sp) 
  #6252, there is only one species has different name.
# Who is the different one? That's find it in Module 0.8
