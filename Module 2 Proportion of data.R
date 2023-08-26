### Module 2 Proportion of data

#To know the ratio of data we have in each order, family and IUCN Red List Category
############################## For Order ##############################
# create order df
ord <- data.frame(ord = unique(edge_data$Order))
# make lpi data from population level into species level
lpi_sp_list <- lpi_data[,c("EDGE.species.name","Family","Order","RL.cat")]
lpi_sp_list <- lpi_sp_list[!duplicated(lpi_sp_list$EDGE.species.name), ]
# let's calculate the ratio
ord$all <- 0
ord$lpi <- 0
for (i in ord$ord) {
  ord$all[ord$ord == i] <- sum(edge_data$Order == i, na.rm = TRUE)
  ord$lpi[ord$ord == i] <- sum(lpi_sp_list$Order == i, na.rm = TRUE)
}
ord$Proportion <- round(ord$lpi / ord$all * 100, 2)

#export
sorted_ord <- ord[order(ord$Proportion, decreasing = TRUE), ]
write.csv(sorted_ord, file = "Proportion table(order).csv", row.names = FALSE)
############################## For Family ##############################
fam <- data.frame(fam = unique(edge_data$Family))
fam$all <- 0
fam$lpi <- 0

for (i in fam$fam) {
  fam$all[fam$fam == i] <- sum(edge_data$Family == i, na.rm = TRUE)
  fam$lpi[fam$fam == i] <- sum(lpi_sp_list$Family == i, na.rm = TRUE)
}
fam$Proportion <- round(fam$lpi / fam$all * 100, 2)

#export
sorted_fam <- fam[order(fam$Proportion, decreasing = TRUE), ]
write.csv(sorted_fam, file = "Proportion table(family).csv", row.names = FALSE)
############################## For Genus ##############################
library(tidyr)
edge_data1 <-edge_data
edge_data1 <- separate(edge_data1, col = Species, into = c("Genus", "Species"), sep = "_", remove = FALSE)
lpi_sp_list <- separate(lpi_sp_list, col = EDGE.species.name, into = c("Genus", "Species"), sep = "_", remove = FALSE)
gen <- data.frame(gen = unique(edge_data1$Genus))
gen$all <- 0
gen$lpi <- 0

for (i in gen$gen) {
  gen$all[gen$gen == i] <- sum(edge_data1$Genus == i, na.rm = TRUE)
  gen$lpi[gen$gen == i] <- sum(lpi_sp_list$Genus == i, na.rm = TRUE)
}
gen$Proportion <- round(gen$lpi / gen$all * 100, 2)

#export
sorted_gen <- gen[order(gen$Proportion, decreasing = TRUE), ]
write.csv(sorted_gen, file = "Proportion table(genus).csv", row.names = FALSE)
############################## IUCN Red List Category ##############################
RL.cat <- unique(edge_data$RL.cat)
RL.cat <- data.frame(cat = RL.cat[!is.na(RL.cat)])
RL.cat$all <- 0
RL.cat$lpi <- 0
for (i in RL.cat$cat) {
  RL.cat$all[RL.cat$cat == i] <- sum(edge_data$RL.cat == i, na.rm = TRUE)
  RL.cat$lpi[RL.cat$cat == i] <- sum(lpi_sp_list$RL.cat == i, na.rm = TRUE)
}
RL.cat$all.proportion <- round(RL.cat$all/sum(RL.cat$all) * 100, 2)
RL.cat$lpi.proportion <- round(RL.cat$lpi/sum(RL.cat$lpi) * 100, 2)
#export
write.csv(RL.cat, file = "Proportion table(RL Category).csv", row.names = FALSE)
# Well done, I also want one version without the cat--DD.
RL.cat <- RL.cat[RL.cat$cat != "DD", ]
RL.cat$all.proportion <- round(RL.cat$all/sum(RL.cat$all) * 100, 2)
RL.cat$lpi.proportion <- round(RL.cat$lpi/sum(RL.cat$lpi) * 100, 2)
#export
write.csv(RL.cat, file = "Proportion table(RL Category without DD).csv", row.names = FALSE)

