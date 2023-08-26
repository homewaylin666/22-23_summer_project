### Module 0.7: Refine the crosswalk

### Load a lpi-edge table, this table contains the crosswalk of edge and current lpi data.
library(readxl)
crosswalk <- read_excel("EDGE_LPI_mammals.xlsx")

############################################### check the lpi side of the crosswalk
length(unique(lpi_data$Binomial)) #795
length(unique(crosswalk$Binomial)) #754

# let's see who are different.
temp <- data.frame(lpi_sp = unique(lpi_data$Binomial))
temp$cross_lpi_sp <- 0
for (i in 1:nrow(temp)) {
  if (any(grepl(temp$lpi_sp[i], crosswalk$Binomial))) {
    temp$cross_lpi_sp[i] <- 1
  }
}
sum(temp$cross_lpi_sp) 
# 754, good, it means the only problem is there are 41 species in lpi are not in crosswalk-lpi.
cross_miss_sp <- data.frame(missing_sp = subset(temp, cross_lpi_sp == 0)$lpi_sp)
cross_miss_sp
#these 41 sp are not in crosswalk.

# Are they in the edge_data?
cross_miss_sp$Found <- 0
for (i in 1:nrow(cross_miss_sp)) {
  if (any(grepl(cross_miss_sp$missing_sp[i], edge_data$Species))) {
    cross_miss_sp$Found[i] <- 1
  }
}
sum(cross_miss_sp$Found) 
# 35 sp are found, it means there are only 6 species from lpi are not on cross walk and not matching.
# Manual search is acceptable, so I could make all 795 lpi sp matching with edge sp.

################# now, I need to add those missing sp to crosswalk
# first, I need to use edge_data to fill the missing species list
cross_miss_sp <- data.frame(Species = cross_miss_sp[,1])
common_cols <- intersect(colnames(crosswalk), colnames(edge_data))
cross_miss_sp <- merge(cross_miss_sp, edge_data[c('Species',common_cols)], by = "Species", all.x = TRUE)
# then, to prepare for combining, I need to create a mirror of crosswalk
cross_miss_sp$EDGE.species.name <- cross_miss_sp$Species
colnames(cross_miss_sp)[colnames(cross_miss_sp) == "Species"] <- "Binomial"
not_common_cols <- setdiff(colnames(crosswalk), colnames(cross_miss_sp))
cross_miss_sp[,c(not_common_cols)]<-NA
# Ok, bind them!
crosswalk <- rbind(crosswalk, cross_miss_sp)
# That's check the species number
length(unique(crosswalk$Binomial)) #795, right!

############################################### check the edge side of the crosswalk
length(unique(crosswalk$EDGE.species.name)) 
#786, it's normal to be different from 795, cause they use different taxonomy

# Are these 786 sp can all be found in edge_data?
temp <- data.frame(cross_edge = unique(crosswalk$EDGE.species.name))
temp$mw_edge <- 0
for (i in 1:nrow(temp)) {
  if (any(grepl(temp$cross_edge[i], edge_data$Species))) {
    temp$mw_edge[i] <- 1
  }
}
sum(temp$mw_edge) #780. No, there are 6 species who are can not.
# Make sence, they are the 6/41, I need to match them manually.
# Let's turn them into NA and output this refined crosswalk, it's our final step.
nm_sp <- subset(temp, mw_edge == 0)$cross_edge
crosswalk$EDGE.species.name[crosswalk$EDGE.species.name %in% nm_sp] <- NA
write.csv(crosswalk, file = "crosswalk_LPI_EDGE.csv", row.names = FALSE)
# All Clear!
