### Module 0.8: Unified taxonomy

################################ Unified lpi and edge by crosswalk ########################
### Load the crosswalk
crosswalk <- read.csv("crosswalk_LPI_EDGE.csv")
### merge the crosswalk
crosswalk
lpi_data <- lpi_data[, !(colnames(lpi_data) %in% c("Order", "Family"))]
lpi_data <- merge(lpi_data, crosswalk[,c('Binomial','Family','Order','RL.cat','EDGE.species.name',
              'TBL.median','ED.median','EDGE.median','EDGE.species')], by = "Binomial")
### output
write.csv(lpi_data, file = "LPI_mammals_2023.csv", row.names = FALSE)

################################ Unified edge and edge tree ########################
## Let's find the only different species between edge_data and edge tree.
temp <- edge_data$Species
temp2 <- tree1$tip.label
t1 <- !temp %in% temp2 
t2 <- !temp2 %in% temp
paste(temp[t1], "<- edge [not matching] tree ->", temp2[t2])
# "Camelus_ferus <- edge [not matching] tree -> Camelus_bactrianus"
# After checking, I know they are the same species, Camelus_ferus is the newest name.
# So, I should change its name in Module 1.

