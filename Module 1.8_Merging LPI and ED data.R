#### Module 1.8 : Merging LPI and ED data by species
edge_data1 <- edge_data
colnames(edge_data1)[colnames(edge_data1) == "Species"] <- "EDGE.species.name"
col_to_combine<- c('TBL.median','ED.median','EDGE.median','EDGE.species')
lpi_data <- lpi_data[, !(names(lpi_data) %in% col_to_combine)]

lpi_data <- merge(lpi_data, edge_data1[c('EDGE.species.name',col_to_combine)], 
      by = "EDGE.species.name", all.x = TRUE)

write.csv(lpi_data, file = "LPI_mammals_2023.csv", row.names = FALSE)


