#### Module 4.9: Extracting LPI lambda into EDGE data
# import the pop lambda file
lpi_lambda <- read.csv("lpi_temp/All_data_pops_lambda.csv")

#create a Sum.lambda column
lpi_lambda$Sum.lambda <- apply(lpi_lambda[, 5:51], 1, sum, na.rm = T)
lpi_lambda$Sum.lambda[lpi_lambda$Binomial == "Calomys_tener"] <- log10(0.0002/0.002)
lpi_lambda$Sum.lambda[lpi_lambda$Binomial == "Delomys_dorsalis"] <- log10(0.1/0.02)

#create a Avg.lambda column
lpi_lambda$Avg.lambda <- apply(lpi_lambda[, 5:51], 1, mean, na.rm = T)
lpi_lambda$Avg.lambda[lpi_lambda$Binomial == "Calomys_tener"] <- log10(0.0002/0.002)
lpi_lambda$Avg.lambda[lpi_lambda$Binomial == "Delomys_dorsalis"] <- log10(0.1/0.02)
#I added the data for these two species manually because they seem to be having trouble with the lambda calculation.

#create a Latest.lambda column
lpi_lambda$Latest.lambda <- apply(lpi_lambda[, grep("X",colnames(lpi_lambda))], 1, function(x) tail(na.omit(x), 1)) 
lpi_lambda$Latest.lambda <- as.numeric(lpi_lambda$Latest.lambda)

# combine lpi_lambda and edge_data
my_data <- edge_data
colnames(my_data)[colnames(my_data) == "Species"] <- "Binomial"
col_to_combine<- c('Sum.lambda','Avg.lambda','Latest.lambda','Freq')
my_data <- merge(my_data, lpi_lambda[c('Binomial',col_to_combine)], 
                  by = "Binomial", all.x = TRUE)
my_data <- my_data[complete.cases(my_data$Avg.lambda), ]
