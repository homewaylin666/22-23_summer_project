#### Module 4: Calculate LPI
library(rlpi)
# I've already loaded the lpi population table in module 0, it's called lpi_data

#Before calculating, I need to turn lpi sp names into edge sp name
colnames(lpi_data)[colnames(lpi_data) == "Binomial"] <- "LPI.species.name"
colnames(lpi_data)[colnames(lpi_data) == "EDGE.species.name"] <- "Binomial"

# Create an infile, select all population but don't use pop with Replicate = 1
index_vector = rep(TRUE, nrow(lpi_data))
rows_to_update <- which(lpi_data$Replicate == 1)
index_vector[rows_to_update] <- FALSE
infile_allpop <- create_infile(lpi_data, index_vector=index_vector, name="All_data")
# An index can be created using this infile, for the period 1970 to 2014 with 100 bootstraps.
all_lpi <- LPIMain(infile_allpop, REF_YEAR = 1970, PLOT_MAX = 2020, BOOT_STRAP_SIZE = 10000, VERBOSE=FALSE,
                   basedir="lpi_temp")
# Remove NAs (trailing years with no data)
all_lpi <- all_lpi[complete.cases(all_lpi), ]
# Plot the resulting index
ggplot_lpi(all_lpi, title = "all_lpi", xlims=c(1970, 2017), ylim=c(0, 2))

