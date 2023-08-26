#### Module 4.5: Calculate ED-LPI
library(rlpi)
# I've already loaded the lpi population table in module 0, it's called lpi_data
#Before calculating, I need to turn the species into population level
lpi_data1<-lpi_data
colnames(lpi_data1)[colnames(lpi_data1) == "Binomial"] <- "SP"
lpi_data1$Binomial <- lpi_data1$ID

#Also, I want to delete all pop with Replicate = 1
lpi_data1 <- lpi_data1[lpi_data1$Replicate != 1, ]

### Create index vectors
#Create a list content all SP name
SP <- unique(lpi_data1$SP)

#Create a loop to get index vectors for all SP and store in index_all_SP
index_all_SP <- list()
for (i in SP) {
  index_vector <- lpi_data1$SP == i 
  index_all_SP[[i]]<- index_vector
}

### Create infiles
# It'll output loads of file, and I'd like to make them stay in the folder I appoint
current_dir <- getwd()
target_folder <- "/Users/homeway/Desktop/Summer Project/data/ED_LPI_infile"
setwd(target_folder)

#Create a loop to make infiles for all SP
infile_all_SP <- list()
for (i in SP) {
  infile <- create_infile(lpi_data1, index_vector=index_all_SP[[i]], name=i,)
  infile_all_SP[[i]] <- infile
}

### Create my own All_SP_infile with ED as weighting
# First, I want a list with all pops files
for (i in SP) {
  infile_all_SP[[i]] <- gsub("infile", "pops", infile_all_SP[[i]])
}

# Then, make it become the first col of my All_SP_infile, create group col by the way
infile_EDLPI <- data.frame(FileName = character(length(infile_all_SP)), stringsAsFactors = FALSE)
infile_EDLPI$FileName <- unlist(infile_all_SP)
infile_EDLPI$Group <- 1

# Now we need to create a list of weight calculate by ED
weight_all_SP<- list()
uniqueSP_lpi_data <- lpi_data1[!duplicated(lpi_data1$SP), ]
for (i in SP) {
  weighting <- uniqueSP_lpi_data[uniqueSP_lpi_data$SP == i, "ED.median"] / sum(uniqueSP_lpi_data$ED.median)
  weight_all_SP[[i]] <- weighting
}
# Add it into infile
infile_EDLPI$Weighting <- unlist(weight_all_SP)
# output our infile!
infile_EDLPI <- infile_EDLPI[infile_EDLPI$FileName != "Rhynchocyon_chrysopygus_pops.txt", ]
########## I had to remove Rhynchocyon_chrysopygus to get the code to run successfully
########## I presume that the reason it is failing is that it only has two years of data and the second year is in 2019, 
########## and although our calculations range from 1970-2020, there doesn't seem to be enough data after 2017 to have a total result and it has to be treated as nothing. 
########## So the data for the species may therefore be problematic.
write.table(infile_EDLPI, "ED_LPI_infile.txt", sep = "\t", quote = FALSE, row.names = FALSE)

### Calculate ED-LPI
ED_LPI <- LPIMain("ED_LPI_infile.txt", REF_YEAR = 1970, PLOT_MAX = 2020, BOOT_STRAP_SIZE = 1000, VERBOSE=FALSE,
                   basedir="ED_LPI_temp", use_weightings = 1)

ED_LPI <- read.delim("ED_LPI_temp/ED_LPI_infile_Results.txt", sep = " ", header = TRUE)

ED_LPI <- ED_LPI[complete.cases(ED_LPI), ]
ggplot_lpi(ED_LPI, title = "ED-LPI", xlims=c(1970, 2017), ylim=c(0, 2))

setwd(current_dir)

### Present it with normal LPI
lpis_comp <- list(all_lpi, ED_LPI)
ggplot_multi_lpi(lpis_comp, xlims=c(1970, 2017), names=c("Unweighted", "Weighted by ED"))

