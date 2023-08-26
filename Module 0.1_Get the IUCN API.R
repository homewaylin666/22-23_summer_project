# Get the IUCN API
rm(list=ls())
setwd("/Users/homeway/Desktop/Summer Project/data/")
# necessary R packages
library(taxize)       #search the IUCN status and distribution of species
library(tidyverse)    #to wrangle the data
library(readxl)       #read xlsx data into R

# import data
head(edge_data)
length(edge_data$RL.ID) #6253
length(edge_data$Species) #6253
length(unique(edge_data$RL.ID)) #5759, the differences are NA, they can still be found in iucn_summary, but all data are NA.
length(unique(edge_data$Species)) #6253
# the results indicate there are many subspecies(for RL) in EDGE Species
# but I search, every population in LPI data has an IUCN ID, so it won't be a big problem, I can still use RL ID as a reference

sp.list <- edge_data %>% distinct(Species)

API = "bb49d841d49b691a1729d4defbb24dfb02ea629e15288a3e3fc619cb455dbd4f"
IUCN.list <- iucn_summary(sp.list$Species, distr_detail = TRUE, key = API)

iucn_status(IUCN.list) %>% as.data.frame() %>%
  rownames_to_column(var = "Species") %>% set_names(c("Species", "IUCN")) %>% head()
# To view information about a specific species, do this：IUCN.list$'Burramys parvus'

# export the species list
IUCN_sp_list <- names(IUCN.list) #length:6253
IUCN_sp_list <- data.frame(IUCN_sp_list)
write.csv(IUCN_sp_list, file = "IUCN_sp_list.csv", row.names = FALSE) 

# Actually, this list are the same as EDGE List, not helping...
