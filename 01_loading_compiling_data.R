# Loading data

# Load packages
library(readxl)
library(tidyverse)

# List of file in folder
file_list <- 
  list.files("original_data/")

# Extract name of species in a list
species_list <- 
  strsplit(file_list,"_") %>% 
  map(1) %>% 
  ## Remove apostrophe from Grant's and Thopmson's
  lapply(gsub, pattern="'", replacement="") %>% 
  ## Convert to vector
  flatten_chr()

# Make empty list to be filled with data
data_list <- 
  list()
# Use loop to make each dataframe an element of the list, and give it the name of the animal species
for(i in 1:length(file_list)){
    data_list[[i]] <- 
      as.data.frame(read_excel(paste0("original_data/", file_list[i])))
}

# Give the name of species to elements of the list
names(data_list) <- 
  species_list


# Remove extra column in Grant's gazelle
data_list$Grants <- 
  data_list$Grants %>% 
  data.frame() %>% 
  dplyr::select(-Male)

# Buffalo missing their residency status
data_list$Buffalo <- 
  data_list$Buffalo %>% 
  mutate(migrant_resident = "R")

# Save of species names
species_list <- 
  data_list$species
## Remove it from data_list
data_list <- 
  data_list[names(data_list) != "species"]

# First sequence of data harmonisation
for(i in 1:length(data_list)){
  ## Make names of columns all the same
  colnames(data_list[[i]]) <- 
    c("year",
      "month",
      "species",
      "newborn",
      "juvenile",
      "female",
      "unid_adult",
      "total",
      "migrant_resident")
  ## Put species column first, and remove total column
  data_list[[i]] <- 
    data_list[[i]] %>% 
    relocate(species, .before = year) %>% 
    dplyr::select(-total)
  ## Migrant and resident wildebeests in two different data sets, include this difference in names
  ifelse(names(data_list)[i] == "Wildebeest" & data_list[[i]]$migrant_resident == "M",
               names(data_list)[i] <- "Wildebeest_migrant",
                     ifelse(names(data_list)[i] == "Wildebeest" & data_list[[i]]$migrant_resident == "R",
                            names(data_list)[i] <- "Wildebeest_resident",
         print("ok")))
}

# Eland female data has missing values, as field identification can be tedious, so remove values (preserved in unid_adult)
data_list$Eland <- 
  data_list$Eland %>% 
  mutate(female = NA)

# Problem of duplicate rows in migratory wildebeest
## Problem 1: both non-adult rows empty
problematic_rows <-
  data_list$Wildebeest_migrant %>% 
  filter(is.na(newborn) & is.na(juvenile))

# Problem 2: same sample divided in two rows (e.g. one with newborn + female, other juvenile + female)
#function(rowchecker), 
#compare row with next one, see if females, sam


  
