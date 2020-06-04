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

# Save a version of the original data to compare that things went OK after cleaning, compiling
data_list_og <- 
  data_list

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
         next))
}

# Eland female data has missing values, as field identification can be tedious, so remove values (preserved in unid_adult)
data_list$Eland <- 
  data_list$Eland %>% 
  mutate(female = NA)

# Problem of duplicate rows in migratory wildebeest
## Create two data frames, one for concatenated rows to add to data later, and one with rows to remove
output_add<-  
  data.frame()
output_remove <-
  data.frame()

## Write a loop that will find those rows, add to corresponding data frame, and concatenate if needed
for(i in 1:nrow(data_list$Wildebeest_migrant)){
  ### First row to be compared    
  first <- 
        data_list$Wildebeest_migrant[i,]
  for(j in 1:nrow(data_list$Wildebeest_migrant)){
    ### Second row to be compared
    second <- 
      data_list$Wildebeest_migrant[j,]
    new_row <- 
      first
    ### This if statement is to make sure the same row is not compared to itself, and all comparisons happen once
    ### i.e. second always after first
    if (i < j) {
      ### Duplicates are ros were the yera-month-female value combination is spread across several rows
      ifelse(first$year == second$year & first$month == second$month & first$female == second$female, 
             ### Problem 1: both non-adult rows empty in first row
              ifelse(is.na(first$newborn) & is.na(first$juvenile), 
                    output_remove <- 
                      rbind(output_remove, first), ### Add to rows to remove
                    ### Problem 2: both non-adult rows empty in second row
                    ifelse(is.na(second$newborn) & is.na(second$juvenile), 
                           output_remove <- 
                             rbind(output_remove, second), ### Add to row to remove
                           ### Problem 3: same sample divided in two rows (e.g. one with newborn + female, other juvenile + female)
                           ifelse(is.na(first$newborn) & !is.na(second$newborn), ### Case 1
                                  c(new_row$newborn <- 
                                    second$newborn, ### Add non-NA value
                                    output_add <- 
                                      rbind(output_add, new_row), ### Add to list of rows to add
                                    output_remove <- 
                                      rbind(output_remove, first, second)), ### Add to list of rows to remove
                                  ifelse(is.na(first$juvenile) & !is.na(second$juvenile), ### Case 2
                                         c(new_row$juvenile <- 
                                             second$juvenile, ### Add non-NA value
                                           output_add <- 
                                             rbind(output_add, new_row), ### Add to list of rows to add
                                           output_remove <- 
                                             rbind(output_remove, first, second)), ### Add to list of rows to remove
                                         print(c(first, second)))))), 
                    next)} ### If i >= j, move on to next value of j
      else 
        next ### If nothing wrong just move on to the next row
  }    }

## Briefly check there is nothing wrong in the values
View(output_add)
View(output_remove)

## Implement changes in the wildebeest
data_list$Wildebeest_migrant <- 
  data_list$Wildebeest_migrant %>% 
  anti_join(output_remove) %>% 
  bind_rows(output_add)


#check that years have not been removed, unqiue values..,  nrow per year...
# empty cells in wildebeest resident