# Loading and compiling data

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
  lapply(gsub, pattern = "'", replacement = "") %>% 
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

# Save list of species names, add naming authority
species_list <- 
  data_list$species %>% 
  mutate(naming_authority = 
           ifelse(specific_epithet == "caffer", "(Sparrman, 1779)",
                  ifelse(specific_epithet == "oryx", "(Pallas, 1766)",
                         ifelse(specific_epithet == "africana", "(Blumenbach, 1797)",
                                ifelse(specific_epithet == "camelopardalus", "(Brisson, 1772)",
                                       ifelse(specific_epithet == "granti", "(Brooke, 1872)",
                                              ifelse(specific_epithet == "melampus", "(Lichtenstein, 1812)",
                                                     ifelse(specific_epithet == "buselaphus", "(Pallas, 1766)",
                                                            ifelse(specific_epithet == "camelus", "(Linnaeus, 1758)",
                                                                   ifelse(specific_epithet == "thomsonii", "(Günther, 1884)",
                                                                          ifelse(specific_epithet == "lunatus", "(Burchell, 1824)",
                                                                                 ifelse(specific_epithet == "africanus", "(Gmelin, 1788)",
                                                                                        ifelse(specific_epithet == "defassa", "(Rüppell, 1835)",
                                                                                               ifelse(specific_epithet == "taurinus", "(Burchell, 1823)",
                                                                                                      "(Boddaert, 1785)"))))))))))))))
  
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
      "common_name",
      "infant",
      "juvenile",
      "female",
      "unid_adult",
      "total",
      "migrant_resident")
  ## Put species column first, and remove total column
  data_list[[i]] <- 
    data_list[[i]] %>% 
    relocate(common_name, .before = year) %>% 
    dplyr::select(-total)
  ## Migrant and resident wildebeests in two different data sets, include this difference in names
  ifelse(names(data_list)[i] == "Wildebeest" & data_list[[i]]$migrant_resident == "M",
               names(data_list)[i] <- "Wildebeest_migrant",
                     ifelse(names(data_list)[i] == "Wildebeest" & data_list[[i]]$migrant_resident == "R",
                            names(data_list)[i] <- "Wildebeest_resident",
         next))
}

# Eland female data have missing values, as field identification can be tedious, so remove values (preserved in unid_adult)
data_list$Eland <- 
  data_list$Eland %>% 
  mutate(female = NA)

# Ostrich data include males in unid_adult column, empty column because unecessary here
data_list$Ostrich <- 
  data_list$Ostrich %>% 
  mutate(unid_adult = NA)

# Ostrich data has NA instead of 0 for females in 2000
data_list$Ostrich$female[is.na(data_list$Ostrich$female)] <- 
  0

# Wildebeest have NA instead of 0 for some years in November and December
data_list$Wildebeest_resident$infant[which(is.na(data_list$Wildebeest_resident$infant)) & (data_list$Wildebeest_resident$month == 11 | data_list$Wildebeest_resident$month == 12)] <- 
  0
data_list$Wildebeest_migrant$infant[which(is.na(data_list$Wildebeest_migrant$infant)) & (data_list$Wildebeest_migrant$month == 11 | data_list$Wildebeest_migrant$month == 12)] <- 
  0

# Two data entry errors in month column for migratory wildebeest
data_list$Wildebeest_migrant$month[data_list$Wildebeest_migrant$year == 1978] <- 
  5
data_list$Wildebeest_migrant$month[data_list$Wildebeest_migrant$year == 1980] <- 
  5

# Problem of duplicate rows in several species
## Create one dataframe to store rows to remove and one o store rows to add at the end
output_remove <- 
  data.frame()
output_add <- 
  data.frame()
## Loop that will examine all species separately
for(data in data_list){
## Loop that will find those rows, add to corresponding data frame, and concatenate if needed
for(i in 1:nrow(data)){
  ### First row to be compared    
  first <- 
    data[i,]
  for(j in 1:nrow(data)){
    ### Second row to be compared
    second <- 
      data[j,]
    new_row <- 
      first
    ### This if statement is to make sure the same row is not compared to itself, and all comparisons happen once
    ### i.e. second always after first
    if (i < j) {
      ### Duplicates are ros were the yera-month-female value combination is spread across several rows
      ifelse(first$year == second$year & first$month == second$month & first$female == second$female, 
             ### Problem 1: both non-adult rows empty in first row
             ifelse(is.na(first$infant) & is.na(first$juvenile), 
                    output_remove <- 
                      rbind(output_remove, first), ### Add to rows to remove
                    ### Problem 2: both non-adult rows empty in second row
                    ifelse(is.na(second$infant) & is.na(second$juvenile), 
                           output_remove <- 
                             rbind(output_remove, second), ### Add to row to remove
                           ### Problem 3: same sample divided in two rows (e.g. one with infant + female, other juvenile + female)
                           ifelse(is.na(first$infant) & !is.na(second$infant), ### Case 1
                                  c(new_row$infant <- 
                                      second$infant, ### Add non-NA value
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
  }
## Briefly check there is nothing wrong in the values
View(output_remove)
View(output_add)

## Implement changes in the data frame
data_list$Topi <- 
  data_list$Topi %>% 
  anti_join(output_remove) %>% 
  bind_rows(output_add %>% 
              filter(common_name == "Topi"))
data_list$Wildebeest_migrant <- 
  data_list$Wildebeest_migrant %>% 
  anti_join(output_remove) %>% 
  bind_rows(output_add %>% 
              filter(common_name == "Wildebeest"))



# Compile all data in single dataframe
data_compiled <- 
  data.frame()
for(i in 1:length(data_list)){
  data_compiled <- 
    rbind(data_compiled, data_list[[i]])
}

# Common names have to match
data_compiled$common_name[data_compiled$common_name == "Waterbuck"] <- 
  "Defassa waterbuck"
data_compiled$common_name[data_compiled$common_name == "Kongoni"] <- 
  "Coke's kongoni"
 

# Add latin names, change column orders and add sampling type and sampling method columns
data_compiled <- 
  data_compiled %>% 
  left_join(species_list) %>% 
  relocate(order, family, genus, specific_epithet, naming_authority, .before = common_name) %>% 
  mutate(sampling_type = ifelse(!is.na(infant) & !is.na(juvenile) & !is.na(female) & is.na(unid_adult),
                                "ijf",
                                ifelse(!is.na(infant) & !is.na(juvenile) & is.na(female) & !is.na(unid_adult),
                                       "ija",
                                       ifelse(!is.na(infant) & is.na(juvenile) & !is.na(female) & is.na(unid_adult),
                                              "if",
                                              ifelse(!is.na(infant) & is.na(juvenile) & is.na(female) & !is.na(unid_adult),
                                                     "ia",
                                                     ifelse(is.na(infant) & !is.na(juvenile) & !is.na(female) & is.na(unid_adult),
                                                            "jf",
                                                            ifelse(is.na(infant) & !is.na(juvenile) & is.na(female) & !is.na(unid_adult),
                                                                   "ja", "problem"))))))) %>% ## to make sure there are no hidden errors 
  mutate(sampling_method = ifelse(specific_epithet %in% c("granti", "africana", "oryx", "camelus", "thomsonii"), "method_2",
                                  ifelse((specific_epithet == "taurinus" & 1926 <= year & year <= 1933) | 
                                           (specific_epithet == "camelopardalus" & 1926 <= year & year <= 1933) | 
                                           (specific_epithet == "caffer" & 1965 <= year & year <= 1973),
                                         "method_3", "method_1")))
    
# Save data in txt format
write.table(data_compiled,
            "compiled_data/rogy_sinclair_serengeti_ungulates.txt",
            sep = "\t",
            row.names = F)

# Read data to make sure it worked
data_check <- 
  read.delim("compiled_data/rogy_sinclair_serengeti_ungulates.txt", 
             header = T, 
             sep = "\t",
             stringsAsFactors = F)
