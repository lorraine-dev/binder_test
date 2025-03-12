### preamble
getwd()
work_dir <- "D:/Google Drive/PhD/Research projects/Methods/State of the field/APSA race and ethnicity data"
setwd(work_dir)

packages <- c("reshape", "plyr", "dplyr", "car", "stargazer", "gridExtra", "olsrr", 
              "foreign", "ggplot2", "ggmap", "mapsapi", "sf", "sp", "data.table", 
              "mapdata", "maps", "raster", "rworldmap", "GADMTools", "rgdal", "nngeo", 
              "mapview", "plm", "gplots", "haven", "lfe", "plm", 
              "haven", "knitr", "AER", "DataCombine", "jtools", "maptools", "mapdata",
              "rgeos", "geosphere", "tidyr", "coefplot", "margins") # combines packages
lapply(packages, library, character.only = TRUE) # loads all packages in "packages" list
rm(packages)

### load APSA race and ethnicity data (first set, confident matches with no duplicates)
certain <- read.csv("FirstNameMILastName.matches.first.set.csv", na.strings = c("", "NA")) %>%
  mutate(apsa_dataset = "certain")
uncertain <- read.csv("FirstNameLastName.matches.second.set.csv", na.strings = c("", "NA")) %>%
  mutate(apsa_dataset = "uncertain")
no_matches <- read.csv("no_matches.csv", na.strings = c("", "NA")) %>%
  mutate(apsa_dataset = "no_matches")

### remove suffixes from certain dataset
colnames(certain) <- gsub(".y", "", colnames(certain))
colnames(certain) <- gsub(".x", "", colnames(certain))

### remove suffixes from uncertain dataset
colnames(uncertain) <- gsub(".y", "", colnames(uncertain))
colnames(uncertain) <- gsub(".x", "", colnames(uncertain))

### remove suffixes from no_matches dataset
colnames(no_matches) <- gsub(".y", "", colnames(no_matches))
colnames(no_matches) <- gsub(".x", "", colnames(no_matches))

### trim whitespace
names(certain) <- trimws(names(certain))
names(uncertain) <- trimws(names(uncertain))
names(no_matches) <- trimws(names(no_matches))

### names dataframe
names_certain <- names(certain)
names_uncertain <- names(uncertain)
names_no_matches <- names(no_matches)

names <- data.frame(names_certain,
                     names_uncertain,
                     names_no_matches)

### check that names are equivalent (they are!)
names <- names %>%
  mutate(certain_uncertain_match = ifelse(names_certain == names_uncertain, 1, 0),
         certain_no_matches_match = ifelse(names_certain == names_no_matches, 1, 0),
         uncertain_no_matches_match = ifelse(names_uncertain == names_no_matches, 1, 0))

### stack data
author_data <- rbind(certain, uncertain, no_matches)

### add index
author_data$index <- seq(1:nrow(author_data))

### remove component datasets
rm(names, names_certain, names_no_matches, names_uncertain)

### identify string distance between names of universities in APSA dataset and in original dataset
library(stringdist)
author_data <- author_data %>%
  mutate(apsa_university_stringdist = stringdist(Current.Institutional.Affiliation.kap, Institution.apsa),
         apsa_university_is_right = ifelse(apsa_dataset == "uncertain" & 
                                             apsa_university_stringdist <= 2, 1, 0),
         apsa_university_is_na = ifelse(apsa_dataset == "uncertain" & 
                                          is.na(apsa_university_stringdist), 1, 0))

### identify authors that remain uncertain, based on (1) uncertain in apsa_dataset 
### AND (2) string distance is <= 2. also create variable for "string distance is NA,"
### which combined with the "uncertain" set means that they are included in the final list
remaining_uncertain <- author_data %>%
  subset(apsa_dataset == "uncertain") %>%
  mutate(remaining_uncertain = ifelse(apsa_university_stringdist > 2, 1, 0))

### subset to remaining uncertain
remaining_uncertain <- remaining_uncertain %>%
  subset(remaining_uncertain == 1)

### write.csv for remaining uncertain
write.csv(remaining_uncertain, "remaining_uncertain.csv")

### remove "remaining_uncertain"
rm(remaining_uncertain)

### load "remaining_uncertain_matched"
remaining_uncertain_matched <- read.csv("remaining_uncertain_matched.csv", na.strings = "") %>%
  dplyr::select(index, match)

### left_join remaining_uncertain_matched to author_data, subset to remove non
author_data <- left_join(author_data, remaining_uncertain_matched,
                         by = c("index" = "index"))
                         
author_data$match <- as.integer(author_data$match)

### identify observations that were (1) certain, (2) matched correctly,
### (3) apsa_university_is_right = 1, or (4) apsa_university_is_na = 1
author_data <- author_data %>% 
  mutate(keep = ifelse(apsa_dataset == "certain" |
                         match == 1 |
                         apsa_university_is_right == 1 |
                         apsa_university_is_na == 1, 1, 0),
         keep = ifelse(apsa_dataset == "no_matches",
                       0, keep))

### keep "keep" observations
author_data <- author_data %>%
  subset(keep == 1)

### pull out author names and APSA codes and remove duplicates
author_names <- author_data %>% 
  dplyr::select(Contact.apsa, NameSort.apsa) %>%
  subset(duplicated(Contact.apsa) == FALSE) %>%
  subset(duplicated(NameSort.apsa) == FALSE)

### re-merge (left_join) author_names with author_data
author_data <- left_join(test, author_data, 
                  by = c("Contact.apsa", "NameSort.apsa")) %>%
  subset(duplicated(Contact.apsa) == FALSE) 

### remove extraneous dataframes
rm(author_names, certain, no_matches, remaining_uncertain_matched, uncertain)

########################################################################

### merge with original author list to add author names

########################################################################

### load original author list
evie <- read.csv("MMCPSR Author Database - Final Sample.csv", na.strings = c("NAV", ""))

### left join evie's original author list to the APSA author data list by (1) last name and (2) institution
full_author_data <- author_data %>%
  left_join(., evie,
            by = c("First.Name..Initial.kap" = "First.Name..Initial",
                   "Last.Name.kap" = "Last.Name",
                   "Current.Institutional.Affiliation.kap" = "Current.Institutional.Affiliation"))

### write CSV with final list
write.csv(author_data, "final_apsa_list.csv")