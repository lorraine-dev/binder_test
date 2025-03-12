### preamble
getwd()
work_dir <- "G:/.shortcut-targets-by-id/1KeBldwTzusBNUhuNSaykGRqHrbdO7APp/MMCPSR/Race, Ethnicity, PhD Year Inst. Data and Lit"

packages <- c("reshape", "plyr", "dplyr", "car", "stargazer", "gridExtra", "olsrr", 
              "foreign", "ggplot2", "ggmap", "mapsapi", "sf", "sp", "data.table", 
              "mapdata", "maps", "raster", "rworldmap", "GADMTools", "rgdal", "nngeo", 
              "mapview", "plm", "gplots", "haven", "lfe", "plm", 
              "haven", "knitr", "AER", "DataCombine", "jtools", "maptools", "mapdata",
              "rgeos", "geosphere", "tidyr", "coefplot", "margins") # combines packages
lapply(packages, library, character.only = TRUE) # loads all packages in "packages" list
rm(packages)

### set working directory for apsa data
work_dir_apsa <- "G:/.shortcut-targets-by-id/1KeBldwTzusBNUhuNSaykGRqHrbdO7APp/MMCPSR/Race, Ethnicity, PhD Year Inst. Data and Lit/APSA race and ethnicity data"
setwd(work_dir_apsa)

### load final APSA race and ethnicity data
author_level <- read.csv("final_apsa_list.csv") %>%
  dplyr::select(-X)

### reset working directory
setwd(work_dir)

### pivot author-level data to "longer" author-article-level dataframe
article_author_level <- author_level %>%
  dplyr::select(FirstName_MI_LastName_str,
                full_name.kap,
                Article.1.Code.kap, 
                Article.2.Code.kap,
                Article.3.Code.kap,
                Article.4.Code.kap,
                Article.5.Code.kap,
                Article.6.Code.kap,
                Article.7.Code.kap,
                Article.8.Code.kap,
                Article.9.Code.kap,
                Article.10.Code.kap,
                Gender.apsa,
                R_E.apsa, 
                PhD.Year,
                PhD.Institution) %>%
  pivot_longer(cols = -c(FirstName_MI_LastName_str,  
                         full_name.kap,
                         Gender.apsa,
                         R_E.apsa, 
                         PhD.Year,
                         PhD.Institution),
               values_to = "article_title",
               names_to = "names") %>%
  subset(article_title != "NA") %>%
  relocate(article_title)

###############################################

### race / ethnicity / gender dummy variables

###############################################

### identify race / ethnicity names
unique(article_author_level$R_E.apsa)

### create race / ethnicity dummy variables
article_author_level <- article_author_level %>%
  mutate(white = ifelse(grepl("white", R_E.apsa, ignore.case = TRUE), 1, 
                        ifelse(is.na(R_E.apsa) | grepl("Prefer", R_E.apsa, ignore.case = TRUE), NA,
                               0)),
         black = ifelse(grepl("black", R_E.apsa, ignore.case = TRUE), 1, 
                        ifelse(is.na(R_E.apsa) | grepl("Prefer", R_E.apsa, ignore.case = TRUE), NA,
                               0)),
         east_asian = ifelse(R_E.apsa == "East Asian or Asian American" |
                               R_E.apsa == "Asian" |
                               R_E.apsa == "Mixed: White and Asian." |
                               R_E.apsa == "Eurasian" |
                               R_E.apsa == "White/Asian", 1, 
                             ifelse(is.na(R_E.apsa) | grepl("Prefer", R_E.apsa, ignore.case = TRUE), NA,
                                    0)),
         south_asian = ifelse(grepl("south asian", R_E.apsa, ignore.case = TRUE), 1, 
                              ifelse(is.na(R_E.apsa) | grepl("Prefer", R_E.apsa, ignore.case = TRUE), NA,
                                     0)),
         latino = ifelse(R_E.apsa == "Latino or Hispanic American", 1, 
                         ifelse(is.na(R_E.apsa) | grepl("Prefer", R_E.apsa, ignore.case = TRUE), NA,
                                0)),
         mena = ifelse(R_E.apsa == "Middle Eastern or Arab American" |
                         R_E.apsa == "Half Anglo-White / Half Middle Eastern" |
                         R_E.apsa == "Turkish", 1, 
                       ifelse(is.na(R_E.apsa) | grepl("Prefer", R_E.apsa, ignore.case = TRUE), NA,
                              0)),
         native = ifelse(R_E.apsa == "Native American or Alaskan Native", 1, 
                         ifelse(is.na(R_E.apsa) | grepl("Prefer", R_E.apsa, ignore.case = TRUE), NA,
                                0)),
         pacific = ifelse(R_E.apsa == "Native Hawaiian or Other Pacific Islander", 1, 
                          ifelse(is.na(R_E.apsa) | grepl("Prefer", R_E.apsa, ignore.case = TRUE), NA,
                                 0))) %>%
  mutate(other = ifelse(white == 0 &
                          black == 0 &
                          east_asian == 0 &
                          south_asian == 0 &
                          latino == 0 &
                          mena == 0 &
                          native == 0 &
                          pacific == 0, 1,
                        ifelse(is.na(R_E.apsa) | grepl("Prefer", R_E.apsa, ignore.case = TRUE), NA,
                               0)))

### create gender dummy variables
article_author_level <- article_author_level %>%
  mutate(male_apsa = ifelse(Gender.apsa == "Male" |
                              Gender.apsa == "Male, Prefer not to disclose", 1, 
                            ifelse(is.na(Gender.apsa) | 
                                     grepl("Prefer", Gender.apsa, ignore.case = TRUE), NA,
                                   0)),
         female_apsa = ifelse(Gender.apsa == "Female", 1, 
                              ifelse(is.na(Gender.apsa) | 
                                       grepl("Prefer", Gender.apsa, ignore.case = TRUE), NA,
                                     0)),
         nonbinary_apsa = ifelse(Gender.apsa == "Nonbinary", 1,  
                                 ifelse(is.na(Gender.apsa) | 
                                          grepl("Prefer", Gender.apsa, ignore.case = TRUE), NA,
                                        0)))

### export author-level CSV
write.csv(article_author_level, "author_level_final.csv")

########################

### Code for PAP

########################

########################

### Gender

########################

### author-level count and proportion for all gender identity categories
summary_gender_author <- article_author_level %>%
  dplyr::select(-c(article_title, names)) %>% 
  distinct() %>%
  summarize(count = c(sum(male_apsa, na.rm = TRUE), 
                      sum(female_apsa, na.rm = TRUE), 
                      sum(nonbinary_apsa, na.rm = TRUE)))

summary_gender_author <- summary_gender_author %>%
  mutate(proportion = round(count / sum(count), 2)) %>%
  mutate(gender = c("male", "female", "nonbinary")) %>%
  dplyr::select(gender, count, proportion)

### author-level count and proportion for all race / ethnic identity categories
summary_race_ethnicity_author <- article_author_level %>%
  dplyr::select(-c(article_title, names)) %>% 
  distinct() %>%
  summarize(count = c(sum(white, na.rm = TRUE),
                      sum(black, na.rm = TRUE),
                      sum(east_asian, na.rm = TRUE),
                      sum(south_asian, na.rm = TRUE),
                      sum(latino, na.rm = TRUE), 
                      sum(mena, na.rm = TRUE),
                      sum(native, na.rm = TRUE),
                      sum(pacific, na.rm = TRUE),
                      sum(other, na.rm = TRUE)))

summary_race_ethnicity_author <- summary_race_ethnicity_author %>%
  mutate(race_ethnicity = c("White", "Black", "East Asian", "South Asian",
                            "Hispanic or Latino", "Middle Eastern / Arab", "Native", 
                            "Pacific Islander", "Other")) %>%
  mutate(proportion = round(count / sum(count), 2)) %>%
  dplyr::select(race_ethnicity, count, proportion)

### article-level authorship structure information for gender categories
gender_article <- article_author_level %>%
  group_by(article_title) %>%
  mutate(single_authored_male = ifelse(male_apsa == 1 & n() == 1, 1, 0),
         single_authored_female = ifelse(female_apsa == 1 & n() == 1, 1, 0),
         co_authored_male = ifelse(mean(male_apsa) == 1 & n() > 1, 1, 0),
         co_authored_female = ifelse(mean(female_apsa) == 1 & n() > 1, 1, 0),
         co_authored_mixed = ifelse(mean(male_apsa) < 1 & n() > 1, 1, 0)) %>%
  dplyr::select(article_title, single_authored_male, single_authored_female,
                co_authored_male, co_authored_female, co_authored_mixed) %>%
  distinct()

### article-level summary for all gender categories
summary_gender_article <- gender_article %>%
  ungroup() %>%
  summarize(count = c(sum(single_authored_male, na.rm = TRUE), 
                      sum(single_authored_female, na.rm = TRUE), 
                      sum(co_authored_male, na.rm = TRUE),
                      sum(co_authored_female, na.rm = TRUE),
                      sum(co_authored_mixed, na.rm = TRUE)))

summary_gender_article <- summary_gender_article %>%
  mutate(proportion = round(count / sum(count), 2)) %>%
  mutate(gender = c("single-authored male", "single-authored female", "co-authored male", "co-authored female", "co-authored mixed-gender")) %>%
  dplyr::select(gender, count, proportion)

### write CSV for article-level gender info
write.csv(gender_article, "gender_article.csv")

### anonymize article-level data
gender_article_anonymized <- gender_article %>%
  mutate(single_authored_male = "Use restricted per APSA user agreement; please contact authors",
         single_authored_female = "Use restricted per APSA user agreement; please contact authors",
         co_authored_male = "Use restricted per APSA user agreement; please contact authors",
         co_authored_female = "Use restricted per APSA user agreement; please contact authors",
         co_authored_mixed = "Use restricted per APSA user agreement; please contact authors")

### write CSV for anonymized article-level gender info
write.csv(gender_article_anonymized, "gender_article_anonymized.csv")

########################

### Race / ethnicity

########################

### article-level authorship structure for race / ethnic identity categories
race_ethnicity_article <- article_author_level %>%
  group_by(article_title) %>%
  mutate(white_authors = ifelse(mean(white) == 1, 1, 0),
         black_authors = ifelse(mean(black) == 1, 1, 0),
         east_asian_authors = ifelse(mean(east_asian) == 1, 1, 0),
         south_asian_authors = ifelse(mean(south_asian) == 1, 1, 0),
         latino_authors = ifelse(mean(latino) == 1, 1, 0),
         mena_authors = ifelse(mean(mena) == 1, 1, 0),
         native_authors = ifelse(mean(native) == 1, 1, 0),
         pacific_authors = ifelse(mean(pacific) == 1, 1, 0),
         other_authors = ifelse(mean(other) == 1, 1, 0),
         mixed_authors = ifelse(mean(white) < 1 & n() > 1, 1, 0)) %>%
  dplyr::select(article_title, white_authors, black_authors,
                east_asian_authors, south_asian_authors, latino_authors,
                mena_authors, native_authors, pacific_authors, other_authors,
                mixed_authors) %>%
  distinct()

### article-level summary for all race / ethnicity identity categories
summary_race_ethnicity_article <- race_ethnicity_article %>%
  ungroup() %>%
  summarize(count = c(sum(white_authors, na.rm = TRUE), 
                      sum(black_authors, na.rm = TRUE), 
                      sum(east_asian_authors, na.rm = TRUE),
                      sum(south_asian_authors, na.rm = TRUE),
                      sum(latino_authors, na.rm = TRUE),
                      sum(mena_authors, na.rm = TRUE),
                      sum(native_authors, na.rm = TRUE),
                      sum(pacific_authors, na.rm = TRUE),
                      sum(other_authors, na.rm = TRUE),
                      sum(mixed_authors, na.rm = TRUE)))

summary_race_ethnicity_article <- summary_race_ethnicity_article %>%
  mutate(proportion = round(count / sum(count), 2)) %>%
  mutate(race_ethnicity = c("all-White authors", "all-Black authors", "all-East Asian authors", 
                            "all-South Asian authors", "all-Latino authors", "all-MENA authors", 
                            "all-Native authors", "all-Pacific Islander authors", "all-Other authors", 
                            "Mixed authors")) %>%
  dplyr::select(race_ethnicity, count, proportion)

### write CSV for article-level race and ethnicity info
write.csv(race_ethnicity_article, "race_ethnicity_article.csv")

### anonymize article-level data
race_ethnicity_article_anonymized <- race_ethnicity_article %>%
  mutate(white_authors = "Use restricted per APSA user agreement; please contact authors",
         black_authors = "Use restricted per APSA user agreement; please contact authors",
         east_asian_authors = "Use restricted per APSA user agreement; please contact authors",
         south_asian_authors = "Use restricted per APSA user agreement; please contact authors",
         latino_authors = "Use restricted per APSA user agreement; please contact authors",
         mena_authors = "Use restricted per APSA user agreement; please contact authors",
         native_authors = "Use restricted per APSA user agreement; please contact authors",
         pacific_authors = "Use restricted per APSA user agreement; please contact authors",
         other_authors = "Use restricted per APSA user agreement; please contact authors",
         mixed_authors = "Use restricted per APSA user agreement; please contact authors")

### write CSV for anonymized article-level race and ethnicity info
write.csv(race_ethnicity_article_anonymized, "race_ethnicity_article_anonymized.csv")