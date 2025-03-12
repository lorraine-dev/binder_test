rm(list = ls())

setwd("~/Documents/Methods Project/Analysis")

install.packages("sjPlot")
install.packages("formattable")
install.packages("hrbrthemes")
install.packages("Hmisc")
install.packages("janitor")
install.packages("mfx")
install.packages("jtools")
install.packages("ggstance")
install.packages("broom.mixed")
install.packages("nnet")
install.packages("margins")
install.packages("remotes")
remotes::install_github("sysilviakim/Kmisc")
install.packages("reshape2")

#load in packages
library("margins")
library("ggplot2")
library("stargazer")
library("dplyr")
library("tidyr")
library("formattable")
library (scales)
library("mfx")
library(readxl)
library(Kmisc)
library(reshape2)


#load data####
load("~/Documents/Methods Project/Analysis/MMCPSR Analysis Data No PT.RData")

author_phd_data <- read.csv("MMCPSR Author Database - Final Sample_071522.csv", na.strings = "")

#remove unnecessary columns
author_phd_data <- subset(author_phd_data, select = -c(7,14,17,20,23,26,29,32,35,38,41,43,44))

#clean data####
#create new column to recode PhD Year
author_phd_data$PHDYEAR <- ifelse(author_phd_data$PhD.Year == "NAV", -999, ifelse(author_phd_data$PhD.Year == "PNDG", 0, ifelse(author_phd_data$PhD.Year == "No Phd", -888, ifelse(author_phd_data$PhD.Year == "No PhD", -888, ifelse(author_phd_data$PhD.Year == "No PhD ", -888, ifelse(author_phd_data$PhD.Year == "Not PS PhD", -555, ifelse(author_phd_data$PhD.Year>0, author_phd_data$PhD.Year, "-777")))))))

#check recode
unique(author_phd_data$PHDYEAR)

#change data type of PHDYEAR to numeric
author_phd_data$PHDYEAR <-as.numeric(author_phd_data$PHDYEAR)

#calculate years from phd to article publication
author_phd_data$yrs_to_article1 <- ifelse(author_phd_data$PHDYEAR ==0,0,author_phd_data$Article.1.Year.published-author_phd_data$PHDYEAR)

author_phd_data$yrs_to_article2 <- ifelse(author_phd_data$PHDYEAR ==0,0,author_phd_data$Article.2.Year.Published -author_phd_data$PHDYEAR)

author_phd_data$yrs_to_article3 <- ifelse(author_phd_data$PHDYEAR ==0,0, author_phd_data$Article.3.Year.Published-author_phd_data$PHDYEAR)

author_phd_data$yrs_to_article4 <- ifelse(author_phd_data$PHDYEAR ==0,0, author_phd_data$Article.4.Year.Published-author_phd_data$PHDYEAR)

author_phd_data$yrs_to_article5 <- ifelse(author_phd_data$PHDYEAR ==0,0, author_phd_data$Article.5.Year.Published-author_phd_data$PHDYEAR)

author_phd_data$yrs_to_article6 <- ifelse(author_phd_data$PHDYEAR ==0,0, author_phd_data$Article.6.Year.Published-author_phd_data$PHDYEAR)

author_phd_data$yrs_to_article7 <- ifelse(author_phd_data$PHDYEAR ==0,0, author_phd_data$Article.7.Year.Published-author_phd_data$PHDYEAR)

author_phd_data$yrs_to_article8 <- ifelse(author_phd_data$PHDYEAR ==0,0,author_phd_data$Article.8.Year.Published-author_phd_data$PHDYEAR)

author_phd_data$yrs_to_article9 <- ifelse(author_phd_data$PHDYEAR ==0,0,author_phd_data$Article.9.Year.Published-author_phd_data$PHDYEAR)

author_phd_data$yrs_to_article10 <- ifelse(author_phd_data$PHDYEAR ==0,0,author_phd_data$Article.10.Year.Published-author_phd_data$PHDYEAR)


#calculate seniority of each author at the time of each article
author_phd_data$status_article1 <- ifelse(author_phd_data$PHDYEAR == 0 |author_phd_data$PHDYEAR > author_phd_data$Article.1.Year.published, "Grad Student", ifelse(author_phd_data$yrs_to_article1 == 0|author_phd_data$yrs_to_article1 < 7, "Junior Scholar", ifelse(author_phd_data$yrs_to_article1 > 6, "Senior Scholar", "NA")))

author_phd_data$status_article2 <- ifelse(author_phd_data$PHDYEAR == 0 |author_phd_data$PHDYEAR > author_phd_data$Article.2.Year.Published, "Grad Student", ifelse(author_phd_data$yrs_to_article2 == 0|author_phd_data$yrs_to_article2 < 7, "Junior Scholar", ifelse(author_phd_data$yrs_to_article2 > 6, "Senior Scholar", "NA")))

author_phd_data$status_article3 <- ifelse(author_phd_data$PHDYEAR == 0 |author_phd_data$PHDYEAR > author_phd_data$Article.3.Year.Published, "Grad Student", ifelse(author_phd_data$yrs_to_article3 == 0|author_phd_data$yrs_to_article3 < 7, "Junior Scholar", ifelse(author_phd_data$yrs_to_article3 > 6, "Senior Scholar", "NA")))

author_phd_data$status_article4 <- ifelse(author_phd_data$PHDYEAR == 0 |author_phd_data$PHDYEAR > author_phd_data$Article.4.Year.Published, "Grad Student", ifelse(author_phd_data$yrs_to_article4 == 0|author_phd_data$yrs_to_article4 < 7, "Junior Scholar", ifelse(author_phd_data$yrs_to_article4 > 6, "Senior Scholar", "NA")))

author_phd_data$status_article5 <- ifelse(author_phd_data$PHDYEAR == 0 |author_phd_data$PHDYEAR > author_phd_data$Article.5.Year.Published, "Grad Student", ifelse(author_phd_data$yrs_to_article5 == 0|author_phd_data$yrs_to_article5 < 7, "Junior Scholar", ifelse(author_phd_data$yrs_to_article5 > 6, "Senior Scholar", "NA")))

author_phd_data$status_article6 <- ifelse(author_phd_data$PHDYEAR == 0 |author_phd_data$PHDYEAR > author_phd_data$Article.6.Year.Published, "Grad Student", ifelse(author_phd_data$yrs_to_article6 == 0|author_phd_data$yrs_to_article6 < 7, "Junior Scholar", ifelse(author_phd_data$yrs_to_article6 > 6, "Senior Scholar", "NA")))

author_phd_data$status_article7 <- ifelse(author_phd_data$PHDYEAR == 0 |author_phd_data$PHDYEAR > author_phd_data$Article.7.Year.Published, "Grad Student", ifelse(author_phd_data$yrs_to_article7 == 0|author_phd_data$yrs_to_article7 < 7, "Junior Scholar", ifelse(author_phd_data$yrs_to_article7 > 6, "Senior Scholar", "NA")))

author_phd_data$status_article8 <- ifelse(author_phd_data$PHDYEAR == 0 |author_phd_data$PHDYEAR > author_phd_data$Article.8.Year.Published, "Grad Student", ifelse(author_phd_data$yrs_to_article8 == 0|author_phd_data$yrs_to_article8 < 7, "Junior Scholar", ifelse(author_phd_data$yrs_to_article8 > 6, "Senior Scholar", "NA")))

author_phd_data$status_article9 <- ifelse(author_phd_data$PHDYEAR == 0 |author_phd_data$PHDYEAR > author_phd_data$Article.9.Year.Published, "Grad Student", ifelse(author_phd_data$yrs_to_article9 == 0|author_phd_data$yrs_to_article9 < 7, "Junior Scholar", ifelse(author_phd_data$yrs_to_article9 > 6, "Senior Scholar", "NA")))

author_phd_data$status_article10 <- ifelse(author_phd_data$PHDYEAR == 0 |author_phd_data$PHDYEAR > author_phd_data$Article.10.Year.Published, "Grad Student", ifelse(author_phd_data$yrs_to_article10 == 0|author_phd_data$yrs_to_article10 < 7, "Junior Scholar", ifelse(author_phd_data$yrs_to_article10 > 6, "Senior Scholar", "NA")))

#Create Author Name variable
author_phd_data$Middle.Initial..Name <- na_if(author_phd_data$Middle.Initial..Name, "NAV")

author_phd_data$Author_Name <- paste_na(author_phd_data$First.Name..Initial,author_phd_data$Middle.Initial..Name, author_phd_data$Last.Name, sep = " ")

#calculate total number of articles per author
author_phd_data$article1 <-ifelse(!is.na(author_phd_data$Article.1.Code), 1,0)
author_phd_data$article2 <-ifelse(!is.na(author_phd_data$Article.2.Code), 1,0)
author_phd_data$article3 <-ifelse(!is.na(author_phd_data$Article.3.Code), 1,0)
author_phd_data$article4 <-ifelse(!is.na(author_phd_data$Article.4.Code), 1,0)
author_phd_data$article5 <-ifelse(!is.na(author_phd_data$Article.5.Code), 1,0)
author_phd_data$article6 <-ifelse(!is.na(author_phd_data$Article.6.Code), 1,0)
author_phd_data$article7 <-ifelse(!is.na(author_phd_data$Article.7.Code), 1,0)
author_phd_data$article8 <-ifelse(!is.na(author_phd_data$Article.8.Code), 1,0)
author_phd_data$article9 <-ifelse(!is.na(author_phd_data$Article.9.Code), 1,0)
author_phd_data$article10 <-ifelse(!is.na(author_phd_data$Article.10.Code), 1,0)

author_phd_data$total_articles <-rowSums(author_phd_data[56:65])


#save dataframe
save(author_phd_data, file = "Author Phd Data.RData")


#Descriptive analysis#####
#number of authors in and outside the discipline (excludes those coded as NAV)

authors_by_discipline <- subset(author_phd_data, author_phd_data$PhD.Year!= "NAV")
authors_by_discipline$PS_scholar <- ifelse(authors_by_discipline$PhD.Year == "Not PS PhD", 0,1)
PS_scholars <- authors_by_discipline %>%count(authors_by_discipline$PS_scholar)
PS_scholars$percent <- formattable::percent(PS_scholars$n/sum(PS_scholars$n), digits = 2)


#remove those with no articles and without PS Phd
PS_author_phd_data <- subset(author_phd_data, author_phd_data$total_articles >0)
PS_author_phd_data <-subset(PS_author_phd_data, PS_author_phd_data$PHDYEAR > -1)

#save dataframe  
save(PS_author_phd_data, file = "PS Authors_complete.RData")


#Average number of articles per author
summary(PS_author_phd_data$total_articles)

#Number of authors by number of articles publishes
num_articles_groups <- PS_author_phd_data %>% count(total_articles)
colnames(num_articles_groups) <- c("Total_articles", "Total_authors")

ggplot(PS_author_phd_data, aes(x=total_articles)) + geom_histogram(color="black", fill="lightblue", binwidth = 1, bins = 8)+ theme_minimal()+xlab("Articles Published") + ylab("Number of Authors")

#create subsets of authors 
authors1 <- PS_author_phd_data %>% 
  dplyr::select(Author_Name,
                PHDYEAR,
                Article.1.Code,
                Article.1.Year.published,
                yrs_to_article1,
                status_article1)

colnames(authors1) <- c("Author.Name",
                       "Phd.Year",
                       "Article.Code",
                       "Article.Year",
                       "yrs.to.article",
                       "status.article")


authors2 <- PS_author_phd_data %>% 
  dplyr::select(Author_Name,
                PHDYEAR,
                Article.2.Code,
                Article.2.Year.Published,
                yrs_to_article2,
                status_article2)

colnames(authors2) <- c("Author.Name",
                        "Phd.Year",
                        "Article.Code",
                        "Article.Year",
                        "yrs.to.article",
                        "status.article")

authors3 <- PS_author_phd_data %>% 
  dplyr::select(Author_Name,
                PHDYEAR,
                Article.3.Code,
                Article.3.Year.Published,
                yrs_to_article3,
                status_article3)

colnames(authors3) <- c("Author.Name",
                        "Phd.Year",
                        "Article.Code",
                        "Article.Year",
                        "yrs.to.article",
                        "status.article")


authors4 <- PS_author_phd_data %>% 
  dplyr::select(Author_Name,
                PHDYEAR,
                Article.4.Code,
                Article.4.Year.Published,
                yrs_to_article4,
                status_article4)

colnames(authors4) <- c("Author.Name",
                        "Phd.Year",
                        "Article.Code",
                        "Article.Year",
                        "yrs.to.article",
                        "status.article")

authors5 <- PS_author_phd_data %>% 
  dplyr::select(Author_Name,
                PHDYEAR,
                Article.5.Code,
                Article.5.Year.Published,
                yrs_to_article5,
                status_article5)

colnames(authors5) <- c("Author.Name",
                        "Phd.Year",
                        "Article.Code",
                        "Article.Year",
                        "yrs.to.article",
                        "status.article")
authors6 <- PS_author_phd_data %>% 
  dplyr::select(Author_Name,
                PHDYEAR,
                Article.6.Code,
                Article.6.Year.Published,
                yrs_to_article6,
                status_article6)

colnames(authors6) <- c("Author.Name",
                        "Phd.Year",
                        "Article.Code",
                        "Article.Year",
                        "yrs.to.article",
                        "status.article")

authors7 <- PS_author_phd_data %>% 
  dplyr::select(Author_Name,
                PHDYEAR,
                Article.7.Code,
                Article.7.Year.Published,
                yrs_to_article7,
                status_article7)

colnames(authors7) <- c("Author.Name",
                        "Phd.Year",
                        "Article.Code",
                        "Article.Year",
                        "yrs.to.article",
                        "status.article")

authors8 <- PS_author_phd_data %>% 
  dplyr::select(Author_Name,
                PHDYEAR,
                Article.8.Code,
                Article.8.Year.Published,
                yrs_to_article8,
                status_article8)

colnames(authors8) <- c("Author.Name",
                        "Phd.Year",
                        "Article.Code",
                        "Article.Year",
                        "yrs.to.article",
                        "status.article")


authors9 <- PS_author_phd_data %>% 
  dplyr::select(Author_Name,
                PHDYEAR,
                Article.9.Code,
                Article.9.Year.Published,
                yrs_to_article9,
                status_article9)

colnames(authors9) <- c("Author.Name",
                        "Phd.Year",
                        "Article.Code",
                        "Article.Year",
                        "yrs.to.article",
                        "status.article")

authors10 <- PS_author_phd_data %>% 
  dplyr::select(Author_Name,
                PHDYEAR,
                Article.10.Code,
                Article.10.Year.Published,
                yrs_to_article10,
                status_article10)

colnames(authors10) <- c("Author.Name",
                        "Phd.Year",
                        "Article.Code",
                        "Article.Year",
                        "yrs.to.article",
                        "status.article")


#join together subsets and drop missing values
PS_authors<- rbind(authors1, authors2, authors3, authors4, authors5, authors6, authors7, authors8, authors9, authors10)
PS_authors <- drop_na(PS_authors)
total.articles  <- PS_authors %>% count(Author.Name)
PS_authors <-inner_join(PS_authors, total.articles)

#save dataframe
save(PS_authors, file = "PS Authors_subset_071522.RData")

#Average years from Ph.D. to publication
summary(PS_authors$yrs.to.article)

#Average years from Ph.D. to publication by publication year
avg_years_pub <- PS_authors %>% group_by(Article.Year) %>%summarise(avg = round(mean(yrs.to.article), digits = 1))

#plot
ggplot(avg_years_pub, aes(x = Article.Year, y = avg)) + geom_line() + scale_color_brewer(palette="Set2") + theme_minimal(base_size = 12) + labs(y = "Average Years to Publication", x = "Publication Year") + ylim(0,12) 

#Number of authors by Ph.D. year
phd_year <- PS_authors %>% count(Phd.Year)

#plot (excludes current grad students, n = 4)
ggplot(subset(phd_year, phd_year$Phd.Year>0), aes(x =Phd.Year, y = n,fill = Ph.Year)) + geom_bar(stat = "identity", fill = "steelblue3") + theme_minimal(base_size = 12) + labs(x = "Phd Year", y = "Number of Authors") +theme(legend.position="none")

#Number of authors by seniority (grad student/junior scholar/senior scholar) by publication year
seniority <- PS_authors %>% count(Article.Year, status.article)

#plot
ggplot(seniority, aes(x = Article.Year, y = n, group = status.article, color = status.article)) + geom_line() + scale_color_brewer(palette="Set2") + theme_minimal(base_size = 12) + labs(y = "Number of Authors", x = "Publication Year")+ theme(legend.position="bottom") + theme(legend.title = element_blank())


#run check for article codes####
#load in all article data
load("~/Documents/Methods Project/Analysis/MMCPSR Analysis Data.RData")

#create an article code variable for linking
MMCPSR_Data$Article.Code <- as.character(strsplit(MMCPSR_Data$Title, ".pdf"))

#join article level data with author level data
article_code_check <- left_join(PS_authors, MMCPSR_Data, by = c("Article.Code" = "Article.Code"))

#subset 
article_code_check_missing <- subset(article_code_check, is.na(article_code_check$Title))

#write to csv
#write.csv(article_code_check_missing, file = "Authors with article code errors.csv")


#Correlations/Bivariate regression####

#FROM HERE THROUGH THE REST OF THE ANALYSIS – EXCLUDE ARTICLES CODED PT + No discernible method

#join authors to mmcpsr data
MMCPSR_emp$Article.Code <- as.character(strsplit(MMCPSR_emp$Title, ".pdf"))
mmcpsr_authors <- left_join(PS_authors, MMCPSR_emp, by = c("Article.Code" = "Article.Code"))
mmcpsr_authors <-drop_na(mmcpsr_authors)

#run check to see if any authors have missing data
check <- subset(mmcpsr_authors, is.na(mmcpsr_authors$Title))

#save data
save(mmcpsr_authors, file = "MMCPSR Authors.RData")


#create individual variables for each seniority level
mmcpsr_authors$Early_Scholar <- ifelse(mmcpsr_authors$status.article == "Senior Scholar", 0, 1)
mmcpsr_authors$Grad_Student <- ifelse(mmcpsr_authors$status.article == "Grad Student", 1, 0)
mmcpsr_authors$Jr_Scholar <- ifelse(mmcpsr_authors$status.article == "Junior Scholar", 1, 0)
mmcpsr_authors$Sr_Scholar <- ifelse(mmcpsr_authors$status.article == "Senior Scholar", 1, 0)

#calculate number of articles published by each author by status 
article_status <- mmcpsr_authors %>% count(Author.Name, status.article)
article_status <- rename(article_status, c("status.article.count" = "n"))

#join to mmcpsr_authors
mmcpsr_authors <- left_join(mmcpsr_authors, article_status, by = c("Author.Name" = "Author.Name", "status.article"="status.article"))

#save data
save(mmcpsr_authors, file = "MMCPSR Authors.RData")

#OLS - seniority vs publishing volume
article_status$Early_Scholar <- ifelse(article_status$status.article == "Senior Scholar", 0,1)

out.28 <- lm(article_status$status.article.count ~article_status$Early_Scholar)
  

#OLS - seniority vs authorship structure
mmcpsr_authors$Solo_Authored <- ifelse(mmcpsr_authors$`Single-authored male` == 1 | mmcpsr_authors$`Single-authored female` == 1, 1, 0)

solo_articles <- mmcpsr_authors %>% count(Author.Name, status.article, Solo_Authored)
solo_articles <- solo_articles %>% pivot_wider(id_cols = c(Author.Name, status.article), names_from = Solo_Authored, values_from = n, values_fill = 0)
colnames(solo_articles) <- c("Author.Name", "status.article", "solo.article.count", "coauthored.count")
solo_articles$Solo_authored <- ifelse(solo_articles$solo.article.count > 0, 1, 0 )
solo_articles$grad_student <- ifelse(solo_articles$status.article == "Grad Student", 1, 0)
solo_articles$jr_scholar <- ifelse(solo_articles$status.article == "Junior Scholar", 1, 0)

out.29a  <- lm(solo_articles$Solo_authored ~ solo_articles$grad_student)

out.29b  <- lm(solo_articles$Solo_authored ~ solo_articles$jr_scholar)

out.29a.probit <- glm(solo_articles$Solo_authored ~ solo_articles$grad_student, family = binomial(link = "probit"))
probitmfx(out.29a.probit, data = solo_articles)

out.29b.probit <- glm(solo_articles$Solo_authored ~ solo_articles$jr_scholar, family = binomial(link = "probit"))
probitmfx(out.29b.probit, data = solo_articles)


#OLS - seniority vs causal identification strategy
#transform data to article level
mmcpsr_articles <- mmcpsr_authors %>% 
  subset(select = c(Article.Code, Phd.Year, Early_Scholar)) %>% pivot_longer(cols = 2:3) %>% pivot_wider(id_cols = Article.Code, names_from = name,  values_from = value) %>% unnest_wider(col = c(Phd.Year, Early_Scholar), names_sep = "_")

mmcpsr_articles$PhdMax<- pmax(mmcpsr_articles$Phd.Year_1, mmcpsr_articles$Phd.Year_2, mmcpsr_articles$Phd.Year_3, mmcpsr_articles$Phd.Year_4,mmcpsr_articles$Phd.Year_5,mmcpsr_articles$Phd.Year_6,mmcpsr_articles$Phd.Year_7,mmcpsr_articles$Phd.Year_8,mmcpsr_articles$Phd.Year_9,mmcpsr_articles$Phd.Year_10,mmcpsr_articles$Phd.Year_11, na.rm = TRUE)

#create indicator, articles has at least one author with a phd granted in 2012 or later
mmcpsr_articles$post2012 <- ifelse(mmcpsr_articles$PhdMax>2011, 1,0)

#create indicator, articles has at least one author who is a grad student or junior scholar
mmcpsr_articles$Early_scholar <- pmax(mmcpsr_articles$Early_Scholar_1,mmcpsr_articles$Early_Scholar_2,mmcpsr_articles$Early_Scholar_3,mmcpsr_articles$Early_Scholar_4,mmcpsr_articles$Early_Scholar_5,mmcpsr_articles$Early_Scholar_6,mmcpsr_articles$Early_Scholar_7,mmcpsr_articles$Early_Scholar_8,mmcpsr_articles$Early_Scholar_9,mmcpsr_articles$Early_Scholar_10, mmcpsr_articles$Early_Scholar_11, na.rm = TRUE)

mmcpsr_emp_sub<- MMCPSR_emp %>% 
  subset(select = c(Article.Code, CausalHigh, CausalControl, CausalMixed, QualExplicit, QuantQual))

mmcpsr_articles_sub <- mmcpsr_articles %>% subset(select = c(Article.Code, post2012, Early_scholar))

mmcpsr_articles_sub <- left_join(mmcpsr_articles_sub, mmcpsr_emp_sub, by = c("Article.Code" = "Article.Code"))

#OLS/probit regressions
out.30a <- lm(mmcpsr_articles_sub$CausalHigh ~ mmcpsr_articles_sub$Early_scholar)

out.30a.probit <- glm(mmcpsr_articles_sub$CausalHigh ~ mmcpsr_articles_sub$Early_scholar, family = binomial(link="probit"))
probitmfx(out.30a.probit, data = mmcpsr_articles_sub)

out.30b <- lm(mmcpsr_articles_sub$CausalControl ~ mmcpsr_articles_sub$Early_scholar)

out.30b.probit <- glm(mmcpsr_articles_sub$CausalControl ~ mmcpsr_articles_sub$Early_scholar, family = binomial(link="probit"))
probitmfx(out.30b.probit, data = mmcpsr_articles_sub)

out.30c <- lm(mmcpsr_articles_sub$CausalMixed~mmcpsr_articles_sub$Early_scholar)

out.30c.probit <- glm(mmcpsr_articles_sub$CausalMixed ~ mmcpsr_articles_sub$Early_scholar, family = binomial(link="probit"))
probitmfx(out.30c.probit, data = mmcpsr_articles_sub)

#OLS - PhD granted after 2012 vs Multi-method research (combining quantitative and qualitative methods)
out.31 <- lm(mmcpsr_articles_sub$QuantQual ~ mmcpsr_articles_sub$post2012)

out.31.probit <- glm(mmcpsr_articles_sub$QuantQual ~ mmcpsr_articles_sub$post2012, family = binomial(link = "probit"))
probitmfx(out.31.probit, data = mmcpsr_articles_sub)


#OLS - PhD granted after 2012 > Explicit qualitative methods

out.32 <- lm(mmcpsr_articles_sub$QualExplicit ~ mmcpsr_articles_sub$post2012)

out.32.probit <- glm(mmcpsr_articles_sub$QualExplicit ~ mmcpsr_articles_sub$post2012, family = binomial(link = "probit"))
probitmfx(out.32.probit, data = mmcpsr_articles_sub)


