setwd("~/Documents/Methods Project/Analysis")

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
library(reshape)

#import data
Courses <- read.csv("MMCPSR_Syllabus Coding - Courses.csv")
ICPSR <- read.csv("MMCPSR_Syllabus Coding - ICPSR.csv")

#format data 
Courses$Method <- ifelse(Courses$method == "Qual General"|
                           Courses$method == "Qual Collection"|
                           Courses$method == "Qual Analysis", "Qual",
                         ifelse(Courses$method == "Mixed Analysis", "Mixed",
                                ifelse(Courses$method == "Unclear", "Unclear", "Quant")))

Courses$Qual <- ifelse(Courses$Method == "Qual", 1, 0)
Courses$Quant <- ifelse(Courses$Method == "Quant", 1, 0)

#Qual courses only
Qual_courses <- Courses %>% filter(Qual == 1)


#Qual textbooks
Qual_course_sub <- subset(Qual_courses, select = c(file_name,
                                                   textbook_1,
                                                   textbook_2,
                                                   textbook_3,
                                                   textbook_4,
                                                   textbook_5,
                                                   textbook_6,
                                                   textbook_7,
                                                   textbook_8,
                                                   textbook_9,
                                                   textbook_10))

Qual_texts <- pivot_longer(Qual_course_sub, cols = "textbook_1":"textbook_10", values_to = "Textbook") 

Qual_texts <- Qual_texts %>% filter(Textbook != "NAV") %>% count(Textbook)

sum(Qual_texts$n)

write.csv(Qual_texts, file = "Qual Textbooks updated.csv")

####Descriptive Analysis####
#number of institutions
institutions <- Courses %>% count(institution)
nrow(institutions)

institutions_n <- Courses %>% 
  filter(box_folder == "DGS Outreach - Quant") %>% 
  count(institution)

institutions_n$n <- as.numeric(institutions_n$n)

nrow(institutions_n)

round(nrow(institutions_n)/110, digits = 3)

#Number of syllabi by institution: The count of syllabi from each institution.

print(arrange(institutions_n, desc(n)))

#plot
ggplot(institutions_n, aes(x = reorder(institution, - n), y = n)) +
  geom_bar(stat = "identity", fill = "steelblue3") +
  xlab("Insitution") + ylab("Number of Syllabi") + theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

#Number of syllabi by year
syllabi_n <- Courses %>% count(year)

#plot
ggplot(data = subset(syllabi_n, syllabi_n$year != "NAV" & syllabi_n$year != "NAV "), aes(x = year, y = n)) +
  geom_bar(stat = "identity", fill = "steelblue3") +
  xlab("Year") + ylab("Number of Syllabi") + theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_text(aes(label = n), vjust = -0.2, size = 3)

#Average number of associated books, by course type

Courses$texts <- rowSums(Courses[35:44] != "NAV")

texts_n <- Courses %>%
  filter(Method != "Mixed" & Method != "Unclear") %>%
  filter(year != "NAV" & year != "NAV ") %>%
  group_by(year, Method) %>% 
  summarize(avg = round(mean(texts), digits = 0))

texts_n

#plot
ggplot(texts_n, aes(x = year, y = avg, fill = Method)) +
  geom_bar(position="dodge", stat = "identity") +
  xlab("Year") + ylab("Average number of texts") + theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  scale_fill_manual(values = c("steelblue","darkblue")) + 
  theme(legend.title = element_blank())


#Number of syllabi including quantitative causal methods by year, by educational setting
Quant <- Courses %>% filter(Quant == 1)
Quant$causal <- ifelse(Quant$stats_id_strategy == 1, 1, 0)
ICPSR$causal <- ifelse(ICPSR$stats_id_strategy == 1, 1, 0)

Quant_causal <- Quant %>% 
  filter(causal == 1 & 
           year != "NAV" &
           year != "NAV ") %>%
  count(year)
Quant_causal$year <- as.integer(Quant_causal$year)

ICPSR_causal <- ICPSR %>% 
  filter(causal == 1 & 
           year != "NAV" &
           year != "NAV ") %>%
  count(year)

causal_join <- full_join(Quant_causal, ICPSR_causal, by = "year")
colnames(causal_join) <- c("Year", "Graduate Programs", "ICPSR")
causal_join[is.na(causal_join)] = 0

causal_join

causal_join <- pivot_longer(causal_join, cols = 2:3, names_to = "Setting", values_to = "Syllabi")

#plot
ggplot(causal_join, aes(x = Year, y = Syllabi, fill = Setting)) +
  geom_bar(position="dodge", stat = "identity") +
  xlab("Year") + ylab("Number of Syllabi") + theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("steelblue","darkblue")) + 
  theme(legend.title = element_blank())



#Number of qualitative syllabi, by focus
Qual_courses$DataAnalysis <- ifelse(Qual_courses$process_tracing == 1| 
                                      Qual_courses$qca == 1|
                                      Qual_courses$congruence == 1|
                                      Qual_courses$counterfactual == 1 |
                                      Qual_courses$case_comparison == 1, 1, 0)

Qual_courses$DataGen <- ifelse(Qual_courses$qual_data_gen == 1,1,0)
Qual_courses$Focus <- ifelse(Qual_courses$DataAnalysis == 1 & Qual_courses$DataGen == 0, "Data Analysis Only",
                             ifelse(Qual_courses$DataAnalysis == 0 & Qual_courses$DataGen == 1,"Data Generation Only","Data Analysis and Generation"))

QualFocus <- Qual_courses %>% count(Focus)

#plot
ggplot(QualFocus, aes(x = Focus, y = n))+
  geom_bar(stat = "identity", fill = "steelblue3") +
  xlab("Course Focus") + ylab("Number of Syllabi") + theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_text(aes(label = n), vjust = -0.2, size = 3)


#Number of syllabi featuring replication exercises, by course type

Exercises <- Courses %>%
  count(Qual, replication_exercise) %>%
  filter(replication_exercise != "NAV" & replication_exercise > 0)
Exercises$Method <- ifelse(Exercises$Qual == 1, "Qualitative", "Quantitative")

#plot
ggplot(Exercises, aes(x = Method, y = n))+
  geom_bar(stat = "identity", fill = "steelblue3") +
  xlab("Course Type") + ylab("Number of Syllabi") + theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_text(aes(label = n), vjust = -0.2, size = 3)


#Proportion of institutions with syllabi including causal identification strategies, by year
Courses$causal <- ifelse(Courses$process_tracing == 1 | 
                           Courses$stats_id_strategy == 1, 1, 0)

Instition_yr <- Courses %>%
  filter(!is.na(institution) & year != "NAV" & year != "NAV ") %>%
  count(institution, year) %>%
  count(year)

Causal <- Courses %>%
  filter(causal == 1 & !is.na(institution) & year != "NAV" & year != "NAV ") %>%
  count(institution, year) %>%
  count(year)

Causal_join <- left_join(Instition_yr, Causal, by = "year")
Causal_join[is.na(Causal_join)] = 0
colnames(Causal_join) <- c("Year","All", "Causal")
Causal_join$prop <- round(Causal_join$Causal/Causal_join$All, digits = 2)

#plot
ggplot(Causal_join, aes(x = Year, y = prop))+
  geom_bar(stat = "identity", fill = "steelblue3") +
  xlab("Year") + ylab("Proportion of Institutions") + theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_text(aes(label = prop), vjust = -0.2, size = 3)


####Bivariate/Correlation Analysis####
#time vs causal id
out.33 <- lm(stats_id_strategy ~ year, data = subset(Courses, Courses$Quant == 1 & stats_id_strategy != "NAV" & year != "NAV" & year != "NAV "))

year_label <- as.character(seq(1994, 2022, 1))

stargazer(out.33, type = "text",
          covariate.labels = year_label,
          dep.var.labels.include = TRUE)


#time vs explicit qual methods
Qual_courses$explict_qual <- ifelse(Qual_courses$process_tracing == "1" |
                                      Qual_courses$case_comparison == "1" |
                                      Qual_courses$qca == "1" |
                                      Qual_courses$congruence == "1" |
                                      Qual_courses$counterfactual == "1", 1, 0)

year_label2 <- as.character(seq(1999, 2020, 1))

out.34 <- lm(explict_qual ~ year, data = subset(Qual_courses, Qual_courses$year != "NAV"))

stargazer(out.34, type = "text",
          covariate.labels = year_label2,
          dep.var.labels.include = TRUE)

#qual vs data gen
Courses$DataGen <- ifelse(Courses$qual_data_gen == 1 |
                            Courses$build_quant_interviews == 1, 1, 0)

out.35 <- lm(DataGen ~ Qual, data = subset(Courses, Courses$Qual == 1 | Courses$Quant == 1))

stargazer(out.35, type = "text",
          covariate.labels = "Qualitative",
          dep.var.labels.include = TRUE)


#quant vs data analysis
Courses$DataAnalysis <- ifelse(Courses$process_tracing == 1| 
                                 Courses$qca == 1|
                                 Courses$congruence == 1|
                                 Courses$counterfactual == 1 |
                                 Courses$case_comparison == 1 |
                                 Courses$simple_probability == 1|
                                 Courses$regression == 1 |
                                 Courses$stats_id_strategy == 1|
                                 Courses$machine_learning == 1, 1, 0)

out.36 <- lm(DataAnalysis ~ Quant, data = subset(Courses, Courses$Qual == 1 | Courses$Quant == 1))

stargazer(out.36, type = "text",
          covariate.labels = "Quantitative",
          dep.var.labels.include = TRUE)




####Survey Data####

#load data
DGS_survey <- read_excel("DGS Methods Survey.xlsx")

#Number of institutions requiring a qualitative methods course
DGS_survey %>% count(QualRequiredCurrently)

DGS_survey %>% count(QualRequiredPast)


#Number of institutions offering qualitative methods course sequence
DGS_survey %>% count(QualSequenceCurrently)

DGS_survey %>% count(QualSequencePast)


#Number of institutions offering mixed-methods courses

DGS_survey %>% count(MixedMethodsCurrently)

DGS_survey %>% count(MixedMethodsPast)


