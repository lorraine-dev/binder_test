rm(list = ls())

ggplot(GenerateData, aes(x = reorder(`Mode of Inquiry`, Percent) , y = Percent, fill = `Mode of Inquiry`)) + 
  geom_bar(stat = "identity") + scale_fill_brewer(palette = "Blues", guide = "none") + theme_minimal(base_size = 12) + labs(y = "Percent of Articles", x = "Mode of Inquiry") + geom_text(aes(label = Percent), vjust ="center", size=3, hjust = "center", nudge_y =.025) + scale_y_continuous(labels = scales::percent, limits = c(0,1))


library("margins")

setwd("~/Documents/Methods Project/Missing Codes Checks")
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

#load in packages
library("ggplot2")
library("plyr")
library("stargazer")
library("dplyr")
library("tidyr")
library("sjPlot")
library("formattable")
library("RColorBrewer")
library(readxl)
library(stringr)
library("hrbrthemes")
library(viridis)
library("corrplot")
library("Hmisc")
library(forcats) 
library("janitor")
library (scales)
library("mfx")
library("jtools")
library(ggstance)
library("broom.mixed")
library("nnet")
library("margins")


#Read in data 
Prelim_Data <- read_excel("Dedoose Media_72921.xls")

Code <- read_excel("Code Presence_72921_c3.xlsx")

#take first three columns of Prelim_Data (title, year, journal)
Prelim <- Prelim_Data[,1:3]

#Join two datasets
Join <- merge(Prelim, Code, by = "Title", all.x = TRUE)


#Rename Join and save as RData. Make sure to update dataframe and dataset names.
Check_72921 <- Join
save(Check_72921, file ="Check_72921_c3.RData")

#load("~/Documents/Methods Project/Prelim Analysis/Prelim_Data_9520.RData")

###MISSING CODES CHECKS####

#Used to ensure articles are correctly coded by checking for missing codes. REMINDER: Change dataframe names before running code

#Check for missing descriptor2
Check_72921$YearDescriptorMissing <- ifelse(is.na(Check_72921$Year), 1, 0)

Check_72921$JournalDescriptorMissing <- ifelse(is.na(Check_72921$Journal), 1, 0)

Missing_Descriptor <-subset(Check_72921[1], Check_72921$YearDescriptorMissing == 1 | Check_72921$JournalDescriptorMissing)
Missing_Descriptor$Issue <- "Missing Descriptor"

#Check for missing subfield
Check_72921$SubfieldMissing <- ifelse(Check_72921$`American Politics`+ Check_72921$`Comparative Politics`+ Check_72921$`Conceptualization and measurement`+Check_72921$Methodology+Check_72921$`Political Theory`+ Check_72921$`International Relations` == 0, 1, 0)

Missing_sub <-subset(Check_72921[1], Check_72921$SubfieldMissing == 1)
Missing_sub$Issue <- "Missing Subfield"


#Check for missing author
Check_72921$AuthorMissing <- ifelse(Check_72921$`Single-authored male`+ Check_72921$`Single-authored female`+ Check_72921$`Co-authored female`+Check_72921$`Co-authored male` + Check_72921$`Co-authored mixed-gender team` == 0, 1, 0)

AuthorMissing <- subset(Check_72921[1], Check_72921$AuthorMissing == 1)
AuthorMissing$Issue <- "Missing Author"


#Check for missing data tendency. 

##generate data -  non human participant
Check_72921$gendataNHP <-ifelse(Check_72921$`Generated data`== 1 |Check_72921$`Text analysis/text mining`== 1|Check_72921$`Generated synthetic data` == 1, 1, 0)

##generate data - Observational human participant
Check_72921$OHPdata <- ifelse(Check_72921$`Ethnography / participant observation` == 1| Check_72921$`Interviews/focus groups`==1 | Check_72921$Survey == 1, 1, 0)

#generate data - Experimental human participant
Check_72921$EHPdata <- ifelse(Check_72921$`Survey experiment` == 1 | Check_72921$Field == 1 | Check_72921$Lab == 1, 1, 0)

#generate data - Human Participants
Check_72921$HPdata <- ifelse(Check_72921$OHPdata == 1 |Check_72921$EHPdata == 1, 1, 0)

##Creates a variable that flags articles that should have an empirical tendency code
Check_72921$data_tendency <- ifelse(Check_72921$`Employed data/information from pre-existing primary or secondary sources`+Check_72921$gendataNHP > 1 | Check_72921$`Employed data/information from pre-existing primary or secondary sources`+ Check_72921$HPdata > 1, 1, 0)

#creates a variable that flags articles that are coded for a empirical tendency
Check_72921$data_tendency2 <- ifelse(Check_72921$`Mostly pre-existing data`  == 1 | Check_72921$`Mostly author-generated data` == 1 | Check_72921$`Fairly balanced` == 1, 1, 0)

#Identifies articles that are missing an empirical base code. These articles have an empirical tendency code, but are missing a second empirical base code. 
Missing_EmpBase <- subset(Check_72921[1], Check_72921$data_tendency2 == 1 & Check_72921$data_tendency == 0)
Missing_EmpBase$Issue <-"Missing Empirical Base"

#Identifies articles that are missing an empirical tendency code. These articles have two different empirical base codes, but are missing an empirical tendency code. 
Missing_EmpTendency <- subset(Check_72921[1], Check_72921$data_tendency2 == 0 & Check_72921$data_tendency == 1)
Missing_EmpTendency$Issue <- "Missing Empirical Tendency"

#Flags articles that are do not have any empirical base (nonHP, OHP, or EHP) code
Check_72921$EmpBase <- ifelse(Check_72921$`Employed data/information from pre-existing primary or secondary sources`==1 | Check_72921$`Generated data`==1 | Check_72921$`Text analysis/text mining` == 1 | Check_72921$`Generated synthetic data` == 1 | Check_72921$`Ethnography / participant observation` == 1 | Check_72921$`Interviews/focus groups` ==1 | Check_72921$Survey == 1 | Check_72921$`Survey experiment` == 1 | Check_72921$Field == 1| Check_72921$Lab == 1, 1, 0)

#Missing OHP source 
Check_72921$OHPSource <- ifelse(Check_72921$`OHP - International body / institution` + Check_72921$`OHP - Domestic government` + Check_72921$`OHP - CSO` + Check_72921$`OHP - Media` + Check_72921$`OHP - Academics/Researchers` + Check_72921$`OHP - Other specified profession` + Check_72921$`OHP - Unspecified affiliation`> 0, 1,0)

#Flag articles that are coded OHP and are missing a source
MissingOHPsource <- subset(Check_72921[1], Check_72921$OHPdata == 1 & Check_72921$OHPSource == 0)
MissingOHPsource$Issue <- "Missing OHP Source"

#Flag articles that are missing an OHP code and are coded for a source
MissingOHP <- subset(Check_72921[1], Check_72921$OHPdata == 0 & Check_72921$OHPSource == 1)
MissingOHP$Issue <- "Missing OHP"

#Missing EHP
Check_72921$EHPSource <- ifelse(Check_72921$`EHP - International body / institution` + Check_72921$`EHP - Domestic government` + Check_72921$`EHP - CSO` + Check_72921$`EHP - Media` + Check_72921$`EHP - Academics/Researchers` + Check_72921$`EHP - Other specified profession` + Check_72921$`EHP - Unspecified affiliation`> 0, 1,0)

#Flag articles that are coded EHP and are missing a source
MissingEHPsource <- subset(Check_72921[1], Check_72921$EHPdata == 1 & Check_72921$EHPSource == 0)
MissingEHPsource$Issue <- "Missing EHP source"

#Flag articles that are missing an EHP code and are coded for a source
MissingEHP <- subset(Check_72921[1], Check_72921$EHPdata == 0 & Check_72921$EHPSource == 1)
MissingEHP$Issue <- "Missing EHP"


##Codes to identify missing methods
##Creates indicator that flags articles that use quant methods 
Check_72921$QuantMethod <- ifelse(Check_72921$`Simple Probability` == 1 | Check_72921$Regression == 1 | Check_72921$`Statistics with an identification strategy` == 1 | Check_72921$`Machine learning` == 1, 1, 0)

##Creates indicator that flags articles that use qual methods 
Check_72921$QualMethod <- ifelse(Check_72921$`Process tracing`== 1| Check_72921$QCA == 1 | Check_72921$`Congruence analysis` == 1| Check_72921$`Counterfactual analysis` == 1 | Check_72921$`Structured case comparison`== 1 |  Check_72921$Other == 1, 1, 0)

##Creates indicator that flags articles that use mixed methods using quant and qual indicators
Check_72921$MixedMethods <- ifelse(Check_72921$QuantMethod + Check_72921$QualMethod + Check_72921$Interpretive + Check_72921$`Formal modeling` > 1, 1, 0)

#Creates a second indicator for mixed methods using methods tendency codes 
Check_72921$MixedMethods2 <- ifelse(Check_72921$`Overwhelmingly interpretive`== 1 | Check_72921$`Overwhelmingly quant` == 1 | Check_72921$`Overwhelmingly qual` == 1 | Check_72921$`Overwhelmingly modeling` == 1 | Check_72921$`No specific focus` == 1, 1, 0)

#Creates the subset of articles that are missing methods tendency code
Missing_MethodsTendency<- subset(Check_72921[1], Check_72921$MixedMethods == 1 & Check_72921$MixedMethods2 == 0)
Missing_MethodsTendency$Issue <- "Missing Methods Tendency"

#Creates the subset of articles that are missing a method but has a methods tendency code
Missing_Method_MM <- subset(Check_72921[1], Check_72921$MixedMethods == 0 & Check_72921$MixedMethods2 == 1)
Missing_Method_MM$Issue <- "Missing Method (Mixed Methods)"


Check_72921$ModelingOnly <- ifelse(Check_72921$MixedMethods == 0 & Check_72921$`Formal modeling` == 1, 1, 0)

Check_72921$InterpOnly <- ifelse(Check_72921$MixedMethods == 0 & Check_72921$Interpretive == 1, 1, 0)

Check_72921$MethodCode <- ifelse(Check_72921$Interpretive + Check_72921$`Formal modeling` + Check_72921$QualMethod + Check_72921$QuantMethod + Check_72921$`No discernible method` > 0, 1,0)

#Creates the subset of articles that are not coded for any method
MissingMethod <- subset(Check_72921[1], Check_72921$MethodCode == 0)
MissingMethod$Issue <- "Missing Method (No Method Applied)"

#missing empirical base and use a method other than formal modeling
Missing_EmpiricalBase <- subset(Check_72921[1], Check_72921$EmpBase == 0 & Check_72921$ModelingOnly == 0)
Missing_EmpiricalBase$Issue <- "Missing Empirical Base"


#save the datafile
save(Check_72921, file ="Check_72921_c3.RData")

#Combine lists of missing codes
Missing_Codes <-rbind(Missing_Descriptor,Missing_EmpBase, Missing_EmpiricalBase, Missing_EmpTendency, Missing_Method_MM, Missing_MethodsTendency, Missing_sub, MissingEHP, MissingEHPsource, MissingOHP, MissingOHPsource, MissingMethod, AuthorMissing)

write.csv(Missing_Codes, file = "Missing Codes Check 72921_c3.csv")

####INTRO####

load(file ="Check_72921_c3.RData")
setwd("~/Documents/Methods Project/Analysis")

#change the name of the cleaned data
MMCPSR_Data <- Check_72921

#Number of articles published per year
#Generate counts and percentages
ArticlesbyYear <- MMCPSR_Data %>% count(Year)
ArticlesbyYear$n <- as.numeric(ArticlesbyYear$n)
ArticlesbyYear$Percent <- formattable::percent(ArticlesbyYear$n/sum(ArticlesbyYear$n), digits =1)

#Create a line graph - articles per year
ggplot(ArticlesbyYear, 
       aes(x=Year, y=n, group = 1)) +
  geom_line(color="orange")  + 
  labs(y = "Number of Articles", angle = 45) + theme_minimal(base_size = 12)

#Create a barplot - articles per year
ggplot(ArticlesbyYear, aes(x = Year, y = n)) + 
  geom_bar(stat = "identity", color = "steelblue3", fill = "steelblue3") + theme_minimal(base_size = 12) + labs(y = "Articles", angle = 45) + geom_text(aes(label = n), vjust ="center", size=3, hjust = "center", nudge_y = 1) 


# Number of articles in each journal
# Generate counts and percentages
Journals <- MMCPSR_Data %>% count(Journal)
Journals$Percent <- formattable::percent(Journals$n/sum(Journals$n), digits =1)

#Create a barplot - articles per journal 
ggplot(Journals, aes(x = reorder(Journal, Percent), y = Percent)) + 
  geom_bar(stat = "identity", color = "steelblue3", fill = "steelblue3") +
  coord_flip() + theme_minimal(base_size = 11) + labs(y = "Percent", x = "Journal") + geom_text(aes(label = Percent), vjust ="center", size=3, hjust = "center", nudge_y = 0.01)


#Number of articles published per year in each journal
#Generate counts and reformat
ArticlesJournYear <- MMCPSR_Data %>%count(Journal, Year)
ArticlesJournYear <- pivot_wider(ArticlesJournYear, names_from = Year, values_from = n, values_fill = 0)


#Number of articles in each subfield
#Generate counts of articles with one subfield applied
AP_freq <- sum(MMCPSR_Data$`American Politics`== 1 & MMCPSR_Data$`Comparative Politics`== 0 & MMCPSR_Data$`International Relations`== 0 & MMCPSR_Data$`Conceptualization and measurement` == 0 & MMCPSR_Data$Methodology == 0 & MMCPSR_Data$`Political Theory`== 0)

CP_freq <- sum(MMCPSR_Data$`American Politics`== 0 & MMCPSR_Data$`Comparative Politics`== 1 & MMCPSR_Data$`International Relations`== 0 & MMCPSR_Data$`Conceptualization and measurement` == 0 & MMCPSR_Data$Methodology == 0 & MMCPSR_Data$`Political Theory`== 0 )

IR_freq <- sum(MMCPSR_Data$`American Politics`== 0 & MMCPSR_Data$`Comparative Politics`== 0 & MMCPSR_Data$`International Relations`== 1 & MMCPSR_Data$`Conceptualization and measurement` == 0 & MMCPSR_Data$Methodology == 0 & MMCPSR_Data$`Political Theory`== 0 )

CM_freq <- sum(MMCPSR_Data$`American Politics`== 0 & MMCPSR_Data$`Comparative Politics`== 0 & MMCPSR_Data$`International Relations`== 0 & MMCPSR_Data$`Conceptualization and measurement` == 1 & MMCPSR_Data$Methodology == 0 & MMCPSR_Data$`Political Theory`== 0 )

Meth_freq <- sum(MMCPSR_Data$`American Politics`== 0 & MMCPSR_Data$`Comparative Politics`== 0 & MMCPSR_Data$`International Relations`== 0 & MMCPSR_Data$`Conceptualization and measurement` == 0 & MMCPSR_Data$Methodology == 1 & MMCPSR_Data$`Political Theory`== 0 )

PT_freq <- sum(MMCPSR_Data$`American Politics`== 0 & MMCPSR_Data$`Comparative Politics`== 0 & MMCPSR_Data$`International Relations`== 0 & MMCPSR_Data$`Conceptualization and measurement` == 0 & MMCPSR_Data$Methodology == 0 & MMCPSR_Data$`Political Theory`== 1 )

#Generate counts of articles with CP/IR, AP/IR, and Meth + Anything
CP_IR_freq <- sum(MMCPSR_Data$`American Politics`== 0 & MMCPSR_Data$`Comparative Politics`== 1 & MMCPSR_Data$`International Relations`== 1 & MMCPSR_Data$`Conceptualization and measurement` == 0 & MMCPSR_Data$Methodology == 0 & MMCPSR_Data$`Political Theory`== 0 )

AP_IR_freq <- sum(MMCPSR_Data$`American Politics`== 1 & MMCPSR_Data$`Comparative Politics`== 0 & MMCPSR_Data$`International Relations`== 1 & MMCPSR_Data$`Conceptualization and measurement` == 0 & MMCPSR_Data$Methodology == 0 & MMCPSR_Data$`Political Theory`== 0 )

MethodCombo_freq <- sum(MMCPSR_Data$Methodology == 1 & MMCPSR_Data$`American Politics`+ MMCPSR_Data$`Comparative Politics`+  MMCPSR_Data$`International Relations`+ MMCPSR_Data$`Conceptualization and measurement` + MMCPSR_Data$`Political Theory`> 0)


#Table - SubfieldsCounts
subfield_count <- data.frame(matrix(NA, nrow = 9, ncol = 2))
colnames(subfield_count) <- c("Subfield", "Frequency")
subfield_count$Subfield[1:9] <- c("American Politics", "Comparative Politics", "Conceptualization and Measurement","International Relations", "Methodology", "Political Theory", "CP/IR", "AP/IR", "Methods +anything ")
subfield_count$Frequency[1:9] <- c(AP_freq, CP_freq, CM_freq, IR_freq,Meth_freq, PT_freq, CP_IR_freq, AP_IR_freq, MethodCombo_freq)
subfield_count$SubfieldAdj <- c("American Politics", "Comparative Politics", "Measurement","IR", "Methodology", "Political Theory", "CP/IR", "AP/IR","Methods+")
subfield_count$Percent <- formattable::percent(subfield_count$Frequency/nrow(MMCPSR_Data), digits = 1)

#Create barplot - Subfields Percents
#counts
ggplot(subfield_count, aes(x = reorder(Subfield, Percent) , y = Percent, fill = Subfield)) + 
  geom_bar(stat = "identity") + scale_fill_brewer(palette = "Blues", guide = FALSE) +
  coord_flip() + theme_minimal(base_size = 13) + labs(y = "Percent of Articles", x = "Subfield") + geom_text(aes(label = Percent), vjust ="center", size=3, hjust = "center", nudge_y =.1) + scale_y_continuous(labels = scales::percent, limits = c(0,1))


#Number of articles in each subfield in each journal 
#Create subfield codes
MMCPSR_Data$AP <- ifelse(MMCPSR_Data$`American Politics`== 1 & MMCPSR_Data$`Comparative Politics`== 0 & MMCPSR_Data$`International Relations`== 0 & MMCPSR_Data$`Conceptualization and measurement` == 0 & MMCPSR_Data$Methodology == 0 & MMCPSR_Data$`Political Theory`== 0,1,0)

MMCPSR_Data$CP <- ifelse(MMCPSR_Data$`American Politics`== 0 & MMCPSR_Data$`Comparative Politics`== 1 & MMCPSR_Data$`International Relations`== 0 & MMCPSR_Data$`Conceptualization and measurement` == 0 & MMCPSR_Data$Methodology == 0 & MMCPSR_Data$`Political Theory`== 0,1,0)

MMCPSR_Data$IR <- ifelse(MMCPSR_Data$`American Politics`== 0 & MMCPSR_Data$`Comparative Politics`== 0 & MMCPSR_Data$`International Relations`== 1 & MMCPSR_Data$`Conceptualization and measurement` == 0 & MMCPSR_Data$Methodology == 0 & MMCPSR_Data$`Political Theory`== 0,1,0 )

MMCPSR_Data$CM <- ifelse(MMCPSR_Data$`American Politics`== 0 & MMCPSR_Data$`Comparative Politics`== 0 & MMCPSR_Data$`International Relations`== 0 & MMCPSR_Data$`Conceptualization and measurement` == 1 & MMCPSR_Data$Methodology == 0 & MMCPSR_Data$`Political Theory`== 0,1,0)

MMCPSR_Data$Methods <- ifelse(MMCPSR_Data$`American Politics`== 0 & MMCPSR_Data$`Comparative Politics`== 0 & MMCPSR_Data$`International Relations`== 0 & MMCPSR_Data$`Conceptualization and measurement` == 0 & MMCPSR_Data$Methodology == 1 & MMCPSR_Data$`Political Theory`== 0,1,0 )

MMCPSR_Data$PT <- ifelse(MMCPSR_Data$`American Politics`== 0 & MMCPSR_Data$`Comparative Politics`== 0 & MMCPSR_Data$`International Relations`== 0 & MMCPSR_Data$`Conceptualization and measurement` == 0 & MMCPSR_Data$Methodology == 0 & MMCPSR_Data$`Political Theory`== 1, 1, 0)

MMCPSR_Data$CP_IR <- ifelse(MMCPSR_Data$`American Politics`== 0 & MMCPSR_Data$`Comparative Politics`== 1 & MMCPSR_Data$`International Relations`== 1 & MMCPSR_Data$`Conceptualization and measurement` == 0 & MMCPSR_Data$Methodology == 0 & MMCPSR_Data$`Political Theory`== 0,1,0 )

MMCPSR_Data$AP_IR <- ifelse(MMCPSR_Data$`American Politics`== 1 & MMCPSR_Data$`Comparative Politics`== 0 & MMCPSR_Data$`International Relations`== 1 & MMCPSR_Data$`Conceptualization and measurement` == 0 & MMCPSR_Data$Methodology == 0 & MMCPSR_Data$`Political Theory`== 0, 1,0 )

MMCPSR_Data$MethodCombo <- ifelse(MMCPSR_Data$Methodology == 1 & MMCPSR_Data$`American Politics`+ MMCPSR_Data$`Comparative Politics`+  MMCPSR_Data$`International Relations`+ MMCPSR_Data$`Conceptualization and measurement` + MMCPSR_Data$`Political Theory`> 0,1,0)

MMCPSR_Data$Subfield <- ifelse(MMCPSR_Data$AP == 1, "AP", ifelse(MMCPSR_Data$CP == 1, "CP", ifelse(MMCPSR_Data$IR == 1, "IR", ifelse(MMCPSR_Data$CM == 1, "CM", ifelse(MMCPSR_Data$PT == 1, "PT", ifelse(MMCPSR_Data$Methods ==1, "Methods", ifelse(MMCPSR_Data$CP_IR == 1, "CP/IR", ifelse(MMCPSR_Data$AP_IR == 1, "AP/IR",ifelse(MMCPSR_Data$MethodCombo == 1, "Methods plus another subfield", "Other subfield combo")))))))))

#Generate counts, join and reformat
APByJourn <-MMCPSR_Data%>%count(Journal, AP)
APByJourn <- subset(APByJourn, APByJourn$AP==1, select = c(Journal, n))
colnames(APByJourn) <- c("Journal","AP articles")

CPByJourn <-MMCPSR_Data%>%count(Journal, CP)
CPByJourn <- subset(CPByJourn, CPByJourn$CP==1, select = c(Journal, n))
colnames(CPByJourn) <- c("Journal","CP articles")

IRByJourn <-MMCPSR_Data%>%count(Journal, IR)
IRByJourn <- subset(IRByJourn, IRByJourn$IR==1, select = c(Journal, n))
colnames(IRByJourn) <- c("Journal","IR articles")

PTByJourn <-MMCPSR_Data%>%count(Journal, PT)
PTByJourn <- subset(PTByJourn, PTByJourn$PT==1, select = c(Journal, n))
colnames(PTByJourn) <- c("Journal","PT articles")

MethodsByJourn <-MMCPSR_Data%>%count(Journal, Methods)
MethodsByJourn <- subset(MethodsByJourn, MethodsByJourn$Methods==1, select = c(Journal, n))
colnames(MethodsByJourn) <- c("Journal","Methods articles")

CMByJourn <-MMCPSR_Data%>%count(Journal, CM)
CMByJourn <- subset(CMByJourn, CMByJourn$CM==1, select = c(Journal, n))
colnames(CMByJourn) <- c("Journal","CM articles")

CP_IRByJourn <-MMCPSR_Data%>%count(Journal, CP_IR)
CP_IRByJourn <- subset(CP_IRByJourn, CP_IRByJourn$CP_IR==1, select = c(Journal, n))
colnames(CP_IRByJourn) <- c("Journal","CP_IR articles")

AP_IRByJourn <-MMCPSR_Data%>%count(Journal, AP_IR)
AP_IRByJourn <- subset(AP_IRByJourn, AP_IRByJourn$AP_IR==1, select = c(Journal, n))
colnames(AP_IRByJourn) <- c("Journal","AP_IR articles")

MethodComboByJourn <-MMCPSR_Data%>%count(Journal, MethodCombo)
MethodComboByJourn <- subset(MethodComboByJourn, MethodComboByJourn$MethodCombo==1, select = c(Journal, n))
colnames(MethodComboByJourn) <- c("Journal","MethodCombo articles")


SubfieldByJourn <- full_join(APByJourn, CPByJourn, by = "Journal")
SubfieldByJourn <- full_join(SubfieldByJourn, IRByJourn, by = "Journal")
SubfieldByJourn <- full_join(SubfieldByJourn, PTByJourn, by = "Journal")
SubfieldByJourn <- full_join(SubfieldByJourn, CMByJourn, by = "Journal")
SubfieldByJourn <- full_join(SubfieldByJourn, MethodsByJourn, by = "Journal")
SubfieldByJourn <- full_join(SubfieldByJourn, MethodComboByJourn, by = "Journal")
SubfieldByJourn <- full_join(SubfieldByJourn, AP_IRByJourn, by = "Journal")
SubfieldByJourn <- full_join(SubfieldByJourn, CP_IRByJourn, by = "Journal")
SubfieldByJourn[is.na(SubfieldByJourn)] <- 0

#save data
save(MMCPSR_Data, file = "MMCPSR Analysis Data.RData")

MethodTime %>%
  ggplot(aes(x=Year, y=Percent, group = Method, color = Method))+
  geom_line()+scale_color_brewer(palette="Set2") +  theme(legend.position = "bottom") + theme_minimal(base_size = 12) + labs(y = "Percent of Articles", vjust = -1) + 
  scale_y_continuous(labels = scales::percent, limits = c(0,1)) + theme(legend.position = "bottom")

FMTime %>%
  ggplot( aes(x=Year, y=Percent, group = 1)) +
  geom_line(color = "orange")+scale_color_brewer(palette="Set2") + theme_minimal(base_size = 11) + 
  scale_y_continuous(labels = scales::percent, limits = c(0,1)) + labs(y = "Percent of Articles", vjust = -1)



####ANALYSIS / Descriptive####

####Race and Gender from APSA Data
#load in data
author_data <- read.csv("/Users/Tranae/Documents/Methods Project/Analysis/final_apsa_list 09222021.csv")

### author-level count and proportion for all gender identity categories
gender_author <- author_data %>%
  subset(!is.na(Gender.apsa)) %>%
  dplyr::summarize(count = c(sum(male_apsa, na.rm=T), sum(female_apsa, na.rm=T), sum(nonbinary_apsa, na.rm=T)))

gender_author <- gender_author %>%
  mutate(proportion = round(count / sum(count), 2)) %>%
  mutate(gender = c("male", "female", "nonbinary")) %>%
  dplyr::select(gender, count, proportion)

### author-level count and proportion for all race / ethnic identity categories
race_ethnicity_author <- author_data %>%
  subset(!is.na(R_E.apsa)) %>%
  dplyr::summarize(count = c(sum(white),
                      sum(black),
                      sum(east_asian),
                      sum(south_asian),
                      sum(latino), 
                      sum(mena),
                      sum(native),
                      sum(pacific),
                      sum(other)))

race_ethnicity_author <- race_ethnicity_author %>%
  mutate(race_ethnicity = c("White", "Black", "East Asian", "South Asian",
                            "Hispanic or Latino", "Middle Eastern / Arab", "Native", 
                            "Pacific Islander", "Other")) %>%
  mutate(proportion = round(count / sum(count), 2)) %>%
  dplyr::select(race_ethnicity, count, proportion)

### pivot gender categories to longer dataframe
gender_article <- author_data %>%
  dplyr::select(FirstName_MI_LastName_str, 
                Article.1.Title.kap, 
                Article.2.Title.kap,
                Article.3.Title.kap,
                Article.4.Title.kap,
                Article.5.Title.kap,
                Article.6.Title.kap,
                Article.7.Title.kap,
                Article.8.Title.kap,
                Article.9.Title.kap,
                Article.10.Title.kap,
                male_apsa,
                female_apsa,
                nonbinary_apsa) %>%
  pivot_longer(cols = -c(FirstName_MI_LastName_str, male_apsa, female_apsa, nonbinary_apsa),
               values_to = "article_title",
               names_to = "names") %>%
  subset(article_title != "NA")

### article-level authorship structure information for gender categories
gender_article <- gender_article %>%
  group_by(article_title) %>%
  mutate(single_authored_male = ifelse(male_apsa == 1 & n() == 1, 1, 0),
         single_authored_female = ifelse(female_apsa == 1 & n() == 1, 1, 0),
         co_authored_male = ifelse(mean(male_apsa) == 1 & n() > 1, 1, 0),
         co_authored_female = ifelse(mean(female_apsa) == 1 & n() > 1, 1, 0),
         co_authored_mixed = ifelse(mean(male_apsa) < 1 & n() > 1, 1, 0))

#calculate count and proportion of gender author structure
gender_article_dedup <- gender_article %>% distinct(article_title, single_authored_male, single_authored_female, co_authored_male, co_authored_female, co_authored_mixed) 

gender_article_count <- data.frame(matrix(NA, nrow = 5, ncol = 3))
colnames(gender_article_count) <- c("Author_Gender", "Frequency", "Percent")
gender_article_count$Author_Gender <- c("Single Authored Male", "Single Authored Female", "Co-authored Male", "Co-authored Female", "Co-authored Mixed Gender")
gender_article_count$Frequency <- c(sum(gender_article_dedup$single_authored_male, na.rm=T), sum(gender_article_dedup$single_authored_female, na.rm=T), sum(gender_article_dedup$co_authored_male, na.rm=T), sum(gender_article_dedup$co_authored_female, na.rm=T), sum(gender_article_dedup$co_authored_mixed, na.rm=T))
gender_article_count$Percent <- formattable::percent(gender_article_count$Frequency/sum(gender_article_count$Frequency), digits = 1)

### pivot race / ethnic identity categories to longer dataframe
race_ethnicity_article <- author_data %>%
  subset(!grepl("Prefer", R_E.apsa, ignore.case = TRUE)) %>%
  dplyr::select(FirstName_MI_LastName_str, 
                Article.1.Title.kap, 
                Article.2.Title.kap,
                Article.3.Title.kap,
                Article.4.Title.kap,
                Article.5.Title.kap,
                Article.6.Title.kap,
                Article.7.Title.kap,
                Article.8.Title.kap,
                Article.9.Title.kap,
                Article.10.Title.kap,
                white,
                black,
                east_asian,
                south_asian,
                latino,
                mena,
                native,
                pacific,
                other) %>%
  pivot_longer(cols = -c(FirstName_MI_LastName_str, white, black, east_asian, south_asian, 
                         latino, mena, native, pacific, other),
               values_to = "article_title",
               names_to = "names") %>%
  subset(article_title != "NA")

### article-level authorship structure for race / ethnic identity categories
race_ethnicity_article <- race_ethnicity_article %>%
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
         mixed_authors = ifelse(mean(white) < 1 & n() > 1, 1, 0))

#calculate count and proportion of race/ethnicity author structure
race_ethnicity_article_dedup <- race_ethnicity_article %>% distinct(article_title, white_authors, black_authors, east_asian_authors, latino_authors, mena_authors, native_authors, pacific_authors, other_authors, mixed_authors) 

race_ethnicity_article_count <- data.frame(matrix(NA, nrow = 9, ncol = 3))
colnames(race_ethnicity_article_count) <- c("Author_Race", "Frequency", "Percent")
race_ethnicity_article_count$Author_Race <- c("White Authors", "Black Authors", "East Asian Authors", "Latino Authors", "MENA Authors", "Native Authors","Pacific Authors", "Other Authors", "Mixed Authors")
race_ethnicity_article_count$Frequency <- c(sum(race_ethnicity_article_dedup$white_authors, na.rm=T), sum(race_ethnicity_article_dedup$black_authors, na.rm=T), sum(race_ethnicity_article_dedup$east_asian_authors, na.rm=T), sum(race_ethnicity_article_dedup$latino_authors, na.rm=T), sum(race_ethnicity_article_dedup$mena_authors, na.rm=T), sum(race_ethnicity_article_dedup$native_authors, na.rm=T), sum(race_ethnicity_article_dedup$pacific_authors, na.rm=T), sum(race_ethnicity_article_dedup$other_authors, na.rm=T), sum(race_ethnicity_article_dedup$mixed_authors, na.rm=T))
race_ethnicity_article_count$Percent <- formattable::percent(race_ethnicity_article_count$Frequency/sum(race_ethnicity_article_count$Frequency), digits = 1)

##Gender
#Create Gender Variable
MMCPSR_Data$Gender <- ifelse(MMCPSR_Data$`Single-authored female`==1, "Single Authored Female", ifelse(MMCPSR_Data$`Single-authored male` == 1, "Single Authored Male", ifelse(MMCPSR_Data$`Co-authored female` == 1, "Co-authored Female", ifelse(MMCPSR_Data$`Co-authored male` == 1, "Co-authored Male", ifelse(MMCPSR_Data$`Co-authored mixed-gender team`==1, "Mixed Gender Team", "")))))

#Create Separate Gender Variables
MMCPSR_Data$AnyWomen <- ifelse(MMCPSR_Data$`Single-authored female` ==1| MMCPSR_Data$`Co-authored female`==1 | MMCPSR_Data$`Co-authored mixed-gender team`==1,1,0)
MMCPSR_Data$AnyMen <- ifelse (MMCPSR_Data$`Single-authored male` == 1| MMCPSR_Data$`Co-authored male` == 1| MMCPSR_Data$`Co-authored mixed-gender team`==1, 1, 0)

#Table - Author Gender, Frequency, and Percent
Gender_count <- as.data.frame(table(MMCPSR_Data$Gender))
colnames(Gender_count) <- c("Author Gender","Frequency")
Gender_count$Percent <- formattable::percent(Gender_count$Frequency/sum(Gender_count$Frequency), digits = 1)

#Two proportion t tests comparing APSA data to handcoded data
#single authored male
t.test(MMCPSR_Data$`Single-authored male`, gender_article_dedup$single_authored_male)

#single authored female
t.test(MMCPSR_Data$`Single-authored female`, gender_article_dedup$single_authored_female)

#coauthored male
t.test(MMCPSR_Data$`Co-authored male`, gender_article_dedup$co_authored_male)

#coauthored female
t.test(MMCPSR_Data$`Co-authored female`, gender_article_dedup$co_authored_female)

#coauthored mixed
t.test(MMCPSR_Data$`Co-authored mixed-gender team`, gender_article_dedup$co_authored_mixed)

#Create barplot - Gender 
Gender_count %>%
  mutate(`Author Gender` = fct_reorder(`Author Gender`, Percent)) %>% ggplot(aes(x= `Author Gender`, y = Percent, fill = `Author Gender`)) + 
  geom_bar(stat = "identity") + scale_fill_brewer(palette = "Blues", guide = "none") + coord_flip() + theme_minimal(base_size = 11) + labs(x = "Author Gender") + geom_text(aes(label=Percent),size = 3, vjust="center", hjust = "center", nudge_y =.1) + scale_y_continuous(labels = scales::percent, limits = c(0,1))

#Table - Author Gender, Frequency, and Percent (any women, any men)
Gender_count2 <-data.frame(matrix(NA, nrow = 2, ncol = 3))
colnames(Gender_count2) <- c("Author Gender", "Frequency", "Percent")
Gender_count2$`Author Gender` <- c("Any women", "Any men")
Gender_count2$Frequency[1] <- sum(MMCPSR_Data$AnyWomen)
Gender_count2$Frequency[2] <- sum(MMCPSR_Data$AnyMen)
Gender_count2$Percent <- formattable::percent(Gender_count2$Frequency/nrow(MMCPSR_Data), digits = 1)


#Variation across subfields
Gender_Subfield <- MMCPSR_Data %>% count(Gender, Subfield)
Gender_Subfield <-  pivot_wider(Gender_Subfield, names_from = Subfield, values_from = n, values_fill = 0)

#Variation over time
#Author gender over time - counts and percentages
GenderTime <- MMCPSR_Data %>% count(Gender, Year)
GenderTime <- left_join(GenderTime, ArticlesbyYear, by = "Year")
GenderTime$Percent <- formattable::percent(GenderTime$n.x/GenderTime$n.y, digits = 1)
GenderTime <- GenderTime[-4]      
colnames(GenderTime) <- c("Gender","Year", "Frequency", "Percent")


#line graph - all author combos over time
GenderTime %>%
  ggplot( aes(x=Year, y=Percent, group=Gender, color=Gender)) +
  geom_line()+scale_color_brewer(palette="Set2") + theme_minimal(base_size = 12) + labs(y = "Percent of Articles", vjust = -1) + 
  scale_y_continuous(labels = scales::percent, limits = c(0,1)) + theme(legend.position = "bottom")

#line graph - single author by gender over time
ggplot(data=subset(GenderTime, Gender == "Single Authored Female"|Gender == "Single Authored Male"), 
       aes(x=Year, y=Percent, group=Gender, color=Gender)) +
  geom_line() +scale_color_brewer(palette="Set2") + theme_minimal(base_size = 12) + labs(y = "Percent of Articles", vjust = -1) + 
  scale_y_continuous(labels = scales::percent, limits = c(0,1)) 

#line graph - co-authored by gender over time
ggplot(data=subset(GenderTime, Gender == "Co-authored Female"|Gender == "Co-authored Male" |Gender == "Mixed Gender Team"), 
       aes(x=Year, y=Percent, group=Gender, color=Gender)) +
  geom_line()+scale_color_brewer(palette="Set2") + theme_minimal(base_size = 12) + 
  scale_y_continuous(labels = scales::percent, limits = c(0,1)) + labs(y = "Percent of Articles", vjust = -1) 

#Variation across journals
#Author gender across journals - counts 
ArticlesbyJournal <- MMCPSR_Data %>% count(Journal)

GenderJournal <- MMCPSR_Data %>% count(Journal, Gender)
GenderJournal <- left_join(GenderJournal, ArticlesbyJournal, by = "Journal")
GenderJournal$Percent <- formattable::percent(GenderJournal$n.x/GenderJournal$n.y, digits = 0)
GenderJournal <- GenderJournal[-4]      
colnames(GenderJournal) <- c("Journal","Gender", "Frequency", "Percent")
GenderJournal <- GenderJournal %>% group_by(Journal) %>% mutate(ylabel = cumsum(Percent))

#bar graph - all author combos by journal
ggplot(GenderJournal, aes(x = reorder(Journal, Percent), y = Percent, fill = Gender)) + 
  geom_bar(position = "dodge", stat = "identity", width = .9) + scale_fill_brewer(palette = "RdYlBu")  + theme_minimal(base_size = 11) + labs(y = "Percent", x = "Journal") + geom_text(aes(label = Percent, y = Percent + 0.01), size = 2.25, position = position_dodge(0.9)) + 
  scale_y_continuous(labels = scales::percent, limits = c(0,1)) + labs(y = "Percent of Articles", vjust = -1) + theme(legend.position = "bottom")


#line graph - single author by gender by journal
ggplot(data=subset(GenderJournal, Gender == "Single Authored Female"|Gender == "Single Authored Male"), 
       aes(x=Journal, y=Percent, group=Gender, fill=Gender)) +
  geom_bar(position = "dodge2", stat = "identity") + scale_fill_brewer(palette = "Blues")  + theme_minimal(base_size = 12) + labs(y = "Percent", x = "Journal") + geom_text(aes(label = Percent, y = Percent + .02), size= 2.7,position = position_dodge(0.9)) + 
  scale_y_continuous(labels = scales::percent, limits = c(0,1)) + theme(legend.position = "bottom")

#bar graph - co-authored by gender by journal
ggplot(data=subset(GenderJournal, Gender == "Co-authored Female"|Gender == "Co-authored Male" |Gender == "Mixed Gender Team"), 
       aes(x=Journal, y=Percent, group=Gender, fill=Gender)) +
  geom_bar(position = "dodge", stat = "identity") + scale_fill_brewer(palette = "Blues")  + theme_minimal(base_size = 13) + labs(y = "Percent", x = "Journal") + geom_text(aes(label = Percent, y = Percent), size=2.7, position = position_dodge(0.9), vjust = -.95) + 
  scale_y_continuous(labels = scales::percent, limits = c(0,1)) 

#save data
save(MMCPSR_Data, file = "MMCPSR Analysis Data.RData")




#FROM HERE THROUGH THE REST OF THE ANALYSIS – EXCLUDE ARTICLES CODED PT + No discernible method
#create subset removing articles coded as Political Theory and no discernible method
MMCPSR_Data$PT_NoMethod <- ifelse(MMCPSR_Data$`Political Theory`+ MMCPSR_Data$`No discernible method`==2, 1, 0)

MMCPSR_emp <-subset(MMCPSR_Data, MMCPSR_Data$PT_NoMethod == 0)

#save data
save(MMCPSR_emp, file = "MMCPSR Analysis Data No PT.RData")

#create count of articles by year
ArticlesbyYear_emp <- MMCPSR_emp %>%count(Year)

##Mode of inquiry
#Create dataframe
GenerateData <- data.frame(matrix(NA, nrow = 3, ncol = 3))
colnames(GenerateData) <- c("Mode of Inquiry", "Frequency", "Percent")
GenerateData$`Mode of Inquiry` <- c("Observational", "Experimental", "Both")

#count of articles that generated data using observational techniques (includes articles that use both, percentage calculated using total empirical articles)
GenerateData[1,2] <- sum(MMCPSR_emp$OHPdata)
GenerateData[1,3] <- sum(MMCPSR_emp$OHPdata)/nrow(MMCPSR_emp)

#count of articles that generated data using experimental techniques (includes articles that use both, percentage calculated using total empirical articles)
GenerateData[2,2] <- sum(MMCPSR_emp$EHPdata)
GenerateData[2,3] <- sum(MMCPSR_emp$EHPdata)/nrow(MMCPSR_emp)

#Generated data using both observational and experimental techniques
GenerateData[3,2] <- sum(MMCPSR_emp$EHPdata ==1 & MMCPSR_emp$OHPdata ==1)
GenerateData[3,3] <- sum(MMCPSR_emp$EHPdata ==1 & MMCPSR_emp$OHPdata ==1)/nrow(MMCPSR_emp)

#format table
GenerateData$Percent <- formattable::percent(GenerateData$Percent, digits = 1)


#create bar plot 
ggplot(GenerateData, aes(x = reorder(`Mode of Inquiry`, Percent) , y = Percent, fill = `Mode of Inquiry`)) + 
  geom_bar(stat = "identity") + scale_fill_brewer(palette = "Blues", guide = FALSE) + theme_minimal(base_size = 12) + labs(y = "Percent of Articles", x = "Mode of Inquiry") + geom_text(aes(label = Percent), vjust ="center", size=3, hjust = "center", nudge_y =.025) + scale_y_continuous(labels = scales::percent, limits = c(0,1)) 

##Strategy for creating empirical base of article (data)
#create a dataframe
EmpiricalBase <- data.frame(matrix(NA, nrow = 5, ncol = 3)) 
colnames(EmpiricalBase) <- c("Empirical Base", "Frequency", "Percent")
EmpiricalBase$`Empirical Base`<- c("Pre-existing Sources", "Non-Human Participant", "Human Participant Observation", "Human Participant Experimental", "Both Pre-existing and Author Generated")

EmpiricalBase[1,2] <- sum(MMCPSR_emp$`Employed data/information from pre-existing primary or secondary sources`==1 & MMCPSR_emp$OHPdata == 0 & MMCPSR_emp$EHPdata == 0 & MMCPSR_emp$gendataNHP == 0)

EmpiricalBase[2,2] <- sum(MMCPSR_emp$`Employed data/information from pre-existing primary or secondary sources`==0 & MMCPSR_emp$OHPdata == 0 & MMCPSR_emp$EHPdata == 0 & MMCPSR_emp$gendataNHP == 1)

EmpiricalBase[3,2] <- sum(MMCPSR_emp$`Employed data/information from pre-existing primary or secondary sources`==0 & MMCPSR_emp$OHPdata == 1 & MMCPSR_emp$EHPdata == 0 & MMCPSR_emp$gendataNHP == 0)

EmpiricalBase[4,2] <- sum(MMCPSR_emp$`Employed data/information from pre-existing primary or secondary sources`==0 & MMCPSR_emp$OHPdata == 0 & MMCPSR_emp$EHPdata == 1 & MMCPSR_emp$gendataNHP == 0)

EmpiricalBase[5,2] <- sum(MMCPSR_emp$`Employed data/information from pre-existing primary or secondary sources`==1 & MMCPSR_emp$OHPdata+ MMCPSR_emp$EHPdata + MMCPSR_emp$gendataNHP > 0)

EmpiricalBase$Percent <- formattable::percent(EmpiricalBase$Frequency/nrow(MMCPSR_emp), digits = 1)


#Number (%) that use both data collected from pre-existing and author-generated data that leaned toward data/info. collected from pre-existing sources
formattable::percent(sum(MMCPSR_emp$`Mostly pre-existing data`)/EmpiricalBase[5,2])

#Number (%) that use both data collected from pre-existing and author-generated data leaned toward author-generated data
formattable::percent(sum(MMCPSR_emp$`Mostly author-generated data`)/EmpiricalBase[5,2])


##Type of Observational HP research
#Number (%) of articles drawing on data collected via Ethno/PO alone + in combo. w/other techniques)
#Raw Count/percent - alone
EthnoSoloCount <- sum(MMCPSR_emp$`Ethnography / participant observation` ==1 & MMCPSR_emp$`Interviews/focus groups` == 0 & MMCPSR_emp$Survey == 0 & MMCPSR_emp$EHPdata == 0 & MMCPSR_emp$gendataNHP == 0 & MMCPSR_emp$`Employed data/information from pre-existing primary or secondary sources`==0)
EthnoSoloCount

formattable::percent(EthnoSoloCount/nrow(MMCPSR_emp), digits = 1)

#Raw Count/percent - in combination with other data collection or generation techniques
sum(MMCPSR_emp$`Ethnography / participant observation`) - EthnoSoloCount
formattable::percent((sum(MMCPSR_emp$`Ethnography / participant observation`) - EthnoSoloCount)/nrow(MMCPSR_emp), digits = 1)


##Number (%) of articles drawing on data collected via Interviews and focus groups (alone + in combo. w/other techniques)
#Raw Count/percent - alone
InterviewSoloCount <- sum(MMCPSR_emp$`Ethnography / participant observation` ==0 & MMCPSR_emp$`Interviews/focus groups` == 1 & MMCPSR_emp$Survey == 0 & MMCPSR_emp$EHPdata == 0 & MMCPSR_emp$gendataNHP == 0 & MMCPSR_emp$`Employed data/information from pre-existing primary or secondary sources`==0)
InterviewSoloCount

formattable::percent(InterviewSoloCount/nrow(MMCPSR_emp), digits = 1)

#Raw Count/percent - in combination with other techniques
sum(MMCPSR_emp$`Interviews/focus groups`) - InterviewSoloCount
formattable::percent((sum(MMCPSR_emp$`Interviews/focus groups`) - EthnoSoloCount)/nrow(MMCPSR_emp), digits = 1)

##Number (%) of articles drawing on data collected via Survey (alone + in combo. w/other techniques)
#Raw Count/percent - alone
SurveySoloCount <- sum(MMCPSR_emp$`Ethnography / participant observation` ==0 & MMCPSR_emp$`Interviews/focus groups` == 0 & MMCPSR_emp$Survey == 1 & MMCPSR_emp$EHPdata == 0 & MMCPSR_emp$gendataNHP == 0 & MMCPSR_emp$`Employed data/information from pre-existing primary or secondary sources`==0)
SurveySoloCount

formattable::percent(SurveySoloCount/nrow(MMCPSR_emp), digits = 1)

#Raw Count/percent - in combination with other techniques
sum(MMCPSR_emp$Survey) - SurveySoloCount
formattable::percent((sum(MMCPSR_emp$Survey) - SurveySoloCount)/nrow(MMCPSR_emp), digits = 1)


#Variation in each across subfields 
#Ethnography/participant observation by subfield 
EthnoSubfield <- MMCPSR_emp%>% count(Subfield,`Ethnography / participant observation`) %>% subset(`Ethnography / participant observation` ==1)
colnames(EthnoSubfield) <- c("Subfield","E", "Ethnography/Participant Observation")
EthnoSubfield <- EthnoSubfield[-2]

InterviewSubfield <- MMCPSR_emp %>% count(Subfield, `Interviews/focus groups`) %>% subset(`Interviews/focus groups` == 1)
colnames(InterviewSubfield) <- c("Subfield","E", "Interview/Focus Groups")
InterviewSubfield <- InterviewSubfield[-2]

SurveySubfield <- MMCPSR_emp %>% count(Subfield, Survey) %>% subset(Survey == 1)
colnames(SurveySubfield) <- c("Subfield","E", "Survey")
SurveySubfield <- SurveySubfield[-2]

Subfields <- MMCPSR_emp %>% count(Subfield)

OHPSubfield <- left_join(Subfields[1], EthnoSubfield, by = "Subfield")
OHPSubfield <- left_join(OHPSubfield, InterviewSubfield, by = "Subfield")
OHPSubfield <- left_join(OHPSubfield, SurveySubfield, by = "Subfield")
OHPSubfield[is.na(OHPSubfield)] <- 0

#Variation in each over time
EthnoTime <- MMCPSR_emp%>% count(`Ethnography / participant observation`,Year)
EthnoTime <- inner_join(EthnoTime, ArticlesbyYear_emp, by = "Year")
EthnoTime <- subset(EthnoTime, EthnoTime$`Ethnography / participant observation`==1)
EthnoTime$Percent <- formattable::percent(EthnoTime$n.x/EthnoTime$n.y, digits = 1)
EthnoTime$`Ethnography / participant observation` <- "Ethnography/Participant Observation"
colnames(EthnoTime) <- c("OHP Type", "Year", "Count", "Total Articles (Year)", "Percent")

InterviewTime <- MMCPSR_emp %>% count(`Interviews/focus groups`, Year)
InterviewTime <- inner_join(InterviewTime, ArticlesbyYear_emp, by = "Year")
InterviewTime <- subset(InterviewTime, InterviewTime$`Interviews/focus groups` ==1)
InterviewTime$Percent <- formattable::percent(InterviewTime$n.x/InterviewTime$n.y, digits = 1)
InterviewTime$`Interviews/focus groups` <- "Interview/Focus Group"
colnames(InterviewTime) <- c("OHP Type", "Year", "Count", "Total Articles (Year)", "Percent")

SurveyTime <- MMCPSR_emp %>% count(Survey, Year)
SurveyTime <- inner_join(SurveyTime, ArticlesbyYear_emp, by = "Year")
SurveyTime <- subset(SurveyTime, SurveyTime$Survey ==1)
SurveyTime$Percent <- formattable::percent(SurveyTime$n.x/SurveyTime$n.y, digits = 1)
SurveyTime$Survey <- "Survey"
colnames(SurveyTime) <- c("OHP Type", "Year", "Count", "Total Articles (Year)", "Percent")

OHPTime <- rbind(EthnoTime, InterviewTime)
OHPTime<- rbind(OHPTime, SurveyTime)

#plot data
OHPTime %>%
  ggplot( aes(x=Year, y=Percent, group=`OHP Type`, color=`OHP Type`)) +
  geom_line()+scale_color_brewer(palette="Set2") + theme_minimal(base_size = 12) + 
  scale_y_continuous(labels = scales::percent, limits = c(0,1)) + theme(legend.position = "bottom")


#Variation in each across journals 
#Ethnography/participant observation by journal
EthnoJournal <- MMCPSR_emp%>% count(Journal,`Ethnography / participant observation`) %>% subset(`Ethnography / participant observation` ==1)
colnames(EthnoJournal) <- c("Journal","E", "Ethnography/Participant Observation")
EthnoJournal <- EthnoJournal[-2]

#Interview/focus group by journal
InterviewJournal <- MMCPSR_emp %>% count(Journal, `Interviews/focus groups`) %>% subset(`Interviews/focus groups` == 1)
colnames(InterviewJournal) <- c("Journal","E", "Interview/Focus Groups")
InterviewJournal <- InterviewJournal[-2]

#survey by journal
SurveyJournal <- MMCPSR_emp %>% count(Journal, Survey) %>% subset(Survey == 1)
colnames(SurveyJournal) <- c("Journal","E", "Survey")
SurveyJournal <- SurveyJournal[-2]

#Join
OHPJournal <- left_join(Journals[1], EthnoJournal, by = "Journal")
OHPJournal <- left_join(OHPJournal, InterviewJournal, by = "Journal")
OHPJournal <- left_join(OHPJournal, SurveyJournal, by = "Journal")

#replace N/A with 0
OHPJournal[is.na(OHPJournal)] <- 0


#Type of Experimental HP research
#Number (%) of articles using Survey experiments (alone + in combo. w/other techniques)
#Count/percent - alone
SurveyExpSoloCount <- sum(MMCPSR_emp$`Survey experiment` ==1 & MMCPSR_emp$Field == 0 & MMCPSR_emp$Lab == 0 & MMCPSR_emp$OHPdata == 0 & MMCPSR_emp$gendataNHP == 0 & MMCPSR_emp$`Employed data/information from pre-existing primary or secondary sources`==0)
SurveyExpSoloCount

formattable::percent(SurveyExpSoloCount/nrow(MMCPSR_emp), digits = 1)

#Raw Count/percent - in combination with other techniques
sum(MMCPSR_emp$`Survey experiment`) - SurveyExpSoloCount
formattable::percent((sum(MMCPSR_emp$`Survey experiment`) - SurveyExpSoloCount)/nrow(MMCPSR_emp), digits = 1)


#Number (%) of articles using Field experiments (alone + in combo. w/other techniques)
#Count/percent - alone
FieldExpSoloCount <- sum(MMCPSR_emp$`Survey experiment` ==0 & MMCPSR_emp$Field == 1 & MMCPSR_emp$Lab == 0 & MMCPSR_emp$OHPdata == 0 & MMCPSR_emp$gendataNHP == 0 & MMCPSR_emp$`Employed data/information from pre-existing primary or secondary sources`==0)
FieldExpSoloCount

formattable::percent(FieldExpSoloCount/nrow(MMCPSR_emp), digits = 1)

#Count/percent - in combination with other techniques
sum(MMCPSR_emp$Field) - FieldExpSoloCount
formattable::percent((sum(MMCPSR_emp$Field) - FieldExpSoloCount)/nrow(MMCPSR_emp), digits = 1)

#Number (%) of articles using Lab experiments (alone + in combo. w/other techniques)
#Count/percent - alone
LabExpSoloCount <- sum(MMCPSR_emp$`Survey experiment` ==0 & MMCPSR_emp$Field == 0 & MMCPSR_emp$Lab == 1 & MMCPSR_emp$OHPdata == 0 & MMCPSR_emp$gendataNHP == 0 & MMCPSR_emp$`Employed data/information from pre-existing primary or secondary sources`==0)
FieldExpSoloCount

formattable::percent(LabExpSoloCount/nrow(MMCPSR_emp), digits = 1)

#Count/percent - in combination with other techniques
sum(MMCPSR_emp$Lab) - LabExpSoloCount
formattable::percent((sum(MMCPSR_emp$Lab) - LabExpSoloCount)/nrow(MMCPSR_emp), digits = 1)


#Variation in each across subfields
SurveyExpSubfield <- MMCPSR_emp %>% count(Subfield, `Survey experiment`) %>% subset(`Survey experiment` == 1)
colnames(SurveyExpSubfield) <- c("Subfield","E", "Survey Experiment")
SurveyExpSubfield <- SurveyExpSubfield[-2]

FieldExpSubfield <- MMCPSR_emp %>% count(Subfield, Field) %>% subset(Field == 1)
colnames(FieldExpSubfield) <- c("Subfield", "E", "Field Experiment")
FieldExpSubfield <- FieldExpSubfield[-2]

LabExpSubfield <- MMCPSR_emp %>% count(Subfield, Lab) %>% subset(Lab == 1)
colnames(LabExpSubfield) <- c("Subfield", "e", "Lab Experiment")
LabExpSubfield <- LabExpSubfield[-2]

EHPSubfield <- left_join(Subfields[1], SurveyExpSubfield, by = "Subfield")
EHPSubfield <- left_join(EHPSubfield, FieldExpSubfield, by = "Subfield")
EHPSubfield <- left_join(EHPSubfield, LabExpSubfield, by = "Subfield")
EHPSubfield[is.na(EHPSubfield)] <- 0

#Variation in each over time
SurveyExpTime <- MMCPSR_emp %>% count(`Survey experiment`, Year)
SurveyExpTime <- inner_join(SurveyExpTime, ArticlesbyYear_emp, by = "Year")
SurveyExpTime <- subset(SurveyExpTime, SurveyExpTime$`Survey experiment` ==1)
SurveyExpTime$Percent <- formattable::percent(SurveyExpTime$n.x/SurveyExpTime$n.y, digits = 1)
SurveyExpTime$`Survey experiment`<- "Survey Experiment"
colnames(SurveyExpTime) <- c("EHP Type", "Year", "Count", "Total Articles (Year)", "Percent")

FieldExpTime <- MMCPSR_emp %>% count(Field, Year)
FieldExpTime <- inner_join(FieldExpTime, ArticlesbyYear_emp, by = "Year")
FieldExpTime <- subset(FieldExpTime, FieldExpTime$Field ==1)
FieldExpTime$Percent <- formattable::percent(FieldExpTime$n.x/FieldExpTime$n.y, digits = 1)
FieldExpTime$Field<- "Field Experiment"
colnames(FieldExpTime) <- c("EHP Type", "Year", "Count", "Total Articles (Year)", "Percent")

LabExpTime <- MMCPSR_emp %>% count(Lab, Year)
LabExpTime <- inner_join(LabExpTime, ArticlesbyYear_emp, by = "Year")
LabExpTime  <- subset(LabExpTime, LabExpTime $Lab ==1)
LabExpTime $Percent <- formattable::percent(LabExpTime $n.x/LabExpTime $n.y, digits = 1)
LabExpTime $Lab<- "Lab Experiment"
colnames(LabExpTime) <- c("EHP Type", "Year", "Count", "Total Articles (Year)", "Percent")

EHPTime <- rbind(SurveyExpTime, FieldExpTime)
EHPTime<- rbind(EHPTime, LabExpTime)

#plot data
EHPTime %>%
  ggplot( aes(x=Year, y=Percent, group=`EHP Type`, color=`EHP Type`)) +
  geom_line()+scale_color_brewer(palette="Set2") + theme_minimal(base_size = 12) + 
  scale_y_continuous(labels = scales::percent, limits = c(0,1)) 


#Variation in each across journals
#Survey experiment by journal
SurveyExpJournal <- MMCPSR_emp%>% count(Journal,`Survey experiment`) %>% subset(`Survey experiment` ==1)
colnames(SurveyExpJournal) <- c("Journal","E", "Survey Experiment")
SurveyExpJournal <- SurveyExpJournal[-2]

#Field Experiment by journal
FieldExpJournal <- MMCPSR_emp %>% count(Journal, Field) %>% subset(Field == 1)
colnames(FieldExpJournal) <- c("Journal","E", "Field Experiment")
FieldExpJournal <- FieldExpJournal[-2]

#Lab Experiment by journal
LabExpJournal <- MMCPSR_emp %>% count(Journal, Lab) %>% subset(Lab == 1)
colnames(LabExpJournal) <- c("Journal","E", "Lab")
LabExpJournal <- LabExpJournal[-2]

#Join
EHPJournal <- left_join(Journals[1], LabExpJournal, by = "Journal")
EHPJournal <- left_join(EHPJournal, FieldExpJournal, by = "Journal")
EHPJournal <- left_join(EHPJournal, SurveyExpJournal, by = "Journal")

#replace N/A with 0
EHPJournal[is.na(EHPJournal)] <- 0


#Source of HP data (observational or experimental)
#Number (%) of articles using data generated through interaction with Int body / inst.
sum(MMCPSR_emp$`EHP - International body / institution` == 1 | MMCPSR_emp$`OHP - International body / institution`==1) 
formattable::percent(sum(MMCPSR_emp$`EHP - International body / institution` == 1 | MMCPSR_emp$`OHP - International body / institution`==1)/nrow(MMCPSR_emp), digits = 1)

#Number (%) of articles using data generated through interaction with Dom gov
sum(MMCPSR_emp$`EHP - Domestic government` == 1 | MMCPSR_emp$`OHP - Domestic government`==1) 
formattable::percent(sum(MMCPSR_emp$`EHP - Domestic government` == 1 | MMCPSR_emp$`OHP - Domestic government`==1) /nrow(MMCPSR_emp), digits = 1)
                         
#Number (%) of articles using data generated through interaction with CSO
sum(MMCPSR_emp$`EHP - CSO` == 1 | MMCPSR_emp$`OHP - CSO`==1) 
formattable::percent(sum(MMCPSR_emp$`EHP - CSO` == 1 | MMCPSR_emp$`OHP - CSO`==1)/nrow(MMCPSR_emp), digits = 1)
                         
#Number (%) of articles using data generated through interaction with Media
sum(MMCPSR_emp$`EHP - Media` == 1 | MMCPSR_emp$`OHP - Media`==1) 
formattable::percent(sum(MMCPSR_emp$`EHP - Media` == 1 | MMCPSR_emp$`OHP - Media`==1)/nrow(MMCPSR_emp), digits = 1)

#Number (%) of articles using data generated through interaction with Academics / researchers
sum(MMCPSR_emp$`EHP - Academics/Researchers` == 1 | MMCPSR_emp$`OHP - Academics/Researchers`==1) 
formattable::percent(sum(MMCPSR_emp$`EHP - Academics/Researchers` == 1 | MMCPSR_emp$`OHP - Academics/Researchers`==1) /nrow(MMCPSR_emp), digits = 1)
 

                       
#Aggregated, variation across subfields
MMCPSR_emp$IntBody <- ifelse(MMCPSR_emp$`EHP - International body / institution` == 1 | MMCPSR_emp$`OHP - International body / institution` == 1, 1, 0)
IntBodySubfield <- MMCPSR_emp %>% count(Subfield, IntBody) %>% subset(IntBody == 1)
colnames(IntBodySubfield) <- c("Subfield", "n", "International Body")
IntBodySubfield <- IntBodySubfield[-2]

MMCPSR_emp$DomGovt <- ifelse(MMCPSR_emp$`EHP - Domestic government`== 1 | MMCPSR_emp$`OHP - Domestic government` == 1, 1, 0)
DomGovtSubfield <- MMCPSR_emp %>% count(Subfield, DomGovt) %>% subset(DomGovt == 1)
colnames(DomGovtSubfield) <- c("Subfield", "n", "Domestic Government")
DomGovtSubfield <- DomGovtSubfield[-2]

MMCPSR_emp$CSO <- ifelse(MMCPSR_emp$`EHP - CSO` == 1 | MMCPSR_emp$`OHP - CSO` ==1, 1,0)
CSOSubfield <- MMCPSR_emp %>% count(Subfield, CSO) %>% subset(CSO == 1)
colnames(CSOSubfield) <- c("Subfield", "n", "CSO")
CSOSubfield <- CSOSubfield[-2]

MMCPSR_emp$Media <- ifelse(MMCPSR_emp$`OHP - Media` == 1 | MMCPSR_emp$`EHP - Media` == 1,1,0)
MediaSubfield <- MMCPSR_emp %>% count(Subfield, Media) %>% subset(Media == 1)
MediaSubfield <- MediaSubfield[-2]
colnames(MediaSubfield) <- c("Subfield", "Media")

MMCPSR_emp$Academics <- ifelse(MMCPSR_emp$`OHP - Academics/Researchers` == 1 | MMCPSR_emp$`EHP - Academics/Researchers` == 1, 1, 0)
AcademicsSubfield <- MMCPSR_emp %>% count(Subfield, Academics) %>% subset(Academics == 1)
AcademicsSubfield <- AcademicsSubfield[-2]
colnames(AcademicsSubfield) <- c("Subfield", "Academics/Researchers")

DataSourceSubfield <- left_join(Subfields[1], IntBodySubfield, by = "Subfield")
DataSourceSubfield <- left_join(DataSourceSubfield, DomGovtSubfield, by = "Subfield")
DataSourceSubfield <- left_join(DataSourceSubfield, CSOSubfield, by = "Subfield")
DataSourceSubfield <- left_join(DataSourceSubfield, MediaSubfield, by = "Subfield")
DataSourceSubfield <- left_join(DataSourceSubfield, AcademicsSubfield, by = "Subfield")
DataSourceSubfield[is.na(DataSourceSubfield)] <- 0

#Aggregated, variation over time
#International Body
IntBodyTime <- MMCPSR_emp %>% count(IntBody, Year)
IntBodyTime <- inner_join(IntBodyTime, ArticlesbyYear_emp, by = "Year")
IntBodyTime <- subset(IntBodyTime, IntBodyTime$IntBody ==1)
IntBodyTime$Percent <- formattable::percent(IntBodyTime$n.x/IntBodyTime$n.y, digits = 1)
IntBodyTime$IntBody<- "International Body"
colnames(IntBodyTime) <- c("Data Source Type", "Year", "Count", "Total Articles (Year)", "Percent")

#Domestic Gov
DomGovtTime <- MMCPSR_emp %>% count(DomGovt, Year)
DomGovtTime <- inner_join(DomGovtTime, ArticlesbyYear_emp, by = "Year")
DomGovtTime <- subset(DomGovtTime, DomGovtTime$DomGovt ==1)
DomGovtTime$Percent <- formattable::percent(DomGovtTime$n.x/DomGovtTime$n.y, digits = 1)
DomGovtTime$DomGovt<- "Domestic Government"
colnames(DomGovtTime) <- c("Data Source Type", "Year", "Count", "Total Articles (Year)", "Percent")

#CSO
CSOTime <- MMCPSR_emp %>% count(CSO, Year)
CSOTime <- inner_join(CSOTime, ArticlesbyYear_emp, by = "Year")
CSOTime <- subset(CSOTime, CSOTime$CSO ==1)
CSOTime$Percent <- formattable::percent(CSOTime$n.x/CSOTime$n.y, digits = 1)
CSOTime$CSO<- "CSO"
colnames(CSOTime) <- c("Data Source Type", "Year", "Count", "Total Articles (Year)", "Percent")

#Media
MediaTime <- MMCPSR_emp %>% count(Media, Year)
MediaTime <- inner_join(MediaTime, ArticlesbyYear_emp, by = "Year")
MediaTime <- subset(MediaTime, MediaTime$Media ==1)
MediaTime$Percent <- formattable::percent(MediaTime$n.x/MediaTime$n.y, digits = 1)
MediaTime$Media<- "Media"
colnames(MediaTime) <- c("Data Source Type", "Year", "Count", "Total Articles (Year)", "Percent")

#Academics
AcademicsTime <- MMCPSR_emp %>% count(Academics, Year)
AcademicsTime <- inner_join(AcademicsTime, ArticlesbyYear_emp, by = "Year")
AcademicsTime <- subset(AcademicsTime, AcademicsTime$Academics ==1)
AcademicsTime$Percent <- formattable::percent(AcademicsTime$n.x/AcademicsTime$n.y, digits = 1)
AcademicsTime$Academics<- "Media"
colnames(AcademicsTime) <- c("Data Source Type", "Year", "Count", "Total Articles (Year)", "Percent")

DataSourceTime<- rbind(AcademicsTime, CSOTime)
DataSourceTime<- rbind(DataSourceTime, IntBodyTime)
DataSourceTime<- rbind(DataSourceTime, MediaTime)
DataSourceTime<- rbind(DataSourceTime, DomGovtTime)

#plot data
DataSourceTime %>%
  ggplot( aes(x=Year, y=Percent, group=`Data Source Type`, color=`Data Source Type`)) +
  geom_line()+scale_color_brewer(palette="Set2") + theme_minimal(base_size = 12) + 
  scale_y_continuous(labels = scales::percent, limits = c(0,1)) + theme(legend.position = "bottom")

#Aggregated, variation across journals
#International body by journal
IntBodyJournal <- MMCPSR_emp %>% count(Journal, IntBody) %>% subset(IntBody == 1)
colnames(IntBodyJournal) <- c("Journal","E", "International Body")
IntBodyJournal <- IntBodyJournal[-2]

#Domestic Government by journal
DomGovtJournal <- MMCPSR_emp %>% count(Journal, DomGovt) %>% subset(DomGovt == 1)
colnames(DomGovtJournal) <- c("Journal","E", "Domestic Government")
DomGovtJournal <- DomGovtJournal[-2]

#CSO by journal
CSOJournal <- MMCPSR_emp %>% count(Journal, CSO) %>% subset(CSO == 1)
colnames(CSOJournal) <- c("Journal","E", "CSO")
CSOJournal <- CSOJournal[-2]

#Media by journal
MediaJournal <- MMCPSR_emp %>% count(Journal, Media) %>% subset(Media == 1)
colnames(MediaJournal) <- c("Journal","E", "Media")
MediaJournal <- MediaJournal[-2]

#Academics by journal
AcademicsJournal <- MMCPSR_emp %>% count(Journal, Academics) %>% subset(Academics == 1)
colnames(AcademicsJournal) <- c("Journal","E", "Academics/Researchers")
AcademicsJournal <- AcademicsJournal[-2]

#Join
DataSourceJournal <- left_join(Journals[1], IntBodyJournal, by = "Journal")
DataSourceJournal <- left_join(DataSourceJournal,DomGovtJournal, by = "Journal")
DataSourceJournal <- left_join(DataSourceJournal, CSOJournal, by = "Journal")
DataSourceJournal <- left_join(DataSourceJournal, MediaJournal, by = "Journal")
DataSourceJournal <- left_join(DataSourceJournal, AcademicsJournal, by = "Journal")

#replace N/A with 0
DataSourceJournal[is.na(DataSourceJournal)] <- 0


##Type of data analysis
#Number (%) of articles that Used only interpretive methods
sum(MMCPSR_emp$Interpretive == 1 & MMCPSR_emp$MixedMethods == 0)
formattable::percent(sum(MMCPSR_emp$Interpretive == 1 & MMCPSR_emp$MixedMethods == 0)/nrow(MMCPSR_emp), digits = 1)

#Number (%) of articles that Used only a qual method (include number / % for each sub-type) Counts for subtype includes articles that may use more than one type of data analysis
MMCPSR_emp$QualOnly <- ifelse(MMCPSR_emp$QualMethod == 1 & MMCPSR_emp$MixedMethods == 0, 1, 0)
sum(MMCPSR_emp$QualOnly == 1)
formattable::percent(sum(MMCPSR_emp$QualOnly)/nrow(MMCPSR_emp), digits = 1)

#PT
sum(MMCPSR_emp$`Process tracing`==1)
formattable::percent(sum(MMCPSR_emp$`Process tracing`==1)/nrow(MMCPSR_emp), digits =1)

#QCA
sum(MMCPSR_emp$QCA==1)
formattable::percent(sum(MMCPSR_emp$QCA==1)/nrow(MMCPSR_emp), digits =1)

#Congruence Analysis
sum(MMCPSR_emp$`Congruence analysis` ==1) 
formattable::percent(sum(MMCPSR_emp$`Congruence analysis`==1)/nrow(MMCPSR_emp), digits =1)

#Counterfact
sum(MMCPSR_emp$`Counterfactual analysis` == 1)
formattable::percent(sum(MMCPSR_emp$`Congruence analysis` == 1)/nrow(MMCPSR_emp), digits = 1)

#SCC
sum(MMCPSR_emp$`Structured case comparison`==1)
formattable::percent(sum(MMCPSR_emp$`Structured case comparison` == 1)/nrow(MMCPSR_emp), digits = 1)

#Other
sum(MMCPSR_emp$Other==1)
formattable::percent(sum(MMCPSR_emp$Other == 1)/nrow(MMCPSR_emp), digits = 1)

#Used illustrative case study
sum(MMCPSR_emp$`Illustrative case study`==1)
formattable::percent(sum(MMCPSR_emp$`Illustrative case study` == 1)/nrow(MMCPSR_emp), digits = 1)

#Used only a quant method (include number / % for each sub-type). Counts for subtype includes articles that may use more than one type of data analysis
MMCPSR_emp$QuantOnly <- ifelse(MMCPSR_emp$QuantMethod == 1 & MMCPSR_emp$MixedMethods == 0, 1, 0)
sum(MMCPSR_emp$QuantOnly == 1)
formattable::percent(sum(MMCPSR_emp$QuantOnly == 1)/nrow(MMCPSR_emp), digits =1)

#Simple prob 
sum(MMCPSR_emp$`Simple Probability` ==1)
formattable::percent(sum(MMCPSR_emp$`Simple Probability` == 1)/nrow(MMCPSR_emp), digits = 1)

#Regression
sum(MMCPSR_emp$Regression ==1 ) 
formattable::percent(sum(MMCPSR_emp$Regression == 1)/nrow(MMCPSR_emp), digits = 1)

#Stats w/an ID strat
sum(MMCPSR_emp$`Statistics with an identification strategy`==1) 
formattable::percent(sum(MMCPSR_emp$`Statistics with an identification strategy`==1)/nrow(MMCPSR_emp), digits = 1)

#Machine learning
sum(MMCPSR_emp$`Machine learning`==1) 
formattable::percent(sum(MMCPSR_emp$`Machine learning`==1)/nrow(MMCPSR_emp), digits = 1)

#Used only FM
sum(MMCPSR_emp$ModelingOnly == 1)
formattable::percent(sum(MMCPSR_emp$ModelingOnly)/nrow(MMCPSR_emp), digits = 1)

#Used no discernible method
sum(MMCPSR_emp$`No discernible method` == 1)
formattable::percent(sum(MMCPSR_emp$`No discernible method`)/nrow(MMCPSR_emp), digits = 1)

#Number (%) of articles that Used more than one method
sum(MMCPSR_emp$MixedMethods == 1)
formattable::percent(sum(MMCPSR_emp$MixedMethods == 1)/nrow(MMCPSR_emp), digits = 1)

#Number(%) of articles multi method articles that are overwhelmingly qual
sum(MMCPSR_emp$`Overwhelmingly qual`==1)
formattable::percent(sum(MMCPSR_emp$`Overwhelmingly qual`/(sum(MMCPSR_emp$MixedMethods == 1))))

#Number(%) of articles multi method articles that are overwhelmingly quant
sum(MMCPSR_emp$`Overwhelmingly quant`==1)
formattable::percent(sum(MMCPSR_emp$`Overwhelmingly quant`/(sum(MMCPSR_emp$MixedMethods == 1))))

#Number(%) of articles multi method articles that are overwhelmingly FM
sum(MMCPSR_emp$`Overwhelmingly modeling`==1)
formattable::percent(sum(MMCPSR_emp$`Overwhelmingly modeling`/(sum(MMCPSR_emp$MixedMethods == 1))))

#Number(%) of articles multi method articles that are overwhelmingly interpretive
sum(MMCPSR_emp$`Overwhelmingly interpretive`==1)
formattable::percent(sum(MMCPSR_emp$`Overwhelmingly interpretive`/(sum(MMCPSR_emp$MixedMethods == 1))))

#Number(%) of articles multi method articles that have no specific methodological focus
sum(MMCPSR_emp$`No specific focus`==1)
formattable::percent(sum(MMCPSR_emp$`No specific focus`/(sum(MMCPSR_emp$MixedMethods == 1))))

#Number (%) of articles that Used more than one qualitative method (only – no quant / FM)
MMCPSR_emp$QualMulti <- ifelse(MMCPSR_emp$QualOnly == 1 & MMCPSR_emp$`Process tracing`+ MMCPSR_emp$QCA + MMCPSR_emp$`Congruence analysis`+ MMCPSR_emp$`Counterfactual analysis` + MMCPSR_emp$`Structured case comparison` + MMCPSR_emp$`Comparative historical analysis` + MMCPSR_emp$Other >1, 1, 0)
sum(MMCPSR_emp$QualMulti == 1)
formattable::percent(sum(MMCPSR_emp$QualMulti == 1)/nrow(MMCPSR_emp), digits = 1)

#Number (%) of articles that Used one or more qual methods and a quantitative method
sum(MMCPSR_emp$QuantMethod == 1 & MMCPSR_emp$QualMethod == 1)
formattable::percent(sum(MMCPSR_emp$QuantMethod == 1 & MMCPSR_emp$QualMethod)/nrow(MMCPSR_emp), digits =1)

#Of these (Quant/Qual), number (%) that leaned toward qual
sum(MMCPSR_emp$QuantMethod == 1 & MMCPSR_emp$QualMethod == 1 & MMCPSR_emp$`Overwhelmingly qual`==1)
formattable::percent(sum(MMCPSR_emp$QuantMethod == 1 & MMCPSR_emp$QualMethod & MMCPSR_emp$`Overwhelmingly qual`==1)/sum(MMCPSR_emp$QuantMethod == 1 & MMCPSR_emp$QualMethod == 1), digits =1)

#Number (%) of articles that Used one or more qual methods and FM
sum(MMCPSR_emp$`Formal modeling`== 1 & MMCPSR_emp$QualMethod == 1)
formattable::percent(sum(MMCPSR_emp$`Formal modeling` == 1 & MMCPSR_emp$QualMethod == 1)/nrow(MMCPSR_emp), digits =1)

#Of these, number (%) that leaned toward qual
sum(MMCPSR_emp$`Formal modeling`== 1 & MMCPSR_emp$QualMethod == 1 & MMCPSR_emp$`Overwhelmingly qual`==1)
formattable::percent(sum(MMCPSR_emp$`Formal modeling` == 1 & MMCPSR_emp$QualMethod & MMCPSR_emp$`Overwhelmingly qual`==1)/sum(MMCPSR_emp$`Formal modeling` == 1 & MMCPSR_emp$QualMethod == 1), digits =1)

#Number (%) of articles that used quant and FM 
sum(MMCPSR_emp$`Formal modeling`== 1 & MMCPSR_emp$QuantMethod == 1)
formattable::percent(sum(MMCPSR_emp$`Formal modeling` == 1 & MMCPSR_emp$QuantMethod == 1)/nrow(MMCPSR_emp), digits =1)

#Natural experiment
#Number (%) of articles capitalizing on a natural experiment
sum(MMCPSR_emp$`Natural Experiment` == 1)
formattable::percent(sum(MMCPSR_emp$`Natural Experiment` == 1)/nrow(MMCPSR_emp), digits = 1)

#Change over time
NatExpTime <-as.data.frame(xtabs(~Year + `Natural Experiment`, data = MMCPSR_emp))
NatExpTime <- subset(NatExpTime, NatExpTime$Natural.Experiment == 1)
NatExpTime <- NatExpTime[-2]
NatExpTime$Percent <- formattable::percent(NatExpTime$Freq/ArticlesbyYear_emp$n, digits = 1)

NatExpTime %>%
  ggplot(aes(x=Year, y=Percent, group = 1)) +
  geom_line(color = "orange") + theme_minimal(base_size = 12) + 
  scale_y_continuous(labels = scales::percent, limits = c(0,1))  

#Synthetic data
#Number (%) of articles using synthetic data
sum(MMCPSR_emp$`Generated synthetic data`==1)
formattable::percent(sum(MMCPSR_emp$`Generated synthetic data`==1)/nrow(MMCPSR_emp), digits = 1)

#Change over time
SyntheticTime <-as.data.frame(xtabs(~Year + `Generated synthetic data`, data = MMCPSR_emp))
SyntheticTime <- subset(SyntheticTime, SyntheticTime$Generated.synthetic.data == 1)
SyntheticTime <- SyntheticTime[-2]
SyntheticTime$Percent <- formattable::percent(SyntheticTime$Freq/ArticlesbyYear_emp$n, digits = 1)

SyntheticTime %>%
  ggplot(aes(x=Year, y=Percent, group = 1))  +
  geom_line(color = "orange") + theme_minimal(base_size = 12) + 
  scale_y_continuous(labels = scales::percent, limits = c(0,1)) 

#Text mining / analysis
#Number (%) of articles using text mining / analysis
sum(MMCPSR_emp$`Text analysis/text mining` == 1)
formattable::percent(sum(MMCPSR_emp$`Text analysis/text mining` == 1)/nrow(MMCPSR_emp), digits =1)

#Change over time
TextTime <-as.data.frame(xtabs(~Year + `Text analysis/text mining`, data = MMCPSR_emp))
TextTime <- subset(TextTime, TextTime$Text.analysis.text.mining == 1)
TextTime <- TextTime[-2]
TextTime$Percent <- formattable::percent(TextTime$Freq/ArticlesbyYear_emp$n, digits = 1)

TextTime %>%
  ggplot(aes(x=Year, y=Percent, group = 1))  +
  geom_line(color = "orange") + theme_minimal(base_size = 12) + 
  scale_y_continuous(labels = scales::percent, limits = c(0,1)) 

save(MMCPSR_emp, file = "MMCPSR Analysis Data No PT.RData")


####ANALYSIS - Correlations / Bivariate regression####
#Main IVs: Subfield	(Bivariate regression),Time (Bivariate regression), Journal (Correlation), Data collection/generation strategy (correlation)

#DV - EHPdata dicotomous variable that equals 1 where experimental data is used 0 if not
#IV - Year (time)
#OLS - experimental data vs time
out.1 <- (lm(MMCPSR_emp$EHPdata~MMCPSR_emp$Year))
summary(out.1)
out.1$coefficients[2]

#probit and marginal effects - experimental data vs time
out.1.probit <-glm(EHPdata~Year, data = MMCPSR_emp, family = binomial(link = "probit"))
summary(out.1.probit)
probitmfx(out.1.probit, data = MMCPSR_emp)


#IV - Subfield
#OLS - experimental data vs subfield
out.2 <- (lm(MMCPSR_emp$EHPdata~MMCPSR_emp$Subfield))
summary(out.1)
out.1$coefficients[2]

#probit and marginal effects - experimental data vs subfield
out.2.probit <-glm(MMCPSR_emp$EHPdata~MMCPSR_emp$Subfield, family = binomial(link = "probit"))
summary(out.2.probit)
probitmfx(out.2.probit, data = MMCPSR_emp)


#DV = strategy for creating empirical base of article (data) Scores:used data/info. collected from pre-existing sources; used author-generated data (obs human participant.); used author-generated data (exp.); used author-generated (non-human participant)

#create DV
MMCPSR_emp$PreExistingOnly <- ifelse(MMCPSR_emp$`Employed data/information from pre-existing primary or secondary sources` == 1 & MMCPSR_emp$`Mostly pre-existing data` + MMCPSR_emp$`Mostly author-generated data` + MMCPSR_emp$`Fairly balanced` == 0, 1, 0)

MMCPSR_emp$OHPOnly <- ifelse(MMCPSR_emp$OHPdata == 1 & MMCPSR_emp$`Mostly pre-existing data` + MMCPSR_emp$`Mostly author-generated data` + MMCPSR_emp$`Fairly balanced` == 0, 1, 0)

MMCPSR_emp$EHPOnly <- ifelse(MMCPSR_emp$EHPdata == 1 & MMCPSR_emp$`Mostly pre-existing data` + MMCPSR_emp$`Mostly author-generated data` + MMCPSR_emp$`Fairly balanced` == 0, 1, 0)

MMCPSR_emp$NHPOnly <- ifelse(MMCPSR_emp$gendataNHP == 1 & MMCPSR_emp$`Mostly pre-existing data` + MMCPSR_emp$`Mostly author-generated data` + MMCPSR_emp$`Fairly balanced` == 0, 1, 0)

MMCPSR_emp$GenData <- ifelse(MMCPSR_emp$gendataNHP == 1 | MMCPSR_emp$OHPdata ==1 | MMCPSR_emp$EHPdata == 1, 1, 0)

MMCPSR_emp$AuthorGen <- ifelse(MMCPSR_emp$OHPOnly == 1| MMCPSR_emp$EHPOnly == 1 | MMCPSR_emp$NHPOnly == 1| MMCPSR_emp$`Mostly author-generated data` == 1, 1, 0)

MMCPSR_emp$EmpiricalStrategy <- ifelse(MMCPSR_emp$OHPOnly == 1,"OHP Only", ifelse(MMCPSR_emp$PreExistingOnly == 1, "Pre-existing Only", ifelse(MMCPSR_emp$EHPOnly == 1, "EHP Only", ifelse(MMCPSR_emp$NHPOnly == 1, "Non-HP Only", ifelse(MMCPSR_emp$`Mostly author-generated data`==1, "Mostly author generated", ifelse(MMCPSR_emp$`Mostly pre-existing data` == 1, "Mostly pre-existing", ifelse(MMCPSR_emp$`Fairly balanced` == 1, "Fairly balanced", "Other")))))))

#IV - Year(time) 
##OLS - exclusively or mostly author generated vs time
out.3 <- lm(MMCPSR_emp$AuthorGen~MMCPSR_emp$Year)
summary(out.2)
out.3$coefficients[2]

#probit and marginal effects
out.3.probit <- glm(MMCPSR_emp$AuthorGen~MMCPSR_emp$Year, family = binomial(link = "probit"))
summary(out.3.probit)
probitmfx(out.3.probit, data = MMCPSR_emp)


#IV - Journal
#create variables
MMCPSR_emp$APSR <- ifelse(MMCPSR_emp$Journal == "APSR", 1, 0)
MMCPSR_emp$AJPS <- ifelse(MMCPSR_emp$Journal == "AJPS", 1, 0)
MMCPSR_emp$JOP <- ifelse(MMCPSR_emp$Journal == "JOP", 1, 0)
MMCPSR_emp$BJPS <- ifelse(MMCPSR_emp$Journal == "BJPS", 1, 0)
MMCPSR_emp$CP <- ifelse(MMCPSR_emp$Journal == "CP", 1, 0)
MMCPSR_emp$CPS <- ifelse(MMCPSR_emp$Journal == "CPS", 1, 0)
MMCPSR_emp$IO <- ifelse(MMCPSR_emp$Journal == "IO", 1, 0)
MMCPSR_emp$ISQ <- ifelse(MMCPSR_emp$Journal == "ISQ", 1, 0)
MMCPSR_emp$PoP <- ifelse(MMCPSR_emp$Journal == "PoP", 1, 0)
MMCPSR_emp$WP <- ifelse(MMCPSR_emp$Journal == "WP", 1, 0)
MMCPSR_emp$Big3 <- ifelse(MMCPSR_emp$Journal == "APSR"|MMCPSR_emp$Journal == "AJPS"|MMCPSR_emp$Journal == "JOP", 1, 0) 
#OLS - Author Gen vs  time (big 3)
out.4 <- lm(MMCPSR_emp$AuthorGen ~ MMCPSR_emp$Year + MMCPSR_emp$Big3 + IMMCPSR_emp$Year*MMCPSR_emp$Big3 )
summary(out.4)

#probit and marginal effects
out.4.probit <- glm(MMCPSR_emp$AuthorGen ~ MMCPSR_emp$Year + MMCPSR_emp$Big3 + IMMCPSR_emp$Year*MMCPSR_emp$Big3, family = binomial(link = "probit"))
summary(out.4.probit)
probitmfx(out.4.probit, data = MMCPSR_emp)


#IV - Subfield
#OLS - Author Gen vs subfield
out.5 <- lm(AuthorGen ~ Subfield, data=MMCPSR_emp)
summary(out.5)

#probit and marginal effects
out.5.probit <- glm(MMCPSR_emp$AuthorGen~MMCPSR_emp$Subfield, family = binomial(link = "probit"))
summary(out.5.probit)
probitmfx(out.5.probit, data = MMCPSR_emp)


#DV = method for analyzing empirical base of article (data)s
#IV - Gender
#create subset
MethodGender_sub <- subset(MMCPSR_emp, select = c("Single-authored male", "Single-authored female", "Co-authored male", "Co-authored female", "Co-authored mixed-gender team", "QualMethod", "QuantMethod", "QualOnly", "Interpretive", "InterpOnly","Formal modeling", "ModelingOnly", "No discernible method"))

#create correlation matrix - author gender vs methods categories 
MethodGender_cor <- rcorr(as.matrix(MethodGender_sub))

#pull out correlations
MethodGenderCorrelations <- as.data.frame(MethodGender_cor[["r"]])
MethodGenderCorrelations <- round(MethodGenderCorrelations, 4)


#pull out p-values
MethodGenderCorrelations_sig <- as.data.frame(MethodGender_cor[["P"]])
MethodGenderCorrelations_sig <- round(MethodGenderCorrelations_sig, 3)

#create variable -  
MMCPSR_emp$FemaleAuthor <- ifelse(MMCPSR_emp$`Single-authored female` == 1 | MMCPSR_emp$`Co-authored female` == 1,1,0)

#OLS - interpretive only vs gender
out.6 <- lm(MMCPSR_emp$InterpOnly~MMCPSR_emp$Gender)
summary(out.6)

#probit and marginal effects
out.6.probit <- glm(MMCPSR_emp$InterpOnly~MMCPSR_emp$Gender, family = binomial(link="probit"))
summary(out.6.probit)
probitmfx(out.6.probit, data = MMCPSR_emp)


#OLS - interpretive only vs gender (single authored female + coauthored female)
out.6b <- lm(MMCPSR_emp$InterpOnly~MMCPSR_emp$FemaleAuthor)
summary(out.6b)

#probit and marginal effects
out.6b.probit <- glm(MMCPSR_emp$InterpOnly~MMCPSR_emp$FemaleAuthor, family = binomial(link="probit"))
summary(out.6b.probit)
probitmfx(out.6b.probit, data = MMCPSR_emp)


#OLS - interpretive (any) vs gender
out.6c <- lm(MMCPSR_emp$Interpretive~MMCPSR_emp$Gender)
summary(out.6c)

#probit and marginal effects
out.6c.probit <- glm(MMCPSR_emp$Interpretive~MMCPSR_emp$Gender, family = binomial(link="probit"))
summary(out.6c.probit)
probitmfx(out.6c.probit, data = MMCPSR_emp)


#OLS - interpretive (any) vs gender (single authored female + coauthored female)
out.6d <- lm(MMCPSR_emp$Interpretive~MMCPSR_emp$FemaleAuthor)
summary(out.6c)

#probit and marginal effects
out.6d.probit <- glm(MMCPSR_emp$Interpretive~MMCPSR_emp$FemaleAuthor, family = binomial(link="probit"))
summary(out.6d.probit)
probitmfx(out.6d.probit, data = MMCPSR_emp)


#OLS - qual only vs gender
out.7 <- lm(MMCPSR_emp$QualOnly~MMCPSR_emp$Gender)
summary(out.7)

#probit and marginal effects
out.7.probit <- glm(MMCPSR_emp$QualOnly~MMCPSR_emp$Gender, family = binomial(link="probit"))
summary(out.7.probit)
probitmfx(out.7.probit, data = MMCPSR_emp)


#OLS - qual (any) vs gender
out.8 <- lm(MMCPSR_emp$QualMethod~MMCPSR_emp$Gender)
summary(out.8)

#probit and marginal effects
out.8.probit <- glm(MMCPSR_emp$QualMethod~MMCPSR_emp$Gender, family = binomial(link="probit"))
summary(out.8.probit)
probitmfx(out.8.probit, data = MMCPSR_emp)


#IV - time
#OLS - stats (any) vs time
out.9 <- lm(MMCPSR_emp$QuantMethod~MMCPSR_emp$Year)
summary(out.9)
out.9$coefficients[2]

#probit and marginal effects
out.9.probit <- glm(MMCPSR_emp$QuantMethod~MMCPSR_emp$Year, family = binomial(link = "probit"))
summary(out.9.probit)
probitmfx(out.9.probit, data = MMCPSR_emp)


#OLS - stats (only) vs time
out.10 <- lm(MMCPSR_emp$QuantOnly~MMCPSR_emp$Year)
summary(out.10)
out.10$coefficients[2]

#probit and marginal effects
out.10.probit <- glm(MMCPSR_emp$QuantOnly~MMCPSR_emp$Year, family = binomial(link = "probit"))
summary(out.10.probit)
probitmfx(out.10.probit, data = MMCPSR_emp)


#DV - causual methods
#Create causual variables
# High standard of causal identification:Statistics with an identification strategy + experimental data collection
MMCPSR_emp$CausalHigh <- ifelse(MMCPSR_emp$`Statistics with an identification strategy` ==1 | MMCPSR_emp$EHPdata == 1, 1,0)

#Causal identification including controlling for unobservables: Statistics with an identification strategy + fixed effects + experimental data collection
MMCPSR_emp$CausalControl <- ifelse(MMCPSR_emp$`Statistics with an identification strategy` ==1 | MMCPSR_emp$EHPdata == 1| MMCPSR_emp$`Fixed effects` == 1, 1,0)

#Mixed-methods causal identification: Statistics with an identification strategy + fixed effects + experimental data collection + process tracing 
MMCPSR_emp$CausalMixed <- ifelse(MMCPSR_emp$`Statistics with an identification strategy` ==1 | MMCPSR_emp$EHPdata == 1| MMCPSR_emp$`Process tracing`== 1 | MMCPSR_emp$`Fixed effects` == 1, 1,0)

#OLS - high causal vs time
out.11 <- lm(MMCPSR_emp$CausalHigh ~I(MMCPSR_emp$Year)^2)
summary(out.12)
out.11$coefficients[2]

#probit and marginal effect
out.11.probit <- glm(MMCPSR_emp$CausalHigh ~ I(MMCPSR_emp$Year)^2, family = binomial(link= "probit"))
summary(out.11.probit)
probitmfx(out.11.probit, data = MMCPSR_emp)


#OLS - causal controlled vs time
out.11b <- lm(MMCPSR_emp$CausalControl ~ I(MMCPSR_emp$Year)^2)
summary(out.11b)
out.11b$coefficients[2]

#probit and marginal effect
out.11b.probit <- glm(MMCPSR_emp$CausalControl ~ I(MMCPSR_emp$Year)^2, family = binomial(link= "probit"))
summary(out.11b.probit)
probitmfx(out.11b.probit, data = MMCPSR_emp)


#OLS -  causal mixed vs time
out.11c <- lm(MMCPSR_emp$CausalMixed ~ I(MMCPSR_emp$Year)^2)
summary(out.11c)
out.11c$coefficients[2]

#probit and marginal effect
out.11c.probit <- glm(MMCPSR_emp$CausalMixed ~ I(MMCPSR_emp$Year)^2, family = binomial(link= "probit"))
summary(out.11c.probit)
probitmfx(out.11c.probit, data = MMCPSR_emp)


#DV - Formal modeling only
#OLS - Modeling only vs time
out.12 <- lm(MMCPSR_emp$ModelingOnly ~ MMCPSR_emp$Year)
summary(out.12)
out.15$coefficients[2]

#probit and marginal effects 
out.12.probit <- glm(MMCPSR_emp$ModelingOnly ~ MMCPSR_emp$Year, family = binomial(link = "probit"))
summary(out.12.probit)
probitmfx(out.12.probit, data = MMCPSR_emp)

#DV - Formal modeling (mixed methods)
#Create variable
MMCPSR_emp$ModelingMixed <- ifelse(MMCPSR_emp$`Formal modeling` + MMCPSR_emp$MixedMethods == 2, 1,0)

#OLS - Modeling (mixed methods) vs time
out.13 <- lm(MMCPSR_emp$ModelingMixed ~ MMCPSR_emp$Year)
summary(out.13)
out.13$coefficients[2]

#probit and marginal effects
out.13.probit <- glm(MMCPSR_emp$ModelingMixed ~ MMCPSR_emp$Year, family = binomial(link = "probit"))
summary(out.13.probit)
probitmfx(out.13.probit, data = MMCPSR_emp)

#DV - qual methods 
#create variables 
MMCPSR_emp$QualExplicit <- ifelse(MMCPSR_emp$`Process tracing`==1 | MMCPSR_emp$QCA == 1 | MMCPSR_emp$`Congruence analysis` ==1 | MMCPSR_emp$`Counterfactual analysis` == 1 | MMCPSR_emp$`Structured case comparison` == 1,1,0)

MMCPSR_emp$QualWeak <- ifelse(MMCPSR_emp$Other == 1 | MMCPSR_emp$`Illustrative case study` == 1| MMCPSR_emp$`No discernible method` == 1, 1, 0)

MMCPSR_emp_qual <- subset(MMCPSR_emp, MMCPSR_emp$QualMethod == 1)

#OLS - explicit qual methods vs time 
out.14 <- lm(MMCPSR_emp_qual$QualExplicit ~ MMCPSR_emp_qual$Year)
summary(out.14)
out.14$coefficients[2]

#probit and marginal effects
out.14.probit <- glm(MMCPSR_emp_qual$QualExplicit ~ MMCPSR_emp_qual$Year, family = binomial(link = "probit"))
summary(out.14.probit)
probitmfx(out.14.probit, data = MMCPSR_emp_qual)

#OLS - weak qual methods vs time 
out.15 <- lm(MMCPSR_emp_qual$QualWeak ~ MMCPSR_emp_qual$Year)
summary(out.15)
out.15$coefficients[2]

#probit and marginal effects
out.15.probit <- glm(MMCPSR_emp_qual$QualWeak ~ MMCPSR_emp_qual$Year, family = binomial(link = "probit"))
summary(out.15.probit)
probitmfx(out.15.probit, data = MMCPSR_emp_qual)


#DV - process tracing
#OLS - process tracing vs time
out.16 <- lm(MMCPSR_emp$`Process tracing` ~ MMCPSR_emp$Year)
summary(out.16)
out.16$coefficients[2]

out.16b <- lm(MMCPSR_emp$`Process tracing` ~ relevel(as.factor(MMCPSR_emp$Year),ref = "2004"))
summary(out.16b)
plot_coefs(out.16b)
plot_coefs(lm(MMCPSR_emp$`Process tracing`~ 0 + as.factor(MMCPSR_emp$Year)))


#probit and marginal effects
out.16.probit <- glm(MMCPSR_emp$`Process tracing` ~ MMCPSR_emp$Year, family = binomial(link = "probit"))
summary(out.16.probit)
probitmfx(out.16.probit, data = MMCPSR_emp)

out.16b.probit <- glm(MMCPSR_emp$`Process tracing` ~ relevel(as.factor(MMCPSR_emp$Year), ref = "2004"), family = binomial(link = "probit"))
summary(out.16b.probit)
out.mfx <- probitmfx(out.16b.probit, data = MMCPSR_emp)


#DV - qual/quant mixed methods
MMCPSR_emp$QuantQual <- ifelse(MMCPSR_emp$MixedMethods ==1 & MMCPSR_emp$QualMethod + MMCPSR_emp$QuantMethod == 2, 1, 0)

#OLS - qual/quant mixed vs time
#create variable
out.17 <- lm(MMCPSR_emp$QuantQual~MMCPSR_emp$Year) 
summary(out.17)
out.17$coefficients[2]

out.17b <- lm(MMCPSR_emp$QuantQual~relevel(as.factor(MMCPSR_emp$Year), ref = "2012"))
summary(out.17b)
plot_coefs(out.17b)
plot_coefs(lm(MMCPSR_emp$QuantQual~ 0 + as.factor(MMCPSR_emp$Year)))

#probit and marginal effects 
out.17.probit <- glm(MMCPSR_emp$QuantQual~MMCPSR_emp$Year, family = binomial(link = "probit"))
summary(out.17.probit)
probitmfx(out.17.probit, data = MMCPSR_emp)

out.17b.probit <- glm(MMCPSR_emp$QuantQual~relevel(as.factor(MMCPSR_emp$Year), ref = "2012"), family = binomial(link = "probit"))
summary(out.17b.probit)
probitmfx(out.17b.probit, data = MMCPSR_emp)

  
#DV - quant methods, women (single authored and co-authored)
#OLS - quant vs time
out.18 <- lm(MMCPSR_emp$QuantMethod~MMCPSR_emp$Year + MMCPSR_emp$FemaleAuthor + MMCPSR_emp$FemaleAuthor*MMCPSR_emp$Year)
summary(out.18)

#probit and marginal effects
out.18.probit <- glm(MMCPSR_emp$QuantMethod~MMCPSR_emp$Year + MMCPSR_emp$FemaleAuthor + MMCPSR_emp$FemaleAuthor*MMCPSR_emp$Year, family = binomial(link= "probit"))
summary(out.18.probit)
probitmfx(out.18.probit, data = MMCPSR_emp)


#OLS - quant only vs time 
out.18b <- lm(MMCPSR_emp$QuantOnly~MMCPSR_emp$Year + MMCPSR_emp$FemaleAuthor + MMCPSR_emp$FemaleAuthor*MMCPSR_emp$Year)
summary(out.18b)
out.18b$coefficients[2]

#probit and marginal effects
out.18b.probit <- glm(MMCPSR_emp$QuantOnly~MMCPSR_emp$Year + MMCPSR_emp$FemaleAuthor + MMCPSR_emp$FemaleAuthor*MMCPSR_emp$Year, family = binomial(link= "probit"))
summary(out.18b.probit)
probitmfx(out.18b.probit, data = MMCPSR_emp)


#subfield
#create subset
MMCPSR_emp_QQ <- subset(MMCPSR_emp, MMCPSR_emp$QuantQual == 1)


#OLS- Quant/qual tendency vs subfield
out.19 <- lm(MMCPSR_emp_QQ$`Overwhelmingly quant`~MMCPSR_emp_QQ$Subfield)
summary(out.19)
plot_coefs(out.19)
plot_coefs(lm(MMCPSR_emp_QQ$`Overwhelmingly quant`~ 0+ MMCPSR_emp_QQ$Subfield))

#probit and marginal effects
out.19.probit <- glm(MMCPSR_emp_QQ$`Overwhelmingly quant`~MMCPSR_emp_QQ$Subfield, family = binomial(link = "probit"), subset = MMCPSR_emp$QuantOnly == 1| MMCPSR_emp$QualOnly == 1)
summary(out.19.probit)
probitmfx(out.19.probit, data = MMCPSR_emp_QQ)



#DV - Journal
#create subset - quant X journal
Quant_Journal_sub <-subset(MMCPSR_emp, select = c("QuantMethod","QuantOnly", "APSR", "AJPS","JOP", "BJPS", "CP", "CPS","IO", "ISQ", "PoP", "WP", "Big3"))

#create correlation matrix
QuantJournals_cor <- rcorr(as.matrix(Quant_Journal_sub))

#pull out correlations
QuantJournalsCorrelations <- as.data.frame(QuantJournals_cor[["r"]])
QuantJournalsCorrelations <- round(QuantJournalsCorrelations, 4)

#pull out significance
QuantJournalsCorrelations_sig <- as.data.frame(QuantJournals_cor[["P"]])
QuantJournalsCorrelations_sig <- round(QuantJournalsCorrelations_sig, 3)


#create subset - causal methods X journal
Causal_Journal_sub <-subset(MMCPSR_emp, select = c("CausalHigh","CausalControl","CausalMixed", "APSR", "AJPS","JOP", "BJPS", "CP", "CPS","IO", "ISQ", "PoP", "WP", "Big3"))

#create correlation matrix
CausalJournals_cor <- rcorr(as.matrix(Causal_Journal_sub ))

#pull out correlations
CausalJournalsCorrelations <- as.data.frame(CausalJournals_cor[["r"]])
CausalJournalsCorrelations <- round(CausalJournalsCorrelations, 4)

#pull out significance
CausalJournalsCorrelations_sig <- as.data.frame(CausalJournals_cor[["P"]])
CausalJournalsCorrelations_sig <- round(CausalJournalsCorrelations_sig, 3)



#Data Collection
#create variable
MMCPSR_emp$GenDataOnly <- ifelse(MMCPSR_emp$GenData==1 & MMCPSR_emp$`Employed data/information from pre-existing primary or secondary sources`==0, 1, 0)

#create subset - causal methods X method
Data_Methods_sub <-subset(MMCPSR_emp, select = c("GenData","PreExistingOnly","QualMethod", "QualOnly", "QuantMethod", "QuantOnly"))

#create correlation matrix
DataMethods_cor <- rcorr(as.matrix(Data_Methods_sub))

#pull out correlations
DataMethodsCorrelations <- as.data.frame(DataMethods_cor[["r"]])
DataMethodsCorrelations <- round(DataMethodsCorrelations, 4)


#pull out significance
DataMethodsCorrelations_sig <- as.data.frame(DataMethods_cor[["P"]])
DataMethodsCorrelations_sig <- round(DataMethodsCorrelations_sig, 3)

DataMethodsCor <- cbind(DataMethodsCorrelations[1:2], DataMethodsCorrelations_sig[1:2])
DataMethodsCor <- DataMethodsCor[3:6,]
DataMethodsCor  <- DataMethodsCor[c(1,3,2,4)]
colnames(DataMethodsCor) <- c("Generate Data (r)", "Generate Data (p-value)","Pre-Existing Only (r)", "Pre-Existing Only (p-value)")
DataMethodsCor


####Conclusion###
#Policy Recommendations
#count and percent of all articles
sum(MMCPSR_Data$`Policy Recommendation`)
formattable::percent(sum(MMCPSR_Data$`Policy Recommendation`)/nrow(MMCPSR_Data), digits = 1)

#IV - Subfield
out.20 <- lm(MMCPSR_emp$`Policy Recommendation`~MMCPSR_emp$Subfield)
summary(out.20)
out.20$coefficients[2]

#probit and marginal effects 
out.20.probit <-glm(MMCPSR_emp$`Policy Recommendation`~MMCPSR_emp$Subfield, family = binomial(link = "probit"))
summary(out.20.probit)
probitmfx(out.20.probit, data = MMCPSR_emp)


#IV - Time (empirical articles only)
#create data set
PolicyTime <- as.data.frame(xtabs(~Year + `Policy Recommendation`, data = MMCPSR_emp))
PolicyTime <- subset(PolicyTime, PolicyTime$Policy.Recommendation == 1)
PolicyTime <- PolicyTime[-2]
PolicyTime$Percent <- formattable::percent(PolicyTime$Freq/ArticlesbyYear_emp$n, digits = 1)

#line graph
PolicyTime %>%
  ggplot( aes(x=Year, y=Percent, group = 1)) +
  geom_line(color = "orange")+scale_color_brewer(palette="Set2") + theme_minimal(base_size = 12) + 
  scale_y_continuous(labels = scales::percent, limits = c(0,1)) + labs(y = "Percent of Articles", vjust = -1)

#DV - time
#OLS - policy recommendations vs time 
out.21 <- lm(MMCPSR_emp$`Policy Recommendation`~MMCPSR_emp$Year)
summary(out.21)
out.21$coefficients[2]

out.21b <- lm(MMCPSR_emp$`Policy Recommendation`~as.factor(MMCPSR_emp$Year))
summary(out.21b)
plot_coefs(out.21b)
plot_coefs(lm(MMCPSR_emp$`Policy Recommendation`~ 0 + as.factor(MMCPSR_emp$Year)))


#probit and marginal effects 
out.21.probit <-glm(MMCPSR_emp$`Policy Recommendation`~MMCPSR_emp$Year)
summary(out.21.probit)
probitmfx(out.21.probit, data = MMCPSR_emp)

out.21b.probit <- lm(MMCPSR_emp$`Policy Recommendation`~as.factor(MMCPSR_emp$Year))
summary(out.21b.probit)
probitmfx(out.21b.probit, data = MMCPSR_emp)

#IV - Journal
#create subset - policy receommendations X journal
Policy_Journal_sub <-subset(MMCPSR_emp, select = c("Policy Recommendation", "APSR", "AJPS","JOP", "BJPS", "CP", "CPS","IO", "ISQ", "PoP", "WP", "Big3"))

#create correlation matrix
PolicyJournals_cor <- rcorr(as.matrix(Policy_Journal_sub))

#pull out correlations
PolicyJournalsCorrelations <- as.data.frame(PolicyJournals_cor[["r"]])
PolicyJournalsCorrelations <- round(PolicyJournalsCorrelations, 4)

#pull out significance
PolicyJournalsCorrelations_sig <- as.data.frame(PolicyJournals_cor[["P"]])
PolicyJournalsCorrelations_sig <- round(PolicyJournalsCorrelations_sig, 3)


save(MMCPSR_emp, file = "MMCPSR Analysis Data No PT.RData")

MethodTime %>%
  ggplot(aes(x=Year, y=Percent, group = Method, color = Method))+
  geom_line()+scale_color_brewer(palette="Set2") +  theme(legend.position = "bottom") + theme_minimal(base_size = 12) + labs(y = "Percent of Articles", vjust = -1) + 
  scale_y_continuous(labels = scales::percent, limits = c(0,1)) + theme(legend.position = "bottom")
