

install.packages("ggplot2")
install.packages("plyr")
install.packages("dplyr")
install.packages("tidyr")
install.packages("sjPlot")
install.packages("formattable")
install.packages ("RColorBrewer")
install.packages("hrbrthemes")
install.packages("viridis")
install.packages("corrplot")
install.packages("Hmisc")
install.packages("janitor")
install.packages("mfx")
install.packages("jtools")
install.packages("ggstance")
install.packages("broom.mixed")
install.packages("nnet")
install.packages("margins")
install.packages("lemon")
install.packages("stargazer")

#load in packages
library("ggplot2")
library("plyr")
library("dplyr")
library("tidyr")
library("sjPlot")
library("formattable")
library("RColorBrewer")
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
library("lemon")
library("stargazer")


load(file ="/Users/Tranae/Documents/Methods Project/Analysis/MMCPSR Analysis Data No PT.RData")


#quant/qual mixed methods over time

QuantQualTime <- as.data.frame(xtabs(~Year+QuantQual, data = MMCPSR_emp))
QuantQualTime <- subset(QuantQualTime, QuantQualTime$QuantQual ==1)
QuantQualTime <- QuantQualTime[-2]
QuantQualTime$Percent <- formattable::percent(QuantQualTime$Freq/ArticlesbyYear_emp$n, digits = 1)

QuantQualTime %>%
  ggplot( aes(x=Year, y=Percent, group = 1)) +
  geom_line(color = "orange")+scale_color_brewer(palette="Set2") + theme_minimal(base_size = 11) + 
  scale_y_continuous(labels = scales::percent, limits = c(0,1)) + labs(y = "Percent of Articles", vjust = -1)

#formal modeling only over time

FMTime <- as.data.frame(xtabs(~Year+ModelingOnly, data = MMCPSR_emp))
FMTime <- subset(FMTime, FMTime$ModelingOnly ==1)
FMTime <- FMTime[-2]
FMTime$Percent <- formattable::percent(FMTime$Freq/ArticlesbyYear_emp$n, digits = 1)

FMTime %>%
  ggplot( aes(x=Year, y=Percent, group = 1)) +
  geom_line(color = "orange")+scale_color_brewer(palette="Set2") + theme_minimal(base_size = 11) + 
  scale_y_continuous(labels = scales::percent, limits = c(0,1)) + labs(y = "Percent of Articles", vjust = -1)


#article by journal count (emp)
ArticlesbyJournal_emp <- MMCPSR_emp %>% count(Journal)
ArticlesbyJournal_emp


#method type over time
MMCPSR_Data$QualOnly <- ifelse(MMCPSR_Data$QualMethod == 1 & MMCPSR_Data$MixedMethods == 0, 1, 0)
MMCPSR_Data$QuantOnly <- ifelse(MMCPSR_Data$QuantMethod == 1 & MMCPSR_Data$MixedMethods == 0, 1, 0)

MMCPSR_Data$MethodType <- ifelse(MMCPSR_Data$MixedMethods == 1, "Mixed Methods", ifelse(MMCPSR_Data$QuantOnly == 1, "Quant Only", ifelse(MMCPSR_Data$QualOnly == 1, "Qual Only", ifelse(MMCPSR_Data$ModelingOnly == 1, "Modeling Only", ifelse(MMCPSR_Data$InterpOnly == 1, "Interpretive Only", ifelse(MMCPSR_Data$`No discernible method` == 1, "No discernible method", ""))))))

MethodTime <- MMCPSR_Data %>% count(MethodType, Year)
MethodTime <- left_join(MethodTime, ArticlesbyYear, by = "Year")
MethodTime$Percent <- formattable::percent(MethodTime$n.x/MethodTime$n.y, digits = 1)
MethodTime <- MethodTime[-4]      
colnames(MethodTime) <- c("Method","Year", "Frequency", "Percent")

MethodTime %>%
  ggplot(aes(x=Year, y=Percent, group = Method, color = Method))+
  geom_line()+scale_color_brewer(palette="Set2") +  theme(legend.position = "bottom") + theme_minimal(base_size = 12) + labs(y = "Percent of Articles", vjust = -1) + 
  scale_y_continuous(labels = scales::percent, limits = c(0,1)) + theme(legend.position = "bottom")

MMCPSR_emp$MethodType <- ifelse(MMCPSR_emp$MixedMethods == 1, "Mixed Methods", ifelse(MMCPSR_emp$QuantOnly == 1, "Quant Only", ifelse(MMCPSR_emp$QualOnly == 1, "Qual Only", ifelse(MMCPSR_emp$ModelingOnly == 1, "Modeling Only", ifelse(MMCPSR_emp$InterpOnly == 1, "Interpretive Only", ifelse(MMCPSR_emp$`No discernible method` == 1, "No discernible method", ""))))))


#methods over time (empirical only)
MethodTime_emp <- MMCPSR_emp %>% count(MethodType, Year)

#quant/qual mixed methods over time

QuantQualTime <- as.data.frame(xtabs(~Year+QuantQual, data = MMCPSR_emp))
QuantQualTime <- subset(QuantQualTime, QuantQualTime$QuantQual ==1)
QuantQualTime <- QuantQualTime[-2]
QuantQualTime$Percent <- formattable::percent(QuantQualTime$Freq/ArticlesbyYear_emp$n, digits = 1)

QuantQualTime %>%
  ggplot( aes(x=Year, y=Percent, group = 1)) +
  geom_line(color = "orange")+scale_color_brewer(palette="Set2") + theme_minimal(base_size = 11) + 
  scale_y_continuous(labels = scales::percent, limits = c(0,1)) + labs(y = "Percent of Articles", vjust = -1)


#formal modeling only over time

FMTime <- as.data.frame(xtabs(~Year+ModelingOnly, data = MMCPSR_emp))
FMTime <- subset(FMTime, FMTime$ModelingOnly ==1)
FMTime <- FMTime[-2]
FMTime$Percent <- formattable::percent(FMTime$Freq/ArticlesbyYear_emp$n, digits = 1)

FMTime %>%
  ggplot( aes(x=Year, y=Percent, group = 1)) +
  geom_line(color = "orange")+scale_color_brewer(palette="Set2") + theme_minimal(base_size = 11) + 
  scale_y_continuous(labels = scales::percent, limits = c(0,1)) + labs(y = "Percent of Articles", vjust = -1)


#article by journal count (emp)
ArticlesbyJournal_emp <- MMCPSR_emp %>% count(Journal)
ArticlesbyJournal_emp


#method type over time
MMCPSR_Data$QualOnly <- ifelse(MMCPSR_Data$QualMethod == 1 & MMCPSR_Data$MixedMethods == 0, 1, 0)
MMCPSR_Data$QuantOnly <- ifelse(MMCPSR_Data$QuantMethod == 1 & MMCPSR_Data$MixedMethods == 0, 1, 0)

MMCPSR_Data$MethodType <- ifelse(MMCPSR_Data$MixedMethods == 1, "Mixed Methods", ifelse(MMCPSR_Data$QuantOnly == 1, "Quant Only", ifelse(MMCPSR_Data$QualOnly == 1, "Qual Only", ifelse(MMCPSR_Data$ModelingOnly == 1, "Modeling Only", ifelse(MMCPSR_Data$InterpOnly == 1, "Interpretive Only", ifelse(MMCPSR_Data$`No discernible method` == 1, "No discernible method", ""))))))

MethodTime <- MMCPSR_Data %>% count(MethodType, Year)
MethodTime <- left_join(MethodTime, ArticlesbyYear, by = "Year")
MethodTime$Percent <- formattable::percent(MethodTime$n.x/MethodTime$n.y, digits = 1)
MethodTime <- MethodTime[-4]      
colnames(MethodTime) <- c("Method","Year", "Frequency", "Percent")

MethodTime %>%
  ggplot(aes(x=Year, y=Percent, group = Method, color = Method))+
  geom_line()+scale_color_brewer(palette="Set2") +  theme(legend.position = "bottom") + theme_minimal(base_size = 12) + labs(y = "Percent of Articles", vjust = -1) + 
  scale_y_continuous(labels = scales::percent, limits = c(0,1)) + theme(legend.position = "bottom")

MMCPSR_emp$MethodType <- ifelse(MMCPSR_emp$MixedMethods == 1, "Mixed Methods", ifelse(MMCPSR_emp$QuantOnly == 1, "Quant Only", ifelse(MMCPSR_emp$QualOnly == 1, "Qual Only", ifelse(MMCPSR_emp$ModelingOnly == 1, "Modeling Only", ifelse(MMCPSR_emp$InterpOnly == 1, "Interpretive Only", ifelse(MMCPSR_emp$`No discernible method` == 1, "No discernible method", ""))))))

#methods over time (empirical only)
MethodTime_emp <- MMCPSR_emp %>% count(MethodType, Year)
MethodTime_emp <- left_join(MethodTime_emp, ArticlesbyYear_emp, by = "Year")
MethodTime_emp$Percent <- formattable::percent(MethodTime_emp$n.x/MethodTime_emp$n.y, digits = 1)
MethodTime_emp <- MethodTime_emp[-4]      
colnames(MethodTime_emp) <- c("Method","Year", "Frequency", "Percent")

MethodTime_emp %>%
  ggplot(aes(x=Year, y=Percent, group = Method, color = Method))+
  geom_line()+scale_color_brewer(palette="Set2") +  theme(legend.position = "bottom") + theme_minimal(base_size = 12) + labs(y = "Percent of Articles", vjust = -1) + 
  scale_y_continuous(labels = scales::percent, limits = c(0,1)) + theme(legend.position = "bottom")


#Method by journals
#Cross-tab – different methods in different journals

#count
MethodTypeJourn_count <- MMCPSR_emp %>% count(Journal,MethodType)
MethodTypeJourn_count <- MethodTypeJourn_count %>% pivot_wider(names_from = Journal, values_from = n, values_fill = 0)


#Data analysis (quant method) vs gender

#OLS - Gender vs quant, alone or in tandem with other methods
output.1 <- lm(MMCPSR_emp$QuantMethod ~ relevel(as.factor(MMCPSR_emp$Gender), ref = "Single Authored Female"))

output.1b <- lm(MMCPSR_emp$QuantMethod ~ MMCPSR_emp$FemaleAuthor)

#probit and marginal effects
output.1.probit <- glm(MMCPSR_emp$QuantMethod ~ relevel(as.factor(MMCPSR_emp$Gender), ref = "Single Authored Female"), family=binomial(link= "probit"))

output.1b.probit <- glm(MMCPSR_emp$QuantMethod ~ MMCPSR_emp$FemaleAuthor, family=binomial(link= "probit"))

probitmfx(output.1.probit, data = MMCPSR_emp)
probitmfx(output.1b.probit, data = MMCPSR_emp)

stargazer(output.1, output.1b, output.1.probit, output.1b.probit, type = "text", 
          covariate.labels = c("Co-authored Female", "Co-authored Male", "Mixed Gender Team", "Single Authored Male"),dep.var.labels.include = TRUE)


#Data analysis (quant only) vs gender

#OLS - Gender vs quant only
output.2 <- lm(MMCPSR_emp$QuantOnly ~ relevel(as.factor(MMCPSR_emp$Gender), ref = "Single Authored Female"))

output.2b <- lm(MMCPSR_emp$QuantOnly ~ MMCPSR_emp$FemaleAuthor)

#probit and marginal effects
output.2.probit <- glm(MMCPSR_emp$QuantOnly ~ relevel(as.factor(MMCPSR_emp$Gender), ref = "Single Authored Female"), family=binomial(link= "probit"))

output.2b.probit <- glm(MMCPSR_emp$QuantOnly ~ MMCPSR_emp$FemaleAuthor, family=binomial(link= "probit"))

probitmfx(output.2.probit, data = MMCPSR_emp)
probitmfx(output.2b.probit, data = MMCPSR_emp)

stargazer(output.2, output.2b, output.2.probit, output.2b.probit, type = "text", 
          covariate.labels = c("Co-authored Female", "Co-authored Male", "Mixed Gender Team", "Single Authored Male"),dep.var.labels.include = TRUE)


#mode of inquiry updated, includes pre-existing plus experimental and pre-existing plus observational

#Create dataframe
GenerateData <- data.frame(matrix(NA, nrow = 3, ncol = 3))
colnames(GenerateData) <- c("Mode of Inquiry", "Frequency", "Percent")
GenerateData$`Mode of Inquiry` <- c("Observational", "Experimental", "Both")

#count of articles that generated data using observational techniques (includes articles that use both, percentage calculated using total empirical articles)
GenerateData[1,2] <- sum(MMCPSR_emp$OHPdata == 1 | MMCPSR_emp$gendataNHP == 1)
GenerateData[1,3] <- sum(MMCPSR_emp$OHPdata == 1 | MMCPSR_emp$gendataNHP == 1)/nrow(MMCPSR_emp)

#count of articles that generated data using experimental techniques (includes articles that use both, percentage calculated using total empirical articles)
GenerateData[2,2] <- sum(MMCPSR_emp$EHPdata)
GenerateData[2,3] <- sum(MMCPSR_emp$EHPdata)/nrow(MMCPSR_emp)

#Generated data using both observational and experimental techniques
#create variable
MMCPSR_emp$ObsData <- ifelse(MMCPSR_emp$OHPdata == 1 | MMCPSR_emp$gendataNHP == 1, 1, 0)
MMCPSR_emp$ObsExp <- ifelse(MMCPSR_emp$ObsData == 1 & MMCPSR_emp$EHPdata == 1, 1, 0)
GenerateData[3,2] <- sum(MMCPSR_emp$ObsExp)
GenerateData[3,3] <- sum(MMCPSR_emp$ObsExp)/nrow(MMCPSR_emp)

#format table
GenerateData$Percent <- formattable::percent(GenerateData$Percent, digits = 1)
GenerateData


#data collection X method correlation matrix extended 

#create subset - causal methods X method
Data_Methods_sub_ext <-subset(MMCPSR_emp, select = c("GenDataOnly","PreExistingOnly", "Mostly pre-existing data", "Mostly author-generated data", "Fairly balanced","QualMethod", "QualOnly", "QuantMethod", "QuantOnly"))

#create correlation matrix
DataMethods_cor_ext <- rcorr(as.matrix(Data_Methods_sub_ext))

#pull out correlations
DataMethodsCorrelations_ext <- as.data.frame(DataMethods_cor_ext[["r"]])
DataMethodsCorrelations_ext <- round(DataMethodsCorrelations_ext, 4)


#pull out significance
DataMethodsCorrelations_sig_ext <- as.data.frame(DataMethods_cor_ext[["P"]])
DataMethodsCorrelations_sig_ext <- round(DataMethodsCorrelations_sig_ext, 3)

DataMethodsCor_ext <- cbind(DataMethodsCorrelations_ext[6:9],DataMethodsCorrelations_sig_ext[6:9])
DataMethodsCor_ext <- DataMethodsCor_ext[1:5,]
DataMethodsCor_ext  <- DataMethodsCor_ext[c(1,5,2,6,3,7,4,8)]
colnames(DataMethodsCor_ext) <- c("Qual Method (r)", "Qual Method (p-value)","Qual Only (r)", "Qual Only (p-value)", "Quant Method (r)", "Quant Method (p-value)","Quant Only (p-value)","Quant Only (r)" )
DataMethodsCor_ext


