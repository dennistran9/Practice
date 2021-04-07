library(data.table)
Inclusion1<- fread("Inclusion.csv")

#Remove unnecessary columns: 1, 2, 3, 4, 5, 7, 9, 10, 11, 18, 20, 22, 30, 

INCLUSION<- subset(Inclusion1, select = -c(V1, V2, V3, V4, V5, V7, V9, V10, V11, V18, V20, V22, V30))
INCLUSION<- INCLUSION[-c(1,2),]

#Rename variables:
library(dplyr)
colnames(INCLUSION)
setnames(INCLUSION, old = c('V6','V8','V12','V13','V14','V15','V16','V17','V19','V21','V23','V24','V25','V26','V27','V28','V29','V31','V32','V33','V34','V35','V36','V37','V38','V39','V40','V41','V42','V43','V44','V45','V46','V47','V48','V49','V50','V51','V52','V53','V54','V55','V56','V57','V58','V59','V60','V61','V62','V63','V64'),
         new = c('Finished','RespID','Age','Race','RaceOther','RaceMulitple','Ethnicity','Gender','SexOrient','HighestEd','EmployStatus','JobCat','JobOther','JobType','JobTypeOther','LastEmployed','WorkExp','TeamSize','DEIPolicies','DEIinfo','FIC1','FIC2','FIC3','FIC4','FIC5','FIC6','U1','U2','U3','U4','U5','U6','FOB1','FOB2','FOB3','FOB4','FOB5','FOB6','DM1','DM2','DM3','DM4','DM5','DM6','FT1','FT2','FT3','FT4','FT5','FT6','ID'))
colnames(INCLUSION)

#Subset the item responses
AllResponses<- subset(INCLUSION, select = c(FIC1:FT6))

#Remove text
AllResponses$FIC1 <- substr(AllResponses$FIC1, 0, 1)
AllResponses$FIC2 <- substr(AllResponses$FIC2, 0, 1)
AllResponses$FIC3 <- substr(AllResponses$FIC3, 0, 1)
AllResponses$FIC4 <- substr(AllResponses$FIC4, 0, 1)
AllResponses$FIC5 <- substr(AllResponses$FIC5, 0, 1)
AllResponses$FIC6 <- substr(AllResponses$FIC6, 0, 1)
AllResponses$U1 <- substr(AllResponses$U1, 0, 1)
AllResponses$U2 <- substr(AllResponses$U2, 0, 1)
AllResponses$U3 <- substr(AllResponses$U3, 0, 1)
AllResponses$U4 <- substr(AllResponses$U4, 0, 1)
AllResponses$U5 <- substr(AllResponses$U5, 0, 1)
AllResponses$U6 <- substr(AllResponses$U6, 0, 1)
AllResponses$FOB1 <- substr(AllResponses$FOB1, 0, 1)
AllResponses$FOB2 <- substr(AllResponses$FOB2, 0, 1)
AllResponses$FOB3 <- substr(AllResponses$FOB3, 0, 1)
AllResponses$FOB4 <- substr(AllResponses$FOB4, 0, 1)
AllResponses$FOB5 <- substr(AllResponses$FOB5, 0, 1)
AllResponses$FOB6 <- substr(AllResponses$FOB6, 0, 1)
AllResponses$DM1 <- substr(AllResponses$DM1, 0, 1)
AllResponses$DM2 <- substr(AllResponses$DM2, 0, 1)
AllResponses$DM3 <- substr(AllResponses$DM3, 0, 1)
AllResponses$DM4 <- substr(AllResponses$DM4, 0, 1)
AllResponses$DM5 <- substr(AllResponses$DM5, 0, 1)
AllResponses$DM6 <- substr(AllResponses$DM6, 0, 1)
AllResponses$FT1 <- substr(AllResponses$FT1, 0, 1)
AllResponses$FT2 <- substr(AllResponses$FT2, 0, 1)
AllResponses$FT3 <- substr(AllResponses$FT3, 0, 1)
AllResponses$FT4 <- substr(AllResponses$FT4, 0, 1)
AllResponses$FT5 <- substr(AllResponses$FT5, 0, 1)
AllResponses$FT6 <- substr(AllResponses$FT6, 0, 1)

#Make Numeric:
AllResponses$FIC1 <- as.numeric(AllResponses$FIC1)
AllResponses$FIC2 <- as.numeric(AllResponses$FIC2)
AllResponses$FIC3 <- as.numeric(AllResponses$FIC3)
AllResponses$FIC4 <- as.numeric(AllResponses$FIC4)
AllResponses$FIC5 <- as.numeric(AllResponses$FIC5)
AllResponses$FIC6 <- as.numeric(AllResponses$FIC6)
AllResponses$U1 <- as.numeric(AllResponses$U1)
AllResponses$U2 <- as.numeric(AllResponses$U2)
AllResponses$U3 <- as.numeric(AllResponses$U3)
AllResponses$U4 <- as.numeric(AllResponses$U4)
AllResponses$U5 <- as.numeric(AllResponses$U5)
AllResponses$U6 <- as.numeric(AllResponses$U6)
AllResponses$FOB1 <- as.numeric(AllResponses$FOB1)
AllResponses$FOB2 <- as.numeric(AllResponses$FOB2)
AllResponses$FOB3 <- as.numeric(AllResponses$FOB3)
AllResponses$FOB4 <- as.numeric(AllResponses$FOB4)
AllResponses$FOB5 <- as.numeric(AllResponses$FOB5)
AllResponses$FOB6 <- as.numeric(AllResponses$FOB6)
AllResponses$DM1 <- as.numeric(AllResponses$DM1)
AllResponses$DM2 <- as.numeric(AllResponses$DM2)
AllResponses$DM3 <- as.numeric(AllResponses$DM3)
AllResponses$DM4 <- as.numeric(AllResponses$DM4)
AllResponses$DM5 <- as.numeric(AllResponses$DM5)
AllResponses$DM6 <- as.numeric(AllResponses$DM6)
AllResponses$FT1 <- as.numeric(AllResponses$FT1)
AllResponses$FT2 <- as.numeric(AllResponses$FT2)
AllResponses$FT3 <- as.numeric(AllResponses$FT3)
AllResponses$FT4 <- as.numeric(AllResponses$FT4)
AllResponses$FT5 <- as.numeric(AllResponses$FT5)
AllResponses$FT6 <- as.numeric(AllResponses$FT6)

#Descriptives:
summary(AllResponses)
  #22 cases with all data missing
CompleteData<- na.omit(AllResponses)

#reverse code:
library(car)
AllResponses$FIC6 <- recode(AllResponses$FIC6, "1=5; 2=4; 3=3; 4=2; 5=1")
AllResponses$U4 <- recode(AllResponses$U4, "1=5; 2=4; 3=3; 4=2; 5=1")
AllResponses$U6 <- recode(AllResponses$U6, "1=5; 2=4; 3=3; 4=2; 5=1")
AllResponses$FOB2 <- recode(AllResponses$FOB2, "1=5; 2=4; 3=3; 4=2; 5=1")
AllResponses$FOB4 <- recode(AllResponses$FOB4, "1=5; 2=4; 3=3; 4=2; 5=1")
AllResponses$DM5 <- recode(AllResponses$DM5, "1=5; 2=4; 3=3; 4=2; 5=1")
AllResponses$FT3 <- recode(AllResponses$FT3, "1=5; 2=4; 3=3; 4=2; 5=1")
AllResponses$FT6 <- recode(AllResponses$FT6, "1=5; 2=4; 3=3; 4=2; 5=1")

#alpha reliability
library(psych)
alpha(AllResponses)
  #Cronbach's alpha = 0.94
  #Inter-item correlation = 0.36
  #mean = 3.8
  #SD = 0.55


#Factor analysis
fa.parallel(AllResponses, fm="pa", fa="fa", n.iter = 500)

FA1<- fa(AllResponses, fm="pa", nfactors=3, rotate="promax")
print.psych(FA1, sort=TRUE)

FA2<- fa(AllResponses, fm="pa", nfactors=5, rotate="promax")
print.psych(FA2, sort=TRUE)



#should probably look to see if other variables influenced the factor loadings?
INCLUSION$TeamSize<- as.factor(INCLUSION$TeamSize)




