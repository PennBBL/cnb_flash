# new script for flash no flash project
# awaiting new data from Mrugank
#
# 09.10.21 Akira Di Sandro


# Load Packages ----
library(ggplot2)
library(psych)
library(dplyr)
library(binr)
library(arules)
library(stringr)
library(visreg)
library(lubridate)


# Load and organize data ----
bigcnb <- read.csv("bigcnb_13Sep2021.csv", na=c("",".","NA",NA))

bigcnb$Dotest <- as.Date(bigcnb$Dotest, "%m/%d/%y")
bigcnb$Dob <- as.Date(bigcnb$Dob, "%m/%d/%y")   # anything with Dob > 2013 should be 100 years earlier
bigcnb <- bigcnb[order(bigcnb$Dob, decreasing = T),]

temp <- bigcnb[bigcnb$Dob > as.Date("01/01/13", "%m/%d/%y") & !is.na(bigcnb$Dob),]$Dob
temp <- temp %m-% years(100) 
bigcnb[bigcnb$Dob > as.Date("01/01/13", "%m/%d/%y") & !is.na(bigcnb$Dob),]$Dob <- temp

bigcnb$age <- floor(as.numeric(bigcnb$Dotest - bigcnb$Dob, units = "weeks")/52.25)

bigcnb$flash <- 0
bigcnb$flash[which(bigcnb$Dotest <= as.Date("2020-12-31"))] <- 1

bigcnb <- bigcnb[order(bigcnb$Datasetid),]


# * Separate into test versions ----
tests <- sort(unique(bigcnb$Version))

ADT36_A <- bigcnb[bigcnb$Version == "ADT36_A" & !is.na(bigcnb$Version),]
ADT60_A <- bigcnb[bigcnb$Version == "ADT60_A" & !is.na(bigcnb$Version),]

CPF_A <- bigcnb[bigcnb$Version == "CPF_A" & !is.na(bigcnb$Version),]
CPF_B <- bigcnb[bigcnb$Version == "CPF_B" & !is.na(bigcnb$Version),]
CPFD_A <- bigcnb[bigcnb$Version == "CPFD_A" & !is.na(bigcnb$Version),]
CPFD_B <- bigcnb[bigcnb$Version == "CPFD_B" & !is.na(bigcnb$Version),]

ER40_A <- bigcnb[bigcnb$Version == "ER40_A" & !is.na(bigcnb$Version),]
ER40_C <- bigcnb[bigcnb$Version == "ER40_C" & !is.na(bigcnb$Version),]
ER40_D <- bigcnb[bigcnb$Version == "ER40_D" & !is.na(bigcnb$Version),]

GNG150 <- bigcnb[bigcnb$Version == "GNG150" & !is.na(bigcnb$Version),]

KCPW_A <- bigcnb[bigcnb$Version == "KCPW_A" & !is.na(bigcnb$Version),]
KCPWD_A <- bigcnb[bigcnb$Version == "KCPWD_A" & !is.na(bigcnb$Version),]

KSPVRT_A <- bigcnb[bigcnb$Version == "KSPVRT_A" & !is.na(bigcnb$Version),]
KSPVRT_B <- bigcnb[bigcnb$Version == "KSPVRT_B" & !is.na(bigcnb$Version),]
KSPVRT_D <- bigcnb[bigcnb$Version == "KSPVRT_D" & !is.na(bigcnb$Version),]

MEDF36_A <- bigcnb[bigcnb$Version == "MEDF36_A" & !is.na(bigcnb$Version),]
MEDF60_A <- bigcnb[bigcnb$Version == "MEDF60_A" & !is.na(bigcnb$Version),]

PCET_A <- bigcnb[bigcnb$Version == "PCET_A" & !is.na(bigcnb$Version),]
SPCET_A <- bigcnb[bigcnb$Version == "SPCET_A" & !is.na(bigcnb$Version),]

PMAT18_B <- bigcnb[bigcnb$Version == "PMAT18_B" & !is.na(bigcnb$Version),]
PMAT24_A <- bigcnb[bigcnb$Version == "PMAT24_A" & !is.na(bigcnb$Version),]
PMAT24_B <- bigcnb[bigcnb$Version == "PMAT24_B" & !is.na(bigcnb$Version),]

SLNB2_90 <- bigcnb[bigcnb$Version == "SLNB2_90" & !is.na(bigcnb$Version),]

SPLOT12 <- bigcnb[bigcnb$Version == "SPLOT12" & !is.na(bigcnb$Version),]
VSPLOT15 <- bigcnb[bigcnb$Version == "VSPLOT15" & !is.na(bigcnb$Version),]
VSPLOT24 <- bigcnb[bigcnb$Version == "VSPLOT24" & !is.na(bigcnb$Version),]

SPVRT_A <- bigcnb[bigcnb$Version == "SPVRT_A" & !is.na(bigcnb$Version),]

SVOLT_A <- bigcnb[bigcnb$Version == "SVOLT_A" & !is.na(bigcnb$Version),]
SVOLTD_A <- bigcnb[bigcnb$Version == "SVOLTD_A" & !is.na(bigcnb$Version),]



# Models and Plotting ----















# temp code to give Mrugank Bblid's of PC/RTCR switched + missing accuracy ----
# ADT36_A
allPC <- ADT36_A[!is.na(ADT36_A$Percent.correct),]
allPC$wrongPC <- 1

allRTCR <- ADT36_A[!is.na(ADT36_A$Speed),]
allRTCR$wrongRTCR <- 1


temp <- merge(ADT36_A,allPC,by.x=1:18,by.y=1:18,all=T)
temp <- merge(temp,allRTCR,by.x=1:18,by.y=1:18,all=T)
temp[is.na(temp$wrongPC),]$wrongPC <- 0
temp[is.na(temp$wrongRTCR),]$wrongRTCR <- 0
temp$missingall <- 0
temp[temp$wrongPC==0,]$missingall <- 1
write.csv(temp, "ADT36A_tofix.csv",row.names = F)

#ADT60_A
allPC <- ADT60_A[!is.na(ADT60_A$Percent.correct),]
allPC$wrongPC <- 1

allRTCR <- ADT60_A[!is.na(ADT60_A$Speed),]
allRTCR$wrongRTCR <- 1


temp <- merge(ADT60_A,allPC,by.x=1:18,by.y=1:18,all=T)
temp <- merge(temp,allRTCR,by.x=1:18,by.y=1:18,all=T)
temp[is.na(temp$wrongPC),]$wrongPC <- 0
temp[is.na(temp$wrongRTCR),]$wrongRTCR <- 0
temp$missingall <- 0
temp[temp$wrongPC==0,]$missingall <- 1
write.csv(temp, "ADT60A_tofix.csv",row.names = F)

# MEDF36_A
temp <- merge(MEDF36_A,MEDF60_A,by=1:18,all=T)
bbl <- sort(unique(temp$Bblid))

write.csv(bbl, "MEDF36and60_tofix.csv",row.names = F)







