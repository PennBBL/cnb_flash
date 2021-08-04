# Code based on kosha_cnb.R that generalizes the script for all forms in the 
# big_cnb_dump.csv


# 08.04.21 Akira Di Sandro


# load packages ----
library(ggplot2)
library(psych)
library(dplyr)
library(binr)
library(arules)
library(stringr)
library(visreg)


# load data ----
bigcnb <- read.csv("cnb_dump_15july2021.csv", na=c("",".","NA"))

bigcnb$flash <- 0
bigcnb$flash[which(bigcnb$test_sessions_v.dotest < as.Date("2021-01-01"))] <- 1

# separate into individual tasks ----
adt36 <- cbind(bigcnb[,c(2,5:8,11,15:16,1674)], bigcnb[,grepl("ADT36_A.", colnames(bigcnb), fixed = TRUE)])
adt60 <- cbind(bigcnb[,c(2,5:8,11,15:16,1674)], bigcnb[,grepl("ADT60_A.", colnames(bigcnb), fixed = TRUE)])

cpfA <- cbind(bigcnb[,c(2,5:8,11,15:16,1674)], bigcnb[,grepl("CPF_A.", colnames(bigcnb), fixed = TRUE)])
cpfdA <- cbind(bigcnb[,c(2,5:8,11,15:16,1674)], bigcnb[,grepl("CPFD_A.", colnames(bigcnb), fixed = TRUE)])
cpfB <- cbind(bigcnb[,c(2,5:8,11,15:16,1674)], bigcnb[,grepl("CPF_B.", colnames(bigcnb), fixed = TRUE)])
cpfdB <- cbind(bigcnb[,c(2,5:8,11,15:16,1674)], bigcnb[,grepl("CPFD_B.", colnames(bigcnb), fixed = TRUE)])

er40A <- cbind(bigcnb[,c(2,5:8,11,15:16,1674)], bigcnb[,grepl("ER40_A.", colnames(bigcnb), fixed = TRUE)])
er40C <- cbind(bigcnb[,c(2,5:8,11,15:16,1674)], bigcnb[,grepl("ER40_C.", colnames(bigcnb), fixed = TRUE)])
er40D <- cbind(bigcnb[,c(2,5:8,11,15:16,1674)], bigcnb[,grepl("ER40_D.", colnames(bigcnb), fixed = TRUE)])

gng <- cbind(bigcnb[,c(2,5:8,11,15:16,1674)], bigcnb[,grepl("GNG150.", colnames(bigcnb), fixed = TRUE)])

kcpwA <- cbind(bigcnb[,c(2,5:8,11,15:16,1674)], bigcnb[,grepl("KCPW_A.", colnames(bigcnb), fixed = TRUE)])
kcpwdA <- cbind(bigcnb[,c(2,5:8,11,15:16,1674)], bigcnb[,grepl("KCPWD_A.", colnames(bigcnb), fixed = TRUE)])

kspvrtA <- cbind(bigcnb[,c(2,5:8,11,15:16,1674)], bigcnb[,grepl("KSPVRT_A.", colnames(bigcnb), fixed = TRUE)])
kspvrtB <- cbind(bigcnb[,c(2,5:8,11,15:16,1674)], bigcnb[,grepl("KSPVRT_B.", colnames(bigcnb), fixed = TRUE)])
kspvrtD <- cbind(bigcnb[,c(2,5:8,11,15:16,1674)], bigcnb[,grepl("KSPVRT_D.", colnames(bigcnb), fixed = TRUE)])

medf60A <- cbind(bigcnb[,c(2,5:8,11,15:16,1674)], bigcnb[,grepl("MEDF60_A.", colnames(bigcnb), fixed = TRUE)])
medf36A <- cbind(bigcnb[,c(2,5:8,11,15:16,1674)], bigcnb[,grepl("MEDF36_A.", colnames(bigcnb), fixed = TRUE)])

mpract <- cbind(bigcnb[,c(2,5:8,11,15:16,1674)], bigcnb[,grepl("MPRACT.", colnames(bigcnb), fixed = TRUE)])

pcetA <- cbind(bigcnb[,c(2,5:8,11,15:16,1674)], bigcnb[,grepl("PCET_A.", colnames(bigcnb), fixed = TRUE)])
spcetA <- cbind(bigcnb[,c(2,5:8,11,15:16,1674)], bigcnb[,grepl("SPCET_A.", colnames(bigcnb), fixed = TRUE)])

pmat18B <- cbind(bigcnb[,c(2,5:8,11,15:16,1674)], bigcnb[,grepl("PMAT18_B.", colnames(bigcnb), fixed = TRUE)])
pmat24A <- cbind(bigcnb[,c(2,5:8,11,15:16,1674)], bigcnb[,grepl("PMAT24_A.", colnames(bigcnb), fixed = TRUE)])
pmat24B <- cbind(bigcnb[,c(2,5:8,11,15:16,1674)], bigcnb[,grepl("PMAT24_B.", colnames(bigcnb), fixed = TRUE)])

sctap <- cbind(bigcnb[,c(2,5:8,11,15:16,1674)], bigcnb[,grepl("SCTAP.", colnames(bigcnb), fixed = TRUE)])

slnb2 <- cbind(bigcnb[,c(2,5:8,11,15:16,1674)], bigcnb[,grepl("SLNB2", colnames(bigcnb), fixed = TRUE)])

spcptnl <- cbind(bigcnb[,c(2,5:8,11,15:16,1674)], bigcnb[,grepl("SPCPTNL", colnames(bigcnb), fixed = TRUE)])
spcptn90 <- cbind(bigcnb[,c(2,5:8,11,15:16,1674)], bigcnb[,grepl("SPCPTN90.", colnames(bigcnb), fixed = TRUE)])

svoltA <- cbind(bigcnb[,c(2,5:8,11,15:16,1674)], bigcnb[,grepl("SVOLT_A.", colnames(bigcnb), fixed = TRUE)])
svoltdA <- cbind(bigcnb[,c(2,5:8,11,15:16,1674)], bigcnb[,grepl("SVOLTD_A.", colnames(bigcnb), fixed = TRUE)])

vsplot24 <- cbind(bigcnb[,c(2,5:8,11,15:16,1674)], bigcnb[,grepl("VSPLOT24.", colnames(bigcnb), fixed = TRUE)])
vsplot15 <- cbind(bigcnb[,c(2,5:8,11,15:16,1674)], bigcnb[,grepl("VSPLOT15.", colnames(bigcnb), fixed = TRUE)])
splot12 <- cbind(bigcnb[,c(2,5:8,11,15:16,1674)], bigcnb[,grepl("SPLOT12.", colnames(bigcnb), fixed = TRUE)])

wrat4B <- cbind(bigcnb[,c(2,5:8,11,15:16,1674)], bigcnb[,grepl("WRAT4.", colnames(bigcnb), fixed = TRUE)])
wrat4G <- cbind(bigcnb[,c(2,5:8,11,15:16,1674)], bigcnb[,grepl("WRAT4B.", colnames(bigcnb), fixed = TRUE)])

kddisc <- cbind(bigcnb[,c(2,5:8,11,15:16,1674)], bigcnb[,grepl("KDDISC.", colnames(bigcnb), fixed = TRUE)])
krdisc <- cbind(bigcnb[,c(2,5:8,11,15:16,1674)], bigcnb[,grepl("KRDISC.", colnames(bigcnb), fixed = TRUE)])
edisc <- cbind(bigcnb[,c(2,5:8,11,15:16,1674)], bigcnb[,grepl("EDISC", colnames(bigcnb), fixed = TRUE)])

abart <- cbind(bigcnb[,c(2,5:8,11,15:16,1674)], bigcnb[,grepl("BART_1.", colnames(bigcnb), fixed = TRUE)])

digsym <- cbind(bigcnb[,c(2,5:8,11,15:16,1674)], bigcnb[,grepl("DIGSYM.", colnames(bigcnb), fixed = TRUE)])

pvtb <- cbind(bigcnb[,c(2,5:8,11,15:16,1674)], bigcnb[,grepl("PVTB.", colnames(bigcnb), fixed = TRUE)])

aim <- cbind(bigcnb[,c(2,5:8,11,15:16,1674)], bigcnb[,grepl("AIM.", colnames(bigcnb), fixed = TRUE)])

trailsA <- cbind(bigcnb[,c(2,5:8,11,15:16,1674)], bigcnb[,grepl("TRAIL_A.", colnames(bigcnb), fixed = TRUE)])
trailsB <- cbind(bigcnb[,c(2,5:8,11,15:16,1674)], bigcnb[,grepl("TRAIL_B.", colnames(bigcnb), fixed = TRUE)])

raven <- cbind(bigcnb[,c(2,5:8,11,15:16,1674)], bigcnb[,grepl("RAVEN.", colnames(bigcnb), fixed = TRUE)])

praD <- cbind(bigcnb[,c(2,5:8,11,15:16,1674)], bigcnb[,grepl("PRA_D.", colnames(bigcnb), fixed = TRUE)])

sfnb2 <- cbind(bigcnb[,c(2,5:8,11,15:16,1674)], bigcnb[,grepl("SFNB2.", colnames(bigcnb), fixed = TRUE)])

cpwA <- cbind(bigcnb[,c(2,5:8,11,15:16,1674)], bigcnb[,grepl("CPW_A.", colnames(bigcnb), fixed = TRUE)])
cpwdA <- cbind(bigcnb[,c(2,5:8,11,15:16,1674)], bigcnb[,grepl("CPWD_A.", colnames(bigcnb), fixed = TRUE)])

spvrtA <- cbind(bigcnb[,c(2,5:8,11,15:16,1674)], bigcnb[,grepl("SPVRT_A.", colnames(bigcnb), fixed = TRUE)])


# refining data separated out into tasks (only site, bblid, age, date, sex, TC, SP)
adt36 <- adt36[!is.na(adt36$ADT36_A.ADT36A_CR),c(1:3,5:6,11,13)]
adt60 <- adt60[!is.na(adt60$ADT60_A.ADT60_CR),c(1:3,5:6,12,14)]

cpfA <- cpfA[!is.na(cpfA$CPF_A.CPF_CR),c(1:3,5:6,11:12)]
cpfdA <- cpfdA[!is.na(cpfdA$CPFD_A.CPFD_CR),c(1:3,5:6,12:13)]
cpfB <- cpfB[!is.na(cpfB$CPF_B.CPF_CR),c(1:3,5:6,11:12)]
cpfdB <- cpfdB[!is.na(cpfdB$CPFD_B.CPFD_CR),c(1:3,5:6,12:13)]

er40A <- er40A[!is.na(er40A$ER40_A.ER40_CR),c(1:3,5:6,12:13)]
er40C <- er40C[!is.na(er40C$ER40_C.ER40C_CR),c(1:3,5:6,12:13)]
er40D <- er40D[!is.na(er40D$ER40_D.ER40D_CR),c(1:3,5:6,11:12)]

gng <- gng[!is.na(gng$GNG150.GNG150_CR),c(1:3,5:6,11,20)]

kcpwA <- kcpwA[!is.na(kcpwA$KCPW_A.CPW_CR),c(1:3,5:6,11:12)]
kcpwdA <- kcpwdA[!is.na(kcpwdA$KCPWD_A.CPWD_CR),c(1:3,5:6,12:13)]

kspvrtA <- kspvrtA[!is.na(kspvrtA$KSPVRT_A.KSPVRTA_CR),c(1:3,5:6,13,16)]
kspvrtB <- kspvrtB[!is.na(kspvrtB$KSPVRT_B.KSPVRTB_CR),c(1:3,5:6,13,16)]
kspvrtD <- kspvrtD[!is.na(kspvrtD$KSPVRT_D.KSPVRTD_CR),c(1:3,5:6,12,15)]













