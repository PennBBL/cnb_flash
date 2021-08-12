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


# refining data separated out into tasks (only site, bblid, age, date, sex, TC, SP) ----
adt36 <- adt36[!is.na(adt36$ADT36_A.ADT36A_CR),c(1:3,5:6,9,11,13)]
adt60 <- adt60[!is.na(adt60$ADT60_A.ADT60_CR),c(1:3,5:6,9,12,14)]

cpfA <- cpfA[!is.na(cpfA$CPF_A.CPF_CR),c(1:3,5:6,9,11:12)]
cpfdA <- cpfdA[!is.na(cpfdA$CPFD_A.CPFD_CR),c(1:3,5:6,9,12:13)]
cpfB <- cpfB[!is.na(cpfB$CPF_B.CPF_CR),c(1:3,5:6,9,11:12)]
cpfdB <- cpfdB[!is.na(cpfdB$CPFD_B.CPFD_CR),c(1:3,5:6,9,12:13)]

er40A <- er40A[!is.na(er40A$ER40_A.ER40_CR),c(1:3,5:6,9,12:13)]
er40C <- er40C[!is.na(er40C$ER40_C.ER40C_CR),c(1:3,5:6,9,12:13)]
er40D <- er40D[!is.na(er40D$ER40_D.ER40D_CR),c(1:3,5:6,9,11:12)]

gng <- gng[!is.na(gng$GNG150.GNG150_CR),c(1:3,5:6,9,11,20)]

kcpwA <- kcpwA[!is.na(kcpwA$KCPW_A.CPW_CR),c(1:3,5:6,9,11:12)]
kcpwdA <- kcpwdA[!is.na(kcpwdA$KCPWD_A.CPWD_CR),c(1:3,5:6,9,12:13)]

kspvrtA <- kspvrtA[!is.na(kspvrtA$KSPVRT_A.KSPVRTA_CR),c(1:3,5:6,9,13,16)]
kspvrtB <- kspvrtB[!is.na(kspvrtB$KSPVRT_B.KSPVRTB_CR),c(1:3,5:6,9,13,16)]
kspvrtD <- kspvrtD[!is.na(kspvrtD$KSPVRT_D.KSPVRTD_CR),c(1:3,5:6,9,12,15)]

medf60A <- medf60A[!is.na(medf60A$MEDF60_A.MEDF60_CR),c(1:3,5:6,9,12,14)]
medf36A <- medf36A[!is.na(medf36A$MEDF36_A.MEDF36A_CR),c(1:3,5:6,9,11,13)]

mpract <- mpract[!is.na(mpract$MPRACT.MP2),c(1:3,5:6,9,12:13)]

pcetA <- pcetA[!is.na(pcetA$PCET_A.PCET_ACC2),c(1:3,5:6,9,25,12)] # use ACC2 CR instead (accounts for correct responses and number of rules learned)
spcetA <- spcetA[!is.na(spcetA$SPCET_A.SPCET_ACC2),c(1:3,5:6,9,25,12)] # same as pcet

pmat18B <- pmat18B[!is.na(pmat18B$PMAT18_B.PMAT18_B_CR),c(1:3,5:6,9,12,14)]
pmat24A <- pmat24A[!is.na(pmat24A$PMAT24_A.PMAT24_A_CR),c(1:3,5:6,9,12,15)]
pmat24B <- pmat24B[!is.na(pmat24B$PMAT24_B.PMAT24_B_CR),c(1:3,5:6,9,12,14)]

sctap <- sctap[!is.na(sctap$SCTAP.SCTAP_TOT),c(1:3,5:6,9,16)] # use sctap.sctap_tot, no response time

slnb2 <- slnb2[!is.na(slnb2$SLNB2_90.SLNB2_TP),c(1:3,5:6,9,11,13)] # true positives

spcptnl <- spcptnl[!is.na(spcptnl$SPCPTNL.SCPL_TP), c(1:3,5:6,9,11,15)] # use true positives
spcptn90 <- spcptn90[!is.na(spcptn90$SPCPTN90.SCPN90_TP), c(1:3,5:6,9,11,15)] # same

svoltA <- svoltA[!is.na(svoltA$SVOLT_A.SVOLT_CR), c(1:3,5:6,9,11:12)]
svoltdA <- svoltdA[!is.na(svoltdA$SVOLTD_A.SVOLTD_CR), c(1:3,5:6,9,12:13)]

vsplot24 <- vsplot24[!is.na(vsplot24$VSPLOT24.VSPLOT24_CR), c(1:3,5:6,9,11,13)]
vsplot15 <- vsplot15[!is.na(vsplot15$VSPLOT15.VSPLOT15_CR), c(1:3,5:6,9,11,13)]
splot12 <- splot12[!is.na(splot12$SPLOT12.SPLOT_CR), c(1:3,5:6,9,12,14)]

wrat4B <- wrat4B[!is.na(wrat4B$WRAT4.WRAT4CR_RAW), c(1:3,5:6,9,15)]     # use raw score, no time
wrat4G <- wrat4G[!is.na(wrat4G$WRAT4B.WRAT4BCR_RAW), c(1:3,5:6,9,15)]

kddisc <- kddisc[!is.na(kddisc$KDDISC.q_01),]
dore <- grep("KDDISC.q_", colnames(kddisc))
kddisc[,dore] <- kddisc[,dore] -1
kddisc$TE <- rowSums(kddisc[,grepl("KDDISC.q_", colnames(kddisc))]) # total endorsements
kddisc$RT <- rowMeans(kddisc[,grepl("KDDISC.trr_", colnames(kddisc))])
temp <- kddisc[,c(1:3,5:6,9,80:81)]
kddisc <- temp

krdisc <- krdisc[!is.na(krdisc$KRDISC.q_01),]
dore <- grep("KRDISC.q_", colnames(krdisc))
krdisc[,dore] <- krdisc[,dore] - 1
krdisc$TE <- rowSums(krdisc[,grepl("KRDISC.q_", colnames(krdisc))]) # total endorsements
krdisc$RT <- rowMeans(krdisc[,grepl("KRDISC.trr_", colnames(krdisc))])
temp <- krdisc[,c(1:3,5:6,9,94:95)]
krdisc <- temp

edisc <- edisc[!is.na(edisc$EDISC.q_1_resp),]                   # i will come back to EDISC later, weird thing with all responses being th same up to q101-134
questions <- edisc[,grepl("EDISC.q_", colnames(edisc))]
resp <- questions[,grep("resp", colnames(questions))] -1
times <- questions[,grep("ttr", colnames(questions))]
edisc$TE <- rowSums(resp) # total endorsements
edisc$RT <- rowMeans(edisc[,grepl("EDISC.trr_", colnames(edisc))])
temp <- edisc[,c(1:3,5:6,9,94:95)]
edisc <- temp

abart <- abart[!is.na(abart$BART_1.ABART_A_TOTAL_PUMPS), c(1:3,5:6,9,11,14)] # total pumps

digsym <- digsym[!is.na(digsym$DIGSYM.DSCOR), c(1:3,5:6,9,11,16)]

pvtb <- pvtb[!is.na(pvtb$PVTB.PVTB_CR), c(1:3,5:6,9,12,16)]

aim <- aim[!is.na(aim$AIM.AIMTOT), c(1:3,5:6,9,37,40)] # use aimTOT

trailsA <- trailsA[!is.na(trailsA$TRAIL_A.TRAILS_A_CR), c(1:3,5:6,9,12,13)]
trailsB <- trailsB[!is.na(trailsB$TRAIL_B.TRAILS_B_CR), c(1:3,5:6,9,12,13)]

raven <- raven[!is.na(raven$RAVEN.RAV_CR), c(1:3,5:6,9,12:13)]

praD <- praD[!is.na(praD$PRA_D.PRADCR_RAW), c(1:3,5:6,9,16)] # use raw scores, no response time

sfnb2 <- spcptn90[!is.na(spcptn90$spcptn90), c(1:3,5:6,9,)] # TP, no data (no columns named sfn)

cpwA <- spcptn90[!is.na(spcptn90$spcptn90), c(1:3,5:6,9,)] # this is also empty
cpwdA <- spcptn90[!is.na(spcptn90$spcptn90), c(1:3,5:6,9,)]

spvrtA <- spvrtA[!is.na(spvrtA$SPVRT_A.PVRTCR), c(1:3,5:6,9,20,23)] # no values? (empty CR and RTCR)


# 08.05.21 making a list of all the tasks that are similar enough to try to make plots and stats

texts <- c("adt36", "adt60", "cpfA","cpfdA", "cpfB", "cpfdB", "er40A", "er40C", 
           "er40D", "gng", "kcpwA", "kcpwdA", "kspvrtA", "kspvrtB", "kspvrtD", 
           "medf60A", "medf36A", "mpract", "pcetA", "spcetA", "pmat18B", "pmat24A", 
           "pmat24B", "slnb2", "spcptnl", "spcptn90", "svoltA","svoltdA", "vsplot24", 
           "vsplot15", "splot12", "kddisc", "krdisc", "edisc", "abart", "digsym", "pvtb", 
           "aim", "trailsA", "trailsB", "raven")
tests <- mget(texts)

noRT <- c("sctap","wrat4B","wrat4G", "praD")
noRT <- mget(noRT)

needhelp <- c("sfnb2", "cpwA", "cpwdA", "spvrtA")
needhelp <- mget(needhelp)


# Stats Loop ----
count <- 1
for (test in tests) {
  # general flash non-flash difference
  test[,4] <- as.Date(test[,4])
  colnames(test) <- c("Site", "BBLID", "Age", "Date", "Sex", "Flash", "TotalCorrect", "MedianRT")
  
  fnfTC <- test %>%
    group_by(Flash,Sex) %>%
    summarise(mean = mean(TotalCorrect,na.rm=T), sd = sd(TotalCorrect,na.rm=T), n = n())  # much fewer tests administered in non-flash years compared to flash years
  
  fnfSP <- test %>%
    group_by(Flash,Sex) %>%
    summarise(mean = mean(MedianRT,na.rm=T), sd = sd(MedianRT,na.rm=T), n = n())
  
  flash_meanSD <- cbind(fnfTC[-5],fnfSP[3:5])
  names(flash_meanSD) <- c("Flash", "Sex", "meanTC", "sdTC", "meanSP", "sdSP", "n")
  
  write.csv(flash_meanSD,paste0("myresults/", texts[count], "_fnf_mean_sd.csv"),na="",row.names=F)
  
  
  
  
  # age-sex group differences
  age <- na.exclude(test$Age)
  
  if (any(count == c(26,35,37))){      # if statement for spcptn90, abart, pvtb to have a separate agegroup thing
    binned <- bins(age, 7, minpts = 1, exact.groups = T)
    lo <- binned$binlo
    hi <- binned$binhi
  }else {
    binned <- bins(age, 7, minpts = 30, exact.groups = T)
    lo <- binned$binlo
    hi <- binned$binhi
  }
  
  ages <- sort(unique(na.exclude(test$Age)))
  agegroups <- c()
  
  for (i in 1:7){
    low <- lo[i]
    hig <- hi[i]
    agegroups <- c(agegroups,paste(ages[low],ages[hig], sep="-"))
    if (i==7){
      agegroups[7] <- paste0(ages[low], "+")
    }
    else if (lo[i]==hi[i]) {
      agegroups[i] <- ages[lo[i]]
    }
  }
  
  for (age in ages){
    if (age %in% ages[lo[1]]:ages[hi[1]]){
      test$AgeGroup[test$Age==age] <- agegroups[1]
    }
    else if (age %in% ages[lo[2]]:ages[hi[2]]) {
      test$AgeGroup[test$Age==age] <- agegroups[2]
    }
    else if (age %in% ages[lo[3]]:ages[hi[3]]) {
      test$AgeGroup[test$Age==age] <- agegroups[3]
    }
    else if (age %in% ages[lo[4]]:ages[hi[4]]) {
      test$AgeGroup[test$Age==age] <- agegroups[4]
    }
    else if (age %in% ages[lo[5]]:ages[hi[5]]) {
      test$AgeGroup[test$Age==age] <- agegroups[5]
    }
    else if (age %in% ages[lo[6]]:ages[hi[6]]) {
      test$AgeGroup[test$Age==age] <- agegroups[6]
    }
    else if (age >= ages[lo[7]]) {
      test$AgeGroup[test$Age==age] <- agegroups[7]
    }
  }
  
  agesexTC <- test %>%
    group_by(AgeGroup,Sex) %>%
    summarise(mean = mean(TotalCorrect,na.rm=T), sd = sd(TotalCorrect,na.rm=T), n = n())
  
  agesexSP <- test %>%
    group_by(AgeGroup,Sex) %>%
    summarise(mean = mean(MedianRT,na.rm=T), sd = sd(MedianRT,na.rm=T), n = n())
  
  
  agesex_mean_sd <- cbind(agesexTC[1:4],agesexSP[,3:5])
  names(agesex_mean_sd)[2:7] <- c("Sex", "meanTC", "sdTC", "meanSP", "sdSP", "n")
  
  write.csv(agesex_mean_sd, paste0("myresults/", texts[count], "_agesex_mean_sd.csv"),na="",row.names=F)
  
  # age and flash/non-flash
  agefTC <- test %>%
    group_by(AgeGroup,Flash) %>%
    summarise(mean = mean(TotalCorrect,na.rm=T), sd = sd(TotalCorrect,na.rm=T), n = n())
  
  agefSP <- test %>%
    group_by(AgeGroup,Flash) %>%
    summarise(mean = mean(MedianRT,na.rm=T), sd = sd(MedianRT,na.rm=T), n = n())
  
  agef_mean_sd <- cbind(agesexTC[1:4],agesexSP[,3:5])
  names(agef_mean_sd)[2:7] <- c("Flash", "meanTC", "sdTC", "meanSP", "sdSP", "n")
  
  write.csv(agef_mean_sd, paste0("myresults/", texts[count], "_agefnf_mean_sd.csv"),na="",row.names=F)
  
  
  
  
  # site differences
  # sites <- sort(unique(test$Site))
  siteTC <- test %>%
    group_by(Site,Sex) %>%
    summarise(mean = mean(TotalCorrect,na.rm=T), sd = sd(TotalCorrect,na.rm=T), n = n())

  siteSP <- test %>%
    group_by(Site,Sex) %>%
    summarise(mean = mean(MedianRT,na.rm=T), sd = sd(MedianRT,na.rm=T), n = n())

  site_mean_sd <- cbind(siteTC[,1:4], siteSP[,3:5])
  names(site_mean_sd) <- c("Site", "Sex", "meanTC", "sdTC", "meanSP", "sdSP", "n")

  write.csv(site_mean_sd,paste0("myresults/", texts[count], "_site_mean_sd.csv"),na="",row.names=F)
  
  # site and flash
  sitefTC <- test %>%
    group_by(Site,Flash) %>%
    summarise(mean = mean(TotalCorrect,na.rm=T), sd = sd(TotalCorrect,na.rm=T), n = n())
  
  sitefSP <- test %>%
    group_by(Site,Flash) %>%
    summarise(mean = mean(MedianRT,na.rm=T), sd = sd(MedianRT,na.rm=T), n = n())
  
  sitefnf_mean_sd <- cbind(sitefTC[,1:4], sitefSP[,3:5])
  names(sitefnf_mean_sd) <- c("Site", "Flash", "meanTC", "sdTC", "meanSP", "sdSP", "n")
  
  write.csv(sitefnf_mean_sd,paste0("myresults/", texts[count], "_sitefnf_mean_sd.csv"),na="",row.names=F)
  
  
  
  
  count <- count + 1
}




# just testing random stuff ----
yay <- 3
plane <- "gey"
test <- 1:20
b <- c("yay", "plane", "test")

c <- mget(b)

for (word in c) {
  txt <- print(word)
  print(paste0("aki", txt))
}


# testing to find "exceptional" tests for age grouping

count <- 1
for (test in tests) {
  print(count)
  age <- na.exclude(test$test_sessions_v.age)
  groups <- bins(age, 7, minpts = 30)$binct
  count <- count + 1
}


# not enough ages for spcptn90 (26) -- too many NAs, abart (35)-- too many NAs, pvtb (37) only 22 entries
tests <- tests[-26]
tests <- tests[-34]
tests <- tests[-35]








