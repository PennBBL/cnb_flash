# This script is for the project Kosha talked to me about on 7/20/21, looking at 
# how participants' accuracy and speed measures have changed over time for each
# task specifically looking for three different things:
#       1) is there a big difference between flash and non-flash versions
#       2) age and sex differences
#       3) site differences
# 
# 07.22.21 Akira Di Sandro

# load packages ----
library(ggplot2)
library(psych)
library(dplyr)
library(binr)
library(arules)
library(stringr)


# load data ----
bigcnb <- read.csv("cnb_dump_15july2021.csv", na=c("",".","NA"))

bigcnb$flash <- 0
bigcnb$flash[which(bigcnb$test_sessions_v.dotest < as.Date("2021-01-01"))] <- 1


# separate into individual tasks ----
adt36 <- cbind(bigcnb[,c(2,5:8,11,15:16,1674)], bigcnb[,grepl("ADT36_A.", colnames(bigcnb), fixed = TRUE)])
adt60 <- cbind(bigcnb[,c(2,5:8,11,15:16, 1674)], bigcnb[,grepl("ADT60_A.", colnames(bigcnb), fixed = TRUE)])

cpfA <- cbind(bigcnb[grepl("test_sessions.bblid", colnames(bigcnb))], bigcnb[,grepl("CPF_A.", colnames(bigcnb), fixed = TRUE)])
cpfdA <- cbind(bigcnb[grepl("test_sessions.bblid", colnames(bigcnb))], bigcnb[,grepl("CPFD_A.", colnames(bigcnb), fixed = TRUE)])
cpfB <- cbind(bigcnb[grepl("test_sessions.bblid", colnames(bigcnb))], bigcnb[,grepl("CPF_B.", colnames(bigcnb), fixed = TRUE)])
cpfdB <- cbind(bigcnb[grepl("test_sessions.bblid", colnames(bigcnb))], bigcnb[,grepl("CPFD_B.", colnames(bigcnb), fixed = TRUE)])

er40A <- cbind(bigcnb[grepl("test_sessions.bblid", colnames(bigcnb))], bigcnb[,grepl("ER40_A.", colnames(bigcnb), fixed = TRUE)])
er40C <- cbind(bigcnb[grepl("test_sessions.bblid", colnames(bigcnb))], bigcnb[,grepl("ER40_C.", colnames(bigcnb), fixed = TRUE)])
er40D <- cbind(bigcnb[grepl("test_sessions.bblid", colnames(bigcnb))], bigcnb[,grepl("ER40_D.", colnames(bigcnb), fixed = TRUE)])

gng <- cbind(bigcnb[grepl("test_sessions.bblid", colnames(bigcnb))], bigcnb[,grepl("GNG150.", colnames(bigcnb), fixed = TRUE)])

kcpwA <- cbind(bigcnb[grepl("test_sessions.bblid", colnames(bigcnb))], bigcnb[,grepl("KCPW_A.", colnames(bigcnb), fixed = TRUE)])
kcpwdA <- cbind(bigcnb[grepl("test_sessions.bblid", colnames(bigcnb))], bigcnb[,grepl("KCPWD_A.", colnames(bigcnb), fixed = TRUE)])

kspvrtA <- cbind(bigcnb[grepl("test_sessions.bblid", colnames(bigcnb))], bigcnb[,grepl("KSPVRT_A.", colnames(bigcnb), fixed = TRUE)])
kspvrtB <- cbind(bigcnb[grepl("test_sessions.bblid", colnames(bigcnb))], bigcnb[,grepl("KSPVRT_B.", colnames(bigcnb), fixed = TRUE)])
kspvrtD <- cbind(bigcnb[grepl("test_sessions.bblid", colnames(bigcnb))], bigcnb[,grepl("KSPVRT_D.", colnames(bigcnb), fixed = TRUE)])

medf60A <- cbind(bigcnb[grepl("test_sessions.bblid", colnames(bigcnb))], bigcnb[,grepl("MEDF60_A.", colnames(bigcnb), fixed = TRUE)])
medf36A <- cbind(bigcnb[grepl("test_sessions.bblid", colnames(bigcnb))], bigcnb[,grepl("MEDF36_A.", colnames(bigcnb), fixed = TRUE)])

mpract <- cbind(bigcnb[grepl("test_sessions.bblid", colnames(bigcnb))], bigcnb[,grepl("MPRACT.", colnames(bigcnb), fixed = TRUE)])

pcetA <- cbind(bigcnb[grepl("test_sessions.bblid", colnames(bigcnb))], bigcnb[,grepl("PCET_A.", colnames(bigcnb), fixed = TRUE)])
spcetA <- cbind(bigcnb[grepl("test_sessions.bblid", colnames(bigcnb))], bigcnb[,grepl("SPCET_A.", colnames(bigcnb), fixed = TRUE)])

pmat18B <- cbind(bigcnb[grepl("test_sessions.bblid", colnames(bigcnb))], bigcnb[,grepl("PMAT18_B.", colnames(bigcnb), fixed = TRUE)])
pmat24A <- cbind(bigcnb[grepl("test_sessions.bblid", colnames(bigcnb))], bigcnb[,grepl("PMAT24_A.", colnames(bigcnb), fixed = TRUE)])
pmat24B <- cbind(bigcnb[grepl("test_sessions.bblid", colnames(bigcnb))], bigcnb[,grepl("PMAT24_B.", colnames(bigcnb), fixed = TRUE)])

sctap <- cbind(bigcnb[grepl("test_sessions.bblid", colnames(bigcnb))], bigcnb[,grepl("SCTAP.", colnames(bigcnb), fixed = TRUE)])

slnb2 <- cbind(bigcnb[grepl("test_sessions.bblid", colnames(bigcnb))], bigcnb[,grepl("SLNB2", colnames(bigcnb), fixed = TRUE)])

spcptnl <- cbind(bigcnb[grepl("test_sessions.bblid", colnames(bigcnb))], bigcnb[,grepl("SPCPTNL", colnames(bigcnb), fixed = TRUE)])
spcptn90 <- cbind(bigcnb[grepl("test_sessions.bblid", colnames(bigcnb))], bigcnb[,grepl("SPCPTN90.", colnames(bigcnb), fixed = TRUE)])

svoltA <- cbind(bigcnb[grepl("test_sessions.bblid", colnames(bigcnb))], bigcnb[,grepl("SVOLT_A.", colnames(bigcnb), fixed = TRUE)])
svoltdA <- cbind(bigcnb[grepl("test_sessions.bblid", colnames(bigcnb))], bigcnb[,grepl("SVOLTD_A.", colnames(bigcnb), fixed = TRUE)])

vsplot24 <- cbind(bigcnb[grepl("test_sessions.bblid", colnames(bigcnb))], bigcnb[,grepl("VSPLOT24.", colnames(bigcnb), fixed = TRUE)])
vsplot15 <- cbind(bigcnb[grepl("test_sessions.bblid", colnames(bigcnb))], bigcnb[,grepl("VSPLOT15.", colnames(bigcnb), fixed = TRUE)])
splot12 <- cbind(bigcnb[grepl("test_sessions.bblid", colnames(bigcnb))], bigcnb[,grepl("SPLOT12.", colnames(bigcnb), fixed = TRUE)])

wrat4B <- cbind(bigcnb[grepl("test_sessions.bblid", colnames(bigcnb))], bigcnb[,grepl("WRAT4.", colnames(bigcnb), fixed = TRUE)])
wrat4G <- cbind(bigcnb[grepl("test_sessions.bblid", colnames(bigcnb))], bigcnb[,grepl("WRAT4B.", colnames(bigcnb), fixed = TRUE)])

kddisc <- cbind(bigcnb[grepl("test_sessions.bblid", colnames(bigcnb))], bigcnb[,grepl("KDDISC.", colnames(bigcnb), fixed = TRUE)])
krdisc <- cbind(bigcnb[grepl("test_sessions.bblid", colnames(bigcnb))], bigcnb[,grepl("KRDISC.", colnames(bigcnb), fixed = TRUE)])
edisc <- cbind(bigcnb[grepl("test_sessions.bblid", colnames(bigcnb))], bigcnb[,grepl("EDISC", colnames(bigcnb), fixed = TRUE)])

abart <- cbind(bigcnb[grepl("test_sessions.bblid", colnames(bigcnb))], bigcnb[,grepl("BART_1.", colnames(bigcnb), fixed = TRUE)])

digsym <- cbind(bigcnb[grepl("test_sessions.bblid", colnames(bigcnb))], bigcnb[,grepl("DIGSYM.", colnames(bigcnb), fixed = TRUE)])

pvtb <- cbind(bigcnb[grepl("test_sessions.bblid", colnames(bigcnb))], bigcnb[,grepl("PVTB.", colnames(bigcnb), fixed = TRUE)])

aim <- cbind(bigcnb[grepl("test_sessions.bblid", colnames(bigcnb))], bigcnb[,grepl("AIM.", colnames(bigcnb), fixed = TRUE)])

trailsA <- cbind(bigcnb[grepl("test_sessions.bblid", colnames(bigcnb))], bigcnb[,grepl("TRAIL_A.", colnames(bigcnb), fixed = TRUE)])
trailsB <- cbind(bigcnb[grepl("test_sessions.bblid", colnames(bigcnb))], bigcnb[,grepl("TRAIL_B.", colnames(bigcnb), fixed = TRUE)])

raven <- cbind(bigcnb[grepl("test_sessions.bblid", colnames(bigcnb))], bigcnb[,grepl("RAVEN.", colnames(bigcnb), fixed = TRUE)])

praD <- cbind(bigcnb[grepl("test_sessions.bblid", colnames(bigcnb))], bigcnb[,grepl("PRA_D.", colnames(bigcnb), fixed = TRUE)])

sfnb2 <- cbind(bigcnb[grepl("test_sessions.bblid", colnames(bigcnb))], bigcnb[,grepl("SFNB2.", colnames(bigcnb), fixed = TRUE)])

cpwA <- cbind(bigcnb[grepl("test_sessions.bblid", colnames(bigcnb))], bigcnb[,grepl("CPW_A.", colnames(bigcnb), fixed = TRUE)])
cpwdA <- cbind(bigcnb[grepl("test_sessions.bblid", colnames(bigcnb))], bigcnb[,grepl("CPWD_A.", colnames(bigcnb), fixed = TRUE)])

spvrtA <- cbind(bigcnb[grepl("test_sessions.bblid", colnames(bigcnb))], bigcnb[,grepl("SPVRT_A.", colnames(bigcnb), fixed = TRUE)])



# Making plots ----
# * ADT36 ----
adt36 <- adt36[rowSums(is.na(adt36[,10:ncol(adt36)])) < (ncol(adt36)-10),]
adt36$test_sessions_v.dotest <- as.Date(adt36$test_sessions_v.dotest)
TC <- adt36$ADT36_A.ADT36A_CR
adt36$ADT36_A.ADT36A_PC <- 100*adt36$ADT36_A.ADT36A_CR/36
PC <- adt36$ADT36_A.ADT36A_PC
SP <- adt36$ADT36_A.ADT36A_RTCR   # code below fixes these so that there is a mean score per date

# basic accuracy and speed plots (flash vs non-flash)
dates <- sort(unique(adt36$test_sessions_v.dotest))
corrected <- as.data.frame(matrix(NA, nrow = length(dates), ncol = 4))
names(corrected) <- c("dates", "tc", "pc", "sp")
corrected[,1] <- dates
for (i in 1:length(dates)) {
  corrected[i,2] <- mean(TC[which(adt36$test_sessions_v.dotest == corrected[i,1])])
  corrected[i,3] <- mean(PC[which(adt36$test_sessions_v.dotest == corrected[i,1])])
  corrected[i,4] <- mean(SP[which(adt36$test_sessions_v.dotest == corrected[i,1])])
}

fnfTC <- ggplot(corrected,aes(x=dates, y=tc)) +
  geom_line(color="dark blue") +
  geom_vline(xintercept = as.Date("2021-01-01"), color = "dark red") +
  labs(x="Date of Test",
       y="Total Correct",
       title = "ADT36 Accuracy Over Time") + 
  theme(plot.margin=unit(c(1,2,1.5,1.2),"cm"))

fnfTC

fnfPC <- ggplot(corrected, aes(x=dates, y=pc)) +     # i'm p sure i still have to edit this cause there are multiple participants per days and i need to average out the scores per day before graphing
  geom_line(color="dark blue") +
  geom_vline(xintercept = as.Date("2021-01-01"), color = "dark red") +
  labs(x="Date of Test",
       y="Percent Correct",
       title = "ADT36 Accuracy by Percentage Over Time") + 
  theme(plot.margin=unit(c(1,2,1.5,1.2),"cm"))

fnfPC

fnfSP <- ggplot(corrected, aes(x=dates, y=sp)) +
  geom_line(color="dark blue") +
  geom_vline(xintercept = as.Date("2021-01-01"), color = "dark red") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  labs(x="Date of Test",
       y="Speed",
       title = "ADT36 Speed of Participants Over Time") + 
  theme(plot.margin=unit(c(1,2,1.5,1.2),"cm"))

fnfSP



# age and sex differences
age <- na.exclude(adt36$test_sessions_v.age)
groups <- bins(age, 7, minpts = 30)$binct

for (i in 1:nrow(adt36)) {
  if (adt36$test_sessions_v.age[i] %in% 8:9) {
    adt36$agegroup[i] <- "8-9"
  }
  else if (adt36$test_sessions_v.age[i] %in% 10:11) {
    adt36$agegroup[i] <- "10-11"
  }
  else if (adt36$test_sessions_v.age[i] %in% 12:13) {
    adt36$agegroup[i] <- "12-13"
  }
  else if (adt36$test_sessions_v.age[i] %in% 14:15) {
    adt36$agegroup[i] <- "14-15"
  }
  else if (adt36$test_sessions_v.age[i] %in% 16:17) {
    adt36$agegroup[i] <- "16-17"
  }
  else if (adt36$test_sessions_v.age[i] %in% 18:20) {
    adt36$agegroup[i] <- "18-20"
  }
  else if (adt36$test_sessions_v.age[i] >= 21) {
    adt36$agegroup[i] <- "21+"
  }
}

adt36 <- adt36[,c(1:3,55,4:54)]

agegroups <- c("8-9","10-11","12-13","14-15","16-17","18-20","21+")

for (age in agegroups) {
  m <- adt36[which(adt36$test_sessions_v.gender == "M" & adt36$agegroup == age),c(3:4,6,12:14)]
  f <- adt36[which(adt36$test_sessions_v.gender == "F" & adt36$agegroup == age),c(3:4,6,12:14)]
  mdates <- sort(unique(m$test_sessions_v.dotest))
  fdates <- sort(unique(f$test_sessions_v.dotest))
  
  male <- as.data.frame(matrix(NA, nrow = length(mdates), ncol = 4))
  names(male) <- c("dates", "tc", "pc", "sp")
  male[,1] <- mdates
  mTC <- m$ADT36_A.ADT36A_CR
  mPC <- m$ADT36_A.ADT36A_PC
  mSP <- m$ADT36_A.ADT36A_RTCR
  
  for (i in 1:length(mdates)) {
    male[i,2] <- mean(mTC[which(m$test_sessions_v.dotest == male[i,1])])
    male[i,3] <- mean(mPC[which(m$test_sessions_v.dotest == male[i,1])])
    male[i,4] <- mean(mSP[which(m$test_sessions_v.dotest == male[i,1])])
  }
  
  female <- as.data.frame(matrix(NA, nrow = length(fdates), ncol = 4))
  names(female) <- c("dates", "tc", "pc", "sp")
  female[,1] <- fdates
  fTC <- f$ADT36_A.ADT36A_CR
  fPC <- f$ADT36_A.ADT36A_PC
  fSP <- f$ADT36_A.ADT36A_RTCR
  
  for (i in 1:length(fdates)) {
    female[i,2] <- mean(fTC[which(f$test_sessions_v.dotest == female[i,1])])
    female[i,3] <- mean(fPC[which(f$test_sessions_v.dotest == female[i,1])])
    female[i,4] <- mean(fSP[which(f$test_sessions_v.dotest == female[i,1])])
  }
  
  mtemp <- as.data.frame(mdates)
  ftemp <- as.data.frame(fdates)
  names(mtemp) <- "dates"
  names(ftemp) <- "dates"
  mergedates <- unique(rbind(mtemp,ftemp))
  fandm <- as.data.frame(matrix(NA, nrow=nrow(mergedates), ncol=7))
  names(fandm) <- c("dates", "mTC", "mPC", "mSP", "fTC", "fPC", "fSP")
  fandm[,1] <- sort(mergedates[,1])
  
  for (i in 1:nrow(fandm)) {
    if (any(mTC[which(male$dates == fandm[i,1])])) {
      fandm[i,2] <- mTC[which(male$dates == fandm[i,1])]
      fandm[i,3] <- mPC[which(male$dates == fandm[i,1])]
      fandm[i,4] <- mSP[which(male$dates == fandm[i,1])]
      if (any(fTC[which(female$dates == fandm[i,1])])) {
        fandm[i,5] <- fTC[which(female$dates == fandm[i,1])]
        fandm[i,6] <- fPC[which(female$dates == fandm[i,1])]
        fandm[i,7] <- fSP[which(female$dates == fandm[i,1])]
      }
    }
    else if (any(fTC[which(female$dates == fandm[i,1])])) {
      fandm[i,5] <- fTC[which(female$dates == fandm[i,1])]
      fandm[i,6] <- fPC[which(female$dates == fandm[i,1])]
      fandm[i,7] <- fSP[which(female$dates == fandm[i,1])]
    }
  }
  
  sexTC <- ggplot(fandm, aes(x=dates)) +         # maybe rename these plots
    geom_line(aes(y=mTC), color="blue") +
    geom_line(aes(y=fTC), color="purple") +
    labs(x="Date of Test",
         y="Score (out of 36)",
         title = paste("Sex Differences in ADT36 Accuracy of Participants Ages", age, "Over Time")) +
    theme(plot.margin=unit(c(1,2,1.5,1.2),"cm"))
  
  var <- str_replace_all(age, "[^[:alnum:]]", "")
  assign(paste0("sexTC", var), sexTC)
  
  sexPC <- ggplot(fandm, aes(x=dates)) +     
    geom_line(aes(y=mPC), color="blue") +
    geom_line(aes(y=fPC), color="purple") +
    labs(x="Date of Test",
         y="Score (out of 100%)",
         title = paste("Sex Differences in ADT36 Accuracy (percentage) of Participants Ages", age, "Over Time")) +
    theme(plot.margin=unit(c(1,2,1.5,1.2),"cm"))
  
  assign(paste0("sexPC", var), sexPC)
  
  sexSP <- ggplot(fandm, aes(x=dates)) +     
    geom_line(aes(y=mSP), color="blue") +
    geom_line(aes(y=fSP), color="purple") +
    labs(x="Date of Test",
         y="Speed (ms)",
         title = paste("Sex Differences in ADT36 Speed (ms) of Participants Ages", age, "Over Time")) +
    theme(plot.margin=unit(c(1,2,1.5,1.2),"cm"))
  
  assign(paste0("sexSP", var), sexSP)
}




# site differences
sites <- sort(unique(adt36$test_sessions.siteid))
sitemeans <- as.data.frame(matrix(NA,nrow = length(sites),ncol = 2))
sdata <- adt36[,c(1,3:7,12:14)]

sitescatTC <- ggplot(sdata, aes(x=test_sessions_v.dotest, y=ADT36_A.ADT36A_CR)) +
  geom_point(aes(color=test_sessions.siteid)) +
  labs(x="Date of Test",
       y="Score (out of 36)",
       title="ADT36 Accuracy Over Time Separated by Site") +
  theme(plot.margin=unit(c(1,2,1.5,1.2),"cm"))

sitescatTC

sitescatPC <- ggplot(sdata, aes(x=test_sessions_v.dotest, y=ADT36_A.ADT36A_PC)) +
  geom_point(aes(color=test_sessions.siteid)) +
  labs(x="Date of Test",
       y="Score (as percentage)",
       title="ADT36 Accuracy (percentage) Over Time Separated by Site") +
  theme(plot.margin=unit(c(1,2,1.5,1.2),"cm"))

sitescatPC

sitescatSP <- ggplot(sdata, aes(x=test_sessions_v.dotest, y=ADT36_A.ADT36A_RTCR)) +
  geom_point(aes(color=test_sessions.siteid)) +
  labs(x="Date of Test",
       y="Speed",
       title="ADT36 Speed Over Time Separated by Site") +
  theme(plot.margin=unit(c(1,2,1.5,1.2),"cm"))

sitescatSP



sdata$ADT36_A.ADT36A_RTCR




