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
library(visreg)


# load data ----
bigcnb <- read.csv("cnb_dump_15july2021.csv", na=c("",".","NA"))

bigcnb$flash <- 0
bigcnb$flash[which(bigcnb$test_sessions_v.dotest < as.Date("2021-01-01"))] <- 1


# separate into individual tasks ----
adt36 <- cbind(bigcnb[,c(2,5:8,11,15:16,1674)], bigcnb[,grepl("ADT36_A.", colnames(bigcnb), fixed = TRUE)])
adt60 <- cbind(bigcnb[,c(2,5:8,11,15:16,1674)], bigcnb[,grepl("ADT60_A.", colnames(bigcnb), fixed = TRUE)])

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
adt36 <- adt36[!is.na(adt36$ADT36_A.ADT36A_CR),]
adt36$test_sessions_v.dotest <- as.Date(adt36$test_sessions_v.dotest)
adt36$ADT36_A.ADT36A_PC <- 100*adt36$ADT36_A.ADT36A_CR/36

# basic accuracy and speed plots (flash vs non-flash)
adt36 <- adt36[!is.na(adt36$ADT36_A.ADT36A_CR),]
adt36$test_sessions_v.dotest <- as.Date(adt36$test_sessions_v.dotest)
adt36$ADT36_A.ADT36A_PC <- 100*adt36$ADT36_A.ADT36A_CR/36

corrected <- adt36[,c(2,5:6,11:13)]
names(corrected) <- c("BBLID", "Dates", "Sex", "TotalCorrect", "PercentCorrect", "Speed")

# basic accuracy and speed plots (flash vs non-flash)
fit <- lm(TotalCorrect ~ Dates, data=corrected)
fnfTC <- visreg(fit, "Dates", ylab = "Score (out of 36)", main= "Accuracy on ADT36 over time")

fit <- lm(PercentCorrect ~ Dates, data=corrected)
fnfPC <- visreg(fit, "Dates", ylab = "Score (as percentage)", main= "Accuracy (percentage) on ADT36 over time")

fit <- lm(Speed ~ Dates, data=corrected)
fnfSP <- visreg(fit, "Dates", ylab = "Speed", main= "Speed on ADT36 over time")
# still need to figure "xvar" out 


# stats
fnfTC <- adt36 %>%
  group_by(flash,test_sessions_v.gender) %>%
  summarise(mean = mean(ADT36_A.ADT36A_CR), sd = sd(ADT36_A.ADT36A_CR), n = n())  # much fewer tests administered in non-flash years compared to flash years

fnfPC <- adt36 %>%
  group_by(flash,test_sessions_v.gender) %>%
  summarise(mean = mean(ADT36_A.ADT36A_PC), sd = sd(ADT36_A.ADT36A_PC), n = n())

fnfSP <- adt36 %>%
  group_by(flash,test_sessions_v.gender) %>%
  summarise(mean = mean(ADT36_A.ADT36A_RTCR), sd = sd(ADT36_A.ADT36A_RTCR), n = n())

adt36_flash_meanSD <- cbind(fnfTC[-5],fnfPC[3:4])
adt36_flash_meanSD <- cbind(adt36_flash_meanSD,fnfSP[3:4])
names(adt36_flash_meanSD) <- c("Flash", "Sex", "meanTC", "sdTC", "meanPC", "sdPC", "meanSP", "sdSP")


write.csv(adt36_flash_meanSD,"myresults/adt36_fnf_mean_sd.csv",na="")




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
  var <- str_replace_all(age, "[^[:alnum:]]", "")
  
  agecorrected <- adt36[which(adt36$agegroup == age),c(2:4,6:7,12:14)]
  names(agecorrected) <- c("BBLID", "Age", "AgeGroup", "Date", "Sex", "TotalCorrect", "PercentCorrect", "Speed")
  
  fit <- lm(TotalCorrect ~ Date*Sex, data=agecorrected)
  agesexTC <- visreg(fit, "Date", by= "Sex", overlay =T, ylab = "Score (out of 36)", main = paste("Sex Differences in ADT36 Accuracy of Participants Ages", age, "Over Time"))
  assign(paste0("sexTC", var), agesexTC)
  
  fit <- lm(PercentCorrect ~ Date*Sex, data=agecorrected)
  agesexPC <- visreg(fit, "Date", by= "Sex", overlay =T, ylab = "Score (as percentage)", main = paste("Sex Differences in ADT36 Accuracy (age percentage) of Participants Ages", age, "Over Time"))
  assign(paste0("sexTC", var), agesexPC)
  
  fit <- lm(Speed ~ Date*Sex, data=agecorrected)
  agesexSP <- visreg(fit, "Date", by= "Sex", overlay =T, ylab = "Speed", main = paste("Sex Differences in ADT36 Speed of Participants Ages", age, "Over Time"))
  assign(paste0("sexTC", var), agesexSP)
  
  # testing to see if plot title and legend overlap (they do rn)
  # png(filename = "letsseebaby.png", width=1000,height=800)
  # visreg(fit, "Date", by= "Sex", overlay =T, ylab = "Score (as percentage)", main = paste("Sex Differences in ADT36 Accuracy (age percentage) of Participants Ages", age, "Over Time"))
  # dev.off() 
}

# stats
agesexTC <- adt36 %>%
  group_by(agegroup,test_sessions_v.gender) %>%
  summarise(mean = mean(ADT36_A.ADT36A_CR), sd = sd(ADT36_A.ADT36A_CR))
agesexTC <- agesexTC[-7,] # 7th row produces NA row for 14-15 range

agesexSP <- adt36 %>%
  group_by(agegroup,test_sessions_v.gender) %>%
  summarise(mean = mean(ADT36_A.ADT36A_RTCR), sd = sd(ADT36_A.ADT36A_RTCR))
agesexSP <- agesexSP[-7,]

agesex_mean_sd <- cbind(agesexTC,agesexSP[,3:4])
names(agesex_mean_sd)[2:6] <- c("gender", "mean_TC", "sd_TC", "mean_SP", "sd_SP")
agesex_mean_sd <- agesex_mean_sd[c(13:14,1:12),]
agesex_mean_sd <- agesex_mean_sd[order(agesex_mean_sd$gender),]

write.csv(agesex_mean_sd, "myresults/agesex_mean_sd.csv",na="")





# site differences
sites <- sort(unique(adt36$test_sessions.siteid))
sites <- sites[which(sites != "ImmuSili")] # exclude LiBI and ImmuSili for missing data
sites <- sites[which(sites != "LiBI")]

for (site in sites) {
  sitecorrected <- adt36[which(adt36$test_sessions.siteid == site),c(1:4,6:7,12:14)]
  names(sitecorrected) <- c("SiteID", "BBLID", "Age", "AgeGroup", "Date", "Sex", "TotalCorrect", "PercentCorrect", "Speed") 
  
  fit <- lm(TotalCorrect ~ Date*Sex, data=sitecorrected)
  sitesexTC <- visreg(fit, "Date", by= "Sex", overlay =T, ylab = "Score (out of 36)", gg=T, main = paste("Sex Differences in ADT36 Accuracy of Participants at the", site, " site Over Time"))
  assign(paste0(site, "_TC"), sitesexTC)
  
  fit <- lm(PercentCorrect ~ Date*Sex, data=sitecorrected)
  sitesexPC <- visreg(fit, "Date", by= "Sex", overlay =T, gg=TRUE, ylab = "Score (as percentage)", main = paste("Sex Differences in ADT36 Accuracy (age percentage) of Participants at the", site, "site Over Time"))
  assign(paste0(site, "_PC"), sitesexPC)
  
  fit <- lm(Speed ~ Date*Sex, data=sitecorrected)
  sitesexSP <- visreg(fit, "Date", by= "Sex", overlay =T, ylab = "Speed", main = paste("Sex Differences in ADT36 Speed of Participants at the", site, "site Over Time"))
  assign(paste0(site, "_SP"), sitesexSP)
} 

# EFR01 looks weird (only one F data point and the line is overestimates, also lots of missing data)

# old code
# sites <- sort(unique(adt36$test_sessions.siteid))
# sitemeans <- as.data.frame(matrix(NA,nrow = length(sites),ncol = 2))
# sdata <- adt36[,c(1,3:7,12:14)]
# 
# sitescatTC <- ggplot(sdata, aes(x=test_sessions_v.dotest, y=ADT36_A.ADT36A_CR)) +
#   geom_point(aes(color=test_sessions.siteid)) +
#   labs(x="Date of Test",
#        y="Score (out of 36)",
#        title="ADT36 Accuracy Over Time Separated by Site") +
#   theme(plot.margin=unit(c(1,2,1.5,1.2),"cm"))
# 
# sitescatTC
# 
# sitescatPC <- ggplot(sdata, aes(x=test_sessions_v.dotest, y=ADT36_A.ADT36A_PC)) +
#   geom_point(aes(color=test_sessions.siteid)) +
#   labs(x="Date of Test",
#        y="Score (as percentage)",
#        title="ADT36 Accuracy (percentage) Over Time Separated by Site") +
#   theme(plot.margin=unit(c(1,2,1.5,1.2),"cm"))
# 
# sitescatPC
# 
# sitescatSP <- ggplot(sdata, aes(x=test_sessions_v.dotest, y=ADT36_A.ADT36A_RTCR)) +
#   geom_point(aes(color=test_sessions.siteid)) +
#   labs(x="Date of Test",
#        y="Speed",
#        title="ADT36 Speed Over Time Separated by Site") +
#   theme(plot.margin=unit(c(1,2,1.5,1.2),"cm"))
# 
# sitescatSP
# 
# # now try doing it with line plots
# sitelineTC <- ggplot(sdata, aes(test_sessions_v.dotest, ADT36_A.ADT36A_CR)) +
#   geom_line(aes(color=test_sessions.siteid)) +
#   labs(x="Date of Test",
#        y="Score (out of 36)",
#        title="ADT36 Accuracy Over Time Separated by Site") +
#   theme(plot.margin=unit(c(1,2,1.5,1.2),"cm"))
# 
# sitelineTC
# 
# 
# test <- ggplot(sdata[which(sdata$test_sessions.siteid == "GOGRANT"),], aes(test_sessions_v.dotest, ADT36_A.ADT36A_CR)) +
#   geom_smooth() +
#   labs(x="Date of Test",
#        y="Score (out of 36)",
#        title="ADT36 Accuracy Over Time (GOGRANT)") +
#   theme(plot.margin=unit(c(1,2,1.5,1.2),"cm"))
# 
# test
# 
# 
# 
# test4 <- ggplot(sdata[which(sdata$test_sessions.siteid == sites[12:13]),], aes(test_sessions_v.dotest, ADT36_A.ADT36A_CR[!is.na(ADT36_A.ADT36A_CR)])) +
#   geom_point(aes(color = test_sessions.siteid)) +
#   geom_smooth(aes(color = test_sessions.siteid)) +
#   labs(x="Date of Test",
#        y="Score (out of 36)",
#        title="ADT36 Accuracy Over Time Separated by Site") +
#   theme(plot.margin=unit(c(1,2,1.5,1.2),"cm"))
# 
# 
# # nothing gets printed out from this
# test2 <- ggplot(sdata[which(sdata$test_sessions.siteid == sites[3:12]),], aes(test_sessions_v.dotest, ADT36_A.ADT36A_CR)) +
#   geom_smooth(aes(color = test_sessions.siteid)) +
#   labs(x="Date of Test",
#        y="Score (out of 36)",
#        title="ADT36 Accuracy Over Time Separated by Site") +
#   theme(plot.margin=unit(c(1,2,1.5,1.2),"cm"))
# 
# 
# test3 <- ggplot(sdata[which(sdata$test_sessions.siteid == sites[13]),], aes(test_sessions_v.dotest, ADT36_A.ADT36A_CR)) +
#   geom_smooth() +
#   labs(x="Date of Test",
#        y="Score (out of 36)",
#        title="ADT36 Accuracy Over Time Separated by Site",
#        subtitle = sites[4]) +
#   theme(plot.margin=unit(c(1,2,1.5,1.2),"cm"))
# 
# # ImmuSili (sites[16]) doesn't plot anything because there's only one data point
# # site[1] has 7 points, [2] 8, [3] 237, [4] 25, [5] 16, [6] 42
# bleh <- as.data.frame(matrix(0, nrow = 22,ncol=2))
# for (i in 1:22) {
#   bleh[i,1] <- sites[i]
#   bleh[i,2] <- nrow(sdata[which(sdata$test_sessions.siteid==sites[i]),])
# }
# # from this loop above I found that [1] 22QIBBC has 7, [2] 7TITMAT has 8, and [16] ImmuSili has 1 point(s) which means they have to be excluded from graph
# 
# 
# # this doesn't work for now
# test1 <- ggplot(sdata, aes(test_sessions_v.dotest, ADT36_A.ADT36A_CR)) +
#   geom_smooth(aes(linetype = test_sessions.siteid)) +
#   labs(x="Date of Test",
#        y="Score (out of 36)",
#        title="ADT36 Accuracy Over Time Separated by Site") +
#   theme(plot.margin=unit(c(1,2,1.5,1.2),"cm"))


# stats analysis for site differences
siteTC <- adt36 %>%
  group_by(test_sessions.siteid,test_sessions_v.gender) %>%
  summarise(mean = mean(ADT36_A.ADT36A_CR), sd = sd(ADT36_A.ADT36A_CR), n = n())
siteTC <- siteTC[which(!is.na(siteTC$test_sessions_v.gender)),]   # LiBI has no age or sex data (missing on this table) and ImmuSili only has one (F) data point
names(siteTC) <- c("siteID", "gender", "meanTC", "sdTC", "n")

siteSP <- adt36 %>%
  group_by(test_sessions.siteid,test_sessions_v.gender) %>%
  summarise(mean = mean(ADT36_A.ADT36A_RTCR), sd = sd(ADT36_A.ADT36A_RTCR), n = n())
siteSP <- siteSP[which(!is.na(siteSP$test_sessions_v.gender)),]
names(siteSP) <- c("siteID", "gender", "meanSP", "sdSP", "n")

siteTC_as <- adt36 %>%
  group_by(test_sessions.siteid,agegroup,test_sessions_v.gender) %>%
  summarise(mean = mean(ADT36_A.ADT36A_CR), sd = sd(ADT36_A.ADT36A_CR), n = n())
siteTC_as <- siteTC_as[which(!is.na(siteTC_as$test_sessions_v.gender)),]
siteTC_as <- siteTC_as[order(siteTC_as$test_sessions_v.gender),]
siteTC_as <- siteTC_as[order(siteTC_as$agegroup),]
siteTC_as <- siteTC_as[order(siteTC_as$test_sessions.siteid),]
siteTC_as <- siteTC_as[c(5,1:4,6:8,21:22,9:20,23:91,104:105,92:103,106:114,127:128,115:126,129:137,147:148,138:146,149:154),]
names(siteTC_as) <- c("siteID", "agegroup", "gender", "meanTC", "sdTC", "n")

site_mean_sd <- cbind(siteTC[,1:4], siteSP[,3:5])

write.csv(site_mean_sd, "myresults/site_mean_sd.csv",na="", row.names = FALSE)


siteSP_as <- adt36 %>%
  group_by(test_sessions.siteid,agegroup,test_sessions_v.gender) %>%
  summarise(mean = mean(ADT36_A.ADT36A_RTCR), sd = sd(ADT36_A.ADT36A_RTCR), n = n())
siteSP_as <- siteSP_as[which(!is.na(siteSP_as$test_sessions_v.gender)),]
siteSP_as <- siteSP_as[order(siteSP_as$test_sessions_v.gender),]
siteSP_as <- siteSP_as[order(siteSP_as$agegroup),]
siteSP_as <- siteSP_as[order(siteSP_as$test_sessions.siteid),]
siteSP_as <- siteSP_as[c(5,1:4,6:8,21:22,9:20,23:91,104:105,92:103,106:114,127:128,115:126,129:137,147:148,138:146,149:154),]
names(siteSP_as) <- c("siteID", "agegroup", "gender", "meanSP", "sdSP", "n")

site_agesex <- cbind(siteTC_as[,1:5], siteSP_as[,4:6])

write.csv(site_agesex, "myresults/siteagesex_mean_sd.csv",na="", row.names = FALSE)


# * ADT60 ----
adt60 <- adt60[!is.na(adt60$ADT60_A.ADT60_CR),]
adt60$test_sessions_v.dotest <- as.Date(adt60$test_sessions_v.dotest)
adt60$ADT60_A.ADT60_PC <- 100*adt60$ADT60_A.ADT60_CR/60

corrected <- adt60[,c(2,5:6,12:14)]
names(corrected) <- c("BBLID", "Dates", "Sex", "TotalCorrect", "PercentCorrect", "Speed")

# basic accuracy and speed plots (flash vs non-flash)
fit <- lm(TotalCorrect ~ Dates, data=corrected)
fnfTC <- visreg(fit, "Dates", ylab = "Score (out of 60)", main= "Accuracy on ADT60 over time")

fit <- lm(PercentCorrect ~ Dates, data=corrected)
fnfPC <- visreg(fit, "Dates", ylab = "Score (as percentage)", main= "Accuracy (percentage) on ADT60 over time")

fit <- lm(Speed ~ Dates, data=corrected)
fnfSP <- visreg(fit, "Dates", ylab = "Speed", main= "Speed on ADT60 over time")
# still need to figure "xvar" out 


# stats
fnfTC <- adt60 %>%
  group_by(flash,test_sessions_v.gender) %>%
  summarise(mean = mean(ADT60_A.ADT60_CR), sd = sd(ADT60_A.ADT60_CR), n = n())  # this test was only administered between 2009-07-24 and 2010-07-16

fnfPC <- adt60 %>%
  group_by(flash,test_sessions_v.gender) %>%
  summarise(mean = mean(ADT60_A.ADT60_PC), sd = sd(ADT60_A.ADT60_PC), n = n())

fnfSP <- adt60 %>%
  group_by(flash,test_sessions_v.gender) %>%
  summarise(mean = mean(ADT60_A.ADT60_RTCR), sd = sd(ADT60_A.ADT60_RTCR), n = n())

adt60_flash_meanSD <- cbind(fnfTC[-5],fnfPC[3:4])
adt60_flash_meanSD <- cbind(adt60_flash_meanSD,fnfSP[3:4])
names(adt60_flash_meanSD) <- c("Flash", "Sex", "meanTC", "sdTC", "meanPC", "sdPC", "meanSP", "sdSP")


write.csv(adt60_flash_meanSD,"myresults/adt60_fnf_mean_sd.csv",na="")



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
  var <- str_replace_all(age, "[^[:alnum:]]", "")
  
  agecorrected <- adt36[which(adt36$agegroup == age),c(2:4,6:7,12:14)]
  names(agecorrected) <- c("BBLID", "Age", "AgeGroup", "Date", "Sex", "TotalCorrect", "PercentCorrect", "Speed")
  
  fit <- lm(TotalCorrect ~ Date*Sex, data=agecorrected)
  agesexTC <- visreg(fit, "Date", by= "Sex", overlay =T, ylab = "Score (out of 36)", main = paste("Sex Differences in ADT36 Accuracy of Participants Ages", age, "Over Time"))
  assign(paste0("sexTC", var), agesexTC)
  
  fit <- lm(PercentCorrect ~ Date*Sex, data=agecorrected)
  agesexPC <- visreg(fit, "Date", by= "Sex", overlay =T, ylab = "Score (as percentage)", main = paste("Sex Differences in ADT36 Accuracy (age percentage) of Participants Ages", age, "Over Time"))
  assign(paste0("sexTC", var), agesexPC)
  
  fit <- lm(Speed ~ Date*Sex, data=agecorrected)
  agesexSP <- visreg(fit, "Date", by= "Sex", overlay =T, ylab = "Speed", main = paste("Sex Differences in ADT36 Speed of Participants Ages", age, "Over Time"))
  assign(paste0("sexTC", var), agesexSP)
  
  # testing to see if plot title and legend overlap (they do rn)
  # png(filename = "letsseebaby.png", width=1000,height=800)
  # visreg(fit, "Date", by= "Sex", overlay =T, ylab = "Score (as percentage)", main = paste("Sex Differences in ADT36 Accuracy (age percentage) of Participants Ages", age, "Over Time"))
  # dev.off() 
}

# stats
agesexTC <- adt36 %>%
  group_by(agegroup,test_sessions_v.gender) %>%
  summarise(mean = mean(ADT36_A.ADT36A_CR), sd = sd(ADT36_A.ADT36A_CR))
agesexTC <- agesexTC[-7,] # 7th row produces NA row for 14-15 range

agesexSP <- adt36 %>%
  group_by(agegroup,test_sessions_v.gender) %>%
  summarise(mean = mean(ADT36_A.ADT36A_RTCR), sd = sd(ADT36_A.ADT36A_RTCR))
agesexSP <- agesexSP[-7,]

agesex_mean_sd <- cbind(agesexTC,agesexSP[,3:4])
names(agesex_mean_sd)[2:6] <- c("gender", "mean_TC", "sd_TC", "mean_SP", "sd_SP")
agesex_mean_sd <- agesex_mean_sd[c(13:14,1:12),]
agesex_mean_sd <- agesex_mean_sd[order(agesex_mean_sd$gender),]

write.csv(agesex_mean_sd, "myresults/agesex_mean_sd.csv",na="")





# site differences
sites <- sort(unique(adt36$test_sessions.siteid))
sites <- sites[which(sites != "ImmuSili")] # exclude LiBI and ImmuSili for missing data
sites <- sites[which(sites != "LiBI")]

for (site in sites) {
  sitecorrected <- adt36[which(adt36$test_sessions.siteid == site),c(1:4,6:7,12:14)]
  names(sitecorrected) <- c("SiteID", "BBLID", "Age", "AgeGroup", "Date", "Sex", "TotalCorrect", "PercentCorrect", "Speed") 
  
  fit <- lm(TotalCorrect ~ Date*Sex, data=sitecorrected)
  sitesexTC <- visreg(fit, "Date", by= "Sex", overlay =T, ylab = "Score (out of 36)", gg=T, main = paste("Sex Differences in ADT36 Accuracy of Participants at the", site, " site Over Time"))
  assign(paste0(site, "_TC"), sitesexTC)
  
  fit <- lm(PercentCorrect ~ Date*Sex, data=sitecorrected)
  sitesexPC <- visreg(fit, "Date", by= "Sex", overlay =T, gg=TRUE, ylab = "Score (as percentage)", main = paste("Sex Differences in ADT36 Accuracy (age percentage) of Participants at the", site, "site Over Time"))
  assign(paste0(site, "_PC"), sitesexPC)
  
  fit <- lm(Speed ~ Date*Sex, data=sitecorrected)
  sitesexSP <- visreg(fit, "Date", by= "Sex", overlay =T, ylab = "Speed", main = paste("Sex Differences in ADT36 Speed of Participants at the", site, "site Over Time"))
  assign(paste0(site, "_SP"), sitesexSP)
} 



# stats analysis for site differences
siteTC <- adt36 %>%
  group_by(test_sessions.siteid,test_sessions_v.gender) %>%
  summarise(mean = mean(ADT36_A.ADT36A_CR), sd = sd(ADT36_A.ADT36A_CR), n = n())
siteTC <- siteTC[which(!is.na(siteTC$test_sessions_v.gender)),]   # LiBI has no age or sex data (missing on this table) and ImmuSili only has one (F) data point
names(siteTC) <- c("siteID", "gender", "meanTC", "sdTC", "n")

siteSP <- adt36 %>%
  group_by(test_sessions.siteid,test_sessions_v.gender) %>%
  summarise(mean = mean(ADT36_A.ADT36A_RTCR), sd = sd(ADT36_A.ADT36A_RTCR), n = n())
siteSP <- siteSP[which(!is.na(siteSP$test_sessions_v.gender)),]
names(siteSP) <- c("siteID", "gender", "meanSP", "sdSP", "n")

siteTC_as <- adt36 %>%
  group_by(test_sessions.siteid,agegroup,test_sessions_v.gender) %>%
  summarise(mean = mean(ADT36_A.ADT36A_CR), sd = sd(ADT36_A.ADT36A_CR), n = n())
siteTC_as <- siteTC_as[which(!is.na(siteTC_as$test_sessions_v.gender)),]
siteTC_as <- siteTC_as[order(siteTC_as$test_sessions_v.gender),]
siteTC_as <- siteTC_as[order(siteTC_as$agegroup),]
siteTC_as <- siteTC_as[order(siteTC_as$test_sessions.siteid),]
siteTC_as <- siteTC_as[c(5,1:4,6:8,21:22,9:20,23:91,104:105,92:103,106:114,127:128,115:126,129:137,147:148,138:146,149:154),]
names(siteTC_as) <- c("siteID", "agegroup", "gender", "meanTC", "sdTC", "n")

site_mean_sd <- cbind(siteTC[,1:4], siteSP[,3:5])

write.csv(site_mean_sd, "myresults/site_mean_sd.csv",na="", row.names = FALSE)


siteSP_as <- adt36 %>%
  group_by(test_sessions.siteid,agegroup,test_sessions_v.gender) %>%
  summarise(mean = mean(ADT36_A.ADT36A_RTCR), sd = sd(ADT36_A.ADT36A_RTCR), n = n())
siteSP_as <- siteSP_as[which(!is.na(siteSP_as$test_sessions_v.gender)),]
siteSP_as <- siteSP_as[order(siteSP_as$test_sessions_v.gender),]
siteSP_as <- siteSP_as[order(siteSP_as$agegroup),]
siteSP_as <- siteSP_as[order(siteSP_as$test_sessions.siteid),]
siteSP_as <- siteSP_as[c(5,1:4,6:8,21:22,9:20,23:91,104:105,92:103,106:114,127:128,115:126,129:137,147:148,138:146,149:154),]
names(siteSP_as) <- c("siteID", "agegroup", "gender", "meanSP", "sdSP", "n")

site_agesex <- cbind(siteTC_as[,1:5], siteSP_as[,4:6])

write.csv(site_agesex, "myresults/siteagesex_mean_sd.csv",na="", row.names = FALSE)



