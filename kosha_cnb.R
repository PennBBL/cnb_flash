# This script is for the project Kosha talked to me about on 7/20/21, looking at 
# how participants' accuracy and speed measures have changed over time for each
# task specifically looking for three different things:
# 1) is there a big difference between flash and non-flash versions
# 2) age and sex differences
# 3) site differences
# 
# 07.22.21 Akira Di Sandro

# load packages ----
library(ggplot2)
library(psych)



# load data ----
bigcnb <- read.csv("cnb_dump_15july2021.csv", na=c("",".","NA"))

# separate into individual tasks ----
adt36 <- cbind(bigcnb[,c(2,5:8,11,15:16)], bigcnb[,grepl("ADT36_A.", colnames(bigcnb), fixed = TRUE)])
adt60 <- cbind(bigcnb[,c(2,5:8,11,15:16)], bigcnb[,grepl("ADT60_A.", colnames(bigcnb), fixed = TRUE)])

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
adt36 <- adt36[rowSums(is.na(adt36[,9:ncol(adt36)])) < (ncol(adt36)-9),]
adt36$test_sessions_v.dotest <- as.Date(adt36$test_sessions_v.dotest)
TC <- adt36$ADT36_A.ADT36A_CR
PC <- 100*TC/36
SP <- adt36$ADT36_A.ADT36A_RTCR

# basic accuracy and speed plots (flash vs non-flash)
flash <- adt36[which(adt36$test_sessions_v.dotest < as.Date("2021-01-13")),]
noflash <- adt36[which(adt36$test_sessions_v.dotest >= as.Date("2021-01-13")),]  # this line and the line above are only temporary. unfortunately, it's not as clear cut

fnfPC <- ggplot(adt36, aes(x=test_sessions_v.dotest, y=PC)) +     # i'm p sure i still have to edit this cause there are multiple participants per days and i need to average out the scores per day before graphing
  geom_line(color="dark blue") +
  geom_vline(xintercept = as.Date("2021-01-13"), color = "dark red") +
  labs(x="Date of Test",
       y="Percent Correct",
       title = "Participants Accuracy by Percentage on the ADT36 Over Time") + 
  theme(plot.margin=unit(c(1,2,1.5,1.2),"cm"))

fnfPC

fnfTC <- ggplot(adt36, aes(x=test_sessions_v.dotest, y=TC)) +
  geom_line(color="dark blue") +
  geom_vline(xintercept = as.Date("2021-01-13"), color = "dark red") +
  labs(x="Date of Test",
       y="Total Correct",
       title = "Participants Accuracy on the ADT36 Over Time") + 
  theme(plot.margin=unit(c(1,2,1.5,1.2),"cm"))

fnfTC

fnfSP <- ggplot(adt36, aes(x=test_sessions_v.dotest, y=SP)) +
  geom_line(color="dark blue") +
  geom_vline(xintercept = as.Date("2021-01-13"), color = "dark red") +
  labs(x="Date of Test",
       y="Speed",
       title = "Participants Speed on the ADT36 Over Time") + 
  theme(plot.margin=unit(c(1,2,1.5,1.2),"cm"))

fnfSP



# age and sex differences
adt36 <- adt36[order(adt36$test_sessions_v.age),]

agegroup <- c(rep(NA,nrow(adt36)))
for (i in 1:nrow(adt36)) {
  if (adt36$test_sessions_v.age[i] <= 18) {
    agegroup[i] <- "0-18"
  }
  else if (18 < adt36$test_sessions_v.age[i] & adt36$test_sessions_v.age[i] <= 25) {
    agegroup[i] <- "19-25"
  }
  else if (25 < adt36$test_sessions_v.age[i] & adt36$test_sessions_v.age[i] <= 35) {
    agegroup[i] <- "26-35"
  }
  else if (35 < adt36$test_sessions_v.age[i] & adt36$test_sessions_v.age[i] <= 45) {
    agegroup[i] <- "36-45"
  }
  else if (45 < adt36$test_sessions_v.age[i] & adt36$test_sessions_v.age[i] <= 55) {
    agegroup[i] <- "46-55"
  }
  else if (55 < adt36$test_sessions_v.age[i] & adt36$test_sessions_v.age[i] <= 65) {
    agegroup[i] <- "56-65"
  }
  else if (65 < adt36$test_sessions_v.age[i]) {
    agegroup[i] <- "65+"
  }
}
adt36 <- cbind(adt36, agegroup)
adt36 <- adt36[,c(1:3,54,4:53)]

male <- adt36[which(adt36$test_sessions_v.gender == "M"),]
female <- adt36[which(adt36$test_sessions_v.gender == "F"),] # there are 309 NA for gender and age





# site differences
sites <- sort(unique(adt36$test_sessions.siteid))
sitemeans <- as.data.frame(matrix(NA,nrow = length(sites),ncol = 2))









