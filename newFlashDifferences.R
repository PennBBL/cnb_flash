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
bigcnb <- read.csv("bigcnb_14Sep21.csv", na=c("",".","NA",NA))

bigcnb$Dotest <- as.Date(bigcnb$Dotest, "%m/%d/%y")
bigcnb$Dob <- as.Date(bigcnb$Dob, "%m/%d/%y")   # anything with Dob > 2013 should be 100 years earlier
bigcnb <- bigcnb[order(bigcnb$Dob, decreasing = T),]

temp <- bigcnb[bigcnb$Dob > as.Date("01/01/13", "%m/%d/%y") & !is.na(bigcnb$Dob),]$Dob
temp <- temp %m-% years(100) 
bigcnb[bigcnb$Dob > as.Date("01/01/13", "%m/%d/%y") & !is.na(bigcnb$Dob),]$Dob <- temp
bigcnb[which(bigcnb$Bblid==12344),6] <- bigcnb[which(bigcnb$Bblid==12344),6] %m-% years(100)

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

# old script only used siteid, bblid, age, dotest, gender, flash, CR, RTCR as the important columns
# i can do that again here, or I can just call on the columns i need, leaving the 
# unneeded ones in still.

# * CPF_A, 40 total ----

CPFAnoage <- CPF_A[which(is.na(CPF_A$age)),] # there are some missing ages
CPF_A <- CPF_A[!is.na(CPF_A$Accuracy),]

firstday <- min(CPF_A$Dotest)
lastday <- max(CPF_A$Dotest)
numdates <- as.numeric(CPF_A$Dotest)
numdates <- numdates - min(numdates)
CPF_A$Dotest <- numdates

genfit <- lm(Accuracy ~ Dotest+flash, data=CPF_A)
summary(genfit)
visreg(genfit, "Dotest", by= "flash", overlay =T, gg=T) + 
  theme_bw() +
  theme(legend.position = "right",
        plot.margin=unit(c(1,2,1.5,1.2),"cm")) +
  labs(x = paste0("Date of test (with ", firstday, " as 0)"),
       title = "Accuracy on CPF_A") +
  scale_y_continuous(name = "Score (out of 40)", breaks = seq(10,42, by=5)) 

# this one did not look ass well
# fit <- lm(Accuracy ~ Dotest*flash, data=CPF_A)
# summary(fit)
# visreg(fit, "Dotest", by= "flash", overlay =T, ylab = "Score (out of 60)", xlab = "Date of test", main = "Accuracy on CPF_A")

flashfit <- lm(Accuracy ~ flash, data=CPF_A)
summary(flashfit)
ad <- visreg(flashfit, "flash", gg=T) +
  theme_bw() + 
  theme(plot.margin=unit(c(1,2,1.5,1.2),"cm")) +
  labs(y = "Score (out of 40)",
       title = "Accuracy on CPF_A depending on Flash/non-Flash") +
  scale_x_continuous(name = "Date of test", breaks = seq(0,1,by=1))

png(filename = "plots/CPF_A_general_alldates.png", height = 1000, width = 1000,res=160)
ad
dev.off()


# flashfit but with only the last year of flash

cutoff <- as.numeric(as.Date("2019-12-31"))-as.numeric(firstday)
lastyear <- CPF_A[CPF_A$Dotest >= cutoff,]

flashfit <- lm(Accuracy ~ flash, data=lastyear)
summary(flashfit)
ly <- visreg(flashfit, "flash", gg=T) +
  theme_bw() + 
  theme(plot.margin=unit(c(1,2,1.5,1.2),"cm")) +
  labs(y = "Score (out of 40)",
       title = "Accuracy on CPF_A depending on Flash/non-Flash (only last year of Flash)") +
  scale_x_continuous(name = "Flash", breaks = seq(0,1,by=1))

png(filename = "plots/CPF_A_general_lastyear.png", height = 1000, width = 1000,res=160)
ly
dev.off()





# looking at age-sex interaction

agesexfit <- lm(Accuracy ~ flash + age + Gender, data=CPF_A)
summary(agesexfit)
v1 <- visreg(agesexfit, "flash", by= "age", cond = list(Gender="M"), breaks = 5, overlay =T, gg=T) +
  theme_bw() +
  theme(plot.margin=unit(c(1,2,1.5,1.2),"cm")) +
  labs(y = "Score (out of 40)", 
       title = "Male Accuracy on CPF_A by age") +
  scale_x_continuous(name = "Flash", breaks = seq(0,1,by=1)) +
  scale_y_continuous(name = "Score (out of 40)", breaks = seq(10,42, by=5))
v2 <- visreg(agesexfit, "flash", by= "age", cond = list(Gender="F"), breaks = 5, overlay =T, gg=T) +
  theme_bw() +
  theme(plot.margin=unit(c(1,2,1.5,1.2),"cm")) +
  labs(y = "Score (out of 40)",
       title = "Female Accuracy on CPF_A by age") +
  scale_x_continuous(name = "Flash", breaks = seq(0,1,by=1)) +
  scale_y_continuous(name = "Score (out of 40)", breaks = seq(10,42, by=5))

png(filename = "plots/CPF_A_agesex_alldates_male.png", height = 1000, width = 500,res=100)
v1
dev.off()

png(filename = "plots/CPF_A_agesex_alldates_female.png", height = 1000, width = 500,res=100)
v2
dev.off()


# trying to put the above two plots in the same display
v <- visregList(visreg(agesexfit, "flash", by= "age", cond = list(Gender="M"), breaks = 5, layout = c(5,1), ylab = "Score (out of 40)", xlab = "Flash?", main = "Male Accuracy on CPF_A by age", plot=FALSE),
                visreg(agesexfit, "flash", by= "age", cond = list(Gender="F"), breaks = 5, layout = c(5,1), ylab = "Score (out of 40)", xlab = "Flash?", main = "Female Accuracy on CPF_A by age", plot=FALSE),
                labels=c("Male", "Female"), collapse=TRUE)
plot(v, ylab="Score (out of 40)")

visreg(agesexfit, "flash", by= "age", cond = list(Gender="M"), breaks = 5, layout = c(5,1), ylab = "Score (out of 40)", xlab = "Flash?", main = "Male Accuracy on CPF_A by age")
visreg(agesexfit, "flash", by= "age", cond = list(Gender="F"), breaks = 5, layout = c(5,1), ylab = "Score (out of 40)", xlab = "Flash?", main = "Female Accuracy on CPF_A by age")

# using gg=T to fix x axis ticks
v1p <- visreg(agesexfit, "flash", by= "age", cond = list(Gender="M"), breaks = 5, layout = c(5,1), gg=T) +
  theme_bw() +
  labs(title = "Male Accuracy on CPF_A by age") +
  scale_x_continuous(name = "Flash", breaks = seq(0,1,by=1)) +
  scale_y_continuous(name = "Score (out of 40)", breaks = seq(10,42, by=5))
v2p <- visreg(agesexfit, "flash", by= "age", cond = list(Gender="F"), breaks = 5, layout = c(5,1), gg=T) +
  theme_bw() +
  labs(title = "Female Accuracy on CPF_A by age") +
  scale_x_continuous(name = "Flash", breaks = seq(0,1,by=1)) +
  scale_y_continuous(name = "Score (out of 40)", breaks = seq(10,42, by=5))

png(filename = "plots/CPF_A_agesex_alldates_malepanels.png", height = 500, width = 1000,res=100)
v1p
dev.off()

png(filename = "plots/CPF_A_agesex_alldates_femalepanels.png", height = 500, width = 1000,res=100)
v2p
dev.off()


# trying to make the opposite happen
# looks like I need to make an agegroup variable to make this happen smoother. this would make it more challenging to generalize tho
v <- visregList(visreg(agesexfit, "flash", by= "Gender", cond = list(age=7:18), overlay =T, ylab = "Score (out of 40)", xlab = "Flash?", main = "Accuracy on CPF_A ages 7 to 18", plot=FALSE),
                visreg(agesexfit, "flash", by= "Gender", cond = list(age=19:26), breaks = 2, layout = c(2,1), ylab = "Score (out of 40)", xlab = "Flash?", main = "Accuracy on CPF_A ages 19 to 26", plot=FALSE),
                visreg(agesexfit, "flash", by= "Gender", cond = list(age=27:35), breaks = 2, layout = c(2,1), ylab = "Score (out of 40)", xlab = "Flash?", main = "Accuracy on CPF_A ages 27 to 35", plot=FALSE),
                visreg(agesexfit, "flash", by= "Gender", cond = list(age=36:46), breaks = 2, layout = c(2,1), ylab = "Score (out of 40)", xlab = "Flash?", main = "Accuracy on CPF_A ages 36 to 46", plot=FALSE),
                visreg(agesexfit, "flash", by= "Gender", cond = list(age=47:62), breaks = 2, layout = c(2,1), ylab = "Score (out of 40)", xlab = "Flash?", main = "Accuracy on CPF_A ages 47 to 62", plot=FALSE),
                labels=c("7 to 18", "19 to 26", "27 to 35","36 to 46", "47 to 62"), collapse=TRUE)
plot(v, ylab="Score (out of 40)")

# age-sex but only for the last year of flash
agesexfit <- lm(Accuracy ~ flash + age + Gender, data=lastyear)
summary(agesexfit)

v1ly <- visreg(agesexfit, "flash", by= "age", cond = list(Gender="M"), breaks = 5, overlay =T, gg=T) +
  theme_bw() +
  theme(plot.margin=unit(c(1,2,1.5,1.2),"cm")) +
  labs(y = "Score (out of 40)", 
       title = "Male Accuracy on CPF_A by age") +
  scale_x_continuous(name = "Flash", breaks = seq(0,1,by=1)) +
  scale_y_continuous(name = "Score (out of 40)", breaks = seq(10,42, by=5))
v2ly <- visreg(agesexfit, "flash", by= "age", cond = list(Gender="F"), breaks = 5, overlay =T, gg=T) +
  theme_bw() +
  theme(plot.margin=unit(c(1,2,1.5,1.2),"cm")) +
  labs(y = "Score (out of 40)",
       title = "Female Accuracy on CPF_A by age") +
  scale_x_continuous(name = "Flash", breaks = seq(0,1,by=1)) +
  scale_y_continuous(name = "Score (out of 40)", breaks = seq(10,42, by=5))

png(filename = "plots/CPF_A_agesex_lastyear_male.png", height = 1000, width = 500,res=100)
v1ly
dev.off()

png(filename = "plots/CPF_A_agesex_lastyear_female.png", height = 1000, width = 500,res=100)
v2ly
dev.off()

# lastyear panels
v1ply <- visreg(agesexfit, "flash", by= "age", cond = list(Gender="M"), breaks = 5, layout = c(5,1), gg=T) +
  theme_bw() +
  labs(title = "Male Accuracy on CPF_A by age") +
  scale_x_continuous(name = "Flash", breaks = seq(0,1,by=1)) +
  scale_y_continuous(name = "Score (out of 40)", breaks = seq(10,42, by=5))
v2ply <- visreg(agesexfit, "flash", by= "age", cond = list(Gender="F"), breaks = 5, layout = c(5,1), gg=T) +
  theme_bw() +
  labs(title = "Female Accuracy on CPF_A by age") +
  scale_x_continuous(name = "Flash", breaks = seq(0,1,by=1)) +
  scale_y_continuous(name = "Score (out of 40)", breaks = seq(10,42, by=5))

png(filename = "plots/CPF_A_agesex_lastyear_malepanels.png", height = 500, width = 1000,res=100)
v1ply
dev.off()

png(filename = "plots/CPF_A_agesex_lastyear_femalepanels.png", height = 500, width = 1000,res=100)
v2ply
dev.off()


# looking at threeway interaction plots
asf <- lm(Accuracy ~ flash * age * Gender, data=lastyear)
summary(asf)
visreg(asf, "flash", by= "age", cond = list(Gender="M"), breaks = 5, overlay =T, ylab = "Score (out of 40)", xlab = "Flash?", main = "Male Accuracy on CPF_A by age")
visreg(asf, "flash", by= "age", cond = list(Gender="F"), breaks = 5, overlay =T, ylab = "Score (out of 40)", xlab = "Flash?", main = "Female Accuracy on CPF_A by age")

visreg(asf, "flash", by= "age", cond = list(Gender="M"), breaks = 5, layout = c(5,1), ylab = "Score (out of 40)", xlab = "Flash?", main = "Male Accuracy on CPF_A by age")
visreg(asf, "flash", by= "age", cond = list(Gender="F"), breaks = 5, layout = c(5,1), ylab = "Score (out of 40)", xlab = "Flash?", main = "Female Accuracy on CPF_A by age")



# looking at site differences

sitefit <- lm(Accuracy ~ flash*Siteid, data=CPF_A)
summary(sitefit)
# EFR01, EVOLPSY, LiBI, MOTIVE, PAISA are the only ones with flash interaction (actual data for non-flash)
sites <- c("EFR01", "EVOLPSY", "LiBI", "MOTIVE", "PAISA")
sCPFA <- CPF_A[which(CPF_A$Siteid %in% sites),]
sitefit <- lm(Accuracy ~ flash*Siteid, data=sCPFA)
summary(sitefit)
sites <- visreg(sitefit, "flash", by= "Siteid", gg=T) +
  theme_bw() +
  labs(title = "Site differencess in Accuracy on CPF_A") +
  scale_x_continuous(name = "Flash", breaks = seq(0,1,by=1)) +
  scale_y_continuous(name = "Score (out of 40)", breaks = seq(10,42,by=5))

png(filename = "plots/CPF_A_sites_alldates.png", height = 500, width = 1000,res=100)
sites
dev.off()

sCPFAly <- lastyear[which(lastyear$Siteid %in% sites),]
sitefit <- lm(Accuracy ~ flash*Siteid, data=sCPFAly)
summary(sitefit)
sites <- visreg(sitefit, "flash", by= "Siteid", gg=T) +
  theme_bw() +
  labs(title = "Site differencess in Accuracy on CPF_A (only last year of Flash)") +
  scale_x_continuous(name = "Flash", breaks = seq(0,1,by=1)) +
  scale_y_continuous(name = "Score (out of 40)", breaks = seq(10,42,by=5))

png(filename = "plots/CPF_A_sites_lastyear.png", height = 500, width = 1000,res=100)
sites
dev.off()


















# test ----
agesexfit <- lm(Accuracy ~ flash + age + Gender, data=CPF_A)
summary(agesexfit)
v1 <- visreg(agesexfit, "flash", by= "age", cond = list(Gender="M"), breaks = 5, overlay =T, gg=T) +
  theme_bw() +
  theme(plot.margin=unit(c(1,2,1.5,1.2),"cm")) +
  labs(y = "Score (out of 40)", 
       title = "Male Accuracy on CPF_A by age") +
  scale_x_continuous(name = "Flash", breaks = seq(0,1,by=1))
v2 <- visreg(agesexfit, "flash", by= "age", cond = list(Gender="F"), breaks = 5, overlay =T, gg=T) +
  theme_bw() +
  theme(plot.margin=unit(c(1,2,1.5,1.2),"cm")) +
  labs(y = "Score (out of 40)",
       title = "Female Accuracy on CPF_A by age") +
  scale_x_continuous(name = "Flash", breaks = seq(0,1,by=1))

# trying to put the above two plots in the same display
v <- visregList(v1,
                v2,
                labels=c("Male", "Female"), collapse=TRUE)
plot(v, ylab="Score (out of 40)")





