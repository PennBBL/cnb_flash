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
library(mgcv)


# Load and organize data ----
bigcnb <- read.csv("bigcnb_14Sep21.csv", na=c("",".","NA",NA))
demos <- read.csv("subjectdemosall_v.csv")

bigcnb$Dotest <- as.Date(bigcnb$Dotest, "%m/%d/%y")
bigcnb$Dob <- as.Date(bigcnb$Dob, "%m/%d/%y")   # anything with Dob > 2013 should be 100 years earlier
bigcnb <- bigcnb[order(bigcnb$Dob, decreasing = T),]

     # move demos stuff here
newdemos <- demos[!duplicated(demos$BBLID),c(1,21)] # from 23356 -> 19367 rows
x <- left_join(bigcnb,newdemos,by=c("Bblid"="BBLID")) # 347,324 rows! yay
x$DOBIRTH <- as.Date(x$DOBIRTH, "%d-%b-%y")
x$newDOB <- if_else(is.na(x$Dob),x$DOBIRTH,x$Dob)
# View(x[,c(2,6,17,18,16)])
# noDOB <- bigcnb$Bblid[is.na(bigcnb$Dob) & bigcnb$flash==0]
# DOBfromdemos <- demos[demos$BBLID %in% noDOB, c(1,21)]
# DOBfromdemos$DOBIRTH <- as.Date(DOBfromdemos$DOBIRTH,"%d-%b-%y")
# x <- left_join(bigcnb,DOBfromdemos,by=c("Bblid"="BBLID"))
# x$newDOB <- if_else(is.na(x$Dob),x$DOBIRTH,x$Dob)
x <- x[,c(1:5,17,7:15)]
names(x)[6]<- "Dob"
bigcnb <- x

temp <- bigcnb[bigcnb$Dob > as.Date("01/01/13", "%m/%d/%y") & !is.na(bigcnb$Dob),]$Dob
temp <- temp %m-% years(100) 
bigcnb[bigcnb$Dob > as.Date("01/01/13", "%m/%d/%y") & !is.na(bigcnb$Dob),]$Dob <- temp
bigcnb[which(bigcnb$Bblid==12344),6] <- bigcnb[which(bigcnb$Bblid==12344),6] %m-% years(100)

bigcnb$age <- floor(as.numeric(bigcnb$Dotest - bigcnb$Dob, units = "weeks")/52.25)
bigcnb$flash <- 0
bigcnb$flash[which(bigcnb$Dotest <= as.Date("2020-12-31"))] <- 1

bigcnb <- bigcnb[order(bigcnb$Datasetid),]
nonflashmissdob <- bigcnb[is.na(bigcnb$Dob) & bigcnb$flash==0,] # 519 post demos merge, 5535 pre demos merge

# * Separate into test versions ----
ADT36_A <- bigcnb[bigcnb$Version == "ADT36_A" & !is.na(bigcnb$Version) & !is.na(bigcnb$Accuracy),]
ADT60_A <- bigcnb[bigcnb$Version == "ADT60_A" & !is.na(bigcnb$Version) & !is.na(bigcnb$Accuracy),]

AIM <- bigcnb[bigcnb$Version == "AIM" & !is.na(bigcnb$Version) & !is.na(bigcnb$Accuracy),]

CPF_A <- bigcnb[bigcnb$Version == "CPF_A" & !is.na(bigcnb$Version) & !is.na(bigcnb$Accuracy),]
CPF_B <- bigcnb[bigcnb$Version == "CPF_B" & !is.na(bigcnb$Version) & !is.na(bigcnb$Accuracy),]
CPFD_A <- bigcnb[bigcnb$Version == "CPFD_A" & !is.na(bigcnb$Version) & !is.na(bigcnb$Accuracy),]
CPFD_B <- bigcnb[bigcnb$Version == "CPFD_B" & !is.na(bigcnb$Version) & !is.na(bigcnb$Accuracy),]

ER40_A <- bigcnb[bigcnb$Version == "ER40_A" & !is.na(bigcnb$Version) & !is.na(bigcnb$Accuracy),]
ER40_C <- bigcnb[bigcnb$Version == "ER40_C" & !is.na(bigcnb$Version) & !is.na(bigcnb$Accuracy),]
ER40_D <- bigcnb[bigcnb$Version == "ER40_D" & !is.na(bigcnb$Version) & !is.na(bigcnb$Accuracy),]

GNG150 <- bigcnb[bigcnb$Version == "GNG150" & !is.na(bigcnb$Version) & !is.na(bigcnb$Accuracy),]

KCPW_A <- bigcnb[bigcnb$Version == "KCPW_A" & !is.na(bigcnb$Version) & !is.na(bigcnb$Accuracy),]
KCPWD_A <- bigcnb[bigcnb$Version == "KCPWD_A" & !is.na(bigcnb$Version) & !is.na(bigcnb$Accuracy),]

KSPVRT_A <- bigcnb[bigcnb$Version == "KSPVRT_A" & !is.na(bigcnb$Version) & !is.na(bigcnb$Accuracy),]
KSPVRT_B <- bigcnb[bigcnb$Version == "KSPVRT_B" & !is.na(bigcnb$Version) & !is.na(bigcnb$Accuracy),]
KSPVRT_D <- bigcnb[bigcnb$Version == "KSPVRT_D" & !is.na(bigcnb$Version) & !is.na(bigcnb$Accuracy),]

MEDF36_A <- bigcnb[bigcnb$Version == "MEDF36_A" & !is.na(bigcnb$Version) & !is.na(bigcnb$Accuracy),]
MEDF60_A <- bigcnb[bigcnb$Version == "MEDF60_A" & !is.na(bigcnb$Version) & !is.na(bigcnb$Accuracy),]

MPRACT <- bigcnb[bigcnb$Version == "MPRACT" & !is.na(bigcnb$Version) & !is.na(bigcnb$Accuracy),]

PCET_A <- bigcnb[bigcnb$Version == "PCET_A" & !is.na(bigcnb$Version) & !is.na(bigcnb$Accuracy),]
SPCET_A <- bigcnb[bigcnb$Version == "SPCET_A" & !is.na(bigcnb$Version) & !is.na(bigcnb$Accuracy),]

PMAT18_B <- bigcnb[bigcnb$Version == "PMAT18_B" & !is.na(bigcnb$Version) & !is.na(bigcnb$Accuracy),]
PMAT24_A <- bigcnb[bigcnb$Version == "PMAT24_A" & !is.na(bigcnb$Version) & !is.na(bigcnb$Accuracy),]
PMAT24_B <- bigcnb[bigcnb$Version == "PMAT24_B" & !is.na(bigcnb$Version) & !is.na(bigcnb$Accuracy),]

SCTAP <- bigcnb[bigcnb$Version == "SCTAP" & !is.na(bigcnb$Version) & !is.na(bigcnb$Accuracy),]

SLNB2_90 <- bigcnb[bigcnb$Version == "SLNB2_90" & !is.na(bigcnb$Version) & !is.na(bigcnb$Accuracy),]

SPLOT12 <- bigcnb[bigcnb$Version == "SPLOT12" & !is.na(bigcnb$Version) & !is.na(bigcnb$Accuracy),]
VSPLOT15 <- bigcnb[bigcnb$Version == "VSPLOT15" & !is.na(bigcnb$Version) & !is.na(bigcnb$Accuracy),]
VSPLOT24 <- bigcnb[bigcnb$Version == "VSPLOT24" & !is.na(bigcnb$Version) & !is.na(bigcnb$Accuracy),]

SPVRT_A <- bigcnb[bigcnb$Version == "SPVRT_A" & !is.na(bigcnb$Version) & !is.na(bigcnb$Accuracy),]

SVOLT_A <- bigcnb[bigcnb$Version == "SVOLT_A" & !is.na(bigcnb$Version) & !is.na(bigcnb$Accuracy),]
SVOLTD_A <- bigcnb[bigcnb$Version == "SVOLTD_A" & !is.na(bigcnb$Version) & !is.na(bigcnb$Accuracy),]

texts <- sort(unique(bigcnb$Version))
# what tests don't have any flash==0 babies
notthese <- c()
tests <- mget(texts)
for (i in 1:length(texts)){
  test <- tests[[i]]
  if (length(unique(test$flash))!=2){
    notthese <- c(notthese,texts[i])
  }
}
'%!in%' <- function(x,y)!('%in%'(x,y))
# notthese <- c("ADT60_A","CPFD_A","CPFD_B","ER40_A","ER40_C","KCPWD_A","KSPVRT_A","KSPVRT_B",
#               "MEDF60_A","MPRACT","PMAT18_B","PMAT24_B","SCTAP","SPCET_A","SPLOT12","SPVRT_A",
#               "SVOLTD_A","VSPLOT24")
notthese <- c()
for (i in 1:length(texts)){   # catch the tests that don't have enough f==0 
  test <- tests[[i]]
  newtest <- test[!is.na(test$Accuracy) & !is.na(test$age) & !is.na(test$Gender),]
  if (length(unique(newtest$flash))!=2){
    notthese <- c(notthese,texts[i])
  } else if (nrow(test[test$flash==0,])<5) {
    notthese <- c(notthese,texts[i])
  }
}
# adding SPLOT12 manually for now
notthese <- c(notthese, "SPLOT12")
texts <- texts[texts %!in% notthese] # getting rid of the tests that only have flash, no non-flash subjects after correcting for the existence of age and sex
tests <- mget(texts)

# Models and Plotting ----

# 9.16.21 implementing what I talked about with Kosha and Tyler



# * two-factor FA, waiting itemwise data for this ----


# * t-tests ----

# general code for t-tests
for (i in 1:length(texts)) {
  test <- tests[[i]]
  
  # alldates
  fit <- gam(Accuracy ~ s(age) + Gender, data = test)
  
  # save summary of this model as a variable
  name <- paste0(texts[i],"fitAD")
  assign(name,summary(fit))
  
  # is this plot necessary?
  visreg(fit,"age", by="Gender")
  
  res <- scale(resid(fit))   # scaled residuals
  newtest <- test[!is.na(test$Accuracy) & !is.na(test$age) & !is.na(test$Gender),]
  nflash <- newtest[newtest$flash==0,]
  nfrownames <- row.names(nflash)
  
  res <- as.data.frame(res)
  rownames(res) <- rownames(newtest)
  names(res) <- "residuals"
  res$flash <- 1
  for (j in 1:nrow(res)) {
    if (row.names(res[j,]) %in% nfrownames) {
      res[j,]$flash <- 0
    } else{}
  }
  
  ttest <- t.test(res$residuals~res$flash)
  # save ttest as a variable
  name <- paste0(texts[i],"ttestAD")
  assign(name,ttest)
  
  # effect sizes
  AD0 <- ttest$estimate[[1]]
  AD1 <- ttest$estimate[[2]]
  effsize <- abs(AD0-AD1)
  name <- paste0(texts[i],"effsizeAD")
  assign(name,effsize)
  
  
  # lastyear
  cutoff <- as.Date("2019-12-31")
  lastyear <- test[test$Dotest >= cutoff,]
  fit <- gam(Accuracy ~ s(age) + Gender, data = lastyear)
  
  # save summary of this model as a variable
  name <- paste0(texts[i],"fitLY")
  assign(name,summary(fit))
  
  visreg(fit,"age", by="Gender")
  
  res <- scale(resid(fit))   # scaled residuals
  newtest <- lastyear[!is.na(lastyear$Accuracy) & !is.na(lastyear$age) & !is.na(lastyear$Gender),]
  nflash <- newtest[newtest$flash==0,]
  nfrownames <- row.names(nflash)
  
  res <- as.data.frame(res)
  rownames(res) <- rownames(newtest)
  names(res) <- "residuals"
  res$flash <- 1
  for (j in 1:nrow(res)) {
    if (row.names(res[j,]) %in% nfrownames) {
      res[j,]$flash <- 0
    } else{}
  }
  
  ttest <- t.test(res$residuals~res$flash)
  # save ttest as a variable
  name <- paste0(texts[i],"ttestLY")
  assign(name,ttest)
  
  # effect sizes
  LY0 <- ttest$estimate[[1]]
  LY1 <- ttest$estimate[[2]]
  effsize <- abs(LY0-LY1)
  name <- paste0(texts[i],"effsizeLY")
  assign(name,effsize)
}





# * correlations ----







# * flash/non-flash intra-subject correlations ----












# old script only used siteid, bblid, age, dotest, gender, flash, CR, RTCR as the important columns
# i can do that again here, or I can just call on the columns i need, leaving the 
# unneeded ones in still.

# * CPF_A (40 total) ----

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

demos <- read.csv("subjectdemosall_v.csv")
sum(is.na(demos$AGE_INTAKE))
sum(is.na(demos$DOBIRTH))
sum(is.na(demos$DOINTAKE))

noDOB <- SVOLT_A$Bblid[is.na(SVOLT_A$Dob) & SVOLT_A$flash==0]

DOBfromdemos <- demos[demos$BBLID %in% noDOB, c(1,21)] # extract bblid and dob from demos
DOBfromdemos <- distinct(DOBfromdemos)
DOBfromdemos$DOBIRTH <- as.Date(DOBfromdemos$DOBIRTH,"%d-%b-%y")

x <- left_join(SVOLT_A,DOBfromdemos,by=c("Bblid"="BBLID"))
x$newDOB <- ifelse(is.na(x$Dob),x$DOBIRTH,x$Dob)
x <- x%>%
  mutate(newdob = if_else(is.na(Dob),DOBIRTH,Dob))

# comparing bigcnb and demos DOB directly
x <- left_join(bigcnb,demos[,c(1,21)],by=c("Bblid"="BBLID"))

# this is giving me 388,323 obs. when I should only have 247,324
noDOB <- bigcnb$Bblid[is.na(bigcnb$Dob) & bigcnb$flash==0]
DOBfromdemos <- demos[demos$BBLID %in% noDOB, c(1,21)]
DOBfromdemos <- distinct(DOBfromdemos)
DOBfromdemos$DOBIRTH <- as.Date(DOBfromdemos$DOBIRTH,"%d-%b-%y")
x <- left_join(bigcnb,DOBfromdemos,by=c("Bblid"="BBLID"))
x$newDOB <- if_else(is.na(x$Dob),x$DOBIRTH,x$Dob)
View(x[,c(6,18:19)])


# checking to see if my code in the beginning is actually adding any DOB from demos
bigcnb <- read.csv("bigcnb_14Sep21.csv", na=c("",".","NA",NA))
demos <- read.csv("subjectdemosall_v.csv")

bigcnb$Dotest <- as.Date(bigcnb$Dotest, "%m/%d/%y")
bigcnb$Dob <- as.Date(bigcnb$Dob, "%m/%d/%y")   # anything with Dob > 2013 should be 100 years earlier
bigcnb <- bigcnb[order(bigcnb$Dob, decreasing = T),]

temp <- bigcnb[bigcnb$Dob > as.Date("01/01/13", "%m/%d/%y") & !is.na(bigcnb$Dob),]$Dob
temp <- temp %m-% years(100) 
bigcnb[bigcnb$Dob > as.Date("01/01/13", "%m/%d/%y") & !is.na(bigcnb$Dob),]$Dob <- temp
bigcnb[which(bigcnb$Bblid==12344),6] <- bigcnb[which(bigcnb$Bblid==12344),6] %m-% years(100)

bigcnb$flash <- 0
bigcnb$flash[which(bigcnb$Dotest <= as.Date("2020-12-31"))] <- 1

oldcnb <- bigcnb
oldcnb$age <- floor(as.numeric(bigcnb$Dotest - bigcnb$Dob, units = "weeks")/52.25)

oldcnb <- oldcnb[order(oldcnb$Datasetid),]

# check distinct BBLids in demos
demosbblid <- sort(unique(demos$BBLID))    # 19366 
bblidsum <- sum(!is.na(demos$BBLID))       # 23355
repbblid <- bblidsum - length(demosbblid)  # 3989

noDOB <- bigcnb$Bblid[is.na(bigcnb$Dob) & bigcnb$flash==0]
DOBfromdemos <- demos[demos$BBLID %in% noDOB, c(1,21)]
DOBfromdemos$DOBIRTH <- as.Date(DOBfromdemos$DOBIRTH,"%d-%b-%y")
x <- left_join(bigcnb,DOBfromdemos,by=c("Bblid"="BBLID"))
x$newDOB <- if_else(is.na(x$Dob),x$DOBIRTH,x$Dob)
View(x[,c(6,17,18)])
x <- x[,c(1:5,18,7:16)]
names(x)[6]<- "Dob"
bigcnb <- x

bigcnb$age <- floor(as.numeric(bigcnb$Dotest - bigcnb$Dob, units = "weeks")/52.25)
bigcnb <- bigcnb[order(bigcnb$Datasetid),]

oldDOB <- sort(unique(oldcnb$Dob))  # 10912
newDOB <- sort(unique(bigcnb$Dob))  # 10948 (difference of 36)

old0DOB <- sort(unique(oldcnb[oldcnb$flash==0,6]))  # 1370
new0DOB <- sort(unique(bigcnb[bigcnb$flash==0,6]))  # 1561 (difference of 191)

oldbbl <- sort(unique(oldcnb$Bblid))
newbbl <- sort(unique(bigcnb$Bblid)) # both 12623



bigcnb <- read.csv("bigcnb_14Sep21.csv", na=c("",".","NA",NA))
demos <- read.csv("subjectdemosall_v.csv")

bigcnb$Dotest <- as.Date(bigcnb$Dotest, "%m/%d/%y")
bigcnb$Dob <- as.Date(bigcnb$Dob, "%m/%d/%y")   # anything with Dob > 2013 should be 100 years earlier
bigcnb <- bigcnb[order(bigcnb$Dob, decreasing = T),]

temp <- bigcnb[bigcnb$Dob > as.Date("01/01/13", "%m/%d/%y") & !is.na(bigcnb$Dob),]$Dob
temp <- temp %m-% years(100) 
bigcnb[bigcnb$Dob > as.Date("01/01/13", "%m/%d/%y") & !is.na(bigcnb$Dob),]$Dob <- temp
bigcnb[which(bigcnb$Bblid==12344),6] <- bigcnb[which(bigcnb$Bblid==12344),6] %m-% years(100)

bigcnb$flash <- 0
bigcnb$flash[which(bigcnb$Dotest <= as.Date("2020-12-31"))] <- 1

oldcnb <- bigcnb
oldcnb$age <- floor(as.numeric(bigcnb$Dotest - bigcnb$Dob, units = "weeks")/52.25)

oldcnb <- oldcnb[order(oldcnb$Datasetid),]

# check distinct BBLids in demos
demosbblid <- sort(unique(demos$BBLID))    # 19366 
bblidsum <- sum(!is.na(demos$BBLID))       # 23355
repbblid <- bblidsum - length(demosbblid)  # 3989

# getting rid of duplicate BBLids in demos
newdemos <- demos[!duplicated(demos$BBLID),c(1,21)] # from 23356 -> 19367 rows
x <- left_join(bigcnb,newdemos,by=c("Bblid"="BBLID")) # 347,324 rows! yay
x$DOBIRTH <- as.Date(x$DOBIRTH, "%d-%b-%y")
x$newDOB <- if_else(is.na(x$Dob),x$DOBIRTH,x$Dob)
View(x[,c(2,6,17,18,16)])
newnonflash <- x[is.na(x$Dob) & !is.na(x$DOBIRTH) & x$flash==0,] # 5016 birthdays added to nonfash
nobbliddup <- newnonflash[!duplicated(newnonflash$Bblid),] # demos file has added DOBs to 238 distinct non-flash BBLids
x <- x[,c(1:5,18,7:16)]
names(x)[6]<- "Dob"
bigcnb <- x












