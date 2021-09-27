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
library(tidyr)
library(reshape2)


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
x <- x[,c(1:5,17,7:15)]
names(x)[6]<- "Dob"
bigcnb <- x

temp <- bigcnb[bigcnb$Dob > as.Date("01/01/13", "%m/%d/%y") & !is.na(bigcnb$Dob),]$Dob
temp <- temp %m-% years(100) 
bigcnb[bigcnb$Dob > as.Date("01/01/13", "%m/%d/%y") & !is.na(bigcnb$Dob),]$Dob <- temp
bigcnb[which(bigcnb$Bblid==12344),6] <- bigcnb[which(bigcnb$Bblid==12344),6] %m-% years(100)

bigcnb$age <- floor(as.numeric(bigcnb$Dotest - bigcnb$Dob, units = "weeks")/52.25)
# fixing the 106 and 107 year olds
temp <- bigcnb[bigcnb$age > 103 & !is.na(bigcnb$age),]$Dob
temp <- temp %m+% years(100) 
bigcnb[bigcnb$age > 103 & !is.na(bigcnb$age),]$Dob <- temp
bigcnb$age <- floor(as.numeric(bigcnb$Dotest - bigcnb$Dob, units = "weeks")/52.25)

bigcnb$flash <- 0
bigcnb$flash[which(bigcnb$Dotest <= as.Date("2020-12-31"))] <- 1

bigcnb <- bigcnb[order(bigcnb$Datasetid),]
# nonflashmissdob <- bigcnb[is.na(bigcnb$Dob) & bigcnb$flash==0,] # 519 post demos merge, 5535 pre demos merge

# * Separate into test versions ----
# check if BART, DIGSYM, TRAIL exist
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

MPRACT <- bigcnb[bigcnb$Version == "MPRACT" & !is.na(bigcnb$Version) & !is.na(bigcnb$Speed),]

PCET_A <- bigcnb[bigcnb$Version == "PCET_A" & !is.na(bigcnb$Version) & !is.na(bigcnb$Accuracy),]
SPCET_A <- bigcnb[bigcnb$Version == "SPCET_A" & !is.na(bigcnb$Version) & !is.na(bigcnb$Accuracy),]

PMAT18_B <- bigcnb[bigcnb$Version == "PMAT18_B" & !is.na(bigcnb$Version) & !is.na(bigcnb$Accuracy),]
PMAT24_A <- bigcnb[bigcnb$Version == "PMAT24_A" & !is.na(bigcnb$Version) & !is.na(bigcnb$Accuracy),]
PMAT24_B <- bigcnb[bigcnb$Version == "PMAT24_B" & !is.na(bigcnb$Version) & !is.na(bigcnb$Accuracy),]

SCTAP <- bigcnb[bigcnb$Version == "SCTAP" & !is.na(bigcnb$Version) & !is.na(bigcnb$Speed),]

SLNB2_90 <- bigcnb[bigcnb$Version == "SLNB2_90" & !is.na(bigcnb$Version) & !is.na(bigcnb$Accuracy),]

SPLOT12 <- bigcnb[bigcnb$Version == "SPLOT12" & !is.na(bigcnb$Version) & !is.na(bigcnb$Accuracy),]
VSPLOT15 <- bigcnb[bigcnb$Version == "VSPLOT15" & !is.na(bigcnb$Version) & !is.na(bigcnb$Accuracy),]
VSPLOT24 <- bigcnb[bigcnb$Version == "VSPLOT24" & !is.na(bigcnb$Version) & !is.na(bigcnb$Accuracy),]

SPVRT_A <- bigcnb[bigcnb$Version == "SPVRT_A" & !is.na(bigcnb$Version) & !is.na(bigcnb$Accuracy),]

SVOLT_A <- bigcnb[bigcnb$Version == "SVOLT_A" & !is.na(bigcnb$Version) & !is.na(bigcnb$Accuracy),]
SVOLTD_A <- bigcnb[bigcnb$Version == "SVOLTD_A" & !is.na(bigcnb$Version) & !is.na(bigcnb$Accuracy),]


texts <- sort(unique(bigcnb$Version))
tests <- mget(texts)
# what tests don't have any flash==0 babies
notthese <- c()
for (i in 1:length(texts)){
  test <- tests[[i]]
  if (length(unique(test$flash))!=2){
    notthese <- c(notthese,texts[i])
  }
}

notthese <- c()
for (i in 1:length(texts)){   # catch the tests that don't have enough f==0 
  test <- tests[[i]]
  newtest <- test[!is.na(test$Accuracy) & !is.na(test$age) & !is.na(test$Gender),]
  if (length(unique(newtest$flash))!=2) {
    notthese <- c(notthese,texts[i])
  } else if (nrow(newtest[newtest$flash==0,])<5) {
    notthese <- c(notthese,texts[i])
  }
}

nottheseSp <- c()
for (i in 1:length(texts)){   # catch the tests that don't have enough f==0 
  test <- tests[[i]]
  newtest <- test[!is.na(test$Speed) & !is.na(test$age) & !is.na(test$Gender),]
  if (length(unique(newtest$flash))!=2) {
    nottheseSp <- c(nottheseSp,texts[i])
  } else if (nrow(newtest[newtest$flash==0,])<5) {
    nottheseSp <- c(nottheseSp,texts[i])             # these ended up being the same as notthese other than MPRACT and SCTAP obviously
  }
}

notthese <- nottheseSp
# adding SPLOT12 manually for now (for both accuracy and speed)
notthese <- c(notthese, "SPLOT12")
'%!in%' <- function(x,y)!('%in%'(x,y))
texts <- texts[texts %!in% notthese] # getting rid of the tests that only have flash, no non-flash subjects after correcting for the existence of age and sex
tests <- mget(texts)

# Models and Plotting ----

# 9.16.21 implementing what I talked about with Kosha and Tyler

# * two-factor FA, waiting itemwise data for this ----


# * t-tests ----

# general code for t-tests
textsAcc <- texts[texts %!in% c("MPRACT","SCTAP")]
testsAcc <- mget(textsAcc)
cutoff <- as.Date("2019-12-31")
for (i in 1:length(textsAcc)) {
  test <- testsAcc[[i]]
  name <- paste0(textsAcc[i],"sumAcc")
  sumAcc <- c(textsAcc[i])
  
  
  # alldates
  fit <- gam(Accuracy ~ s(age) + Gender, data = test)
  
  # save summary of this model as a variable
  fitAD <- summary(fit)
  sumAcc <- c(sumAcc,"fitAD")
  
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
  
  ttestAD <- t.test(res$residuals~res$flash)
  # save ttest as a variable
  sumAcc <- c(sumAcc,"ttestAD")
  
  # effect sizes
  AD0 <- ttestAD$estimate[[1]]
  AD1 <- ttestAD$estimate[[2]]
  effsizeAD <- abs(AD0-AD1)
  sumAcc <- c(sumAcc,"effsizeAD")
  
  # box plot
  boxAD <- ggplot(res, aes(x=flash,y=residuals,group=flash)) +
    geom_boxplot() +
    labs(title = paste(textsAcc[i],"(All dates)"))
  sumAcc <- c(sumAcc,"boxAD")
  nAD <- res %>%
    group_by(flash) %>%
    summarise(mean=mean(residuals),median=median(residuals),n=n())
  sumAcc <- c(sumAcc,"nAD")
  
  
  # lastyear
  lastyear <- test[test$Dotest >= cutoff,]
  fit <- gam(Accuracy ~ s(age) + Gender, data = lastyear)
  
  # save summary of this model as a variable
  fitLY <- summary(fit)
  sumAcc <- c(sumAcc,"fitLY")
  
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
  
  ttestLY <- t.test(res$residuals~res$flash)
  # save ttest as a variable
  sumAcc <- c(sumAcc,"ttestLY")
  
  # effect sizes
  LY0 <- ttestLY$estimate[[1]]
  LY1 <- ttestLY$estimate[[2]]
  effsizeLY <- abs(LY0-LY1)
  sumAcc <- c(sumAcc,"effsizeLY")
  
  # box plot
  boxLY <- ggplot(res, aes(x=flash,y=residuals, group=flash)) +
    geom_boxplot() +
    labs(title = paste(textsAcc[i],"(Last year)"))
  sumAcc <- c(sumAcc,"boxLY")
  nLY <- res %>%
    group_by(flash) %>%
    summarise(mean=mean(residuals),median=median(residuals),n=n())
  sumAcc <- c(sumAcc,"nLY")
  
  sumAcc <- c(sumAcc[1],mget(sumAcc[-1]))
  assign(name,sumAcc)
}

# speed
for (i in 1:length(texts)) {
  test <- tests[[i]]
  name <- paste0(texts[i],"sumSp")
  sumSp <- c(texts[i])
  
  # alldates
  fit <- gam(Speed ~ s(age) + Gender, data = test)
  
  # save summary of this model as a variable
  fitAD <- summary(fit)
  sumSp <- c(sumSp,"fitAD")
  
  # is this plot necessary?
  visreg(fit,"age", by="Gender")
  
  res <- scale(resid(fit))   # scaled residuals
  newtest <- test[!is.na(test$Speed) & !is.na(test$age) & !is.na(test$Gender),]
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
  
  ttestAD <- t.test(res$residuals~res$flash)
  # save ttest as a variable
  sumSp <- c(sumSp,"ttestAD")
  
  # effect sizes
  AD0 <- ttestAD$estimate[[1]]
  AD1 <- ttestAD$estimate[[2]]
  effsizeAD <- abs(AD0-AD1)
  sumSp <- c(sumSp,"effsizeAD")
  
  # box plot
  boxAD <- ggplot(res, aes(x=flash,y=residuals,group=flash)) +
    geom_boxplot() +
    labs(title = paste(texts[i],"(All dates)"))
  sumSp <- c(sumSp,"boxAD")
  nAD <- res %>%
    group_by(flash) %>%
    summarise(mean=mean(residuals),median=median(residuals),n=n())
  sumSp <- c(sumSp,"nAD")
  
  if (texts[i]!="VSPLOT24") {
    # lastyear
    lastyear <- test[test$Dotest >= cutoff,]
    fit <- gam(Speed ~ s(age) + Gender, data = lastyear)
    
    # save summary of this model as a variable
    fitLY <- summary(fit)
    sumSp <- c(sumSp,"fitLY")
    
    visreg(fit,"age", by="Gender")
    
    res <- scale(resid(fit))   # scaled residuals
    newtest <- lastyear[!is.na(lastyear$Speed) & !is.na(lastyear$age) & !is.na(lastyear$Gender),]
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
    
    ttestLY <- t.test(res$residuals~res$flash)
    # save ttest as a variable
    sumSp <- c(sumSp,"ttestLY")
    
    # effect sizes
    LY0 <- ttestLY$estimate[[1]]
    LY1 <- ttestLY$estimate[[2]]
    effsizeLY <- abs(LY0-LY1)
    sumSp <- c(sumSp,"effsizeLY")
    
    # box plot
    boxLY <- ggplot(res, aes(x=flash,y=residuals, group=flash)) +
      geom_boxplot() +
      labs(title = paste(texts[i],"(Last year)"))
    sumSp <- c(sumSp,"boxLY")
    nLY <- res %>%
      group_by(flash) %>%
      summarise(mean=mean(residuals),median=median(residuals),n=n())
    sumSp <- c(sumSp,"nLY")
  }
  
  sumSp <- c(sumSp[1],mget(sumSp[-1]))
  assign(name,sumSp)
}

# checking for Ns making sure there are enough for a valid comparison
# can also check for effect sizes

tocheck <- as.data.frame(matrix(NA,length(textsAcc),5))
rownames(tocheck) <- textsAcc
names(tocheck) <- c("nAD", "effsizeAD", "nLY", "effsizeLY", "problematic")

sumAcc <- paste0(textsAcc,"sumAcc")
sumAcc <- mget(sumAcc)
# bloop <- c()

for (i in 1:length(sumAcc)) {
  dat <- sumAcc[[i]]
  
  nAD <- dat$nAD$n
  nLY <- dat$nLY$n
  # bloop <- c(bloop, nAD,nLY)
  if (any(nAD<20)) {
    tocheck$nAD[i] <- min(nAD)
  }
  if (any(nLY<20)) {
    tocheck$nLY[i] <- min(nLY)
  }
  
  effectAD <- dat$effsizeAD
  effectLY <- dat$effsizeLY
  
  if (effectAD >= 0.15) {
    tocheck$effsizeAD[i] <- effectAD
  }
  if (effectLY >= 0.15) {
    tocheck$effsizeLY[i] <- effectLY
  }
}
tocheck$problematic <- ifelse(!is.na(tocheck$nAD),1,
                              ifelse(!is.na(tocheck$effsizeAD),1,
                                     ifelse(!is.na(tocheck$nLY),1,
                                            ifelse(!is.na(tocheck$effsizeLY),1,0))))
problematic <- tocheck[tocheck$problematic==1,]
allgood <- rownames(tocheck[tocheck$problematic==0,])
# allgood <- c(allgood,"SPCET_A")



# * flash/non-flash intra-subject correlations ----

# separate by flash and compare IDs
test <- "ADT36_A"
test <- mget(test)[[1]]

flash <- unique(test[test$flash==1 & !is.na(test$Bblid),2])
nflash <- unique(test[test$flash==0 & !is.na(test$Bblid),2])

both <- test[test$Bblid %in% intersect(flash,nflash),]
both <- both[order(both$Bblid),c(2,5:7,14:17)]

flash <- both[both$flash==1,]
flash <- flash[order(flash$Bblid,flash$Dotest),]
nflash <- both[both$flash==0,]
nflash <- nflash[order(nflash$Bblid,nflash$Dotest),]

flashcount <- flash %>%          # ayy glaze it
  group_by(Bblid) %>%
  summarise(n=n())
nflashcount <- nflash %>%
  group_by(Bblid) %>%
  summarise(n=n())
flashcount <- flashcount[order(flashcount$n, decreasing = T),]
nflashcount <- nflashcount[order(nflashcount$n, decreasing = T),]

# df where rows are all unique Bblid and then columns for Dotest, age, speed and acc for each test point

# wideboth <- both[,1:3]
# wideboth <- wideboth[!duplicated(wideboth[,"Bblid"]),]
# 
# flash <- left_join(flash,flashcount,by="Bblid")
# nflash <- left_join(nflash,nflashcount,by="Bblid")
# 
# wideflash <- spread(flash,Dotest,Accuracy)
# wideflash2 <- dcast(melt(my.df, id.vars=c("ID", "TIME")),flash,Dotest,Accuracy)
# dcast(melt(my.df, id.vars=c("ID", "TIME")), ID~variable+TIME)

tpflash <- flash[,c(1,4,6:7)]    #[t]ime [p]oint [flash]
tpflash$timepoint <- 1
for (i in 1:(nrow(tpflash)-1)) {
  if (tpflash$Bblid[i+1] == tpflash$Bblid[i]) {
    tpflash$timepoint[i+1] <- tpflash$timepoint[i] + 1
  }
}
tpnflash <- nflash[,c(1,4,6:7)]    #[t]ime [p]oint [n]on-[flash]
tpnflash$timepoint <- 1
for (i in 1:(nrow(tpnflash)-1)) {
  if (tpnflash$Bblid[i+1] == tpnflash$Bblid[i]) {
    tpnflash$timepoint[i+1] <- tpnflash$timepoint[i] + 1
  }
}

wideflash <- reshape(tpflash,
                    idvar = "Bblid",
                    timevar = "timepoint",
                    direction = "wide")
widenflash <- reshape(tpnflash,
                     idvar = "Bblid",
                     timevar = "timepoint",
                     direction = "wide")

# make df with difference in score (diff) as well as days between test dates (interval)
     # make new columns for difference in accuracy between test points
wideflash$t2_t1diff <- ifelse(!is.na(wideflash$Accuracy.2),
                              wideflash$Accuracy.2 - wideflash$Accuracy.1,NA)
wideflash$t3_t2diff <- ifelse(!is.na(wideflash$Accuracy.3),
                              wideflash$Accuracy.3 - wideflash$Accuracy.2,NA)
wideflash$t4_t3diff <- ifelse(!is.na(wideflash$Accuracy.4),
                              wideflash$Accuracy.4 - wideflash$Accuracy.3,NA)
wideflash$t5_t4diff <- ifelse(!is.na(wideflash$Accuracy.5),
                              wideflash$Accuracy.5 - wideflash$Accuracy.4,NA)
wideflash$t6_t5diff <- ifelse(!is.na(wideflash$Accuracy.6),
                              wideflash$Accuracy.6 - wideflash$Accuracy.5,NA)
wideflash$t7_t6diff <- ifelse(!is.na(wideflash$Accuracy.7),
                              wideflash$Accuracy.7 - wideflash$Accuracy.6,NA)
     # make new columns for "interval" aka time difference (in days) between test points
wideflash$t2_t1time <- ifelse(!is.na(wideflash$Accuracy.2),
                              difftime(wideflash$Dotest.2,wideflash$Dotest.1,units = "days"),NA)
wideflash$t3_t2time <- ifelse(!is.na(wideflash$Accuracy.3),
                              difftime(wideflash$Dotest.3,wideflash$Dotest.2,units = "days"),NA)
wideflash$t4_t3time <- ifelse(!is.na(wideflash$Accuracy.4),
                              difftime(wideflash$Dotest.4,wideflash$Dotest.3,units = "days"),NA)
wideflash$t5_t4time <- ifelse(!is.na(wideflash$Accuracy.5),
                              difftime(wideflash$Dotest.5,wideflash$Dotest.4,units = "days"),NA)
wideflash$t6_t5time <- ifelse(!is.na(wideflash$Accuracy.6),
                              difftime(wideflash$Dotest.6,wideflash$Dotest.5,units = "days"),NA)
wideflash$t7_t6time <- ifelse(!is.na(wideflash$Accuracy.7),
                              difftime(wideflash$Dotest.7,wideflash$Dotest.6,units = "days"),NA)

     # make sure all time differences of 0 are written as 1
wideflash[wideflash$t2_t1time==0 & !is.na(wideflash$t2_t1time),"t2_t1time"] <- 1
wideflash[wideflash$t3_t2time==0 & !is.na(wideflash$t3_t2time),"t3_t2time"] <- 1
wideflash[wideflash$t4_t3time==0 & !is.na(wideflash$t4_t3time),"t4_t3time"] <- 1
wideflash[wideflash$t5_t4time==0 & !is.na(wideflash$t5_t4time),"t5_t4time"] <- 1
wideflash[wideflash$t6_t5time==0 & !is.na(wideflash$t6_t5time),"t6_t5time"] <- 1
wideflash[wideflash$t7_t6time==0 & !is.na(wideflash$t7_t6time),"t7_t6time"] <- 1


# code from Tyler
diff <- T2_scores - T1_scores
new_diff <- lm(diff~interval, data=diff_int)$residuals
new_T2_scores <- T2_scores + new_diff






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












