# new script for flash no flash project
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
bigcnb <- read.csv("bigcnb_28Sep21.csv", na=c("",".","NA",NA))  # 241,797 rows 9.28.21
names(bigcnb)[2:3] <- c("datasetid", "bblid")
demos <- read.csv("subjectdemosall_v.csv")

bigcnb$bblid <- as.numeric(bigcnb$bblid)                   # getting rid of fake bblids
rm <- bigcnb[bigcnb$bblid<10000 & !is.na(bigcnb$bblid),]   # left with 241,772 rows 9.28.21
bigcnb <- setdiff(bigcnb,rm)

bigcnb$dotest <- as.Date(bigcnb$dotest, "%m/%d/%y")
bigcnb$dob <- as.Date(bigcnb$dob, "%m/%d/%y")              # anything with Dob > 2013 should be 100 years earlier
bigcnb <- bigcnb[order(bigcnb$dob, decreasing = T),]

newdemos <- demos[!duplicated(demos$BBLID),c(1,21)] # from 23356 -> 19367 rows
x <- left_join(bigcnb,newdemos,by=c("bblid"="BBLID")) # 241,772 rows!
x$DOBIRTH <- as.Date(x$DOBIRTH, "%d-%b-%y")
x$newDOB <- if_else(is.na(x$dob),x$DOBIRTH,x$dob)
x <- x[,c(1:6,18,8:16)]
names(x)[7]<- "dob"
bigcnb <- x

temp <- bigcnb[bigcnb$dob > as.Date("01/01/13", "%m/%d/%y") & !is.na(bigcnb$dob),]$dob
temp <- temp %m-% years(100) 
bigcnb[bigcnb$dob > as.Date("01/01/13", "%m/%d/%y") & !is.na(bigcnb$dob),]$dob <- temp
bigcnb[which(bigcnb$bblid==12344),7] <- bigcnb[which(bigcnb$bblid==12344),7] %m-% years(100)

bigcnb$age <- floor(as.numeric(bigcnb$dotest - bigcnb$dob, units = "weeks")/52.25)
# fixing the 106 and 107 year olds
temp <- bigcnb[bigcnb$age > 103 & !is.na(bigcnb$age),]$dob
temp <- temp %m+% years(100) 
bigcnb[bigcnb$age > 103 & !is.na(bigcnb$age),]$dob <- temp
bigcnb$age <- floor(as.numeric(bigcnb$dotest - bigcnb$dob, units = "weeks")/52.25)

bigcnb$flash <- 0
bigcnb$flash[which(bigcnb$dotest <= as.Date("2020-12-31"))] <- 1

bigcnb <- bigcnb[order(bigcnb$bblid),]

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
  newtest <- test[!is.na(test$Accuracy) & !is.na(test$age) & !is.na(test$gender),]
  if (length(unique(newtest$flash))!=2) {
    notthese <- c(notthese,texts[i])
  } else if (nrow(newtest[newtest$flash==0,])<5) {
    notthese <- c(notthese,texts[i])
  }
}

nottheseSp <- c()
for (i in 1:length(texts)){   # catch the tests that don't have enough f==0 
  test <- tests[[i]]
  newtest <- test[!is.na(test$Speed) & !is.na(test$age) & !is.na(test$gender),]
  if (length(unique(newtest$flash))!=2) {
    nottheseSp <- c(nottheseSp,texts[i])
  } else if (nrow(newtest[newtest$flash==0,])<5) {
    nottheseSp <- c(nottheseSp,texts[i])             # these ended up being the same as notthese other than MPRACT and SCTAP obviously
  }
}

notthese <- nottheseSp
texts <- setdiff(texts, notthese) # getting rid of the tests that only have flash, no non-flash subjects after correcting for the existence of age and sex
tests <- mget(texts)

# Models and Plotting ----

# 9.16.21 implementing what I talked about with Kosha and Tyler

# * two-factor FA, waiting itemwise data for this ----


# * t-tests ----

# general code for t-tests
textsAcc <- setdiff(texts, c("MPRACT","SCTAP"))
testsAcc <- mget(textsAcc)
cutoff <- as.Date("2019-12-31")
for (i in 1:length(textsAcc)) {
  test <- testsAcc[[i]]
  name <- paste0(textsAcc[i],"sumAcc")
  sumAcc <- c(textsAcc[i])
  
  
  # alldates
  fit <- gam(Accuracy ~ s(age) + gender, data = test)
  
  # save summary of this model as a variable
  fitAD <- summary(fit)
  sumAcc <- c(sumAcc,"fitAD")
  
  # is this plot necessary?
  visreg(fit,"age", by="gender")
  
  res <- scale(resid(fit))   # scaled residuals
  newtest <- test[!is.na(test$Accuracy) & !is.na(test$age) & !is.na(test$gender),]
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
  lastyear <- test[test$dotest >= cutoff,]
  fit <- gam(Accuracy ~ s(age) + gender, data = lastyear)
  
  # save summary of this model as a variable
  fitLY <- summary(fit)
  sumAcc <- c(sumAcc,"fitLY")
  
  visreg(fit,"age", by="gender")
  
  res <- scale(resid(fit))   # scaled residuals
  newtest <- lastyear[!is.na(lastyear$Accuracy) & !is.na(lastyear$age) & !is.na(lastyear$gender),]
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
  fit <- gam(Speed ~ s(age) + gender, data = test)
  
  # save summary of this model as a variable
  fitAD <- summary(fit)
  sumSp <- c(sumSp,"fitAD")
  
  # is this plot necessary?
  visreg(fit,"age", by="gender")
  
  res <- scale(resid(fit))   # scaled residuals
  newtest <- test[!is.na(test$Speed) & !is.na(test$age) & !is.na(test$gender),]
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
    lastyear <- test[test$dotest >= cutoff,]
    fit <- gam(Speed ~ s(age) + gender, data = lastyear)
    
    # save summary of this model as a variable
    fitLY <- summary(fit)
    sumSp <- c(sumSp,"fitLY")
    
    visreg(fit,"age", by="gender")
    
    res <- scale(resid(fit))   # scaled residuals
    newtest <- lastyear[!is.na(lastyear$Speed) & !is.na(lastyear$age) & !is.na(lastyear$gender),]
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


# checking the same thing for speed 
tochecksp <- as.data.frame(matrix(NA,length(texts),5))
rownames(tochecksp) <- texts
names(tochecksp) <- c("nAD", "effsizeAD", "nLY", "effsizeLY", "problematic")

sumSp <- paste0(texts,"sumSp")
sumSp <- mget(sumSp)
# bloop <- c()

for (i in 1:length(sumSp)) {
  dat <- sumSp[[i]]
  
  nAD <- dat$nAD$n
  nLY <- dat$nLY$n
  # bloop <- c(bloop, nAD,nLY)
  if (any(nAD<20)) {
    tochecksp$nAD[i] <- min(nAD)
  }
  if (any(nLY<20)) {
    tochecksp$nLY[i] <- min(nLY)
  }
  
  effectAD <- dat$effsizeAD
  effectLY <- dat$effsizeLY
  
  if (effectAD >= 0.15) {
    tochecksp$effsizeAD[i] <- effectAD
  }
  if (effectLY >= 0.15) {
    tochecksp$effsizeLY[i] <- effectLY
  }
}
tochecksp$problematic <- ifelse(!is.na(tochecksp$nAD),1,
                              ifelse(!is.na(tochecksp$effsizeAD),1,
                                     ifelse(!is.na(tochecksp$nLY),1,
                                            ifelse(!is.na(tochecksp$effsizeLY),1,0))))
problematic <- tochecksp[tochecksp$problematic==1,]
allgood <- rownames(tochecksp[tochecksp$problematic==0,])



# * flash/non-flash intra-subject correlations ----

# separate by flash and compare IDs
test <- "ADT36_A"
test <- mget(test)[[1]]

hist <- ggplot(test,aes(x=Accuracy)) + geom_histogram()    # histogram to look at item acc frequency

test <- test[!is.na(test$dob),]
fit <- gam(Accuracy ~ s(age), data = test)  # regress out age first
test$acc_res <- scale(resid(fit))
fit <- gam(Speed ~ s(age), data = test)
test$spe_res <- scale(resid(fit))

flash <- unique(test[test$flash==1 & !is.na(test$unique_id),])
nflash <- unique(test[test$flash==0 & !is.na(test$unique_id),])

both <- intersect(flash$bblid, nflash$bblid)
both <- test[test$bblid %in% both,]
both <- both[order(both$bblid),]

flash <- both[both$flash==1,]
flash <- flash[order(flash$bblid,flash$dotest),]
nflash <- both[both$flash==0,]
nflash <- nflash[order(nflash$bblid,nflash$dotest),]

flashcount <- flash %>%          # only made to check the max, not necessary
  group_by(bblid) %>%
  summarise(n=n())
nflashcount <- nflash %>%
  group_by(bblid) %>%
  summarise(n=n())
flashcount <- flashcount[order(flashcount$n, decreasing = T),]
nflashcount <- nflashcount[order(nflashcount$n, decreasing = T),]

maxflash <- flashcount$n[1]
maxnflash <- nflashcount$n[1]

# df where rows are all unique Bblid and then columns for Dotest, age, speed and acc for each test point
tpflash <- flash[,c(1:3,8,17,19:20)]    #[t]ime [p]oint [flash]
tpflash$timepoint <- 1
for (i in 1:(nrow(tpflash)-1)) {
  if (tpflash$bblid[i+1] == tpflash$bblid[i]) {
    tpflash$timepoint[i+1] <- tpflash$timepoint[i] + 1
  }
}
tpnflash <- nflash[,c(1:3,8,17,19:20)]    #[t]ime [p]oint [n]on-[flash] used to be columns 1,4,6:7
tpnflash$timepoint <- 1
for (i in 1:(nrow(tpnflash)-1)) {
  if (tpnflash$bblid[i+1] == tpnflash$bblid[i]) {
    tpnflash$timepoint[i+1] <- tpnflash$timepoint[i] + 1
  }
}

wideflash <- reshape(tpflash[,3:8],
                    idvar = "bblid",
                    timevar = "timepoint",
                    direction = "wide")
widenflash <- reshape(tpnflash[,3:8],
                     idvar = "bblid",
                     timevar = "timepoint",
                     direction = "wide")

# make df with difference in score (diff) as well as days between test dates (interval)
     # make new columns for difference in accuracy between test points
widediff <- c()
widetime <- c()
for (i in 2:maxflash) {
  diff <- ifelse(!is.na(wideflash[,(4*i)]),wideflash[,(4*i)] - wideflash[,(4*(i-1))],NA)
  time <- ifelse(!is.na(wideflash[,(4*i)]),difftime(wideflash[,(4*i-2)],wideflash[,(4*i-6)],units = "days"),NA)
  
  widediff <- data.frame(cbind(widediff,diff))
  widetime <- data.frame(cbind(widetime,time))
  
  names(widediff)[i-1] <- paste0("t",i,"_",i-1,"diff")
  names(widetime)[i-1] <- paste0("t",i,"_",i-1,"time")
}

widendiff <- c()
widentime <- c()
for (i in 2:maxnflash) {
  diff <- ifelse(!is.na(widenflash[,(4*i)]),widenflash[,(4*i)] - widenflash[,(4*(i-1))],NA)
  time <- ifelse(!is.na(widenflash[,(4*i)]),difftime(widenflash[,(4*i-2)],widenflash[,(4*i-6)],units = "days"),NA)
  
  widendiff <- data.frame(cbind(widendiff,diff))
  widentime <- data.frame(cbind(widentime,time))
  
  names(widendiff)[i-1] <- paste0("t",i,"_",i-1,"diff")
  names(widentime)[i-1] <- paste0("t",i,"_",i-1,"time")
}

wideflash <- cbind(wideflash,widediff,widetime)
widenflash <- cbind(widenflash,widendiff,widentime)

for (i in 1:(maxflash-1)) {
  wideflash <- wideflash[order(wideflash[,(1+4*maxflash + i)]),]
}
for (i in 1:(maxnflash-1)) {
  widenflash <- widenflash[order(widenflash[,(1+4*maxnflash + i)]),]
}


# new df for storing all scores (original and new)
newflash <- wideflash[,1:22]

# code from Tyler
new1 <- c()
new2 <- c()
for (i in 2:maxflash-1) {
  diff <- lm(widediff[,i]~widetime[,i])$residuals
  newscore1 <- c(wideflash[1:length(diff),paste0("acc_res.",i+1)] + diff, rep(NA,nrow(wideflash) - length(diff)))
  meandif <- mean(wideflash[,paste0("acc_res.",i+1)],na.rm=T) - mean(wideflash[,paste0("acc_res.",i)],na.rm=T)
  newscore2 <- wideflash[,paste0("acc_res.",i+1)] + meandif
  
  new1 <- data.frame(cbind(new1,newscore1))
  new2 <- data.frame(cbind(new2,newscore2))
  
  names(new1)[i] <- paste0("t",i+1,"_",i,"newscore1")
  names(new2)[i] <- paste0("t",i+1,"_",i,"newscore2")
}

wideflash <- cbind(wideflash,new1,new2)

newn1 <- c()
newn2 <- c()
for (i in 2:maxnflash-1) {
  diff <- lm(widendiff[,i]~widentime[,i])$residuals
  newscore1 <- c(widenflash[1:length(diff),paste0("acc_res.",i+1)] + diff, rep(NA,nrow(widenflash) - length(diff)))
  meandif <- mean(widenflash[,paste0("acc_res.",i+1)],na.rm=T) - mean(widenflash[,paste0("acc_res.",i)],na.rm=T)
  newscore2 <- widenflash[,paste0("acc_res.",i+1)] + meandif
  
  newn1 <- data.frame(cbind(newn1,newscore1))
  newn2 <- data.frame(cbind(newn2,newscore2))
  
  names(newn1)[i] <- paste0("t",i+1,"_",i,"newscore1")
  names(newn2)[i] <- paste0("t",i+1,"_",i,"newscore2")
}

widenflash <- cbind(widenflash,newn1,newn2)












