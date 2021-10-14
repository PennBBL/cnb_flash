# new script for flash vs non-flash project
#
# 09.10.21 Akira Di Sandro


# Load Packages ----
library(ggplot2)
library(psych)
library(dplyr)
library(arules)
library(visreg)
library(lubridate)
library(mgcv)
library(tidyr)
library(reshape2)
library(irr)


# Load and reorganize data ----
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

# * two-factor FA, waiting itemwise data for this ----


# * t-tests ----

# ** difference in accuracy means ----
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
  res <- res[abs(res$residuals) <= 5,]
  
  
  ttestAD <- t.test(res$residuals~res$flash)
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
  res <- res[abs(res$residuals) <= 5,]
  
  ttestLY <- t.test(res$residuals~res$flash)
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

# ** difference in speed means ----
for (i in 1:length(texts)) {
  test <- tests[[i]]
  name <- paste0(texts[i],"sumSp")
  sumSp <- c(texts[i])
  
  # alldates
  fit <- gam(Speed ~ s(age) + gender, data = test)
  
  fitAD <- summary(fit)
  sumSp <- c(sumSp,"fitAD")
  
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
  res <- res[abs(res$residuals) <= 5,]
  
  ttestAD <- t.test(res$residuals~res$flash)
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
    res <- res[abs(res$residuals) <= 5,]
    
    ttestLY <- t.test(res$residuals~res$flash)
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

# ** check that there's enough N's and low effect sizes ----
tocheck <- as.data.frame(matrix(NA,length(textsAcc),5))
rownames(tocheck) <- textsAcc
names(tocheck) <- c("nAD", "effsizeAD", "nLY", "effsizeLY", "problematic")

sumAcc <- paste0(textsAcc,"sumAcc")
sumAcc <- mget(sumAcc)

for (i in 1:length(sumAcc)) {
  dat <- sumAcc[[i]]
  
  nAD <- dat$nAD$n
  nLY <- dat$nLY$n

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

for (i in 1:length(sumSp)) {
  dat <- sumSp[[i]]
  
  nAD <- dat$nAD$n
  nLY <- dat$nLY$n
  
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
problematicsp <- tochecksp[tochecksp$problematic==1,]
allgoodsp <- rownames(tochecksp[tochecksp$problematic==0,])




# checking medians for problematic ones
tocheck <- union(rownames(problematic),rownames(problematicsp))
testcheck <- mget(tocheck)

stats <- c()
for (i in 1:length(tocheck)){
  test <- testcheck[[i]]
  
  median <- test %>% 
    group_by(flash) %>% 
    summarise(meanAcc = mean(Accuracy,na.rm=T),meanSp = mean(Speed,na.rm=T),
              medianAcc = median(Accuracy,na.rm=T),medianSp = median(Speed,na.rm=T),n = n())
  temp <- data.frame(cbind(rep(tocheck[i],2),median))
  temp <- temp[,c(1:2,7,3,5,4,6)]
  stats <- rbind(stats,temp)
}
names(stats)[1] <- "test"

ADT_LY <- ADT36_A[ADT36_A$dotest >= cutoff,]     # ADT36 A is the only test that has a problematic LY comparison
median <- ADT_LY %>% 
  group_by(flash) %>% 
  summarise(meanAcc = mean(Accuracy,na.rm=T),meanSp = mean(Speed,na.rm=T),
            medianAcc = median(Accuracy,na.rm=T),medianSp = median(Speed,na.rm=T),n = n())
temp <- data.frame(cbind(rep("ADT36_A",2),median))
temp <- temp[,c(1:2,7,3,5,4,6)]
stats[stats$test=="ADT36_A",] <-temp



# * flash/non-flash intra-subject correlations ----
textsAcc <- setdiff(textsAcc, c("CPF_A"))
testsAcc <- mget(textsAcc)
for (j in 1:length(textsAcc)){
  test <- testsAcc[[j]]
  test <- test[!is.na(test$bblid) & !is.na(test$Speed),]
  
  hist <- ggplot(test,aes(x=Accuracy)) + geom_histogram()    # histogram to look at item acc frequency
  hist
  
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
  
  flashcount <- flash %>%          
    group_by(bblid) %>%
    summarise(n=n())
  nflashcount <- nflash %>%
    group_by(bblid) %>%
    summarise(n=n())
  flashcount <- flashcount[order(flashcount$n, decreasing = T),]
  nflashcount <- nflashcount[order(nflashcount$n, decreasing = T),]
  
  maxflash <- na.omit(flashcount)$n[1]
  maxnflash <- na.omit(nflashcount)$n[1]
  
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
  
  
  if (maxflash > 1){
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
    
    wideflash <- cbind(wideflash,widediff,widetime)
    for (i in 1:(maxflash-1)) {
      wideflash <- wideflash[order(wideflash[,(1+4*maxflash + i)]),]
    }
    
    new1 <- c()
    new2 <- c()
    for (i in 2:maxflash-1) {
      diff <- lm(widediff[,i]~widetime[,i])$residuals
      newscore1 <- c(wideflash[1:length(diff),paste0("acc_res.",i+1)] + diff, rep(NA,nrow(wideflash) - length(diff)))
      meandif <- mean(wideflash[,paste0("acc_res.",i+1)],na.rm=T) - mean(wideflash[,paste0("acc_res.",i)],na.rm=T)
      newscore2 <- wideflash[,paste0("acc_res.",i+1)] + meandif
      
      new1 <- data.frame(cbind(new1,newscore1))
      new2 <- data.frame(cbind(new2,newscore2))
      
      names(new1)[i] <- paste0("t",i+1,"newscore1")
      names(new2)[i] <- paste0("t",i+1,"newscore2")
    }
    
    wideflash <- cbind(wideflash,new1,new2)
  }
  
  
  if (maxnflash > 1) {
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
    
    widenflash <- cbind(widenflash,widendiff,widentime)
    for (i in 1:(maxnflash-1)) {
      widenflash <- widenflash[order(widenflash[,(1+4*maxnflash + i)]),]
    }
    
    newn1 <- c()
    newn2 <- c()
    for (i in 2:maxnflash-1) {
      diff <- lm(widendiff[,i]~widentime[,i])$residuals
      newscore1 <- c(widenflash[1:length(diff),paste0("acc_res.",i+1)] + diff, rep(NA,nrow(widenflash) - length(diff)))
      meandif <- mean(widenflash[,paste0("acc_res.",i+1)],na.rm=T) - mean(widenflash[,paste0("acc_res.",i)],na.rm=T)
      newscore2 <- widenflash[,paste0("acc_res.",i+1)] + meandif
      
      newn1 <- data.frame(cbind(newn1,newscore1))
      newn2 <- data.frame(cbind(newn2,newscore2))
      
      names(newn1)[i] <- paste0("t",i+1,"newscore1")
      names(newn2)[i] <- paste0("t",i+1,"newscore2")
    }
    
    widenflash <- cbind(widenflash,newn1,newn2)
  }
  assign(paste0(textsAcc[j],"wideflash"),wideflash)
  assign(paste0(textsAcc[j],"widenflash"),widenflash)
}

# ** getting rid of outliers, test by test ----
wideflash <- ADT36_Awideflash
widenflash <- ADT36_Awidenflash
accflash <- wideflash[,grepl("bblid", colnames(wideflash)) | grepl("acc_res.1", colnames(wideflash)) | grepl("newscore", colnames(wideflash))]
names(accflash)[-1] <- paste0("f_",names(accflash)[-1])
accnflash <- widenflash[,grepl("bblid", colnames(widenflash)) | grepl("acc_res.1", colnames(widenflash)) | grepl("newscore", colnames(widenflash))]
names(accnflash)[-1] <- paste0("n_",names(accnflash)[-1])
acc <- merge(accflash,accnflash, by=1)
acc_cor <- cor(acc[,-1], use="pairwise")
pairs.panels(acc[,c(2,13)],lm=TRUE)    # looking at t1 flash vs nflash
acc$rm <- ifelse(abs(acc$f_acc_res.1-acc$n_acc_res.1)>=2,1,0)
acc <- acc[acc$rm == 0,-which(names(acc) %in% "rm")]
pairs.panels(acc[,c(2,13)],lm=TRUE)
ADT36_A_acc <- acc  
acctxt <- c("ADT36_A_acc")

wideflash <- AIMwideflash
widenflash <- AIMwidenflash
accflash <- wideflash[,grepl("bblid", colnames(wideflash)) | grepl("acc_res.1", colnames(wideflash)) | grepl("newscore", colnames(wideflash))]
names(accflash)[-1] <- paste0("f_",names(accflash)[-1])
accnflash <- widenflash[,grepl("bblid", colnames(widenflash)) | grepl("acc_res.1", colnames(widenflash)) | grepl("newscore", colnames(widenflash))]
names(accnflash)[-1] <- paste0("n_",names(accnflash)[-1])
acc <- merge(accflash,accnflash, by=1)
acc_cor <- cor(acc[,-1], use="pairwise")
pairs.panels(acc[,c(2,11)],lm=TRUE)    # looking at t1 flash vs nflash
acc$rm <- ifelse(abs(acc$f_acc_res.1-acc$n_acc_res.1)>=2,1,0)
acc <- acc[acc$rm == 0,-which(names(acc) %in% "rm")]
pairs.panels(acc[,c(2,11)],lm=TRUE)
AIM_acc <- acc  
acctxt <- c(acctxt,"AIM_acc")

wideflash <- CPF_Bwideflash
widenflash <- CPF_Bwidenflash
accflash <- wideflash[,grepl("bblid", colnames(wideflash)) | grepl("acc_res.1", colnames(wideflash)) | grepl("newscore", colnames(wideflash))]
names(accflash)[-1] <- paste0("f_",names(accflash)[-1])
accnflash <- widenflash[,grepl("bblid", colnames(widenflash)) | grepl("acc_res.1", colnames(widenflash)) | grepl("newscore", colnames(widenflash))]
names(accnflash)[-1] <- paste0("n_",names(accnflash)[-1])
acc <- merge(accflash,accnflash, by=1)
acc_cor <- cor(acc[,-1], use="pairwise")
pairs.panels(acc[,c(2,11)],lm=TRUE)    # looking at t1 flash vs nflash
acc$rm <- ifelse(abs(acc$f_acc_res.1-acc$n_acc_res.1)>=2,1,0)
acc <- acc[acc$rm == 0,-which(names(acc) %in% "rm")]
pairs.panels(acc[,c(2,11)],lm=TRUE)
CPF_B_acc <- acc  
acctxt <- c(acctxt,"CPF_B_acc")

wideflash <- ER40_Dwideflash
widenflash <- ER40_Dwidenflash
accflash <- wideflash[,grepl("bblid", colnames(wideflash)) | grepl("acc_res.1", colnames(wideflash)) | grepl("newscore", colnames(wideflash))]
names(accflash)[-1] <- paste0("f_",names(accflash)[-1])
accnflash <- widenflash[,grepl("bblid", colnames(widenflash)) | grepl("acc_res.1", colnames(widenflash)) | grepl("newscore", colnames(widenflash))]
names(accnflash)[-1] <- paste0("n_",names(accnflash)[-1])
acc <- merge(accflash,accnflash, by=1)
acc_cor <- cor(acc[,-1], use="pairwise")
pairs.panels(acc[,c(2,13)],lm=TRUE)    # looking at t1 flash vs nflash
acc$rm <- ifelse(abs(acc$f_acc_res.1-acc$n_acc_res.1)>=2,1,0)
acc <- acc[acc$rm == 0,-which(names(acc) %in% "rm")]
pairs.panels(acc[,c(2,13)],lm=TRUE)
ER40_D_acc <- acc  
acctxt <- c(acctxt,"ER40_D_acc")

wideflash <- GNG150wideflash
widenflash <- GNG150widenflash
accflash <- wideflash[,grepl("bblid", colnames(wideflash)) | grepl("acc_res.1", colnames(wideflash)) | grepl("newscore", colnames(wideflash))]
names(accflash)[-1] <- paste0("f_",names(accflash)[-1])
accnflash <- widenflash[,grepl("bblid", colnames(widenflash)) | grepl("acc_res.1", colnames(widenflash)) | grepl("newscore", colnames(widenflash))]
names(accnflash)[-1] <- paste0("n_",names(accnflash)[-1])
acc <- merge(accflash,accnflash, by=1)
acc_cor <- cor(acc[,-1], use="pairwise")
pairs.panels(acc[,c(2,13)],lm=TRUE)    # looking at t1 flash vs nflash
acc$rm <- ifelse(abs(acc$f_acc_res.1-acc$n_acc_res.1)>=2,1,0)
acc <- acc[acc$rm == 0,-which(names(acc) %in% "rm")]
pairs.panels(acc[,c(2,13)],lm=TRUE)
GNG150_acc <- acc  
acctxt <- c(acctxt,"GNG150_acc")

wideflash <- KCPW_Awideflash
widenflash <- KCPW_Awidenflash
accflash <- wideflash[,grepl("bblid", colnames(wideflash)) | grepl("acc_res.1", colnames(wideflash)) | grepl("newscore", colnames(wideflash))]
names(accflash)[-1] <- paste0("f_",names(accflash)[-1])
accnflash <- widenflash[,grepl("bblid", colnames(widenflash)) | grepl("acc_res.1", colnames(widenflash)) | grepl("newscore", colnames(widenflash))]
names(accnflash)[-1] <- paste0("n_",names(accnflash)[-1])
acc <- merge(accflash,accnflash, by=1)
acc_cor <- cor(acc[,-1], use="pairwise")
pairs.panels(acc[,c(2,15)],lm=TRUE)    # looking at t1 flash vs nflash
acc$rm <- ifelse(abs(acc$f_acc_res.1-acc$n_acc_res.1)>=2,1,0)
acc <- acc[acc$rm == 0,-which(names(acc) %in% "rm")]
pairs.panels(acc[,c(2,15)],lm=TRUE)
KCPW_A_acc <- acc  
acctxt <- c(acctxt,"KCPW_A_acc")

wideflash <- KSPVRT_Dwideflash
widenflash <- KSPVRT_Dwidenflash
accflash <- wideflash[,grepl("bblid", colnames(wideflash)) | grepl("acc_res.1", colnames(wideflash)) | grepl("newscore", colnames(wideflash))]
names(accflash)[-1] <- paste0("f_",names(accflash)[-1])
accnflash <- widenflash[,grepl("bblid", colnames(widenflash)) | grepl("acc_res.1", colnames(widenflash)) | grepl("newscore", colnames(widenflash))]
names(accnflash)[-1] <- paste0("n_",names(accnflash)[-1])
acc <- merge(accflash,accnflash, by=1)
acc_cor <- cor(acc[,-1], use="pairwise")
pairs.panels(acc[,c(2,13)],lm=TRUE)    # looking at t1 flash vs nflash
acc$rm <- ifelse(abs(acc$f_acc_res.1-acc$n_acc_res.1)>=2,1,0)
acc <- acc[acc$rm == 0,-which(names(acc) %in% "rm")]
pairs.panels(acc[,c(2,13)],lm=TRUE)
KSPVRT_D_acc <- acc  
acctxt <- c(acctxt,"KSPVRT_D_acc")

wideflash <- MEDF36_Awideflash
widenflash <- MEDF36_Awidenflash
accflash <- wideflash[,grepl("bblid", colnames(wideflash)) | grepl("acc_res.1", colnames(wideflash)) | grepl("newscore", colnames(wideflash))]
names(accflash)[-1] <- paste0("f_",names(accflash)[-1])
accnflash <- widenflash[,grepl("bblid", colnames(widenflash)) | grepl("acc_res.1", colnames(widenflash)) | grepl("newscore", colnames(widenflash))]
names(accnflash)[-1] <- paste0("n_",names(accnflash)[-1])
acc <- merge(accflash,accnflash, by=1)
acc_cor <- cor(acc[,-1], use="pairwise")
pairs.panels(acc[,c(2,13)],lm=TRUE)    # looking at t1 flash vs nflash
acc$rm <- ifelse(abs(acc$f_acc_res.1-acc$n_acc_res.1)>=2,1,0)
acc <- acc[acc$rm == 0,-which(names(acc) %in% "rm")]
pairs.panels(acc[,c(2,13)],lm=TRUE)
MEDF36_A_acc <- acc  
acctxt <- c(acctxt,"MEDF36_A_acc")

wideflash <- PCET_Awideflash
widenflash <- PCET_Awidenflash
accflash <- wideflash[,grepl("bblid", colnames(wideflash)) | grepl("acc_res.1", colnames(wideflash)) | grepl("newscore", colnames(wideflash))]
names(accflash)[-1] <- paste0("f_",names(accflash)[-1])
accnflash <- widenflash[,grepl("bblid", colnames(widenflash)) | grepl("acc_res.1", colnames(widenflash)) | grepl("newscore", colnames(widenflash))]
names(accnflash)[-1] <- paste0("n_",names(accnflash)[-1])
acc <- merge(accflash,accnflash, by=1)
acc_cor <- cor(acc[,-1], use="pairwise")
pairs.panels(acc[,c(2,13)],lm=TRUE)    # looking at t1 flash vs nflash
acc$rm <- ifelse(abs(acc$f_acc_res.1-acc$n_acc_res.1)>=2,1,0)
acc <- acc[acc$rm == 0,-which(names(acc) %in% "rm")]
pairs.panels(acc[,c(2,13)],lm=TRUE)
PCET_A_acc <- acc  
acctxt <- c(acctxt,"PCET_A_acc")

wideflash <- PMAT24_Awideflash
widenflash <- PMAT24_Awidenflash
accflash <- wideflash[,grepl("bblid", colnames(wideflash)) | grepl("acc_res.1", colnames(wideflash)) | grepl("newscore", colnames(wideflash))]
names(accflash)[-1] <- paste0("f_",names(accflash)[-1])
accnflash <- widenflash[,grepl("bblid", colnames(widenflash)) | grepl("acc_res.1", colnames(widenflash)) | grepl("newscore", colnames(widenflash))]
names(accnflash)[-1] <- paste0("n_",names(accnflash)[-1])
acc <- merge(accflash,accnflash, by=1)
acc_cor <- cor(acc[,-1], use="pairwise")
pairs.panels(acc[,c(2,11)],lm=TRUE)    # looking at t1 flash vs nflash
acc$rm <- ifelse(abs(acc$f_acc_res.1-acc$n_acc_res.1)>=2,1,0)
acc <- acc[acc$rm == 0,-which(names(acc) %in% "rm")]
pairs.panels(acc[,c(2,11)],lm=TRUE)
PMAT24_A_acc <- acc  
acctxt <- c(acctxt,"PMAT24_A_acc")

wideflash <- SLNB2_90wideflash
widenflash <- SLNB2_90widenflash
accflash <- wideflash[,grepl("bblid", colnames(wideflash)) | grepl("acc_res.1", colnames(wideflash)) | grepl("newscore", colnames(wideflash))]
names(accflash)[-1] <- paste0("f_",names(accflash)[-1])
accnflash <- widenflash[,grepl("bblid", colnames(widenflash)) | grepl("acc_res.1", colnames(widenflash)) | grepl("newscore", colnames(widenflash))]
names(accnflash)[-1] <- paste0("n_",names(accnflash)[-1])
acc <- merge(accflash,accnflash, by=1)
acc_cor <- cor(acc[,-1], use="pairwise")
pairs.panels(acc[,c(2,15)],lm=TRUE)    # looking at t1 flash vs nflash
acc$rm <- ifelse(abs(acc$f_acc_res.1-acc$n_acc_res.1)>=2,1,0)
acc <- acc[acc$rm == 0,-which(names(acc) %in% "rm")]
pairs.panels(acc[,c(2,15)],lm=TRUE)
SLNB2_90_acc <- acc  
acctxt <- c(acctxt,"SLNB2_90_acc")

wideflash <- SVOLT_Awideflash
widenflash <- SVOLT_Awidenflash
accflash <- wideflash[,grepl("bblid", colnames(wideflash)) | grepl("acc_res.1", colnames(wideflash)) | grepl("newscore", colnames(wideflash))]
names(accflash)[-1] <- paste0("f_",names(accflash)[-1])
accnflash <- widenflash[,grepl("bblid", colnames(widenflash)) | grepl("acc_res.1", colnames(widenflash)) | grepl("newscore", colnames(widenflash))]
names(accnflash)[-1] <- paste0("n_",names(accnflash)[-1])
acc <- merge(accflash,accnflash, by=1)
acc_cor <- cor(acc[,-1], use="pairwise")
pairs.panels(acc[,c(2,15)],lm=TRUE)    # looking at t1 flash vs nflash
acc$rm <- ifelse(abs(acc$f_acc_res.1-acc$n_acc_res.1)>=2,1,0)
acc <- acc[acc$rm == 0,-which(names(acc) %in% "rm")]
pairs.panels(acc[,c(2,15)],lm=TRUE)
SVOLT_A_acc <- acc  
acctxt <- c(acctxt,"SVOLT_A_acc")

wideflash <- VSPLOT15wideflash
widenflash <- VSPLOT15widenflash
accflash <- wideflash[,grepl("bblid", colnames(wideflash)) | grepl("acc_res.1", colnames(wideflash)) | grepl("newscore", colnames(wideflash))]
names(accflash)[-1] <- paste0("f_",names(accflash)[-1])
accnflash <- widenflash[,grepl("bblid", colnames(widenflash)) | grepl("acc_res.1", colnames(widenflash)) | grepl("newscore", colnames(widenflash))]
names(accnflash)[-1] <- paste0("n_",names(accnflash)[-1])
acc <- merge(accflash,accnflash, by=1)
acc_cor <- cor(acc[,-1], use="pairwise")
pairs.panels(acc[,c(2,9)],lm=TRUE)    # looking at t1 flash vs nflash
acc$rm <- ifelse(abs(acc$f_acc_res.1-acc$n_acc_res.1)>=2,1,0)
acc <- acc[acc$rm == 0,-which(names(acc) %in% "rm")]
pairs.panels(acc[,c(2,9)],lm=TRUE)
VSPLOT15_acc <- acc  
acctxt <- c(acctxt,"VSPLOT15_acc")

accs <- mget(acctxt)

# ** acc_cor and icc for each test ----
for (i in 1:length(acctxt)) {
  acc <- accs[[i]]
  acc_cor <- cor(acc[,-1], use="pairwise")
  
  assign(paste0(textsAcc[i],"acc_cor"),acc_cor)
  # wrap icc stuff in try()
  icc_fnf <- icc(acc[,grepl("acc_res",colnames(acc))],type="agreement",model="twoway")$value
  try(icc_f12_1 <- icc(acc[,grepl("f_acc_res",colnames(acc)) | grepl("f_t2newscore1",colnames(acc))],type="agreement",model="twoway")$value)
  try(icc_f13_1 <- icc(acc[,grepl("f_acc_res",colnames(acc)) | grepl("f_t2newscore1",colnames(acc)) | grepl("f_t3newscore1",colnames(acc))],type="agreement",model="twoway")$value)
  try(icc_f14_1 <- icc(acc[,grepl("f_acc_res",colnames(acc)) | grepl("f_t2newscore1",colnames(acc)) | grepl("f_t3newscore1",colnames(acc)) | grepl("f_t4newscore1",colnames(acc))],type="agreement",model="twoway")$value)
  try(icc_f12_2 <- icc(acc[,grepl("f_acc_res",colnames(acc)) | grepl("f_t2newscore2",colnames(acc))],type="agreement",model="twoway")$value)
  try(icc_f13_2 <- icc(acc[,grepl("f_acc_res",colnames(acc)) | grepl("f_t2newscore2",colnames(acc)) | grepl("f_t3newscore2",colnames(acc))],type="agreement",model="twoway")$value)
  try(icc_f14_2 <- icc(acc[,grepl("f_acc_res",colnames(acc)) | grepl("f_t2newscore2",colnames(acc)) | grepl("f_t3newscore2",colnames(acc)) | grepl("f_t4newscore2",colnames(acc))],type="agreement",model="twoway")$value)
  
  icc <- data.frame(icc_fnf)
  try(if (icc_f12_1) {icc <- cbind(icc, icc_f12_1)})
  try(if (icc_f13_1) {icc <- cbind(icc, icc_f13_1)})
  try(if (icc_f14_1) {icc <- cbind(icc, icc_f14_1)})
  try(if (icc_f12_2) {icc <- cbind(icc, icc_f12_2)})
  try(if (icc_f13_2) {icc <- cbind(icc, icc_f13_2)})
  try(if (icc_f14_2) {icc <- cbind(icc, icc_f14_2)})
  assign(paste0(textsAcc[i],"icc"),icc)
}

write.csv(ADT36_Aacc_cor,"myresults/instrasubject_corr/ADT36_Aacc_cor.csv")
write.csv(AIMacc_cor,"myresults/instrasubject_corr/AIMacc_cor.csv")
write.csv(CPF_Bacc_cor,"myresults/instrasubject_corr/CPF_Bacc_cor.csv")
write.csv(ER40_Dacc_cor,"myresults/instrasubject_corr/ER40_Dacc_cor.csv")
write.csv(GNG150acc_cor,"myresults/instrasubject_corr/GNG150acc_cor.csv")
write.csv(KCPW_Aacc_cor,"myresults/instrasubject_corr/KCPW_Aacc_cor.csv")
write.csv(KSPVRT_Dacc_cor,"myresults/instrasubject_corr/KSPVRT_Dacc_cor.csv")
write.csv(MEDF36_Aacc_cor,"myresults/instrasubject_corr/MEDF36_Aacc_cor.csv")
write.csv(PCET_Aacc_cor,"myresults/instrasubject_corr/PCET_Aacc_cor.csv")
write.csv(PMAT24_Aacc_cor,"myresults/instrasubject_corr/PMAT24_Aacc_cor.csv")
write.csv(SLNB2_90acc_cor,"myresults/instrasubject_corr/SLNB2_90acc_cor.csv")
write.csv(SVOLT_Aacc_cor,"myresults/instrasubject_corr/SVOLT_Aacc_cor.csv")
write.csv(VSPLOT15acc_cor,"myresults/instrasubject_corr/VSPLOT15acc_cor.csv")

write.csv( ADT36_Aicc, "myresults/instrasubject_corr/ADT36_Aicc.csv")
write.csv(     AIMicc,     "myresults/instrasubject_corr/AIMicc.csv")
write.csv(   CPF_Bicc,   "myresults/instrasubject_corr/CPF_Bicc.csv")
write.csv(  ER40_Dicc,  "myresults/instrasubject_corr/ER40_Dicc.csv")
write.csv(  GNG150icc,  "myresults/instrasubject_corr/GNG150icc.csv")
write.csv(  KCPW_Aicc,  "myresults/instrasubject_corr/KCPW_Aicc.csv")
write.csv(KSPVRT_Dicc,"myresults/instrasubject_corr/KSPVRT_Dicc.csv")
write.csv(MEDF36_Aicc,"myresults/instrasubject_corr/MEDF36_Aicc.csv")
write.csv(  PCET_Aicc,  "myresults/instrasubject_corr/PCET_Aicc.csv")
write.csv(PMAT24_Aicc,"myresults/instrasubject_corr/PMAT24_Aicc.csv")
write.csv(SLNB2_90icc,"myresults/instrasubject_corr/SLNB2_90icc.csv")
write.csv( SVOLT_Aicc, "myresults/instrasubject_corr/SVOLT_Aicc.csv")
write.csv(VSPLOT15icc,"myresults/instrasubject_corr/VSPLOT15icc.csv")










