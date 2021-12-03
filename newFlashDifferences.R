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
library(plotly)
library(tidyverse)
library(mirt)


# Load and reorganize data ----
bigcnb <- read.csv("bigcnb_28Sep21.csv", na=c("",".","NA",NA))  # 241,797 rows 9.28.21
names(bigcnb)[2:3] <- c("datasetid", "bblid")
demos <- read.csv("subjectdemosall_v.csv")

pcpt <- read.csv("cnb_pivot_spctn.csv")
pcpt <- pcpt[!is.na(pcpt$TP),c(15,17,16,2,7,12,8:9,20,18,19,20,21,23,23,22,6)]
names(pcpt)[1:16] <- names(bigcnb)
pcpt$bblid <- as.numeric(pcpt$bblid)
pcpt$dotest <- as.Date(pcpt$dotest)
pcpt$dob <- as.Date(pcpt$dob)
pcpt$Accuracy <- as.numeric(pcpt$Accuracy)
pcpt$age <- as.numeric(pcpt$age)
pcpt <- pcpt[pcpt$bblid>=10000,] # 64272 x 17

bigcnb$bblid <- as.numeric(bigcnb$bblid)                   # getting rid of fake bblids
bigcnb <- bigcnb[bigcnb$bblid>=10000 & !is.na(bigcnb$bblid),]   # left with 173,556 rows

bigcnb$dotest <- as.Date(bigcnb$dotest, "%m/%d/%y")
bigcnb$dob <- as.Date(bigcnb$dob, "%m/%d/%y")              # anything with Dob > 2013 should be 100 years earlier
bigcnb <- bigcnb[order(bigcnb$dob, decreasing = T),]

newdemos <- demos[!duplicated(demos$BBLID),c(1,21)] # from 23356 -> 19367 rows
x <- left_join(bigcnb,newdemos,by=c("bblid"="BBLID")) # 173,556 rows!
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

bigcnb <- rbind(bigcnb,pcpt)    # now 237,828
bigcnb$flash <- 0
bigcnb$flash[which(bigcnb$dotest <= as.Date("2020-12-31"))] <- 1
bigcnb$Speed <- ifelse(log(bigcnb$Speed)!= -Inf, log(bigcnb$Speed),0)

bigcnb <- bigcnb[order(bigcnb$bblid),]

bigcnb <- bigcnb[!is.na(bigcnb$Version),] # 210,509

write.csv(bigcnb, "bigcnb_9Nov21.csv", row.names = F)

# * Separate into test versions ----
# check if BART, DIGSYM, TRAIL exist
ADT36_A <- bigcnb[bigcnb$Version == "ADT36_A" & !is.na(bigcnb$Accuracy) & bigcnb$code != "N" & bigcnb$code != "F" & bigcnb$code != "V3",]
ADT60_A <- bigcnb[bigcnb$Version == "ADT60_A" & !is.na(bigcnb$Accuracy) & bigcnb$code != "N",]

AIM <- bigcnb[bigcnb$Version == "AIM" & !is.na(bigcnb$Accuracy),]

CPF_A <- bigcnb[bigcnb$Version == "CPF_A" & !is.na(bigcnb$Accuracy) & bigcnb$code != "N",]
CPF_B <- bigcnb[bigcnb$Version == "CPF_B" & !is.na(bigcnb$Accuracy) & bigcnb$code != "N",]
CPFD_A <- bigcnb[bigcnb$Version == "CPFD_A" & !is.na(bigcnb$Accuracy) & bigcnb$code != "N",]
CPFD_B <- bigcnb[bigcnb$Version == "CPFD_B" & !is.na(bigcnb$Accuracy) & bigcnb$code != "N",]

ER40_A <- bigcnb[bigcnb$Version == "ER40_A" & !is.na(bigcnb$Accuracy) & bigcnb$code != "N",]
ER40_C <- bigcnb[bigcnb$Version == "ER40_C" & !is.na(bigcnb$Accuracy) & bigcnb$code != "N",]
ER40_D <- bigcnb[bigcnb$Version == "ER40_D" & !is.na(bigcnb$Accuracy) & bigcnb$code != "N",]

GNG150 <- bigcnb[bigcnb$Version == "GNG150" & !is.na(bigcnb$Accuracy) & bigcnb$code != "N",]

KCPW_A <- bigcnb[bigcnb$Version == "KCPW_A" & !is.na(bigcnb$Accuracy) & bigcnb$code != "N",]
KCPWD_A <- bigcnb[bigcnb$Version == "KCPWD_A" & !is.na(bigcnb$Accuracy) & bigcnb$code != "N",]

KSPVRT_A <- bigcnb[bigcnb$Version == "KSPVRT_A" & !is.na(bigcnb$Accuracy) & bigcnb$code != "N",]
KSPVRT_B <- bigcnb[bigcnb$Version == "KSPVRT_B" & !is.na(bigcnb$Accuracy) & bigcnb$code != "N",]
KSPVRT_D <- bigcnb[bigcnb$Version == "KSPVRT_D" & !is.na(bigcnb$Accuracy) & bigcnb$code != "N",]

MEDF36_A <- bigcnb[bigcnb$Version == "MEDF36_A" & !is.na(bigcnb$Accuracy) & bigcnb$code != "N",]
MEDF60_A <- bigcnb[bigcnb$Version == "MEDF60_A" & !is.na(bigcnb$Accuracy) & bigcnb$code != "N",]

MPRACT <- bigcnb[bigcnb$Version == "MPRACT" & !is.na(bigcnb$Speed) & bigcnb$code != "N",]

PCET_A <- bigcnb[bigcnb$Version == "PCET_A" & !is.na(bigcnb$Accuracy) & bigcnb$code != "N",]
SPCET_A <- bigcnb[bigcnb$Version == "SPCET_A" & !is.na(bigcnb$Accuracy) & bigcnb$code != "N",]

PMAT18_B <- bigcnb[bigcnb$Version == "PMAT18_B" & !is.na(bigcnb$Accuracy) & bigcnb$code != "N",]
PMAT24_A <- bigcnb[bigcnb$Version == "PMAT24_A" & !is.na(bigcnb$Accuracy) & bigcnb$code != "N",]
PMAT24_B <- bigcnb[bigcnb$Version == "PMAT24_B" & !is.na(bigcnb$Accuracy) & bigcnb$code != "N",]

SCTAP <- bigcnb[bigcnb$Version == "SCTAP" & !is.na(bigcnb$Speed) & bigcnb$code != "N",]

SLNB2_90 <- bigcnb[bigcnb$Version == "SLNB2_90" & !is.na(bigcnb$Accuracy) & bigcnb$code != "N",]

SPCPTNL <- bigcnb[bigcnb$Version == "SPCPTNL" & bigcnb$Sub_version == "SCPT" & !is.na(bigcnb$Accuracy),]
SPCPTN90 <- bigcnb[bigcnb$Version == "SPCPTN90" & !is.na(bigcnb$Accuracy),]

SPLOT12 <- bigcnb[bigcnb$Version == "SPLOT12" & !is.na(bigcnb$Accuracy),]
VSPLOT15 <- bigcnb[bigcnb$Version == "VSPLOT15" & !is.na(bigcnb$Accuracy),]
VSPLOT24 <- bigcnb[bigcnb$Version == "VSPLOT24" & !is.na(bigcnb$Accuracy),]

SPVRT_A <- bigcnb[bigcnb$Version == "SPVRT_A" & !is.na(bigcnb$Accuracy) & bigcnb$code != "N",]

SVOLT_A <- bigcnb[bigcnb$Version == "SVOLT_A" & !is.na(bigcnb$Accuracy) & bigcnb$code != "N",]
SVOLTD_A <- bigcnb[bigcnb$Version == "SVOLTD_A" & !is.na(bigcnb$Accuracy) & bigcnb$code != "N",]


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

# downloading itemwise data
cpt_iw <- read.csv("cpt_itemlevel/athena_3360_2080.csv")    # CPT [i]tem[w]ise
cpt_iw$test_sessions_v.dotest <- as.Date(cpt_iw$test_sessions_v.dotest)
cpt_iw$flash <- ifelse(cpt_iw$test_sessions_v.dotest <= as.Date("2020-12-31"),1,0)

# separating out into each test
id <- cpt_iw[,c(1,5,7,5257)]
pcptnl <- cpt_iw[,grepl("PCPTNL.CPT_Q", colnames(cpt_iw))]
pcptn360 <- cpt_iw[,grepl("PCPTN360.CPT_Q", colnames(cpt_iw))]
spcptnl <- cpt_iw[,grepl("SPCPTNL.SCPT_Q", colnames(cpt_iw))]
spcptn90 <- cpt_iw[,grepl("SPCPTN90.SCPT_Q", colnames(cpt_iw))]

pcptnl <- pcptnl[,grepl("CORR", colnames(pcptnl))]
pcptn360 <- pcptn360[,grepl("CORR", colnames(pcptn360))]
spcptnl <- spcptnl[,grepl("CORR", colnames(spcptnl))]
spcptn90 <- spcptn90[,grepl("CORR", colnames(spcptn90))]

pcptnl <- cbind(id, pcptnl)
pcptn360 <- cbind(id, pcptn360)
spcptnl <- cbind(id, spcptnl)
spcptn90 <- cbind(id, spcptn90)

# now keep the ones that don't have NA
noNA_NL <- pcptnl[rowSums(is.na(pcptnl)) <5,]        # pcpt[nl]     flash
noNA_N <- pcptn360[rowSums(is.na(pcptn360)) <5,]     # pcpt[n]360   flash
noNA_sNL <- spcptnl[rowSums(is.na(spcptnl)) <5,]     # spcpt[nl]    both
noNA_sN <- spcptn90[rowSums(is.na(spcptn90)) <5,]    # spcpt[n]90   both

# only need sNL and sN since these tests were administered both flash and non-flash
sNL_f <- noNA_sNL[noNA_sNL$flash ==1,grepl("CORR",colnames(noNA_sNL))]  # noNA_sNL flash group
sNL_n <- noNA_sNL[noNA_sNL$flash ==0,grepl("CORR",colnames(noNA_sNL))]  # noNA_sNL non-flash group
sN_f <- noNA_sN[noNA_sN$flash ==1,grepl("CORR",colnames(noNA_sN))]  # noNA_sN flash group
sN_n <- noNA_sN[noNA_sN$flash ==0,grepl("CORR",colnames(noNA_sN))]  # noNA_sN non-flash group



# sNL flash
x <- sNL_f
xcor <- polychoric(x)$rho
fa.parallel(xcor,n.obs=nrow(x))    # Parallel analysis suggests that the number of factors =  41  and the number of components =  18
nfactors(xcor,n.obs=nrow(x))       # 2, 17, 20 factors

mod <- mirt(x,2)
oblimin_loadings <- fa.sort(irt.fa(x,2)$fa)    # need the line under because of th extra plot
promax_loadings <- fa.sort(irt.fa(x,2,rotate="promax")$fa)  # loadings has factor loadings, phi has interfactor correlations
obli_exp_sNL_f <- data.frame(round(oblimin_loadings$loadings,3)[1:180,1:2])
pro_exp_sNL_f <- data.frame(round(promax_loadings$loadings,3)[1:180,1:2])
obli_sum_sNL_f <- data.frame(summary(mod)$rotF)
pro_sum_sNL_f <-  data.frame(summary(mod,rotate="promax")$rotF[1:180,1:2])

# sNL non-flash
x <- sNL_n
xcor <- polychoric(x)$rho
fa.parallel(xcor,n.obs=nrow(x))    # Parallel analysis suggests that the number of factors =  50  and the number of components =  39 
nfactors(xcor,n.obs=nrow(x))       # 2, 6, 20 factors

mod <- mirt(x,2)
oblimin_loadings <- fa.sort(irt.fa(x,2)$fa)    # need the line under because of th extra plot
promax_loadings <- fa.sort(irt.fa(x,2,rotate="promax")$fa)  # loadings has factor loadings, phi has interfactor correlations
obli_exp_sNL_n <- data.frame(round(oblimin_loadings$loadings,3)[1:180,1:2])
pro_exp_sNL_n <- data.frame(round(promax_loadings$loadings,3)[1:180,1:2])
obli_sum_sNL_n <- data.frame(summary(mod)$rotF)
pro_sum_sNL_n <-  data.frame(summary(mod,rotate="promax")$rotF[1:180,1:2])

mod <- mirt(x,6)
oblimin_loadings <- fa.sort(irt.fa(x,6)$fa)    # need the line under because of th extra plot
promax_loadings <- fa.sort(irt.fa(x,6,rotate="promax")$fa)  # loadings has factor loadings, phi has interfactor correlations
obli_exp_sNL_f6 <- data.frame(round(oblimin_loadings$loadings,3)[1:180,1:6])
pro_exp_sNL_f6 <- data.frame(round(promax_loadings$loadings,3)[1:180,1:6])
obli_sum_sNL_f6 <- data.frame(summary(mod)$rotF)
pro_sum_sNL_f6 <-  data.frame(summary(mod,rotate="promax")$rotF[1:180,1:2])

# (a) CPT (SPCPTNL) flash vs non-flash comparison ----
names(obli_exp_sNL_f) <- c("FlashF1","FlashF2")
names(obli_exp_sNL_n) <- c("NFlashF1","NFlashF2")
obli_exp_sNL <- left_join(rownames_to_column(obli_exp_sNL_f),rownames_to_column(obli_exp_sNL_n),by="rowname")
obli_exp_sNLcorr <- cor(obli_exp_sNL[,c(2,4)])   # 0.949

names(pro_exp_sNL_f) <- c("FlashF1","FlashF2")
names(pro_exp_sNL_n) <- c("NFlashF1","NFlashF2")
pro_exp_sNL <- left_join(rownames_to_column(pro_exp_sNL_f),rownames_to_column(pro_exp_sNL_n),by="rowname")
pro_exp_sNLcorr <- cor(pro_exp_sNL[,c(2,4)])    # 1.000

names(obli_sum_sNL_f) <- c("FlashF1","FlashF2")
names(obli_sum_sNL_n) <- c("NFlashF1","NFlashF2")
obli_sum_sNL <- left_join(rownames_to_column(obli_sum_sNL_f),rownames_to_column(obli_sum_sNL_n),by="rowname")
obli_sum_sNLcorr <- cor(obli_sum_sNL[,c(2,4)])   # 0.939

names(pro_sum_sNL_f) <- c("FlashF1","FlashF2")
names(pro_sum_sNL_n) <- c("NFlashF1","NFlashF2")
pro_sum_sNL <- left_join(rownames_to_column(pro_sum_sNL_f),rownames_to_column(pro_sum_sNL_n),by="rowname")
pro_sum_sNLcorr <- cor(pro_sum_sNL[,c(2,4)])     # 0.943


# sN flash
x <- sN_f
xcor <- polychoric(x)$rho
fa.parallel(xcor,n.obs=nrow(x))    # Parallel analysis suggests that the number of factors =  17  and the number of components =  12
nfactors(xcor,n.obs=nrow(x))       # 2, 3, 15, 20 factors

mod <- mirt(x,2)
oblimin_loadings <- fa.sort(irt.fa(x,2)$fa)    # need the line under because of th extra plot
promax_loadings <- fa.sort(irt.fa(x,2,rotate="promax")$fa)  # loadings has factor loadings, phi has interfactor correlations
obli_exp_sN_f <- data.frame(round(oblimin_loadings$loadings,3)[1:90,1:2])
pro_exp_sN_f <- data.frame(round(promax_loadings$loadings,3)[1:90,1:2])
obli_sum_sN_f <- data.frame(summary(mod)$rotF)
pro_sum_sN_f <-  data.frame(summary(mod,rotate="promax")$rotF[1:90,1:2])

mod <- mirt(x,3)
oblimin_loadings <- fa.sort(irt.fa(x,3)$fa)    # need the line under because of th extra plot
promax_loadings <- fa.sort(irt.fa(x,3,rotate="promax")$fa)  # loadings has factor loadings, phi has interfactor correlations
obli_exp_sN_f3 <- data.frame(round(oblimin_loadings$loadings,3)[1:90,1:3])
pro_exp_sN_f3 <- data.frame(round(promax_loadings$loadings,3)[1:90,1:3])
obli_sum_sN_f3 <- data.frame(summary(mod)$rotF)
pro_sum_sN_f3 <-  data.frame(summary(mod,rotate="promax")$rotF[1:90,1:2])

# sN non-flash
x <- sN_n
xcor <- polychoric(x)$rho
fa.parallel(xcor,n.obs=nrow(x))    # Parallel analysis suggests that the number of factors =  50  and the number of components =  38 
nfactors(xcor,n.obs=nrow(x))       # 2, 6, 20 factors

mod <- mirt(x,2)
oblimin_loadings <- fa.sort(irt.fa(x,2)$fa)    # need the line under because of th extra plot
promax_loadings <- fa.sort(irt.fa(x,2,rotate="promax")$fa)  # loadings has factor loadings, phi has interfactor correlations
obli_exp_sN_n <- data.frame(round(oblimin_loadings$loadings,3)[1:90,1:2])
pro_exp_sN_n <- data.frame(round(promax_loadings$loadings,3)[1:90,1:2])
obli_sum_sN_n <- data.frame(summary(mod)$rotF)
pro_sum_sN_n <-  data.frame(summary(mod,rotate="promax")$rotF[1:90,1:2])

mod <- mirt(x,6)
oblimin_loadings <- fa.sort(irt.fa(x,6)$fa)    # need the line under because of th extra plot
promax_loadings <- fa.sort(irt.fa(x,6,rotate="promax")$fa)  # loadings has factor loadings, phi has interfactor correlations
obli_exp_sN_n6 <- data.frame(round(oblimin_loadings$loadings,3)[1:90,1:6])
pro_exp_sN_n6 <- data.frame(round(promax_loadings$loadings,3)[1:90,1:6])
obli_sum_sN_n6 <- data.frame(summary(mod)$rotF)
pro_sum_sN_n6 <-  data.frame(summary(mod,rotate="promax")$rotF[1:90,1:2])

# (b) CPT (SPCPTN90) flash vs non-flash comparison ----
names(obli_exp_sN_f) <- c("FlashF1","FlashF2")
names(obli_exp_sN_n) <- c("NFlashF1","NFlashF2")
obli_exp_sN <- left_join(rownames_to_column(obli_exp_sN_f),rownames_to_column(obli_exp_sN_n),by="rowname")
obli_exp_sNcorr <- cor(obli_exp_sN[,c(2,4)])   # 0.826

names(pro_exp_sN_f) <- c("FlashF1","FlashF2")
names(pro_exp_sN_n) <- c("NFlashF1","NFlashF2")
pro_exp_sN <- left_join(rownames_to_column(pro_exp_sN_f),rownames_to_column(pro_exp_sN_n),by="rowname")
pro_exp_sNcorr <- cor(pro_exp_sN[,c(2,4)])    # 1.000

names(obli_sum_sN_f) <- c("FlashF1","FlashF2")
names(obli_sum_sN_n) <- c("NFlashF1","NFlashF2")
obli_sum_sN <- left_join(rownames_to_column(obli_sum_sN_f),rownames_to_column(obli_sum_sN_n),by="rowname")
obli_sum_sNcorr <- cor(obli_sum_sN[,c(2,4)])   # 0.800

names(pro_sum_sN_f) <- c("FlashF1","FlashF2")
names(pro_sum_sN_n) <- c("NFlashF1","NFlashF2")
pro_sum_sN <- left_join(rownames_to_column(pro_sum_sN_f),rownames_to_column(pro_sum_sN_n),by="rowname")
pro_sum_sNcorr <- cor(pro_sum_sN[,c(2,4)])     # 0.815






# * t-tests ----

# ** difference in accuracy means ----
textsAcc <- setdiff(texts, c("MPRACT","SCTAP"))
testsAcc <- mget(textsAcc)   # 15 tests
cutoff <- as.Date("2019-12-31")
for (i in 1:length(textsAcc)) {
  test <- testsAcc[[i]]
  name <- paste0(textsAcc[i],"sumAcc")
  sumAcc <- c(textsAcc[i])
  
  # alldates
  fit <- gam(Accuracy ~ s(age,k=3) + gender, data = test)
  
  # save summary of this model as a variable
  fitAD <- summary(fit)
  sumAcc <- c(sumAcc,"fitAD")
  
  visreg(fit,"age", by="gender", main=paste(textsAcc[i],"AD"))
  
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
  boxAD <- ggplot(res, aes(x=factor(flash),y=residuals,group=flash)) +
    geom_boxplot() +
    labs(title = paste("Accuracy Difference in",textsAcc[i],"(All dates)")) +
    ylab("Residuals") +
    scale_x_discrete("Flash",breaks=0:1,labels=c("Non-Flash", "Flash")) +
    ylim(-5,5)
  sumAcc <- c(sumAcc,"boxAD")
  nAD <- res %>%
    group_by(flash) %>%
    summarise(mean=mean(residuals),median=median(residuals),n=n())
  sumAcc <- c(sumAcc,"nAD")
  
  
  # lastyear
  lastyear <- test[test$dotest >= cutoff,]
  fit <- gam(Accuracy ~ s(age,k=3) + gender, data = lastyear)
  
  # save summary of this model as a variable
  fitLY <- summary(fit)
  sumAcc <- c(sumAcc,"fitLY")
  
  visreg(fit,"age", by="gender", main=paste(textsAcc[i],"LY"))
  
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
  boxLY <- ggplot(res, aes(x=factor(flash),y=residuals, group=flash)) +
    geom_boxplot() +
    labs(title = paste("Accuracy Difference in",textsAcc[i],"(Last year)")) +
    ylab("Residuals") +
    scale_x_discrete("Flash",breaks=0:1,labels=c("Non-Flash", "Flash")) +
    ylim(-5,5)
  sumAcc <- c(sumAcc,"boxLY")
  nLY <- res %>%
    group_by(flash) %>%
    summarise(mean=mean(residuals),median=median(residuals),n=n())
  sumAcc <- c(sumAcc,"nLY")
  
  # use newtest to figure out proportion of participants included in both Flash and non-Flash groups
  f_sub <- unique(newtest[newtest$flash==1,3])               # subjects in the flash group
  both_sub <- intersect(f_sub, newtest[newtest$flash==0,3])  # subjects included in both groups
  sumAcc <- c(sumAcc,"both_sub")
  
  sumAcc <- c(sumAcc[1],mget(sumAcc[-1]))
  assign(name,sumAcc)
}

# ** difference in speed means ----
for (i in 1:length(texts)) {
  test <- tests[[i]]
  name <- paste0(texts[i],"sumSp")
  sumSp <- c(texts[i])
  
  # alldates
  fit <- gam(Speed ~ s(age,k=3) + gender, data = test)
  
  fitAD <- summary(fit)
  sumSp <- c(sumSp,"fitAD")
  
  visreg(fit,"age", by="gender", main=paste(texts[i],"AD"))
  
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
  boxAD <- ggplot(res, aes(x=factor(flash),y=residuals,group=flash)) +
    geom_boxplot() +
    labs(title = paste("Speed Differences in",texts[i],"(All dates)")) +
    ylab("Residuals") +
    scale_x_discrete("Flash",breaks=0:1,labels=c("Non-Flash", "Flash")) +
    ylim(-5,5)
  sumSp <- c(sumSp,"boxAD")
  nAD <- res %>%
    group_by(flash) %>%
    summarise(mean=mean(residuals),median=median(residuals),n=n())
  sumSp <- c(sumSp,"nAD")
  
  if (texts[i]!="VSPLOT24") {
    # lastyear
    lastyear <- test[test$dotest >= cutoff,]
    fit <- gam(Speed ~ s(age,k=3) + gender, data = lastyear)
    
    fitLY <- summary(fit)
    sumSp <- c(sumSp,"fitLY")
    
    visreg(fit,"age", by="gender", main=paste(texts[i],"LY"))
    
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
    boxLY <- ggplot(res, aes(x=factor(flash),y=residuals, group=flash)) +
      geom_boxplot() +
      labs(title = paste("Speed Differences in",texts[i],"(Last year)")) +
      ylab("Residuals") +
      scale_x_discrete("Flash",breaks=0:1,labels=c("Non-Flash", "Flash")) +
      ylim(-5,5)
    sumSp <- c(sumSp,"boxLY")
    nLY <- res %>%
      group_by(flash) %>%
      summarise(mean=mean(residuals),median=median(residuals),n=n())
    sumSp <- c(sumSp,"nLY")
  }
  
  # use newtest to figure out proportion of participants included in both Flash and non-Flash groups
  f_sub <- unique(newtest[newtest$flash==1,3])               # subjects in the flash group
  both_sub <- intersect(f_sub, newtest[newtest$flash==0,3])  # subjects included in both groups
  sumAcc <- c(sumAcc,"both_sub")
  
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




# * flash/non-flash intra-subject correlations ----
textsAcc <- setdiff(textsAcc, c("CPF_A"))
testsAcc <- mget(textsAcc)   # 15 tests
for (j in 1:length(textsAcc)){
  test <- testsAcc[[j]]
  test <- test[!is.na(test$bblid) & !is.na(test$Accuracy),]
  
  hist <- ggplot(test,aes(x=Accuracy)) + geom_histogram()    # histogram to look at item acc frequency
  hist
  
  test <- test[!is.na(test$age),]
  if (any(j==c(2,3,11))) {     # regress age out
    fit <- gam(Accuracy ~ s(age,k=3), data = test)
  } else {
    fit <- gam(Accuracy ~ s(age), data = test)
  }
  visreg(fit,main=textsAcc[j])
  test$acc_res <- scale(resid(fit))
  test$spe_res <- 0 # spe_res doesn't matter here
  
  test <- test[abs(test$acc_res)<3,]
  
  flash <- unique(test[test$flash==1 & !is.na(test$unique_id),3])
  nflash <- unique(test[test$flash==0 & !is.na(test$unique_id),3])
  
  both <- intersect(flash, nflash)
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
  
  tpflash <- flash[,c(1:4,8,17,19:20)]    #[t]ime [p]oint [flash]
  tpflash$timepoint <- 1
  for (i in 1:(nrow(tpflash)-1)) {
    if (tpflash$bblid[i+1] == tpflash$bblid[i]) {
      tpflash$timepoint[i+1] <- tpflash$timepoint[i] + 1
    }
  }
  tpnflash <- nflash[,c(1:4,8,17,19:20)]    #[t]ime [p]oint [n]on-[flash] used to be columns 1,4,6:7
  tpnflash$timepoint <- 1
  for (i in 1:(nrow(tpnflash)-1)) {
    if (tpnflash$bblid[i+1] == tpnflash$bblid[i]) {
      tpnflash$timepoint[i+1] <- tpnflash$timepoint[i] + 1
    }
  }
  
  fsite <- tpflash[tpflash$timepoint==1,3:4]
  nfsite <- tpnflash[tpnflash$timepoint==1,3:4]
  siteid <- merge(fsite,nfsite,by=1)
  names(siteid)[2:3] <- c("fsiteid","nfsiteid")
  
  wideflash <- reshape(tpflash[,c(3,5:9)],
                       idvar = "bblid",
                       timevar = "timepoint",
                       direction = "wide")
  widenflash <- reshape(tpnflash[,c(3,5:9)],
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
  assign(paste0(textsAcc[j],"siteid"),siteid)
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
acc <- data.frame(merge(accflash,accnflash, by=1))
acc_cor <- cor(acc[,-1], use="pairwise")
pairs.panels(acc[,c(2,13)],lm=TRUE)    # looking at t1 flash vs nflash
acc <- acc[abs(acc$f_acc_res.1-acc$n_acc_res.1)<2,]
pairs.panels(acc[,c(2,13)],lm=TRUE)
acc$f_acc_res.1 <- winsor(acc$f_acc_res.1,trim=0.01)
acc$n_acc_res.1 <- winsor(acc$n_acc_res.1,trim=0.01)
pairs.panels(acc[,c(2,13)],lm=TRUE)
ADT36_A_acc <- acc  
acctxt <- c("ADT36_A_acc")

wideflash <- AIMwideflash
widenflash <- AIMwidenflash
accflash <- wideflash[,grepl("bblid", colnames(wideflash)) | grepl("acc_res.1", colnames(wideflash)) | grepl("newscore", colnames(wideflash))]
names(accflash)[-1] <- paste0("f_",names(accflash)[-1])
accnflash <- widenflash[,grepl("bblid", colnames(widenflash)) | grepl("acc_res.1", colnames(widenflash)) | grepl("newscore", colnames(widenflash))]
names(accnflash)[-1] <- paste0("n_",names(accnflash)[-1])
acc <- data.frame(merge(accflash,accnflash, by=1))
acc_cor <- cor(acc[,-1], use="pairwise")
pairs.panels(acc[,c(2,11)],lm=TRUE)    # looking at t1 flash vs nflash
acc <- acc[abs(acc$f_acc_res.1-acc$n_acc_res.1)<2,]
pairs.panels(acc[,c(2,11)],lm=TRUE)
acc$f_acc_res.1 <- winsor(acc$f_acc_res.1,trim=0.01)
acc$n_acc_res.1 <- winsor(acc$n_acc_res.1,trim=0.01)
pairs.panels(acc[,c(2,11)],lm=TRUE)
AIM_acc <- acc  
acctxt <- c(acctxt,"AIM_acc")

wideflash <- CPF_Bwideflash
widenflash <- CPF_Bwidenflash
accflash <- wideflash[,grepl("bblid", colnames(wideflash)) | grepl("acc_res.1", colnames(wideflash)) | grepl("newscore", colnames(wideflash))]
names(accflash)[-1] <- paste0("f_",names(accflash)[-1])
accnflash <- widenflash[,grepl("bblid", colnames(widenflash)) | grepl("acc_res.1", colnames(widenflash)) | grepl("newscore", colnames(widenflash))]
names(accnflash)[-1] <- paste0("n_",names(accnflash)[-1])
acc <- data.frame(merge(accflash,accnflash, by=1))
acc_cor <- cor(acc[,-1], use="pairwise")
pairs.panels(acc[,c(2,11)],lm=TRUE)    # looking at t1 flash vs nflash
acc <- acc[abs(acc$f_acc_res.1-acc$n_acc_res.1)<2,]
pairs.panels(acc[,c(2,11)],lm=TRUE)
acc$f_acc_res.1 <- winsor(acc$f_acc_res.1,trim=0.01)
acc$n_acc_res.1 <- winsor(acc$n_acc_res.1,trim=0.01)
pairs.panels(acc[,c(2,11)],lm=TRUE)
CPF_B_acc <- acc  
acctxt <- c(acctxt,"CPF_B_acc")

wideflash <- ER40_Dwideflash
widenflash <- ER40_Dwidenflash
accflash <- wideflash[,grepl("bblid", colnames(wideflash)) | grepl("acc_res.1", colnames(wideflash)) | grepl("newscore", colnames(wideflash))]
names(accflash)[-1] <- paste0("f_",names(accflash)[-1])
accnflash <- widenflash[,grepl("bblid", colnames(widenflash)) | grepl("acc_res.1", colnames(widenflash)) | grepl("newscore", colnames(widenflash))]
names(accnflash)[-1] <- paste0("n_",names(accnflash)[-1])
acc <- data.frame(merge(accflash,accnflash, by=1))
acc_cor <- cor(acc[,-1], use="pairwise")
pairs.panels(acc[,c(2,13)],lm=TRUE)    # looking at t1 flash vs nflash
acc <- acc[abs(acc$f_acc_res.1-acc$n_acc_res.1)<2,]
pairs.panels(acc[,c(2,13)],lm=TRUE)
acc$f_acc_res.1 <- winsor(acc$f_acc_res.1,trim=0.01)
acc$n_acc_res.1 <- winsor(acc$n_acc_res.1,trim=0.01)
pairs.panels(acc[,c(2,13)],lm=TRUE)
ER40_D_acc <- acc  
acctxt <- c(acctxt,"ER40_D_acc")

wideflash <- GNG150wideflash
widenflash <- GNG150widenflash
accflash <- wideflash[,grepl("bblid", colnames(wideflash)) | grepl("acc_res.1", colnames(wideflash)) | grepl("newscore", colnames(wideflash))]
names(accflash)[-1] <- paste0("f_",names(accflash)[-1])
accnflash <- widenflash[,grepl("bblid", colnames(widenflash)) | grepl("acc_res.1", colnames(widenflash)) | grepl("newscore", colnames(widenflash))]
names(accnflash)[-1] <- paste0("n_",names(accnflash)[-1])
acc <- data.frame(merge(accflash,accnflash, by=1))
acc_cor <- cor(acc[,-1], use="pairwise")
pairs.panels(acc[,c(2,13)],lm=TRUE)    # looking at t1 flash vs nflash
acc <- acc[abs(acc$f_acc_res.1-acc$n_acc_res.1)<2,]
pairs.panels(acc[,c(2,13)],lm=TRUE)
acc$f_acc_res.1 <- winsor(acc$f_acc_res.1,trim=0.01)
acc$n_acc_res.1 <- winsor(acc$n_acc_res.1,trim=0.01)
pairs.panels(acc[,c(2,13)],lm=TRUE)
GNG150_acc <- acc  
acctxt <- c(acctxt,"GNG150_acc")

wideflash <- KCPW_Awideflash
widenflash <- KCPW_Awidenflash
accflash <- wideflash[,grepl("bblid", colnames(wideflash)) | grepl("acc_res.1", colnames(wideflash)) | grepl("newscore", colnames(wideflash))]
names(accflash)[-1] <- paste0("f_",names(accflash)[-1])
accnflash <- widenflash[,grepl("bblid", colnames(widenflash)) | grepl("acc_res.1", colnames(widenflash)) | grepl("newscore", colnames(widenflash))]
names(accnflash)[-1] <- paste0("n_",names(accnflash)[-1])
acc <- data.frame(merge(accflash,accnflash, by=1))
acc_cor <- cor(acc[,-1], use="pairwise")
pairs.panels(acc[,c(2,15)],lm=TRUE)    # looking at t1 flash vs nflash
acc <- acc[abs(acc$f_acc_res.1-acc$n_acc_res.1)<2,]
pairs.panels(acc[,c(2,15)],lm=TRUE)
acc$f_acc_res.1 <- winsor(acc$f_acc_res.1,trim=0.01)
acc$n_acc_res.1 <- winsor(acc$n_acc_res.1,trim=0.01)
pairs.panels(acc[,c(2,15)],lm=TRUE)
KCPW_A_acc <- acc  
acctxt <- c(acctxt,"KCPW_A_acc")

wideflash <- KSPVRT_Dwideflash
widenflash <- KSPVRT_Dwidenflash
accflash <- wideflash[,grepl("bblid", colnames(wideflash)) | grepl("acc_res.1", colnames(wideflash)) | grepl("newscore", colnames(wideflash))]
names(accflash)[-1] <- paste0("f_",names(accflash)[-1])
accnflash <- widenflash[,grepl("bblid", colnames(widenflash)) | grepl("acc_res.1", colnames(widenflash)) | grepl("newscore", colnames(widenflash))]
names(accnflash)[-1] <- paste0("n_",names(accnflash)[-1])
acc <- data.frame(merge(accflash,accnflash, by=1))
acc_cor <- cor(acc[,-1], use="pairwise")
pairs.panels(acc[,c(2,13)],lm=TRUE)    # looking at t1 flash vs nflash
acc <- acc[abs(acc$f_acc_res.1-acc$n_acc_res.1)<2,]
pairs.panels(acc[,c(2,13)],lm=TRUE)
acc$f_acc_res.1 <- winsor(acc$f_acc_res.1,trim=0.01)
acc$n_acc_res.1 <- winsor(acc$n_acc_res.1,trim=0.01)
pairs.panels(acc[,c(2,13)],lm=TRUE)
KSPVRT_D_acc <- acc  
acctxt <- c(acctxt,"KSPVRT_D_acc")

wideflash <- MEDF36_Awideflash
widenflash <- MEDF36_Awidenflash
accflash <- wideflash[,grepl("bblid", colnames(wideflash)) | grepl("acc_res.1", colnames(wideflash)) | grepl("newscore", colnames(wideflash))]
names(accflash)[-1] <- paste0("f_",names(accflash)[-1])
accnflash <- widenflash[,grepl("bblid", colnames(widenflash)) | grepl("acc_res.1", colnames(widenflash)) | grepl("newscore", colnames(widenflash))]
names(accnflash)[-1] <- paste0("n_",names(accnflash)[-1])
acc <- data.frame(merge(accflash,accnflash, by=1))
acc_cor <- cor(acc[,-1], use="pairwise")
pairs.panels(acc[,c(2,13)],lm=TRUE)    # looking at t1 flash vs nflash
acc <- acc[abs(acc$f_acc_res.1-acc$n_acc_res.1)<2,]
pairs.panels(acc[,c(2,13)],lm=TRUE)
acc$f_acc_res.1 <- winsor(acc$f_acc_res.1,trim=0.01)
acc$n_acc_res.1 <- winsor(acc$n_acc_res.1,trim=0.01)
pairs.panels(acc[,c(2,13)],lm=TRUE)
MEDF36_A_acc <- acc  
acctxt <- c(acctxt,"MEDF36_A_acc")

wideflash <- PCET_Awideflash
widenflash <- PCET_Awidenflash
accflash <- wideflash[,grepl("bblid", colnames(wideflash)) | grepl("acc_res.1", colnames(wideflash)) | grepl("newscore", colnames(wideflash))]
names(accflash)[-1] <- paste0("f_",names(accflash)[-1])
accnflash <- widenflash[,grepl("bblid", colnames(widenflash)) | grepl("acc_res.1", colnames(widenflash)) | grepl("newscore", colnames(widenflash))]
names(accnflash)[-1] <- paste0("n_",names(accnflash)[-1])
acc <- data.frame(merge(accflash,accnflash, by=1))
acc_cor <- cor(acc[,-1], use="pairwise")
pairs.panels(acc[,c(2,5)],lm=TRUE)    # looking at t1 flash vs nflash
acc <- acc[abs(acc$f_acc_res.1-acc$n_acc_res.1)<2,]
pairs.panels(acc[,c(2,5)],lm=TRUE)
acc$f_acc_res.1 <- winsor(acc$f_acc_res.1,trim=0.01)
acc$n_acc_res.1 <- winsor(acc$n_acc_res.1,trim=0.01)
pairs.panels(acc[,c(2,5)],lm=TRUE)
PCET_A_acc <- acc
acctxt <- c(acctxt,"PCET_A_acc")

wideflash <- PMAT24_Awideflash
widenflash <- PMAT24_Awidenflash
accflash <- wideflash[,grepl("bblid", colnames(wideflash)) | grepl("acc_res.1", colnames(wideflash)) | grepl("newscore", colnames(wideflash))]
names(accflash)[-1] <- paste0("f_",names(accflash)[-1])
accnflash <- widenflash[,grepl("bblid", colnames(widenflash)) | grepl("acc_res.1", colnames(widenflash)) | grepl("newscore", colnames(widenflash))]
names(accnflash)[-1] <- paste0("n_",names(accnflash)[-1])
acc <- data.frame(merge(accflash,accnflash, by=1))
acc_cor <- cor(acc[,-1], use="pairwise")
pairs.panels(acc[,c(2,11)],lm=TRUE)    # looking at t1 flash vs nflash
acc <- acc[abs(acc$f_acc_res.1-acc$n_acc_res.1)<2,]
pairs.panels(acc[,c(2,11)],lm=TRUE)
acc$f_acc_res.1 <- winsor(acc$f_acc_res.1,trim=0.01)
acc$n_acc_res.1 <- winsor(acc$n_acc_res.1,trim=0.01)
pairs.panels(acc[,c(2,11)],lm=TRUE)
PMAT24_A_acc <- acc  
acctxt <- c(acctxt,"PMAT24_A_acc")

wideflash <- SLNB2_90wideflash
widenflash <- SLNB2_90widenflash
accflash <- wideflash[,grepl("bblid", colnames(wideflash)) | grepl("acc_res.1", colnames(wideflash)) | grepl("newscore", colnames(wideflash))]
names(accflash)[-1] <- paste0("f_",names(accflash)[-1])
accnflash <- widenflash[,grepl("bblid", colnames(widenflash)) | grepl("acc_res.1", colnames(widenflash)) | grepl("newscore", colnames(widenflash))]
names(accnflash)[-1] <- paste0("n_",names(accnflash)[-1])
acc <- data.frame(merge(accflash,accnflash, by=1))
acc_cor <- cor(acc[,-1], use="pairwise")
pairs.panels(acc[,c(2,15)],lm=TRUE)    # looking at t1 flash vs nflash
acc <- acc[abs(acc$f_acc_res.1-acc$n_acc_res.1)<2,]
pairs.panels(acc[,c(2,15)],lm=TRUE)
acc$f_acc_res.1 <- winsor(acc$f_acc_res.1,trim=0.01)
acc$n_acc_res.1 <- winsor(acc$n_acc_res.1,trim=0.01)
pairs.panels(acc[,c(2,15)],lm=TRUE)
SLNB2_90_acc <- acc  
acctxt <- c(acctxt,"SLNB2_90_acc")

# wideflash <- SPCPTN90wideflash                   # SPCPTN90 only has two participants that have taken both flash and non-flash
# widenflash <- SPCPTN90widenflash
# accflash <- wideflash[,grepl("bblid", colnames(wideflash)) | grepl("acc_res.1", colnames(wideflash)) | grepl("newscore", colnames(wideflash))]
# names(accflash)[-1] <- paste0("f_",names(accflash)[-1])
# accnflash <- widenflash[,grepl("bblid", colnames(widenflash)) | grepl("acc_res.1", colnames(widenflash)) | grepl("newscore", colnames(widenflash))]
# names(accnflash)[-1] <- paste0("n_",names(accnflash)[-1])
# acc <- merge(accflash,accnflash, by=1)
# acc_cor <- cor(acc[,-1], use="pairwise")
# pairs.panels(acc[,c(2,3)],lm=TRUE)    # looking at t1 flash vs nflash
# acc$rm <- ifelse(abs(acc$f_acc_res.1-acc$n_acc_res.1)>=2,1,0)
# acc <- acc[acc$rm == 0,-which(names(acc) %in% "rm")]
# pairs.panels(acc[,c(2,15)],lm=TRUE)
# SPCPTN90_acc <- acc
# acctxt <- c(acctxt,"SLNB2_90_acc")

wideflash <- SPCPTNLwideflash
widenflash <- SPCPTNLwidenflash
accflash <- wideflash[,grepl("bblid", colnames(wideflash)) | grepl("acc_res.1", colnames(wideflash)) | grepl("newscore", colnames(wideflash))]
names(accflash)[-1] <- paste0("f_",names(accflash)[-1])
accnflash <- widenflash[,grepl("bblid", colnames(widenflash)) | grepl("acc_res.1", colnames(widenflash)) | grepl("newscore", colnames(widenflash))]
names(accnflash)[-1] <- paste0("n_",names(accnflash)[-1])
acc <- data.frame(merge(accflash,accnflash, by=1))
acc_cor <- cor(acc[,-1], use="pairwise")
pairs.panels(acc[,c(2,17)],lm=TRUE)    # looking at t1 flash vs nflash
acc <- acc[abs(acc$f_acc_res.1-acc$n_acc_res.1)<2,]
pairs.panels(acc[,c(2,17)],lm=TRUE)
acc$f_acc_res.1 <- winsor(acc$f_acc_res.1,trim=0.01)
acc$n_acc_res.1 <- winsor(acc$n_acc_res.1,trim=0.01)
pairs.panels(acc[,c(2,17)],lm=TRUE)
SPCPTNL_acc <- acc  
acctxt <- c(acctxt,"SPCPTNL_acc")

wideflash <- SVOLT_Awideflash
widenflash <- SVOLT_Awidenflash
accflash <- wideflash[,grepl("bblid", colnames(wideflash)) | grepl("acc_res.1", colnames(wideflash)) | grepl("newscore", colnames(wideflash))]
names(accflash)[-1] <- paste0("f_",names(accflash)[-1])
accnflash <- widenflash[,grepl("bblid", colnames(widenflash)) | grepl("acc_res.1", colnames(widenflash)) | grepl("newscore", colnames(widenflash))]
names(accnflash)[-1] <- paste0("n_",names(accnflash)[-1])
acc <- data.frame(merge(accflash,accnflash, by=1))
acc_cor <- cor(acc[,-1], use="pairwise")
pairs.panels(acc[,c(2,9)],lm=TRUE)    # looking at t1 flash vs nflash
acc <- acc[abs(acc$f_acc_res.1-acc$n_acc_res.1)<2,]
pairs.panels(acc[,c(2,9)],lm=TRUE)
acc$f_acc_res.1 <- winsor(acc$f_acc_res.1,trim=0.01)
acc$n_acc_res.1 <- winsor(acc$n_acc_res.1,trim=0.01)
pairs.panels(acc[,c(2,9)],lm=TRUE)
SVOLT_A_acc <- acc  
acctxt <- c(acctxt,"SVOLT_A_acc")

wideflash <- VSPLOT15wideflash
widenflash <- VSPLOT15widenflash
accflash <- wideflash[,grepl("bblid", colnames(wideflash)) | grepl("acc_res.1", colnames(wideflash)) | grepl("newscore", colnames(wideflash))]
names(accflash)[-1] <- paste0("f_",names(accflash)[-1])
accnflash <- widenflash[,grepl("bblid", colnames(widenflash)) | grepl("acc_res.1", colnames(widenflash)) | grepl("newscore", colnames(widenflash))]
names(accnflash)[-1] <- paste0("n_",names(accnflash)[-1])
acc <- data.frame(merge(accflash,accnflash, by=1))
acc_cor <- cor(acc[,-1], use="pairwise")
pairs.panels(acc[,c(2,9)],lm=TRUE)    # looking at t1 flash vs nflash
acc <- acc[abs(acc$f_acc_res.1-acc$n_acc_res.1)<2,]
pairs.panels(acc[,c(2,9)],lm=TRUE)
acc$f_acc_res.1 <- winsor(acc$f_acc_res.1,trim=0.01)
acc$n_acc_res.1 <- winsor(acc$n_acc_res.1,trim=0.01)
pairs.panels(acc[,c(2,9)],lm=TRUE)
VSPLOT15_acc <- acc  
acctxt <- c(acctxt,"VSPLOT15_acc")

accs <- mget(acctxt)

# ** acc_cor and icc for each test ----
textsAcc <- setdiff(textsAcc, c("SPCPTN90"))
testsAcc <- mget(textsAcc)    # 14 tests
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
  try(icc_n12_1 <- icc(acc[,grepl("n_acc_res",colnames(acc)) | grepl("n_t2newscore1",colnames(acc))],type="agreement",model="twoway")$value)
  try(icc_n13_1 <- icc(acc[,grepl("n_acc_res",colnames(acc)) | grepl("n_t2newscore1",colnames(acc)) | grepl("n_t3newscore1",colnames(acc))],type="agreement",model="twoway")$value)
  try(icc_n14_1 <- icc(acc[,grepl("n_acc_res",colnames(acc)) | grepl("n_t2newscore1",colnames(acc)) | grepl("n_t3newscore1",colnames(acc)) | grepl("n_t4newscore1",colnames(acc))],type="agreement",model="twoway")$value)
  try(icc_n12_2 <- icc(acc[,grepl("n_acc_res",colnames(acc)) | grepl("n_t2newscore2",colnames(acc))],type="agreement",model="twoway")$value)
  try(icc_n13_2 <- icc(acc[,grepl("n_acc_res",colnames(acc)) | grepl("n_t2newscore2",colnames(acc)) | grepl("n_t3newscore2",colnames(acc))],type="agreement",model="twoway")$value)
  try(icc_n14_2 <- icc(acc[,grepl("n_acc_res",colnames(acc)) | grepl("n_t2newscore2",colnames(acc)) | grepl("n_t3newscore2",colnames(acc)) | grepl("n_t4newscore2",colnames(acc))],type="agreement",model="twoway")$value)
  
  icc <- data.frame(matrix(c(icc_fnf,rep(NA,13)),nrow=2))
  names(icc) <- c("icc_fnf","icc_f12_1","icc_f13_1","icc_f14_1","icc_f12_2","icc_f13_2","icc_f14_2")
  try(if (icc_f12_1) {icc[1,2] <- icc_f12_1})
  try(if (icc_f13_1) {icc[1,3] <- icc_f13_1})
  try(if (icc_f14_1) {icc[1,4] <- icc_f14_1})
  try(if (icc_f12_2) {icc[1,5] <- icc_f12_2})
  try(if (icc_f13_2) {icc[1,6] <- icc_f13_2})
  try(if (icc_f14_2) {icc[1,7] <- icc_f14_2})
  try(if (icc_n12_1) {icc[2,2] <- icc_n12_1})
  try(if (icc_n13_1) {icc[2,3] <- icc_n13_1})
  try(if (icc_n14_1) {icc[2,4] <- icc_n14_1})
  try(if (icc_n12_2) {icc[2,5] <- icc_n12_2})
  try(if (icc_n13_2) {icc[2,6] <- icc_n13_2})
  try(if (icc_n14_2) {icc[2,7] <- icc_n14_2})
  
  assign(paste0(textsAcc[i],"icc"),icc)
}

write.csv(ADT36_Aacc_cor, "myresults/instrasubject_corr/acc/ADT36_Aacc_cor.csv")
write.csv(AIMacc_cor,     "myresults/instrasubject_corr/acc/AIMacc_cor.csv")
write.csv(CPF_Bacc_cor,   "myresults/instrasubject_corr/acc/CPF_Bacc_cor.csv")
write.csv(ER40_Dacc_cor,  "myresults/instrasubject_corr/acc/ER40_Dacc_cor.csv")
write.csv(GNG150acc_cor,  "myresults/instrasubject_corr/acc/GNG150acc_cor.csv")
write.csv(KCPW_Aacc_cor,  "myresults/instrasubject_corr/acc/KCPW_Aacc_cor.csv")
write.csv(KSPVRT_Dacc_cor,"myresults/instrasubject_corr/acc/KSPVRT_Dacc_cor.csv")
write.csv(MEDF36_Aacc_cor,"myresults/instrasubject_corr/acc/MEDF36_Aacc_cor.csv")
write.csv(PCET_Aacc_cor,  "myresults/instrasubject_corr/acc/PCET_Aacc_cor.csv")
write.csv(PMAT24_Aacc_cor,"myresults/instrasubject_corr/acc/PMAT24_Aacc_cor.csv")
write.csv(SLNB2_90acc_cor,"myresults/instrasubject_corr/acc/SLNB2_90acc_cor.csv")
write.csv(SPCPTNLacc_cor, "myresults/instrasubject_corr/acc/SPCPTNLacc_cor.csv")
write.csv(SVOLT_Aacc_cor, "myresults/instrasubject_corr/acc/SVOLT_Aacc_cor.csv")
write.csv(VSPLOT15acc_cor,"myresults/instrasubject_corr/acc/VSPLOT15acc_cor.csv")

write.csv( ADT36_Aicc,"myresults/instrasubject_corr/acc/ADT36_Aicc.csv")
write.csv(     AIMicc,"myresults/instrasubject_corr/acc/AIMicc.csv")
write.csv(   CPF_Bicc,"myresults/instrasubject_corr/acc/CPF_Bicc.csv")
write.csv(  ER40_Dicc,"myresults/instrasubject_corr/acc/ER40_Dicc.csv")
write.csv(  GNG150icc,"myresults/instrasubject_corr/acc/GNG150icc.csv")
write.csv(  KCPW_Aicc,"myresults/instrasubject_corr/acc/KCPW_Aicc.csv")
write.csv(KSPVRT_Dicc,"myresults/instrasubject_corr/acc/KSPVRT_Dicc.csv")
write.csv(MEDF36_Aicc,"myresults/instrasubject_corr/acc/MEDF36_Aicc.csv")
write.csv(  PCET_Aicc,"myresults/instrasubject_corr/acc/PCET_Aicc.csv")
write.csv(PMAT24_Aicc,"myresults/instrasubject_corr/acc/PMAT24_Aicc.csv")
write.csv(SLNB2_90icc,"myresults/instrasubject_corr/acc/SLNB2_90icc.csv")
write.csv( SPCPTNLicc,"myresults/instrasubject_corr/acc/SPCPTNLicc.csv")
write.csv( SVOLT_Aicc,"myresults/instrasubject_corr/acc/SVOLT_Aicc.csv")
write.csv(VSPLOT15icc,"myresults/instrasubject_corr/acc/VSPLOT15icc.csv")


# site differences ----
pairs.panels(ADT36_A_acc[,c(2,13)],lm=TRUE)
ADT36_A_acc <- left_join(ADT36_A_acc,ADT36_Asiteid,by="bblid")

adtsites <- ggplot(data=ADT36_A_acc, aes(x=f_acc_res.1,y=n_acc_res.1)) + 
  geom_point() + geom_smooth(method = lm)
adtsites
adtsitepanels <- adtsites + facet_grid(rows=vars(fsiteid), cols=vars(nfsiteid)) +
  coord_cartesian(ylim = c(-2, 2),xlim = c(-2, 2)) +
  labs(title="ADT36 Accuracy site differences")
adtsitepanels

# using face wrap instead to adjust coordinates + only show plots with real data
adtsites + facet_wrap(vars(fsiteid, nfsiteid)) +
  coord_cartesian(ylim = c(-2, 2),xlim = c(-2, 2))


pairs.panels(AIM_acc[,c(2,11)],lm=TRUE)
AIM_acc <- left_join(AIM_acc,AIMsiteid,by="bblid")

aimsites <- ggplot(data=AIM_acc, aes(x=f_acc_res.1,y=n_acc_res.1)) + 
  geom_point() + geom_smooth(method = lm)
aimsites
aimsitepanels <- aimsites + facet_grid(rows=vars(fsiteid), cols=vars(nfsiteid)) +
  coord_cartesian(ylim = c(-2, 2),xlim = c(-2, 2)) +
  labs(title="AIM Accuracy site differences")
aimsitepanels


pairs.panels(CPF_B_acc[,c(2,11)],lm=TRUE)
CPF_B_acc <- left_join(CPF_B_acc,CPF_Bsiteid,by="bblid")

cpfsites <- ggplot(data=CPF_B_acc, aes(x=f_acc_res.1,y=n_acc_res.1)) + 
  geom_point() + geom_smooth(method = lm)
cpfsites
cpfsitepanels <- cpfsites + facet_grid(rows=vars(fsiteid), cols=vars(nfsiteid)) +
  coord_cartesian(ylim = c(-2, 2),xlim = c(-2, 2)) +
  labs(title="CPF Accuracy site differences")
cpfsitepanels


pairs.panels(ER40_D_acc[,c(2,13)],lm=TRUE)
ER40_D_acc <- left_join(ER40_D_acc,ER40_Dsiteid,by="bblid")

er40sites <- ggplot(data=ER40_D_acc, aes(x=f_acc_res.1,y=n_acc_res.1)) + 
  geom_point() + geom_smooth(method = lm)
er40sites
er40sitepanels <- er40sites + facet_grid(rows=vars(fsiteid), cols=vars(nfsiteid)) +
  coord_cartesian(ylim = c(-2, 2),xlim = c(-2, 2)) +
  labs(title="ER40 Accuracy site differences")
er40sitepanels


pairs.panels(GNG150_acc[,c(2,13)],lm=TRUE)
GNG150_acc <- left_join(GNG150_acc,GNG150siteid,by="bblid")

gngsites <- ggplot(data=GNG150_acc, aes(x=f_acc_res.1,y=n_acc_res.1)) + 
  geom_point() + geom_smooth(method = lm)
gngsites
gngsitepanels <- gngsites + facet_grid(rows=vars(fsiteid), cols=vars(nfsiteid)) +
  coord_cartesian(ylim = c(-2, 2),xlim = c(-2, 2)) +
  labs(title="GNG150 Accuracy site differences")
gngsitepanels


pairs.panels(KCPW_A_acc[,c(2,15)],lm=TRUE)
KCPW_A_acc <- left_join(KCPW_A_acc,KCPW_Asiteid,by="bblid")

kcpwsites <- ggplot(data=KCPW_A_acc, aes(x=f_acc_res.1,y=n_acc_res.1)) + 
  geom_point() + geom_smooth(method = lm)
kcpwsites
kcpwsitepanels <- kcpwsites + facet_grid(rows=vars(fsiteid), cols=vars(nfsiteid)) +
  coord_cartesian(ylim = c(-2, 2),xlim = c(-2, 2)) +
  labs(title="KCPW Accuracy site differences")
kcpwsitepanels


pairs.panels(KSPVRT_D_acc[,c(2,13)],lm=TRUE)
KSPVRT_D_acc <- left_join(KSPVRT_D_acc,KSPVRT_Dsiteid,by="bblid")

pvrtsites <- ggplot(data=KSPVRT_D_acc, aes(x=f_acc_res.1,y=n_acc_res.1)) + 
  geom_point() + geom_smooth(method = lm)
pvrtsites
pvrtsitepanels <- pvrtsites + facet_grid(rows=vars(fsiteid), cols=vars(nfsiteid)) +
  coord_cartesian(ylim = c(-2, 2),xlim = c(-2.5, 2.5)) +
  labs(title="PVRT Accuracy site differences")
pvrtsitepanels


pairs.panels(MEDF36_A_acc[,c(2,13)],lm=TRUE)
MEDF36_A_acc <- left_join(MEDF36_A_acc,MEDF36_Asiteid,by="bblid")

medfsites <- ggplot(data=MEDF36_A_acc, aes(x=f_acc_res.1,y=n_acc_res.1)) + 
  geom_point() + geom_smooth(method = lm)
medfsites
medfsitepanels <- medfsites + facet_grid(rows=vars(fsiteid), cols=vars(nfsiteid)) +
  coord_cartesian(ylim = c(-2.5, 2),xlim = c(-2.5, 2)) +
  labs(title="MEDF Accuracy site differences")
medfsitepanels


pairs.panels(PCET_A_acc[,c(2,5)],lm=TRUE)
PCET_A_acc <- left_join(PCET_A_acc,PCET_Asiteid,by="bblid")

pcetsites <- ggplot(data=PCET_A_acc, aes(x=f_acc_res.1,y=n_acc_res.1)) + 
  geom_point() + geom_smooth(method = lm)
pcetsites
pcetsitepanels <- pcetsites + facet_grid(rows=vars(fsiteid), cols=vars(nfsiteid)) +
  coord_cartesian(ylim = c(-2, 2),xlim = c(-2, 2)) +
  labs(title="PCET Accuracy site differences")
pcetsitepanels


pairs.panels(PMAT24_A_acc[,c(2,11)],lm=TRUE)
PMAT24_A_acc <- left_join(PMAT24_A_acc,PMAT24_Asiteid,by="bblid")

pmatsites <- ggplot(data=PMAT24_A_acc, aes(x=f_acc_res.1,y=n_acc_res.1)) + 
  geom_point() + geom_smooth(method = lm)
pmatsites
pmatsitepanels <- pmatsites + facet_grid(rows=vars(fsiteid), cols=vars(nfsiteid)) +
  coord_cartesian(ylim = c(-3, 2.5),xlim = c(-2.5, 2.5)) +
  labs(title="PMAT Accuracy site differences")
pmatsitepanels


pairs.panels(SLNB2_90_acc[,c(2,15)],lm=TRUE)
SLNB2_90_acc <- left_join(SLNB2_90_acc,SLNB2_90siteid,by="bblid")

slnbsites <- ggplot(data=SLNB2_90_acc, aes(x=f_acc_res.1,y=n_acc_res.1)) + 
  geom_point() + geom_smooth(method = lm)
slnbsites
slnbsitepanels <- slnbsites + facet_grid(rows=vars(fsiteid), cols=vars(nfsiteid)) +
  coord_cartesian(ylim = c(-2, 2),xlim = c(-2.5, 2)) +
  labs(title="SLNB2 90 Accuracy site differences")
slnbsitepanels


pairs.panels(SPCPTNL_acc[,c(2,17)],lm=TRUE)
SPCPTNL_acc <- left_join(SPCPTNL_acc,SPCPTNLsiteid,by="bblid")

cptsites <- ggplot(data=SPCPTNL_acc, aes(x=f_acc_res.1,y=n_acc_res.1)) + 
  geom_point() + geom_smooth(method = lm)
cptsites
cptsitepanels <- cptsites + facet_grid(rows=vars(fsiteid), cols=vars(nfsiteid)) +
  coord_cartesian(ylim = c(-2, 2),xlim = c(-2, 2)) +
  labs(title="SPCPTNL Accuracy site differences")
cptsitepanels


pairs.panels(SVOLT_A_acc[,c(2,9)],lm=TRUE)
SVOLT_A_acc <- left_join(SVOLT_A_acc,SVOLT_Asiteid,by="bblid")

votsites <- ggplot(data=SVOLT_A_acc, aes(x=f_acc_res.1,y=n_acc_res.1)) + 
  geom_point() + geom_smooth(method = lm)
votsites
voltsitepanels <- votsites + facet_grid(rows=vars(fsiteid), cols=vars(nfsiteid)) +
  coord_cartesian(ylim = c(-2.5, 2),xlim = c(-2.5, 2)) +
  labs(title="VOLT Accuracy site differences")
voltsitepanels


pairs.panels(VSPLOT15_acc[,c(2,9)],lm=TRUE)
VSPLOT15_acc <- left_join(VSPLOT15_acc,VSPLOT15siteid,by="bblid")

plotsites <- ggplot(data=VSPLOT15_acc, aes(x=f_acc_res.1,y=n_acc_res.1)) + 
  geom_point() + geom_smooth(method = lm)
plotsites
plotsitepanels <- plotsites + facet_grid(rows=vars(fsiteid), cols=vars(nfsiteid)) +
  coord_cartesian(ylim = c(-2, 2),xlim = c(-2, 2)) +
  labs(title="PLOT Accuracy site differences")
plotsitepanels





# * flash/non-flash intrasubject speed comparison ----
texts <- setdiff(texts,"CPF_A")    # CPF_A only has one non-flash data point, should have 17 tests left
tests <- mget(texts) # 17 tests
for (j in 1:length(texts)){
  test <- tests[[j]]
  test <- test[!is.na(test$bblid) & !is.na(test$Speed),]
  test$Speed <- ifelse(log(test$Speed)!= -Inf, log(test$Speed),0)
  
  hist <- ggplot(test,aes(x=Speed)) + geom_histogram()    # histogram to look at item acc frequency
  hist
  
  test <- test[!is.na(test$age),]
  if (any(j==11)) {     # regress age out
    fit <- gam(Speed ~ s(age,k=3), data = test)
  } else {
    fit <- gam(Speed ~ s(age), data = test)
  }
  visreg(fit,main=texts[j])    # regress out age first
  test$spe_res <- scale(resid(fit))
  test$acc_res <- 0 # acc_res doesn't matter
  
  test <- test[abs(test$spe_res)<3,]
  
  flash <- unique(test[test$flash==1 & !is.na(test$unique_id),3])
  nflash <- unique(test[test$flash==0 & !is.na(test$unique_id),3])
  
  both <- intersect(flash, nflash)
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
  
  tpflash <- flash[,c(1:4,8,17,19:20)]    #[t]ime [p]oint [flash]
  tpflash$timepoint <- 1
  for (i in 1:(nrow(tpflash)-1)) {
    if (tpflash$bblid[i+1] == tpflash$bblid[i]) {
      tpflash$timepoint[i+1] <- tpflash$timepoint[i] + 1
    }
  }
  tpnflash <- nflash[,c(1:4,8,17,19:20)]    #[t]ime [p]oint [n]on-[flash] used to be columns 1,4,6:7
  tpnflash$timepoint <- 1
  for (i in 1:(nrow(tpnflash)-1)) {
    if (tpnflash$bblid[i+1] == tpnflash$bblid[i]) {
      tpnflash$timepoint[i+1] <- tpnflash$timepoint[i] + 1
    }
  }
  
  fsite <- tpflash[tpflash$timepoint==1,3:4]
  nfsite <- tpnflash[tpnflash$timepoint==1,3:4]
  siteid <- merge(fsite,nfsite,by=1)
  names(siteid)[2:3] <- c("fsiteid","nfsiteid")
  
  wideflash <- reshape(tpflash[,c(3,5:9)],
                       idvar = "bblid",
                       timevar = "timepoint",
                       direction = "wide")
  widenflash <- reshape(tpnflash[,c(3,5:9)],
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
      newscore1 <- c(wideflash[1:length(diff),paste0("spe_res.",i+1)] + diff, rep(NA,nrow(wideflash) - length(diff)))
      meandif <- mean(wideflash[,paste0("spe_res.",i+1)],na.rm=T) - mean(wideflash[,paste0("spe_res.",i)],na.rm=T)
      newscore2 <- wideflash[,paste0("spe_res.",i+1)] + meandif
      
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
      newscore1 <- c(widenflash[1:length(diff),paste0("spe_res.",i+1)] + diff, rep(NA,nrow(widenflash) - length(diff)))
      meandif <- mean(widenflash[,paste0("spe_res.",i+1)],na.rm=T) - mean(widenflash[,paste0("spe_res.",i)],na.rm=T)
      newscore2 <- widenflash[,paste0("spe_res.",i+1)] + meandif
      
      newn1 <- data.frame(cbind(newn1,newscore1))
      newn2 <- data.frame(cbind(newn2,newscore2))
      
      names(newn1)[i] <- paste0("t",i+1,"newscore1")
      names(newn2)[i] <- paste0("t",i+1,"newscore2")
    }
    
    widenflash <- cbind(widenflash,newn1,newn2)
  }
  assign(paste0(texts[j],"Spsiteid"),siteid)
  assign(paste0(texts[j],"Spwideflash"),wideflash)
  assign(paste0(texts[j],"Spwidenflash"),widenflash)
}

# ** getting rid of outliers, test by test ----
wideflash <- ADT36_ASpwideflash
widenflash <- ADT36_ASpwidenflash
speflash <- wideflash[,grepl("bblid", colnames(wideflash)) | grepl("spe_res.1", colnames(wideflash)) | grepl("newscore", colnames(wideflash))]
names(speflash)[-1] <- paste0("f_",names(speflash)[-1])
spenflash <- widenflash[,grepl("bblid", colnames(widenflash)) | grepl("spe_res.1", colnames(widenflash)) | grepl("newscore", colnames(widenflash))]
names(spenflash)[-1] <- paste0("n_",names(spenflash)[-1])
spe <- data.frame(merge(speflash,spenflash, by=1))
spe_cor <- cor(spe[,-1], use="pairwise")
pairs.panels(spe[,c(2,13)],lm=TRUE)    # looking at t1 flash vs nflash
spe <- spe[abs(spe$f_spe_res.1-spe$n_spe_res.1)<2,]
pairs.panels(spe[,c(2,13)],lm=TRUE)
spe$f_spe_res.1 <- winsor(spe$f_spe_res.1,trim=0.01)
spe$n_spe_res.1 <- winsor(spe$n_spe_res.1,trim=0.01)
pairs.panels(spe[,c(2,13)],lm=TRUE)
ADT36_A_spe <- spe  
spetxt <- c("ADT36_A_spe")

wideflash <- AIMSpwideflash
widenflash <- AIMSpwidenflash
speflash <- wideflash[,grepl("bblid", colnames(wideflash)) | grepl("spe_res.1", colnames(wideflash)) | grepl("newscore", colnames(wideflash))]
names(speflash)[-1] <- paste0("f_",names(speflash)[-1])
spenflash <- widenflash[,grepl("bblid", colnames(widenflash)) | grepl("spe_res.1", colnames(widenflash)) | grepl("newscore", colnames(widenflash))]
names(spenflash)[-1] <- paste0("n_",names(spenflash)[-1])
spe <- data.frame(merge(speflash,spenflash, by=1))
spe_cor <- cor(spe[,-1], use="pairwise")
pairs.panels(spe[,c(2,11)],lm=TRUE)    # looking at t1 flash vs nflash
spe <- spe[abs(spe$f_spe_res.1-spe$n_spe_res.1)<2,]
pairs.panels(spe[,c(2,11)],lm=TRUE)
spe$f_spe_res.1 <- winsor(spe$f_spe_res.1,trim=0.01)
spe$n_spe_res.1 <- winsor(spe$n_spe_res.1,trim=0.01)
pairs.panels(spe[,c(2,11)],lm=TRUE)
AIM_spe <- spe  
spetxt <- c(spetxt,"AIM_spe")

wideflash <- CPF_BSpwideflash
widenflash <- CPF_BSpwidenflash
speflash <- wideflash[,grepl("bblid", colnames(wideflash)) | grepl("spe_res.1", colnames(wideflash)) | grepl("newscore", colnames(wideflash))]
names(speflash)[-1] <- paste0("f_",names(speflash)[-1])
spenflash <- widenflash[,grepl("bblid", colnames(widenflash)) | grepl("spe_res.1", colnames(widenflash)) | grepl("newscore", colnames(widenflash))]
names(spenflash)[-1] <- paste0("n_",names(spenflash)[-1])
spe <- data.frame(merge(speflash,spenflash, by=1))
spe_cor <- cor(spe[,-1], use="pairwise")
pairs.panels(spe[,c(2,11)],lm=TRUE)    # looking at t1 flash vs nflash
spe <- spe[abs(spe$f_spe_res.1-spe$n_spe_res.1)<2,]
pairs.panels(spe[,c(2,11)],lm=TRUE)
spe$f_spe_res.1 <- winsor(spe$f_spe_res.1,trim=0.01)
spe$n_spe_res.1 <- winsor(spe$n_spe_res.1,trim=0.01)
pairs.panels(spe[,c(2,11)],lm=TRUE)
CPF_B_spe <- spe  
spetxt <- c(spetxt,"CPF_B_spe")

wideflash <- ER40_DSpwideflash
widenflash <- ER40_DSpwidenflash
speflash <- wideflash[,grepl("bblid", colnames(wideflash)) | grepl("spe_res.1", colnames(wideflash)) | grepl("newscore", colnames(wideflash))]
names(speflash)[-1] <- paste0("f_",names(speflash)[-1])
spenflash <- widenflash[,grepl("bblid", colnames(widenflash)) | grepl("spe_res.1", colnames(widenflash)) | grepl("newscore", colnames(widenflash))]
names(spenflash)[-1] <- paste0("n_",names(spenflash)[-1])
spe <- data.frame(merge(speflash,spenflash, by=1))
spe_cor <- cor(spe[,-1], use="pairwise")
pairs.panels(spe[,c(2,13)],lm=TRUE)    # looking at t1 flash vs nflash
spe <- spe[abs(spe$f_spe_res.1-spe$n_spe_res.1)<2,]
pairs.panels(spe[,c(2,13)],lm=TRUE)
spe$f_spe_res.1 <- winsor(spe$f_spe_res.1,trim=0.01)
spe$n_spe_res.1 <- winsor(spe$n_spe_res.1,trim=0.01)
pairs.panels(spe[,c(2,13)],lm=TRUE)
ER40_D_spe <- spe  
spetxt <- c(spetxt,"ER40_D_spe")

wideflash <-  GNG150Spwideflash
widenflash <- GNG150Spwidenflash
speflash <- wideflash[,grepl("bblid", colnames(wideflash)) | grepl("spe_res.1", colnames(wideflash)) | grepl("newscore", colnames(wideflash))]
names(speflash)[-1] <- paste0("f_",names(speflash)[-1])
spenflash <- widenflash[,grepl("bblid", colnames(widenflash)) | grepl("spe_res.1", colnames(widenflash)) | grepl("newscore", colnames(widenflash))]
names(spenflash)[-1] <- paste0("n_",names(spenflash)[-1])
spe <- data.frame(merge(speflash,spenflash, by=1))
spe_cor <- cor(spe[,-1], use="pairwise")
pairs.panels(spe[,c(2,13)],lm=TRUE)    # looking at t1 flash vs nflash
spe <- spe[abs(spe$f_spe_res.1-spe$n_spe_res.1)<2,]
pairs.panels(spe[,c(2,13)],lm=TRUE)
spe$f_spe_res.1 <- winsor(spe$f_spe_res.1,trim=0.01)
spe$n_spe_res.1 <- winsor(spe$n_spe_res.1,trim=0.01)
pairs.panels(spe[,c(2,13)],lm=TRUE)
GNG150_spe <- spe  
spetxt <- c(spetxt,"GNG150_spe")

wideflash <-  KCPW_ASpwideflash
widenflash <- KCPW_ASpwidenflash
speflash <- wideflash[,grepl("bblid", colnames(wideflash)) | grepl("spe_res.1", colnames(wideflash)) | grepl("newscore", colnames(wideflash))]
names(speflash)[-1] <- paste0("f_",names(speflash)[-1])
spenflash <- widenflash[,grepl("bblid", colnames(widenflash)) | grepl("spe_res.1", colnames(widenflash)) | grepl("newscore", colnames(widenflash))]
names(spenflash)[-1] <- paste0("n_",names(spenflash)[-1])
spe <- data.frame(merge(speflash,spenflash, by=1))
spe_cor <- cor(spe[,-1], use="pairwise")
pairs.panels(spe[,c(2,15)],lm=TRUE)    # looking at t1 flash vs nflash
spe <- spe[abs(spe$f_spe_res.1-spe$n_spe_res.1)<2,]
pairs.panels(spe[,c(2,15)],lm=TRUE)
spe$f_spe_res.1 <- winsor(spe$f_spe_res.1,trim=0.01)
spe$n_spe_res.1 <- winsor(spe$n_spe_res.1,trim=0.01)
pairs.panels(spe[,c(2,15)],lm=TRUE)
KCPW_A_spe <- spe  
spetxt <- c(spetxt,"KCPW_A_spe")

wideflash <-  KSPVRT_DSpwideflash
widenflash <- KSPVRT_DSpwidenflash
speflash <- wideflash[,grepl("bblid", colnames(wideflash)) | grepl("spe_res.1", colnames(wideflash)) | grepl("newscore", colnames(wideflash))]
names(speflash)[-1] <- paste0("f_",names(speflash)[-1])
spenflash <- widenflash[,grepl("bblid", colnames(widenflash)) | grepl("spe_res.1", colnames(widenflash)) | grepl("newscore", colnames(widenflash))]
names(spenflash)[-1] <- paste0("n_",names(spenflash)[-1])
spe <- data.frame(merge(speflash,spenflash, by=1))
spe_cor <- cor(spe[,-1], use="pairwise")
pairs.panels(spe[,c(2,13)],lm=TRUE)    # looking at t1 flash vs nflash
spe <- spe[abs(spe$f_spe_res.1-spe$n_spe_res.1)<2,]
pairs.panels(spe[,c(2,13)],lm=TRUE)
spe$f_spe_res.1 <- winsor(spe$f_spe_res.1,trim=0.01)
spe$n_spe_res.1 <- winsor(spe$n_spe_res.1,trim=0.01)
pairs.panels(spe[,c(2,13)],lm=TRUE)
KSPVRT_D_spe <- spe  
spetxt <- c(spetxt,"KSPVRT_D_spe")

wideflash <-  MEDF36_ASpwideflash
widenflash <- MEDF36_ASpwidenflash
speflash <- wideflash[,grepl("bblid", colnames(wideflash)) | grepl("spe_res.1", colnames(wideflash)) | grepl("newscore", colnames(wideflash))]
names(speflash)[-1] <- paste0("f_",names(speflash)[-1])
spenflash <- widenflash[,grepl("bblid", colnames(widenflash)) | grepl("spe_res.1", colnames(widenflash)) | grepl("newscore", colnames(widenflash))]
names(spenflash)[-1] <- paste0("n_",names(spenflash)[-1])
spe <- data.frame(merge(speflash,spenflash, by=1))
spe_cor <- cor(spe[,-1], use="pairwise")
pairs.panels(spe[,c(2,13)],lm=TRUE)    # looking at t1 flash vs nflash
spe <- spe[abs(spe$f_spe_res.1-spe$n_spe_res.1)<2,]
pairs.panels(spe[,c(2,13)],lm=TRUE)
spe$f_spe_res.1 <- winsor(spe$f_spe_res.1,trim=0.01)
spe$n_spe_res.1 <- winsor(spe$n_spe_res.1,trim=0.01)
pairs.panels(spe[,c(2,13)],lm=TRUE)
MEDF36_A_spe <- spe  
spetxt <- c(spetxt,"MEDF36_A_spe")

wideflash <-  MPRACTSpwideflash
widenflash <- MPRACTSpwidenflash
speflash <- wideflash[,grepl("bblid", colnames(wideflash)) | grepl("spe_res.1", colnames(wideflash)) | grepl("newscore", colnames(wideflash))]
names(speflash)[-1] <- paste0("f_",names(speflash)[-1])
spenflash <- widenflash[,grepl("bblid", colnames(widenflash)) | grepl("spe_res.1", colnames(widenflash)) | grepl("newscore", colnames(widenflash))]
names(spenflash)[-1] <- paste0("n_",names(spenflash)[-1])
spe <- data.frame(merge(speflash,spenflash, by=1))
spe_cor <- cor(spe[,-1], use="pairwise")
pairs.panels(spe[,c(2,15)],lm=TRUE)    # looking at t1 flash vs nflash
spe <- spe[abs(spe$f_spe_res.1-spe$n_spe_res.1)<2,]
pairs.panels(spe[,c(2,15)],lm=TRUE)
spe$f_spe_res.1 <- winsor(spe$f_spe_res.1,trim=0.01)
spe$n_spe_res.1 <- winsor(spe$n_spe_res.1,trim=0.01)
pairs.panels(spe[,c(2,15)],lm=TRUE)
MPRACT_spe <- spe  
spetxt <- c(spetxt,"MPRACT_spe")

wideflash <-  PCET_ASpwideflash
widenflash <- PCET_ASpwidenflash
speflash <- wideflash[,grepl("bblid", colnames(wideflash)) | grepl("spe_res.1", colnames(wideflash)) | grepl("newscore", colnames(wideflash))]
names(speflash)[-1] <- paste0("f_",names(speflash)[-1])
spenflash <- widenflash[,grepl("bblid", colnames(widenflash)) | grepl("spe_res.1", colnames(widenflash)) | grepl("newscore", colnames(widenflash))]
names(spenflash)[-1] <- paste0("n_",names(spenflash)[-1])
spe <- data.frame(merge(speflash,spenflash, by=1))
spe_cor <- cor(spe[,-1], use="pairwise")
pairs.panels(spe[,c(2,5)],lm=TRUE)    # looking at t1 flash vs nflash
spe <- spe[abs(spe$f_spe_res.1-spe$n_spe_res.1)<2,]
pairs.panels(spe[,c(2,5)],lm=TRUE)
spe$f_spe_res.1 <- winsor(spe$f_spe_res.1,trim=0.01)
spe$n_spe_res.1 <- winsor(spe$n_spe_res.1,trim=0.01)
pairs.panels(spe[,c(2,5)],lm=TRUE)
PCET_A_spe <- spe
spetxt <- c(spetxt,"PCET_A_spe")

wideflash <-  PMAT24_ASpwideflash
widenflash <- PMAT24_ASpwidenflash
speflash <- wideflash[,grepl("bblid", colnames(wideflash)) | grepl("spe_res.1", colnames(wideflash)) | grepl("newscore", colnames(wideflash))]
names(speflash)[-1] <- paste0("f_",names(speflash)[-1])
spenflash <- widenflash[,grepl("bblid", colnames(widenflash)) | grepl("spe_res.1", colnames(widenflash)) | grepl("newscore", colnames(widenflash))]
names(spenflash)[-1] <- paste0("n_",names(spenflash)[-1])
spe <- data.frame(merge(speflash,spenflash, by=1))
spe_cor <- cor(spe[,-1], use="pairwise")
pairs.panels(spe[,c(2,11)],lm=TRUE)    # looking at t1 flash vs nflash
spe <- spe[abs(spe$f_spe_res.1-spe$n_spe_res.1)<2,]
pairs.panels(spe[,c(2,11)],lm=TRUE)
spe$f_spe_res.1 <- winsor(spe$f_spe_res.1,trim=0.01)
spe$n_spe_res.1 <- winsor(spe$n_spe_res.1,trim=0.01)
pairs.panels(spe[,c(2,11)],lm=TRUE)
PMAT24_A_spe <- spe  
spetxt <- c(spetxt,"PMAT24_A_spe")

wideflash <-  SCTAPSpwideflash
widenflash <- SCTAPSpwidenflash
speflash <- wideflash[,grepl("bblid", colnames(wideflash)) | grepl("spe_res.1", colnames(wideflash)) | grepl("newscore", colnames(wideflash))]
names(speflash)[-1] <- paste0("f_",names(speflash)[-1])
spenflash <- widenflash[,grepl("bblid", colnames(widenflash)) | grepl("spe_res.1", colnames(widenflash)) | grepl("newscore", colnames(widenflash))]
names(spenflash)[-1] <- paste0("n_",names(spenflash)[-1])
spe <- data.frame(merge(speflash,spenflash, by=1))
spe_cor <- cor(spe[,-1], use="pairwise")
pairs.panels(spe[,c(2,15)],lm=TRUE)    # looking at t1 flash vs nflash
spe <- spe[abs(spe$f_spe_res.1-spe$n_spe_res.1)<2,]
pairs.panels(spe[,c(2,15)],lm=TRUE)
spe$f_spe_res.1 <- winsor(spe$f_spe_res.1,trim=0.01)
spe$n_spe_res.1 <- winsor(spe$n_spe_res.1,trim=0.01)
pairs.panels(spe[,c(2,15)],lm=TRUE)
SCTAP_spe <- spe  
spetxt <- c(spetxt,"SCTAP_spe")

wideflash <-  SLNB2_90Spwideflash
widenflash <- SLNB2_90Spwidenflash
speflash <- wideflash[,grepl("bblid", colnames(wideflash)) | grepl("spe_res.1", colnames(wideflash)) | grepl("newscore", colnames(wideflash))]
names(speflash)[-1] <- paste0("f_",names(speflash)[-1])
spenflash <- widenflash[,grepl("bblid", colnames(widenflash)) | grepl("spe_res.1", colnames(widenflash)) | grepl("newscore", colnames(widenflash))]
names(spenflash)[-1] <- paste0("n_",names(spenflash)[-1])
spe <- data.frame(merge(speflash,spenflash, by=1))
spe_cor <- cor(spe[,-1], use="pairwise")
pairs.panels(spe[,c(2,15)],lm=TRUE)    # looking at t1 flash vs nflash
spe <- spe[abs(spe$f_spe_res.1-spe$n_spe_res.1)<2,]
pairs.panels(spe[,c(2,15)],lm=TRUE)
spe$f_spe_res.1 <- winsor(spe$f_spe_res.1,trim=0.01)
spe$n_spe_res.1 <- winsor(spe$n_spe_res.1,trim=0.01)
pairs.panels(spe[,c(2,15)],lm=TRUE)
SLNB2_90_spe <- spe  
spetxt <- c(spetxt,"SLNB2_90_spe")

# wideflash <-  SPCPTN90Spwideflash                   # SPCPTN90 only has two participants that have taken both flash and non-flash
# widenflash <- SPCPTN90Spwidenflash
# speflash <- wideflash[,grepl("bblid", colnames(wideflash)) | grepl("spe_res.1", colnames(wideflash)) | grepl("newscore", colnames(wideflash))]
# names(speflash)[-1] <- paste0("f_",names(speflash)[-1])
# spenflash <- widenflash[,grepl("bblid", colnames(widenflash)) | grepl("spe_res.1", colnames(widenflash)) | grepl("newscore", colnames(widenflash))]
# names(spenflash)[-1] <- paste0("n_",names(spenflash)[-1])
# spe <- merge(speflash,spenflash, by=1)
# spe_cor <- cor(spe[,-1], use="pairwise")
# pairs.panels(spe[,c(2,11)],lm=TRUE)    # looking at t1 flash vs nflash
# spe$rm <- ifelse(abs(spe$f_spe_res.1-spe$n_spe_res.1)>=2,1,0)
# spe <- spe[spe$rm == 0,-which(names(spe) %in% "rm")]
# pairs.panels(spe[,c(2,11)],lm=TRUE)
# SLNB2_90_spe <- spe
# spetxt <- c(spetxt,"SLNB2_90_spe")

wideflash <-  SPCPTNLSpwideflash
widenflash <- SPCPTNLSpwidenflash
speflash <- wideflash[,grepl("bblid", colnames(wideflash)) | grepl("spe_res.1", colnames(wideflash)) | grepl("newscore", colnames(wideflash))]
names(speflash)[-1] <- paste0("f_",names(speflash)[-1])
spenflash <- widenflash[,grepl("bblid", colnames(widenflash)) | grepl("spe_res.1", colnames(widenflash)) | grepl("newscore", colnames(widenflash))]
names(spenflash)[-1] <- paste0("n_",names(spenflash)[-1])
spe <- data.frame(merge(speflash,spenflash, by=1))
spe_cor <- cor(spe[,-1], use="pairwise")
pairs.panels(spe[,c(2,17)],lm=TRUE)    # looking at t1 flash vs nflash
spe <- spe[abs(spe$f_spe_res.1-spe$n_spe_res.1)<2,]
pairs.panels(spe[,c(2,17)],lm=TRUE)
spe$f_spe_res.1 <- winsor(spe$f_spe_res.1,trim=0.01)
spe$n_spe_res.1 <- winsor(spe$n_spe_res.1,trim=0.01)
pairs.panels(spe[,c(2,17)],lm=TRUE)
SPCPTNL_spe <- spe  
spetxt <- c(spetxt,"SPCPTNL_spe")

wideflash <-  SVOLT_ASpwideflash
widenflash <- SVOLT_ASpwidenflash
speflash <- wideflash[,grepl("bblid", colnames(wideflash)) | grepl("spe_res.1", colnames(wideflash)) | grepl("newscore", colnames(wideflash))]
names(speflash)[-1] <- paste0("f_",names(speflash)[-1])
spenflash <- widenflash[,grepl("bblid", colnames(widenflash)) | grepl("spe_res.1", colnames(widenflash)) | grepl("newscore", colnames(widenflash))]
names(spenflash)[-1] <- paste0("n_",names(spenflash)[-1])
spe <- data.frame(merge(speflash,spenflash, by=1))
spe_cor <- cor(spe[,-1], use="pairwise")
pairs.panels(spe[,c(2,9)],lm=TRUE)    # looking at t1 flash vs nflash
spe <- spe[abs(spe$f_spe_res.1-spe$n_spe_res.1)<2,]
pairs.panels(spe[,c(2,9)],lm=TRUE)
spe$f_spe_res.1 <- winsor(spe$f_spe_res.1,trim=0.01)
spe$n_spe_res.1 <- winsor(spe$n_spe_res.1,trim=0.01)
pairs.panels(spe[,c(2,9)],lm=TRUE)
SVOLT_A_spe <- spe  
spetxt <- c(spetxt,"SVOLT_A_spe")

wideflash <- VSPLOT15Spwideflash
widenflash <- VSPLOT15Spwidenflash
speflash <- wideflash[,grepl("bblid", colnames(wideflash)) | grepl("spe_res.1", colnames(wideflash)) | grepl("newscore", colnames(wideflash))]
names(speflash)[-1] <- paste0("f_",names(speflash)[-1])
spenflash <- widenflash[,grepl("bblid", colnames(widenflash)) | grepl("spe_res.1", colnames(widenflash)) | grepl("newscore", colnames(widenflash))]
names(spenflash)[-1] <- paste0("n_",names(spenflash)[-1])
spe <- data.frame(merge(speflash,spenflash, by=1))
spe_cor <- cor(spe[,-1], use="pairwise")
pairs.panels(spe[,c(2,9)],lm=TRUE)    # looking at t1 flash vs nflash
spe <- spe[abs(spe$f_spe_res.1-spe$n_spe_res.1)<2,]
pairs.panels(spe[,c(2,9)],lm=TRUE)
spe$f_spe_res.1 <- winsor(spe$f_spe_res.1,trim=0.01)
spe$n_spe_res.1 <- winsor(spe$n_spe_res.1,trim=0.01)
pairs.panels(spe[,c(2,9)],lm=TRUE)
VSPLOT15_spe <- spe  
spetxt <- c(spetxt,"VSPLOT15_spe")

spes <- mget(spetxt)

# ** spe_cor and icc for each test ----
texts <- setdiff(texts, c("SPCPTN90"))
tests <- mget(texts)   # 16 tests
for (i in 1:length(spetxt)) {
  spe <- spes[[i]]
  spe_cor <- cor(spe[,-1], use="pairwise")
  
  assign(paste0(texts[i],"spe_cor"),spe_cor)
  # wrap icc stuff in try()
  icc_fnf <- icc(spe[,grepl("spe_res",colnames(spe))],type="agreement",model="twoway")$value
  try(icc_f12_1 <- icc(spe[,grepl("f_spe_res",colnames(spe)) | grepl("f_t2newscore1",colnames(spe))],type="agreement",model="twoway")$value)
  try(icc_f13_1 <- icc(spe[,grepl("f_spe_res",colnames(spe)) | grepl("f_t2newscore1",colnames(spe)) | grepl("f_t3newscore1",colnames(spe))],type="agreement",model="twoway")$value)
  try(icc_f14_1 <- icc(spe[,grepl("f_spe_res",colnames(spe)) | grepl("f_t2newscore1",colnames(spe)) | grepl("f_t3newscore1",colnames(spe)) | grepl("f_t4newscore1",colnames(spe))],type="agreement",model="twoway")$value)
  try(icc_f12_2 <- icc(spe[,grepl("f_spe_res",colnames(spe)) | grepl("f_t2newscore2",colnames(spe))],type="agreement",model="twoway")$value)
  try(icc_f13_2 <- icc(spe[,grepl("f_spe_res",colnames(spe)) | grepl("f_t2newscore2",colnames(spe)) | grepl("f_t3newscore2",colnames(spe))],type="agreement",model="twoway")$value)
  try(icc_f14_2 <- icc(spe[,grepl("f_spe_res",colnames(spe)) | grepl("f_t2newscore2",colnames(spe)) | grepl("f_t3newscore2",colnames(spe)) | grepl("f_t4newscore2",colnames(spe))],type="agreement",model="twoway")$value)
  try(icc_n12_1 <- icc(spe[,grepl("n_spe_res",colnames(spe)) | grepl("n_t2newscore1",colnames(spe))],type="agreement",model="twoway")$value)
  try(icc_n13_1 <- icc(spe[,grepl("n_spe_res",colnames(spe)) | grepl("n_t2newscore1",colnames(spe)) | grepl("n_t3newscore1",colnames(spe))],type="agreement",model="twoway")$value)
  try(icc_n14_1 <- icc(spe[,grepl("n_spe_res",colnames(spe)) | grepl("n_t2newscore1",colnames(spe)) | grepl("n_t3newscore1",colnames(spe)) | grepl("n_t4newscore1",colnames(spe))],type="agreement",model="twoway")$value)
  try(icc_n12_2 <- icc(spe[,grepl("n_spe_res",colnames(spe)) | grepl("n_t2newscore2",colnames(spe))],type="agreement",model="twoway")$value)
  try(icc_n13_2 <- icc(spe[,grepl("n_spe_res",colnames(spe)) | grepl("n_t2newscore2",colnames(spe)) | grepl("n_t3newscore2",colnames(spe))],type="agreement",model="twoway")$value)
  try(icc_n14_2 <- icc(spe[,grepl("n_spe_res",colnames(spe)) | grepl("n_t2newscore2",colnames(spe)) | grepl("n_t3newscore2",colnames(spe)) | grepl("n_t4newscore2",colnames(spe))],type="agreement",model="twoway")$value)
  
  icc <- data.frame(matrix(c(icc_fnf,rep(NA,13)),nrow=2))
  names(icc) <- c("icc_fnf","icc_f12_1","icc_f13_1","icc_f14_1","icc_f12_2","icc_f13_2","icc_f14_2")
  try(if (icc_f12_1) {icc[1,2] <- icc_f12_1})
  try(if (icc_f13_1) {icc[1,3] <- icc_f13_1})
  try(if (icc_f14_1) {icc[1,4] <- icc_f14_1})
  try(if (icc_f12_2) {icc[1,5] <- icc_f12_2})
  try(if (icc_f13_2) {icc[1,6] <- icc_f13_2})
  try(if (icc_f14_2) {icc[1,7] <- icc_f14_2})
  try(if (icc_n12_1) {icc[2,2] <- icc_n12_1})
  try(if (icc_n13_1) {icc[2,3] <- icc_n13_1})
  try(if (icc_n14_1) {icc[2,4] <- icc_n14_1})
  try(if (icc_n12_2) {icc[2,5] <- icc_n12_2})
  try(if (icc_n13_2) {icc[2,6] <- icc_n13_2})
  try(if (icc_n14_2) {icc[2,7] <- icc_n14_2})
  assign(paste0(texts[i],"spe_icc"),icc)
}

write.csv( ADT36_Aspe_cor,"myresults/instrasubject_corr/spe/ADT36_Aspe_cor.csv")
write.csv(     AIMspe_cor,"myresults/instrasubject_corr/spe/AIMspe_cor.csv")
write.csv(   CPF_Bspe_cor,"myresults/instrasubject_corr/spe/CPF_Bspe_cor.csv")
write.csv(  ER40_Dspe_cor,"myresults/instrasubject_corr/spe/ER40_Dspe_cor.csv")
write.csv(  GNG150spe_cor,"myresults/instrasubject_corr/spe/GNG150spe_cor.csv")
write.csv(  KCPW_Aspe_cor,"myresults/instrasubject_corr/spe/KCPW_Aspe_cor.csv")
write.csv(KSPVRT_Dspe_cor,"myresults/instrasubject_corr/spe/KSPVRT_Dspe_cor.csv")
write.csv(MEDF36_Aspe_cor,"myresults/instrasubject_corr/spe/MEDF36_Aspe_cor.csv")
write.csv(  MPRACTspe_cor,"myresults/instrasubject_corr/spe/MPRACTspe_cor.csv")
write.csv(  PCET_Aspe_cor,"myresults/instrasubject_corr/spe/PCET_Aspe_cor.csv")
write.csv(PMAT24_Aspe_cor,"myresults/instrasubject_corr/spe/PMAT24_Aspe_cor.csv")
write.csv(   SCTAPspe_cor,"myresults/instrasubject_corr/spe/SCTAPspe_cor.csv")
write.csv(SLNB2_90spe_cor,"myresults/instrasubject_corr/spe/SLNB2_90spe_cor.csv")
write.csv( SPCPTNLspe_cor,"myresults/instrasubject_corr/spe/SPCPTNLspe_cor.csv")
write.csv( SVOLT_Aspe_cor,"myresults/instrasubject_corr/spe/SVOLT_Aspe_cor.csv")
write.csv(VSPLOT15spe_cor,"myresults/instrasubject_corr/spe/VSPLOT15spe_cor.csv")

write.csv( ADT36_Aspe_icc,"myresults/instrasubject_corr/spe/ADT36_Aspe_icc.csv")
write.csv(     AIMspe_icc,"myresults/instrasubject_corr/spe/AIMspe_icc.csv")
write.csv(   CPF_Bspe_icc,"myresults/instrasubject_corr/spe/CPF_Bspe_icc.csv")
write.csv(  ER40_Dspe_icc,"myresults/instrasubject_corr/spe/ER40_Dspe_icc.csv")
write.csv(  GNG150spe_icc,"myresults/instrasubject_corr/spe/GNG150spe_icc.csv")
write.csv(  KCPW_Aspe_icc,"myresults/instrasubject_corr/spe/KCPW_Aspe_icc.csv")
write.csv(KSPVRT_Dspe_icc,"myresults/instrasubject_corr/spe/KSPVRT_Dspe_icc.csv")
write.csv(MEDF36_Aspe_icc,"myresults/instrasubject_corr/spe/MEDF36_Aspe_icc.csv")
write.csv(  MPRACTspe_icc,"myresults/instrasubject_corr/spe/MPRACTspe_icc.csv")
write.csv(  PCET_Aspe_icc,"myresults/instrasubject_corr/spe/PCET_Aspe_icc.csv")
write.csv(PMAT24_Aspe_icc,"myresults/instrasubject_corr/spe/PMAT24_Aspe_icc.csv")
write.csv(   SCTAPspe_icc,"myresults/instrasubject_corr/spe/SCTAPspe_icc.csv")
write.csv(SLNB2_90spe_icc,"myresults/instrasubject_corr/spe/SLNB2_90spe_icc.csv")
write.csv( SPCPTNLspe_icc,"myresults/instrasubject_corr/spe/SPCPTNLspe_icc.csv")
write.csv( SVOLT_Aspe_icc,"myresults/instrasubject_corr/spe/SVOLT_Aspe_icc.csv")
write.csv(VSPLOT15spe_icc,"myresults/instrasubject_corr/spe/VSPLOT15spe_icc.csv")








# site differences ----

# Tests of interests are: CPF A (40), ER40 D (40), SPCPTNL (60), GNG150 (150), SCTAP, PMAT24 A, 
#                         SVOLT A, MPRACT, MEDF36 A, SLNB2 90

pairs.panels(ADT36_A_spe[,c(2,13)],lm=TRUE)
ADT36_A_spe <- left_join(ADT36_A_spe,ADT36_ASpsiteid,by="bblid")

adtspsites <- ggplot(data=ADT36_A_spe, aes(x=f_spe_res.1,y=n_spe_res.1)) + 
  geom_point() + geom_smooth(method = lm)
adtspsites
adtspsitepanels <- adtspsites + facet_grid(rows=vars(fsiteid), cols=vars(nfsiteid)) +
  coord_cartesian(ylim = c(-2.2, 2),xlim = c(-2, 2)) +
  labs(title="ADT Speed site differences")
adtspsitepanels

# using face wrap instead to adjust coordinates + only show plots with real data
sites + facet_wrap(vars(fsiteid, nfsiteid)) +
  coord_cartesian(ylim = c(-2, 2),xlim = c(-2, 2))


pairs.panels(AIM_spe[,c(2,13)],lm=TRUE)
AIM_spe <- left_join(AIM_spe,AIMSpsiteid,by="bblid")

aimspsites <- ggplot(data=AIM_spe, aes(x=f_spe_res.1,y=n_spe_res.1)) + 
  geom_point() + geom_smooth(method = lm)
aimspsites
aimspsitepanels <- aimspsites + facet_grid(rows=vars(fsiteid), cols=vars(nfsiteid)) +
  coord_cartesian(ylim = c(-2, 2),xlim = c(-2, 2)) +
  labs(title="AIM Speed site differences")
aimspsitepanels


pairs.panels(CPF_B_spe[,c(2,11)],lm=TRUE)
CPF_B_spe <- left_join(CPF_B_spe,CPF_BSpsiteid,by="bblid")

cpfspsites <- ggplot(data=CPF_B_spe, aes(x=f_spe_res.1,y=n_spe_res.1)) + 
  geom_point() + geom_smooth(method = lm)
cpfspsites
cpfspsitepanels <- cpfspsites + facet_grid(rows=vars(fsiteid), cols=vars(nfsiteid)) +
  coord_cartesian(ylim = c(-2, 2),xlim = c(-2, 2)) +
  labs(title="CPF Speed site differences")
cpfspsitepanels


pairs.panels(ER40_D_spe[,c(2,13)],lm=TRUE)
ER40_D_spe <- left_join(ER40_D_spe,ER40_DSpsiteid,by="bblid")

ersspsites <- ggplot(data=ER40_D_spe, aes(x=f_spe_res.1,y=n_spe_res.1)) + 
  geom_point() + geom_smooth(method = lm)
ersspsites
erspsitepanels <- ersspsites + facet_grid(rows=vars(fsiteid), cols=vars(nfsiteid)) +
  coord_cartesian(ylim = c(-2.2, 2),xlim = c(-2, 2)) +
  labs(title="ER40 Speed site differences")
erspsitepanels


pairs.panels(GNG150_spe[,c(2,13)],lm=TRUE)
GNG150_spe <- left_join(GNG150_spe,GNG150Spsiteid,by="bblid")

gngspsites <- ggplot(data=GNG150_spe, aes(x=f_spe_res.1,y=n_spe_res.1)) + 
  geom_point() + geom_smooth(method = lm)
gngspsites
gngspsitepanels <- gngspsites + facet_grid(rows=vars(fsiteid), cols=vars(nfsiteid)) +
  coord_cartesian(ylim = c(-3, 2),xlim = c(-2, 2)) +
  labs(title="GNG150 Speed site differences")
gngspsitepanels


pairs.panels(KCPW_A_spe[,c(2,15)],lm=TRUE)
KCPW_A_spe <- left_join(KCPW_A_spe,KCPW_ASpsiteid,by="bblid")

cpwspsites <- ggplot(data=KCPW_A_spe, aes(x=f_spe_res.1,y=n_spe_res.1)) + 
  geom_point() + geom_smooth(method = lm)
cpwspsites
cpwspsitepanels <- cpwspsites + facet_grid(rows=vars(fsiteid), cols=vars(nfsiteid)) +
  coord_cartesian(ylim = c(-2.5, 2.2),xlim = c(-2, 3)) +
  labs(title="KCPW Speed site differences")
cpwspsitepanels


pairs.panels(KSPVRT_D_spe[,c(2,13)],lm=TRUE)
KSPVRT_D_spe <- left_join(KSPVRT_D_spe,KSPVRT_DSpsiteid,by="bblid")

pvrtspsites <- ggplot(data=KSPVRT_D_spe, aes(x=f_spe_res.1,y=n_spe_res.1)) + 
  geom_point() + geom_smooth(method = lm)
pvrtspsites
pvrtspsitepanels <- pvrtspsites + facet_grid(rows=vars(fsiteid), cols=vars(nfsiteid)) +
  coord_cartesian(ylim = c(-2, 2.2),xlim = c(-2, 2.5)) +
  labs(title="KSPVRT Speed site differences")
pvrtspsitepanels


pairs.panels(MEDF36_A_spe[,c(2,13)],lm=TRUE)
MEDF36_A_spe <- left_join(MEDF36_A_spe,MEDF36_ASpsiteid,by="bblid")

medfspsites <- ggplot(data=MEDF36_A_spe, aes(x=f_spe_res.1,y=n_spe_res.1)) + 
  geom_point() + geom_smooth(method = lm)
medfspsites
medfspsitepanels <- medfspsites + facet_grid(rows=vars(fsiteid), cols=vars(nfsiteid)) +
  coord_cartesian(ylim = c(-2.1, 2),xlim = c(-2, 2)) +
  labs(title="MEDF36 Speed site differences")
medfspsitepanels


pairs.panels(MPRACT_spe[,c(2,15)],lm=TRUE)
MPRACT_spe <- left_join(MPRACT_spe,MPRACTSpsiteid,by="bblid")

mpractsites <- ggplot(data=MPRACT_spe, aes(x=f_spe_res.1,y=n_spe_res.1)) + 
  geom_point() + geom_smooth(method = lm)
mpractsites
mpractsitepanels <- mpractsites + facet_grid(rows=vars(fsiteid), cols=vars(nfsiteid)) +
  coord_cartesian(ylim = c(-2, 2),xlim = c(-2, 2)) +
  labs(title="MPRACT Speed site differences")
mpractsitepanels


pairs.panels(PCET_A_spe[,c(2,5)],lm=TRUE)
PCET_A_spe <- left_join(PCET_A_spe,PCET_ASpsiteid,by="bblid")

pcetspsites <- ggplot(data=PCET_A_spe, aes(x=f_spe_res.1,y=n_spe_res.1)) + 
  geom_point() + geom_smooth(method = lm)
pcetspsites
pcetspsitepanels <- pcetspsites + facet_grid(rows=vars(fsiteid), cols=vars(nfsiteid)) +
  coord_cartesian(ylim = c(-2, 2),xlim = c(-2, 2)) +
  labs(title="PCET Speed site differences")
pcetspsitepanels


pairs.panels(PMAT24_A_spe[,c(2,11)],lm=TRUE)
PMAT24_A_spe <- left_join(PMAT24_A_spe,PMAT24_ASpsiteid,by="bblid")

pmatspsites <- ggplot(data=PMAT24_A_spe, aes(x=f_spe_res.1,y=n_spe_res.1)) + 
  geom_point() + geom_smooth(method = lm)
pmatspsites
pmatspsitepanels <- pmatspsites + facet_grid(rows=vars(fsiteid), cols=vars(nfsiteid)) +
  coord_cartesian(ylim = c(-2, 2.7),xlim = c(-2, 2)) +
  labs(title="PMAT24 Speed site differences")
pmatspsitepanels


pairs.panels(SCTAP_spe[,c(2,15)],lm=TRUE)
SCTAP_spe <- left_join(SCTAP_spe,SCTAPSpsiteid,by="bblid")

sctapsites <- ggplot(data=SCTAP_spe, aes(x=f_spe_res.1,y=n_spe_res.1)) + 
  geom_point() + geom_smooth(method = lm)
sctapsites
sctapsitepanels <- sctapsites + facet_grid(rows=vars(fsiteid), cols=vars(nfsiteid)) +
  coord_cartesian(ylim = c(-2, 2),xlim = c(-2, 2.5)) +
  labs(title="SCTAP Speed site differences")
sctapsitepanels


pairs.panels(SLNB2_90_spe[,c(2,15)],lm=TRUE)
SLNB2_90_spe <- left_join(SLNB2_90_spe,SLNB2_90Spsiteid,by="bblid")

slnbspsites <- ggplot(data=SLNB2_90_spe, aes(x=f_spe_res.1,y=n_spe_res.1)) + 
  geom_point() + geom_smooth(method = lm)
slnbspsites
slnbspsitepanels <- slnbspsites + facet_grid(rows=vars(fsiteid), cols=vars(nfsiteid)) +
  coord_cartesian(ylim = c(-2, 2),xlim = c(-2, 2.2)) +
  labs(title="SLNB2 90 Speed site differences")
slnbspsitepanels


pairs.panels(SPCPTNL_spe[,c(2,17)],lm=TRUE)
SPCPTNL_spe <- left_join(SPCPTNL_spe,SPCPTNLSpsiteid,by="bblid")

cptspsites <- ggplot(data=SPCPTNL_spe, aes(x=f_spe_res.1,y=n_spe_res.1)) + 
  geom_point() + geom_smooth(method = lm)
cptspsites
cptspsitepanels <- cptspsites + facet_grid(rows=vars(fsiteid), cols=vars(nfsiteid)) +
  coord_cartesian(ylim = c(-2, 2),xlim = c(-2.2, 2.7)) +
  labs(title="SPCPTNL Speed site differences")
cptspsitepanels


pairs.panels(SVOLT_A_spe[,c(2,9)],lm=TRUE)
SVOLT_A_spe <- left_join(SVOLT_A_spe,SVOLT_ASpsiteid,by="bblid")

voltspsites <- ggplot(data=SVOLT_A_spe, aes(x=f_spe_res.1,y=n_spe_res.1)) + 
  geom_point() + geom_smooth(method = lm)
voltspsites
voltspsitepanels <- voltspsites + facet_grid(rows=vars(fsiteid), cols=vars(nfsiteid)) +
  coord_cartesian(ylim = c(-2, 2),xlim = c(-2, 2)) +
  labs(title="SVOLT Speed site differences")
voltspsitepanels


pairs.panels(VSPLOT15_spe[,c(2,9)],lm=TRUE)
VSPLOT15_spe <- left_join(VSPLOT15_spe,VSPLOT15Spsiteid,by="bblid")

plotspsites <- ggplot(data=VSPLOT15_spe, aes(x=f_spe_res.1,y=n_spe_res.1)) + 
  geom_point() + geom_smooth(method = lm)
plotspsites
plotspsitepanels <- plotspsites + facet_grid(rows=vars(fsiteid), cols=vars(nfsiteid)) +
  coord_cartesian(ylim = c(-2, 2),xlim = c(-2, 2)) +
  labs(title="VSPLOT15 Speed site differences")
plotspsitepanels











gg <- ggplot(data=ADT36_A,aes(x=dotest, y=Accuracy, color=siteid, shape=factor(flash))) +
  geom_point(alpha=0.5)



# trying stuff

for (j in 1:length(textsAcc)){
  test <- testsAcc[[j]]
  test <- test[!is.na(test$bblid) & !is.na(test$Accuracy),]
  
  hist <- ggplot(test,aes(x=Accuracy)) + geom_histogram()    # histogram to look at item acc frequency
  hist
  
  test <- test[!is.na(test$age),]
  fit <- gam(Accuracy ~ s(age), data = test)  # regress out age first
  visreg(fit)
  test$acc_res <- scale(resid(fit))
  test$spe_res <- 0 # spe_res doesn't matter here
  
  test <- test[abs(test$acc_res)<3,]
  
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



# new site difference plot that Kosha wanted ----

sitetextsAcc <- sort(c("KSPVRT_D","PCET_A","SPCPTNL","GNG150"))
sitetestsAcc <- mget(sitetextsAcc)

for (i in 1:length(sitetextsAcc)){
  test <- sitetestsAcc[[i]]
  test <- test[test$dotest>cutoff & !is.na(test$Accuracy) & !is.na(test$age) & !is.na(test$gender),]
  
  fit <- gam(Accuracy ~ s(age,k=3) + gender, data = test)
  res <- data.frame(scale(resid(fit)))
  
  names <- data.frame(matrix(rownames(test),ncol = 1))
  res <- cbind(names,res)
  names(res) <- c("row","Residuals")
  
  test <- cbind(names,test)
  names(test)[1] <- "row"
  test <- left_join(test,res)
  test <- test[abs(test$Residuals)<5,]
  
  gg <- ggplot(test, aes(x=factor(siteid),y=Residuals, color=factor(flash))) + 
    geom_boxplot(outlier.shape=NA) +
    geom_point(position=position_jitterdodge()) +
    theme_bw() + xlab("Site ID") +
    scale_color_discrete(name = "Test Administration", labels = c("Non-Flash", "Flash")) +
    labs(title = paste("Flash difference of", sitetextsAcc[i], "Accuracy Residuals at different sites"))
  
  assign(paste0(sitetextsAcc[i],"_sitedif"),gg)
}




sitetexts <- sort(c("MPRACT","SPCPTNL","GNG150"))
sitetests <- mget(sitetexts)

for (i in 1:length(sitetexts)){
  test <- sitetests[[i]]
  test <- test[test$dotest>cutoff & !is.na(test$Speed) & !is.na(test$age) & !is.na(test$gender),]
  
  fit <- gam(Speed ~ s(age,k=3) + gender, data = test)
  res <- data.frame(scale(resid(fit)))
  
  names <- data.frame(matrix(rownames(test),ncol = 1))
  res <- cbind(names,res)
  names(res) <- c("row","Residuals")
  
  test <- cbind(names,test)
  names(test)[1] <- "row"
  test <- left_join(test,res)
  test <- test[abs(test$Residuals)<5,]
  
  # gg <- ggplot(test, aes(x=factor(siteid),y=Residuals, color=factor(flash))) + 
  #   geom_boxplot(outlier.shape=NA) +
  #   geom_point(position=position_jitterdodge()) +
  #   theme_bw() + xlab("Site ID") +
  #   scale_color_discrete(name = "Test Administration", labels = c("Non-Flash", "Flash")) +
  #   labs(title = paste("Flash difference of", sitetexts[i], "Speed Residuals at different sites"))
  # 
  print("yo")
  # assign(paste0(sitetexts[i],"_sitedifSp"),gg)
}









