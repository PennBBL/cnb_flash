# factor analysis of itemwise data (part of flash difference script)

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
library(kableExtra)


# downloading itemwise data
# last year of flash data ----

cpt_iw <- read.csv("cpt_itemlevel/athena_3360_2080.csv")    # CPT [i]tem[w]ise
cpt_iw$test_sessions_v.dotest <- as.Date(cpt_iw$test_sessions_v.dotest)
cpt_iw$flash <- ifelse(cpt_iw$test_sessions_v.dotest <= as.Date("2020-12-31"),1,0)
cpt_iw <- cpt_iw[cpt_iw$test_sessions_v.dotest > as.Date("2019-12-31"),]

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
noNA_NL <- pcptnl[rowSums(is.na(pcptnl)) <5,]        # pcpt[nl]     0 obs
noNA_N <- pcptn360[rowSums(is.na(pcptn360)) <5,]     # pcpt[n]360   0 obs
noNA_sNL <- spcptnl[rowSums(is.na(spcptnl)) <5,]     # spcpt[nl]    both
noNA_sN <- spcptn90[rowSums(is.na(spcptn90)) <5,]    # spcpt[n]90   both

colnames(noNA_sNL)[5:184] <- sprintf("item_%03d",1:180)
colnames(noNA_sN)[5:94] <- sprintf("item_%03d",1:90)

sNL_f <- noNA_sNL[noNA_sNL$flash ==1,grepl("item",colnames(noNA_sNL))]  # noNA_sNL flash group
sNL_n <- noNA_sNL[noNA_sNL$flash ==0,grepl("item",colnames(noNA_sNL))]  # noNA_sNL non-flash group
sN_f <- noNA_sN[noNA_sN$flash ==1,grepl("item",colnames(noNA_sN))]  # noNA_sN flash group
sN_n <- noNA_sN[noNA_sN$flash ==0,grepl("item",colnames(noNA_sN))]  # noNA_sN non-flash group

sNLL_f <- sNL_f[,1:90]    # SPCPTNL letters, flash group       1494 x 90
sNLL_n <- sNL_n[,1:90]    # SPCPTNL letters, non-flash group   1512 x 90
sNLN_f <- sNL_f[,91:180]  # SPCPTNL numbers, flash group       1494 x 90
sNLN_n <- sNL_n[,91:180]  # SPCPTNL numbers, non-flash group   1512 x 90

sNLL_f$PC <- rowSums(sNLL_f)/90   # percent correct, n = 1415
qu <- quantile(sNLL_f$PC,0.05,na.rm=TRUE)
sNLL_f <- sNLL_f[sNLL_f$PC > qu,1:90]

sNLL_n$PC <- rowSums(sNLL_n)/90                    # n = 1431
qu <- quantile(sNLL_n$PC,0.05,na.rm=TRUE)
sNLL_n <- sNLL_n[sNLL_n$PC > qu,1:90]

sNLN_f$PC <- rowSums(sNLN_f)/90                    # n = 1418  
qu <- quantile(sNLN_f$PC,0.05,na.rm=TRUE)
sNLN_f <- sNLN_f[sNLN_f$PC > qu,1:90]

sNLN_n$PC <- rowSums(sNLN_n)/90                    # n = 1431
qu <- quantile(sNLN_n$PC,0.05,na.rm=TRUE)
sNLN_n <- sNLN_n[sNLN_n$PC > qu,1:90]

sN_f$PC <- rowSums(sN_f)/90                        # n = 111   
qu <- quantile(sN_f$PC,0.05,na.rm=TRUE)
sN_f <- sN_f[sN_f$PC > qu,1:90]

sN_n$PC <- rowSums(sN_n)/90                        # n = 127
qu <- quantile(sN_n$PC,0.05,na.rm=TRUE)
sN_n <- sN_n[sN_n$PC > qu,1:90]

# sNLL (letters, first half) flash
x <- sNLL_f
alpha_snllf <- alpha(x)$total$std.alpha
xcor <- polychoric(x)$rho
fa.parallel(xcor,n.obs=nrow(x))    # Parallel analysis suggests that the number of factors =  32  and the number of components =  25
nfactors(xcor,n.obs=nrow(x))       # 2, 3, 20 factors

sNLLf_mod <- mirt(x,2)
mod <- sNLLf_mod
oblimin_loadings <- fa.sort(irt.fa(x,2,plot=F)$fa)    
promax_loadings <- fa.sort(irt.fa(x,2,rotate="promax",plot=F)$fa) 
obli_exp_sNLL_f <- data.frame(oblimin_loadings$loadings[1:90,1:2])
pro_exp_sNLL_f <-  data.frame(promax_loadings$loadings[1:90,1:2])
obli_sum_sNLL_f <- data.frame(-summary(mod)$rotF)
pro_sum_sNLL_f <-  data.frame(-summary(mod,rotate="promax")$rotF[1:90,1:2])
ifc_oesNLL_f <- oblimin_loadings$Phi                # 0.249
ifc_pesNLL_f <- promax_loadings$Phi                 # 0.328
ifc_ossNLL_f <- summary(mod)$fcor                   # 0.393
ifc_pssNLL_f <- summary(mod,rotate="promax")$fcor   # 0.480

pdf("SPCPTNL_FLet.pdf",height=15,width=5)
fa.diagram(irt.fa(x,2,plot=F),cut=0.25,rsize = 5,
           main="Factor Analysis of SPCPTNL (letters, Flash)")
dev.off()

# sNLL (letters, first half) non-flash
x <- sNLL_n
alpha_snlln <- alpha(x,check.keys=TRUE)$total$std.alpha
xcor <- polychoric(x)$rho
fa.parallel(xcor,n.obs=nrow(x))    # Parallel analysis suggests that the number of factors =  30  and the number of components =  26 
nfactors(xcor,n.obs=nrow(x))       # 2, 4, 13, 20 factors

sNLLn_mod <- mirt(x,2)
mod <- sNLLn_mod
oblimin_loadings <- fa.sort(irt.fa(x,2,plot=F)$fa)    
promax_loadings <- fa.sort(irt.fa(x,2,rotate="promax",plot=F)$fa) 
obli_exp_sNLL_n <- data.frame(oblimin_loadings$loadings[1:90,1:2])
pro_exp_sNLL_n <-  data.frame(promax_loadings$loadings[1:90,1:2])
obli_sum_sNLL_n <- data.frame(summary(mod)$rotF)
obli_sum_sNLL_n$F1 <- -obli_sum_sNLL_n$F1
pro_sum_sNLL_n <-  data.frame(summary(mod,rotate="promax")$rotF[1:90,1:2])
pro_sum_sNLL_n$F1 <- -pro_sum_sNLL_n$F1
ifc_oesNLL_n <- oblimin_loadings$Phi                 # 0.242
ifc_pesNLL_n <- promax_loadings$Phi                  # 0.359
ifc_ossNLL_n <- -summary(mod)$fcor                   # 0.396
ifc_pssNLL_n <- -summary(mod,rotate="promax")$fcor   # 0.570

pdf("SPCPTNL_NLet.pdf",height=15,width=5)
fa.diagram(irt.fa(x,2,plot=F),cut=0.25,rsize = 5,
           main="Factor Analysis of SPCPTNL (letters, Non-Flash)")
dev.off()

# (a) CPT (SPCPTNL, letters/first half) flash vs non-flash comparison ----
names(obli_exp_sNLL_f) <- c("FlashF1","FlashF2")
names(obli_exp_sNLL_n) <- c("NFlashF1","NFlashF2")
obli_exp_sNLL <- left_join(rownames_to_column(obli_exp_sNLL_f),rownames_to_column(obli_exp_sNLL_n),by="rowname")
obli_exp_sNLLcorr1 <- cor(obli_exp_sNLL[,c(2,4)])   # 0.851
obli_exp_sNLLcorr2 <- cor(obli_exp_sNLL[,c(3,5)])   # 0.953

names(pro_exp_sNLL_f) <- c("FlashF1","FlashF2")
names(pro_exp_sNLL_n) <- c("NFlashF1","NFlashF2")
pro_exp_sNLL <- left_join(rownames_to_column(pro_exp_sNLL_f),rownames_to_column(pro_exp_sNLL_n),by="rowname")
pro_exp_sNLLcorr1 <- -cor(pro_exp_sNLL[,c(2,4)])    # 0.912
pro_exp_sNLLcorr2 <- -cor(pro_exp_sNLL[,c(3,5)])    # 0.862

names(obli_sum_sNLL_f) <- c("FlashF1","FlashF2")
names(obli_sum_sNLL_n) <- c("NFlashF1","NFlashF2")
obli_sum_sNLL <- left_join(rownames_to_column(obli_sum_sNLL_f),rownames_to_column(obli_sum_sNLL_n),by="rowname")
obli_sum_sNLLcorr1 <- cor(obli_sum_sNLL[,c(2,4)])   # 0.885
obli_sum_sNLLcorr2 <- cor(obli_sum_sNLL[,c(3,5)])   # 0.939

names(pro_sum_sNLL_f) <- c("FlashF1","FlashF2")
names(pro_sum_sNLL_n) <- c("NFlashF1","NFlashF2")
pro_sum_sNLL <- left_join(rownames_to_column(pro_sum_sNLL_f),rownames_to_column(pro_sum_sNLL_n),by="rowname")
pro_sum_sNLLcorr1 <- cor(pro_sum_sNLL[,c(2,4)])     # 0.904
pro_sum_sNLLcorr2 <- cor(pro_sum_sNLL[,c(3,5)])     # 0.940

# sNLN (numbers, last half) flash
x <- sNLN_f
alpha_snlnf <- alpha(x,check.keys = T)$total$std.alpha
xcor <- polychoric(x)$rho
fa.parallel(xcor,n.obs=nrow(x))    # Parallel analysis suggests that the number of factors =  28  and the number of components =  22
nfactors(xcor,n.obs=nrow(x))       # 2, 4, 20 factors

sNLNf_mod <- mirt(x,2)
mod <- sNLNf_mod
oblimin_loadings <- fa.sort(irt.fa(x,2,plot=F)$fa)    
promax_loadings <- fa.sort(irt.fa(x,2,rotate="promax",plot=F)$fa) 
obli_exp_sNLN_f <- data.frame(oblimin_loadings$loadings[1:90,1:2])
pro_exp_sNLN_f <-  data.frame(promax_loadings$loadings[1:90,1:2])
obli_sum_sNLN_f <- data.frame(-summary(mod)$rotF)
obli_sum_sNLN_f$F2 <- -obli_sum_sNLN_f$F2
pro_sum_sNLN_f <-  data.frame(-summary(mod,rotate="promax")$rotF[1:90,1:2])
pro_sum_sNLN_f$F2 <- -pro_sum_sNLN_f$F2
ifc_oesNLN_f <- oblimin_loadings$Phi                # 0.092
ifc_pesNLN_f <- promax_loadings$Phi                 # 0.086
ifc_ossNLN_f <- -summary(mod)$fcor                  # 0.252
ifc_pssNLN_f <- -summary(mod,rotate="promax")$fcor  # 0.241

pdf("SPCPTNL_FNum.pdf",height=15,width=5)
fa.diagram(irt.fa(x,2,plot=F),cut=0.25,rsize = 5,
           main="Factor Analysis of SPCPTNL (numbers, Flash)")
dev.off()

# sNLN (numbers, last half) non-flash
x <- sNLN_n
alpha_snlnn <- alpha(x,check.keys=TRUE)$total$std.alpha
xcor <- polychoric(x)$rho
fa.parallel(xcor,n.obs=nrow(x))    # Parallel analysis suggests that the number of factors =  30  and the number of components =  26 
nfactors(xcor,n.obs=nrow(x))       # 2, 4, 16, 20 factors

sNLNn_mod <- mirt(x,2)
mod <- sNLNn_mod
oblimin_loadings <- fa.sort(irt.fa(x,2,plot=F)$fa)    
promax_loadings <- fa.sort(irt.fa(x,2,rotate="promax",plot = F)$fa) 
obli_exp_sNLN_n <- data.frame(oblimin_loadings$loadings[1:90,1:2])
pro_exp_sNLN_n <-  data.frame(promax_loadings$loadings[1:90,1:2])
obli_sum_sNLN_n <- -data.frame(summary(mod)$rotF)
pro_sum_sNLN_n <-  -data.frame(summary(mod,rotate="promax")$rotF[1:90,1:2])
ifc_oesNLN_n <- oblimin_loadings$Phi                # 0.099
ifc_pesNLN_n <- promax_loadings$Phi                 # 0.140
ifc_ossNLN_n <- summary(mod)$fcor                   # 0.224
ifc_pssNLN_n <- summary(mod,rotate="promax")$fcor   # 0.314

pdf("SPCPTNL_NNum.pdf",height=15,width=5)
fa.diagram(irt.fa(x,2,plot=F),cut=0.25,rsize = 5,
           main="Factor Analysis of SPCPTNL (numbers, Non-Flash)")
dev.off()

# (b) CPT (SPCPTNL, numbers/last half) flash vs non-flash comparison ----
names(obli_exp_sNLN_f) <- c("FlashF1","FlashF2")
names(obli_exp_sNLN_n) <- c("NFlashF1","NFlashF2")
obli_exp_sNLN <- left_join(rownames_to_column(obli_exp_sNLN_f),rownames_to_column(obli_exp_sNLN_n),by="rowname")
obli_exp_sNLNcorr1 <- cor(obli_exp_sNLN[,c(2,4)])   # 0.960
obli_exp_sNLNcorr2 <- cor(obli_exp_sNLN[,c(3,5)])   # 0.890

names(pro_exp_sNLN_f) <- c("FlashF1","FlashF2")
names(pro_exp_sNLN_n) <- c("NFlashF1","NFlashF2")
pro_exp_sNLN <- left_join(rownames_to_column(pro_exp_sNLN_f),rownames_to_column(pro_exp_sNLN_n),by="rowname")
pro_exp_sNLNcorr1 <- cor(pro_exp_sNLN[,c(2,4)])    # 0.961
pro_exp_sNLNcorr2 <- cor(pro_exp_sNLN[,c(3,5)])    # 0.896 

names(obli_sum_sNLN_f) <- c("FlashF1","FlashF2")
names(obli_sum_sNLN_n) <- c("NFlashF1","NFlashF2")
obli_sum_sNLN <- left_join(rownames_to_column(obli_sum_sNLN_f),rownames_to_column(obli_sum_sNLN_n),by="rowname")
obli_sum_sNLNcorr1 <- cor(obli_sum_sNLN[,c(3,4)])   # 0.957
obli_sum_sNLNcorr2 <- cor(obli_sum_sNLN[,c(2,5)])   # 0.871

names(pro_sum_sNLN_f) <- c("FlashF1","FlashF2")
names(pro_sum_sNLN_n) <- c("NFlashF1","NFlashF2")
pro_sum_sNLN <- left_join(rownames_to_column(pro_sum_sNLN_f),rownames_to_column(pro_sum_sNLN_n),by="rowname")
pro_sum_sNLNcorr1 <- cor(pro_sum_sNLN[,c(3,4)])     # 0.957
pro_sum_sNLNcorr2 <- cor(pro_sum_sNLN[,c(2,5)])     # 0.900

# sN flash
x <- sN_f
alpha_snf <- alpha(x,check.keys=TRUE)$total$std.alpha
xcor <- polychoric(x)$rho
fa.parallel(xcor,n.obs=nrow(x))    # Parallel analysis suggests that the number of factors =  18  and the number of components =  17
nfactors(xcor,n.obs=nrow(x))       # 2, 4, 10, 16, 20 factors

sNf_LY_mod <- mirt(x,2)
mod <- sNf_LY_mod
oblimin_loadings <- fa.sort(irt.fa(x,2,plot = F)$fa)   
promax_loadings <- fa.sort(irt.fa(x,2,rotate="promax",plot = F)$fa)  
obli_exp_sN_f <- data.frame(oblimin_loadings$loadings[1:90,1:2])
pro_exp_sN_f <-  data.frame(promax_loadings$loadings[1:90,1:2])
obli_sum_sN_f <- data.frame(summary(mod)$rotF)
obli_sum_sN_f$F1 <- -obli_sum_sN_f$F1
pro_sum_sN_f <-  data.frame(summary(mod,rotate="promax")$rotF[1:90,1:2])
pro_sum_sN_f$F1 <- -pro_sum_sN_f$F1
ifc_oesN_f <- oblimin_loadings$Phi                # 0.332
ifc_pesN_f <- promax_loadings$Phi                 # 0.505
ifc_ossN_f <- -summary(mod)$fcor                   # 0.300
ifc_pssN_f <- -summary(mod,rotate="promax")$fcor   # 0.445

pdf("SPCPTN90_F.pdf",height=15,width=5)
fa.diagram(irt.fa(x,2,plot=F),cut=0.25,rsize = 5,
           main="Factor Analysis of SPCPTN90 (Flash)")
dev.off()

# sN non-flash
x <- sN_n
alpha_snn <- alpha(x,check.keys=TRUE)$total$std.alpha
xcor <- polychoric(x)$rho
fa.parallel(xcor,n.obs=nrow(x))    # Parallel analysis suggests that the number of factors =  15  and the number of components =  13 
nfactors(xcor,n.obs=nrow(x))       # 4, 5, 15, 20 factors

sNn_LY_mod <- mirt(x,2)
mod <- sNn_LY_mod
oblimin_loadings <- fa.sort(irt.fa(x,2,plot = F)$fa)    
promax_loadings <- fa.sort(irt.fa(x,2,rotate="promax",plot = F)$fa)
obli_exp_sN_n <- data.frame(oblimin_loadings$loadings[1:90,1:2])
pro_exp_sN_n <-  data.frame(promax_loadings$loadings[1:90,1:2])
obli_sum_sN_n <- data.frame(summary(mod)$rotF)
obli_sum_sN_n$F1 <- -obli_sum_sN_n$F1
pro_sum_sN_n <-  data.frame(summary(mod,rotate="promax")$rotF[1:90,1:2])
pro_sum_sN_n$F1 <- -pro_sum_sN_n$F1
ifc_oesN_n <- oblimin_loadings$Phi                 # 0.356
ifc_pesN_n <- promax_loadings$Phi                  # 0.367
ifc_ossN_n <- -summary(mod)$fcor                   # 0.318
ifc_pssN_n <- -summary(mod,rotate="promax")$fcor   # 0.406

pdf("SPCPTN90_N.pdf",height=15,width=5)
fa.diagram(irt.fa(x,2,plot=F),cut=0.25,rsize = 5,
           main="Factor Analysis of SPCPTN90 (Non-Flash)")
dev.off()

# (c) CPT (SPCPTN90) flash vs non-flash comparison ----
names(obli_exp_sN_f) <- c("FlashF1","FlashF2")
names(obli_exp_sN_n) <- c("NFlashF1","NFlashF2")
obli_exp_sN <- left_join(rownames_to_column(obli_exp_sN_f),rownames_to_column(obli_exp_sN_n),by="rowname")
obli_exp_sNcorr1 <- cor(obli_exp_sN[,c(2,4)])   # 0.718
obli_exp_sNcorr2 <- cor(obli_exp_sN[,c(3,5)])   # 0.762

names(pro_exp_sN_f) <- c("FlashF1","FlashF2")
names(pro_exp_sN_n) <- c("NFlashF1","NFlashF2")
pro_exp_sN <- left_join(rownames_to_column(pro_exp_sN_f),rownames_to_column(pro_exp_sN_n),by="rowname")
pro_exp_sNcorr1 <- cor(pro_exp_sN[,c(3,4)])     # 0.757
pro_exp_sNcorr2 <- cor(pro_exp_sN[,c(2,5)])     # 0.742

names(obli_sum_sN_f) <- c("FlashF1","FlashF2")
names(obli_sum_sN_n) <- c("NFlashF1","NFlashF2")
obli_sum_sN <- left_join(rownames_to_column(obli_sum_sN_f),rownames_to_column(obli_sum_sN_n),by="rowname")
obli_sum_sNcorr1 <- cor(obli_sum_sN[,c(2,4)])   # 0.639
obli_sum_sNcorr2 <- cor(obli_sum_sN[,c(3,5)])   # 0.509

names(pro_sum_sN_f) <- c("FlashF1","FlashF2")
names(pro_sum_sN_n) <- c("NFlashF1","NFlashF2")
pro_sum_sN <- left_join(rownames_to_column(pro_sum_sN_f),rownames_to_column(pro_sum_sN_n),by="rowname")
pro_sum_sNcorr1 <- cor(pro_sum_sN[,c(2,4)])     # 0.649
pro_sum_sNcorr2 <- cor(pro_sum_sN[,c(3,5)])     # 0.538


# summary table ----
summary <- data.frame(matrix(NA,nrow=6,ncol = 5))
names(summary) <- c("Exp. (oblimin)","Exp. (promax)","MIRT (oblimin)","MIRT (promax)","Alpha")
rownames(summary) <- c("SPCPTNL - letters, Flash","SPCPTNL - letters, Non-Flash","SPCPTNL - numbers, Flash","SPCPTNL - numbers, Non-Flash","SPCPTN90, Flash","SPCPTN90 Non-Flash")
summary[1,] <- c(ifc_oesNLL_f[1,2],ifc_pesNLL_f[1,2],ifc_ossNLL_f[1,2],ifc_pssNLL_f[1,2],alpha_snllf)
summary[2,] <- c(ifc_oesNLL_n[1,2],ifc_pesNLL_n[1,2],ifc_ossNLL_n[1,2],ifc_pssNLL_n[1,2],alpha_snlln)
summary[3,] <- c(ifc_oesNLN_f[1,2],ifc_pesNLN_f[1,2],ifc_ossNLN_f[1,2],ifc_pssNLN_f[1,2],alpha_snlnf)
summary[4,] <- c(ifc_oesNLN_n[1,2],ifc_pesNLN_n[1,2],ifc_ossNLN_n[1,2],ifc_pssNLN_n[1,2],alpha_snlnn)
summary[5,] <- c(ifc_oesN_f[1,2],ifc_pesN_f[1,2],ifc_ossN_f[1,2],ifc_pssN_f[1,2],alpha_snf)
summary[6,] <- c(ifc_oesN_n[1,2],ifc_pesN_n[1,2],ifc_ossN_n[1,2],ifc_pssN_n[1,2],alpha_snn)
summary <- round(summary,3)

summary %>%
  kbl(caption="Inter-factor correlations of Exploratory Item-Factor and MIRT analylses for SPCPTNL and SPCPTN90",
      align=rep('c', 5)) %>%
  kable_classic(full_width = F, html_font = "Cambria") %>%
  save_kable(file = "CPT_LY_interfactor.html", self_contained = T)

write.csv(summary,"SPCPT_LY_flashdif_interfactorcorr.csv")




# speed differences ----
pcptnl <- cpt_iw[,grepl("PCPTNL.CPT_Q", colnames(cpt_iw))]
pcptn360 <- cpt_iw[,grepl("PCPTN360.CPT_Q", colnames(cpt_iw))]
spcptnl <- cpt_iw[,grepl("SPCPTNL.SCPT_Q", colnames(cpt_iw))]
spcptn90 <- cpt_iw[,grepl("SPCPTN90.SCPT_Q", colnames(cpt_iw))]

pcptnl <- pcptnl[,grepl("TTR", colnames(pcptnl))]
pcptn360 <- pcptn360[,grepl("TTR", colnames(pcptn360))]
spcptnl <- spcptnl[,grepl("TTR", colnames(spcptnl))]
spcptn90 <- spcptn90[,grepl("TTR", colnames(spcptn90))]

pcptnl <- cbind(id, pcptnl)
pcptn360 <- cbind(id, pcptn360)
spcptnl <- cbind(id, spcptnl)
spcptn90 <- cbind(id, spcptn90)

# now keep the ones that don't have NA
noNA_NL <- pcptnl[rowSums(!is.na(pcptnl)) > 5,]        # pcpt[nl]     flash
noNA_N <- pcptn360[rowSums(!is.na(pcptn360)) > 5,]     # pcpt[n]360   flash
noNA_sNL <- spcptnl[rowSums(!is.na(spcptnl)) > 5,]     # spcpt[nl]    both
noNA_sN <- spcptn90[rowSums(!is.na(spcptn90)) > 5,]    # spcpt[n]90   both

colnames(noNA_sNL)[5:184] <- sprintf("item_%03d",1:180)
colnames(noNA_sN)[5:94] <- sprintf("item_%03d",1:90)


# matching ----
# -- making sure that there is a TTR for all CORR == 1, TTR is NA for CORR == 0
spcptnl <- cpt_iw[,grepl("SPCPTNL.SCPT_Q", colnames(cpt_iw))]
spcptn90 <- cpt_iw[,grepl("SPCPTN90.SCPT_Q", colnames(cpt_iw))]
sNL_resp <- spcptnl[,grepl("RESP", colnames(spcptnl))]
sN_resp <- spcptn90[,grepl("RESP", colnames(spcptn90))]
sNL_resp<- cbind(id, sNL_resp)
sN_resp  <- cbind(id, sN_resp)
sNL_resp <- sNL_resp[rowSums(!is.na(sNL_resp)) > 5,] 
sN_resp <- sN_resp[rowSums(!is.na(sN_resp)) > 5,] 

sNL_speed <- noNA_sNL
sN_speed <- noNA_sN
both <- intersect(unique(sNL_speed$test_sessions.datasetid),unique(sNL_resp$test_sessions.datasetid))
sNL_speed <- sNL_speed[sNL_speed$test_sessions.datasetid %in% both,]   # doesn't change anything because all of the TTR data exists in corr too but not in CORR
sNL_resp <- sNL_resp[sNL_resp$test_sessions.datasetid %in% both,]
both <- intersect(unique(sN_speed$test_sessions.datasetid),unique(sN_resp$test_sessions.datasetid))
sN_speed <- sN_speed[sN_speed$test_sessions.datasetid %in% both,]
sN_resp <- sN_resp[sN_resp$test_sessions.datasetid %in% both,]

sNL_speed <- sNL_speed[match(sNL_speed$test_sessions.datasetid,sNL_resp$test_sessions.datasetid),5:184]
sN_speed <- sN_speed[match(sN_speed$test_sessions.datasetid,sN_resp$test_sessions.datasetid),5:94]
sNL_resp <- sNL_resp[5:184]
sN_resp <- sN_resp[5:94]

sN_corr <- sN_speed
sN_corr[!is.na(sN_corr)] <- NA
for (i in 1:nrow(sN_speed)){
  spe <- sN_speed[i,]
  resp <- sN_resp[i,]
  sN_corr[i,] <- ifelse(resp==1, ifelse(!is.na(spe),1,0), ifelse(is.na(spe),1,NA))
}
sNL_corr <- sNL_speed
sNL_corr[!is.na(sNL_corr)] <- NA
for (i in 1:nrow(sNL_speed)){
  spe <- sNL_speed[i,]
  resp <- sNL_resp[i,]
  sNL_corr[i,] <- ifelse(resp==1, ifelse(!is.na(spe),1,0), ifelse(is.na(spe),1,NA))
}

sNL_corr$sum <- rowSums(sNL_corr,na.rm=T)
sN_corr$sum <- rowSums(sN_corr,na.rm=T)

noNA_sNL <- subset(noNA_sNL, (rowSums(sign(noNA_sNL[,5:184]) <= 0,na.rm=T) == 0))
noNA_sN <- subset(noNA_sN, (rowSums(sign(noNA_sN[,5:94]) <= 0,na.rm=T) == 0))

temp <- noNA_sNL[,5:184]
temp[temp >= 1200 | 200 > temp] <- NA
noNA_sNL[,5:184] <- temp
temp <- noNA_sN[,5:94]
temp[temp >= 1200 | 200 > temp] <- NA
noNA_sN[,5:94] <- temp

colsums_sNL <- colSums(!is.na(noNA_sNL[,5:184]),na.rm = T)
colsums_sN <- colSums(!is.na(noNA_sN[,5:94]),na.rm = T)

min(colsums_sNL)   # no items with less than 20 respondents
min(colsums_sN)    

noNA_sN <- cbind(noNA_sN[,1:4],noNA_sN[,c(names(colsums_sN[colsums_sN>20]))])


# only need sNL and sN since these tests were administered both flash and non-flash
sNL_fs <- noNA_sNL[noNA_sNL$flash ==1,grepl("item",colnames(noNA_sNL))]  # noNA_sNL flash group
sNL_ns <- noNA_sNL[noNA_sNL$flash ==0,grepl("item",colnames(noNA_sNL))]  # noNA_sNL non-flash group
sN_fs <- noNA_sN[noNA_sN$flash ==1,grepl("item",colnames(noNA_sN))]  # noNA_sN flash group
sN_ns <- noNA_sN[noNA_sN$flash ==0,grepl("item",colnames(noNA_sN))]  # noNA_sN non-flash group


sNL_fmeans <- data.frame(colMeans(sNL_fs,na.rm = T))
sNL_nmeans <- data.frame(colMeans(sNL_ns,na.rm = T))
sN_fmeans <- data.frame(colMeans(sN_fs,na.rm = T))
sN_nmeans <- data.frame(colMeans(sN_ns,na.rm = T))

sNL_fsd <- data.frame(apply(sNL_fs, 2, sd,na.rm=T))
sNL_nsd <- data.frame(apply(sNL_ns, 2, sd,na.rm=T))
sN_fsd <- data.frame(apply(sN_fs, 2, sd,na.rm=T))
sN_nsd <- data.frame(apply(sN_ns, 2, sd,na.rm=T))

sNL_fmeansd <- cbind(rownames_to_column(sNL_fmeans),sNL_fsd)
names(sNL_fmeansd) <- c("Item","MeanTTR","sd")
sNL_fmeansd$Flash <- 1
sNL_nmeansd <- cbind(rownames_to_column(sNL_nmeans),sNL_nsd)
names(sNL_nmeansd) <- c("Item","MeanTTR","sd")
sNL_nmeansd$Flash <- 0
sN_fmeansd <- cbind(rownames_to_column(sN_fmeans),sN_fsd)
names(sN_fmeansd) <- c("Item","MeanTTR","sd")
sN_fmeansd$Flash <- 1
sN_nmeansd <- cbind(rownames_to_column(sN_nmeans),sN_nsd)
names(sN_nmeansd) <- c("Item","MeanTTR","sd")
sN_nmeansd$Flash <- 0

sNL_meansd1 <- rbind(sNL_fmeansd[1:90,],sNL_nmeansd[1:90,])
sNL_meansd2 <- rbind(sNL_fmeansd[91:180,],sNL_nmeansd[91:180,])
sN_meansd <- rbind(sN_fmeansd,sN_nmeansd)





# plot of means with error bars
spe1_means <- ggplot(sNL_meansd1,aes(x=Item,y=MeanTTR,color=factor(Flash))) +
  geom_point(position=position_dodge(.9)) +
  geom_errorbar(aes(ymin=MeanTTR-sd, ymax=MeanTTR+sd), width=.2,position=position_dodge(.9)) +
  theme_bw() + ylab("Response Time (ms)") +
  scale_color_discrete(name = "Test Administration", labels = c("Non-Flash (n=1511)", "Flash (n=1493)")) +
  labs(title = paste("Flash differences in SPCPTNL Speed (first 90 items)")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggsave("myresults/meanTTR_peritem/SPCPTNL_LY_1_means.png",height = 10,width = 30)



spe2_means <- ggplot(sNL_meansd2,aes(x=Item,y=MeanTTR,color=factor(Flash))) +
  geom_point(position=position_dodge(.9)) +
  geom_errorbar(aes(ymin=MeanTTR-sd, ymax=MeanTTR+sd), width=.2,position=position_dodge(.9)) +
  theme_bw() + ylab("Response Time (ms)") +
  scale_color_discrete(name = "Test Administration", labels = c("Non-Flash (n=1511)", "Flash (n=1493)")) +
  labs(title = paste("Flash differences in SPCPTNL Speed (last 90 items)")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggsave("myresults/meanTTR_peritem/SPCPTNL_LY_2_means.png",height = 10,width = 30)



spe3_means <- ggplot(sN_meansd,aes(x=Item,y=MeanTTR,color=factor(Flash))) +
  geom_point(position=position_dodge(.9)) +
  geom_errorbar(aes(ymin=MeanTTR-sd, ymax=MeanTTR+sd), width=.2,position=position_dodge(.9)) +
  theme_bw() + ylab("Response Time (ms)") +
  scale_color_discrete(name = "Test Administration", labels = c("Non-Flash (n=133)", "Flash (n=117)")) +
  labs(title = paste("Flash differences in SPCPTN90 Speed")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggsave("myresults/meanTTR_peritem/SPCPTN90_LY_means.png",height = 10,width = 30)





# log transform plots

noNA_sNL[,5:184] <- sapply(noNA_sNL[,5:184]+1,log)
noNA_sN[,5:64] <- sapply(noNA_sN[,5:64]+1,log)

sNL_fs <- noNA_sNL[noNA_sNL$flash ==1,grepl("item",colnames(noNA_sNL))]  # noNA_sNL flash group
sNL_ns <- noNA_sNL[noNA_sNL$flash ==0,grepl("item",colnames(noNA_sNL))]  # noNA_sNL non-flash group
sN_fs <- noNA_sN[noNA_sN$flash ==1,grepl("item",colnames(noNA_sN))]  # noNA_sN flash group
sN_ns <- noNA_sN[noNA_sN$flash ==0,grepl("item",colnames(noNA_sN))]  # noNA_sN non-flash group


sNL_fmeans <- data.frame(colMeans(sNL_fs,na.rm = T))
sNL_nmeans <- data.frame(colMeans(sNL_ns,na.rm = T))
sN_fmeans <- data.frame(colMeans(sN_fs,na.rm = T))
sN_nmeans <- data.frame(colMeans(sN_ns,na.rm = T))

sNL_fsd <- data.frame(apply(sNL_fs, 2, sd,na.rm=T))
sNL_nsd <- data.frame(apply(sNL_ns, 2, sd,na.rm=T))
sN_fsd <- data.frame(apply(sN_fs, 2, sd,na.rm=T))
sN_nsd <- data.frame(apply(sN_ns, 2, sd,na.rm=T))

sNL_fmeansd <- cbind(rownames_to_column(sNL_fmeans),sNL_fsd)
names(sNL_fmeansd) <- c("Item","MeanTTR","sd")
sNL_fmeansd$Flash <- 1
sNL_nmeansd <- cbind(rownames_to_column(sNL_nmeans),sNL_nsd)
names(sNL_nmeansd) <- c("Item","MeanTTR","sd")
sNL_nmeansd$Flash <- 0
sN_fmeansd <- cbind(rownames_to_column(sN_fmeans),sN_fsd)
names(sN_fmeansd) <- c("Item","MeanTTR","sd")
sN_fmeansd$Flash <- 1
sN_nmeansd <- cbind(rownames_to_column(sN_nmeans),sN_nsd)
names(sN_nmeansd) <- c("Item","MeanTTR","sd")
sN_nmeansd$Flash <- 0

sNL_meansd1 <- rbind(sNL_fmeansd[1:90,],sNL_nmeansd[1:90,])
sNL_meansd2 <- rbind(sNL_fmeansd[91:180,],sNL_nmeansd[91:180,])
sN_meansd <- rbind(sN_fmeansd,sN_nmeansd)


log_spe1_means <- ggplot(sNL_meansd1,aes(x=Item,y=MeanTTR,color=factor(Flash))) +
  geom_point(position=position_dodge(.9)) +
  geom_errorbar(aes(ymin=MeanTTR-sd, ymax=MeanTTR+sd), width=.2,position=position_dodge(.9)) +
  theme_bw() + ylab("Log transformed Response Time (ms)") +
  scale_color_discrete(name = "Test Administration", labels = c("Non-Flash (n=1511)", "Flash (n=1493)")) +
  labs(title = paste("Flash differences in SPCPTNL Speed (first 90, log transformed)")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggsave("myresults/meanTTR_peritem/SPCPTNL_LY_1_logmeans.png",height = 10,width = 30)


log_spe2_means <- ggplot(sNL_meansd2,aes(x=Item,y=MeanTTR,color=factor(Flash))) +
  geom_point(position=position_dodge(.9)) +
  geom_errorbar(aes(ymin=MeanTTR-sd, ymax=MeanTTR+sd), width=.2,position=position_dodge(.9)) +
  theme_bw() + ylab("Log transformed Response Time (ms)") +
  scale_color_discrete(name = "Test Administration", labels = c("Non-Flash (n=1511)", "Flash (n=1493)")) +
  labs(title = paste("Flash differences in SPCPTNL Speed (last 90, log transformed)")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggsave("myresults/meanTTR_peritem/SPCPTNL_LY_2_logmeans.png",height = 10,width = 30)


log_spe3_means <- ggplot(sN_meansd,aes(x=Item,y=MeanTTR,color=factor(Flash))) +
  geom_point(position=position_dodge(.9)) +
  geom_errorbar(aes(ymin=MeanTTR-sd, ymax=MeanTTR+sd), width=.2,position=position_dodge(.9)) +
  theme_bw() + ylab("Log transformed Response Time (ms)") +
  scale_color_discrete(name = "Test Administration", labels = c("Non-Flash (n=133)", "Flash (n=117)")) +
  labs(title = paste("Flash differences in SPCPTN90 Speed (Log transformed)")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggsave("myresults/meanTTR_peritem/SPCPTN90_LY_logmeans.png",height = 10,width = 30)







# OLD CODE (FOR ALL DATES) ---- ###############################################

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

sNL_acc <- noNA_sNL
sN_acc <- noNA_sN

# only need sNL and sN since these tests were administered both flash and non-flash
sNL_f <- noNA_sNL[noNA_sNL$flash ==1,grepl("CORR",colnames(noNA_sNL))]  # noNA_sNL flash group
sNL_n <- noNA_sNL[noNA_sNL$flash ==0,grepl("CORR",colnames(noNA_sNL))]  # noNA_sNL non-flash group
sN_f <- noNA_sN[noNA_sN$flash ==1,grepl("CORR",colnames(noNA_sN))]  # noNA_sN flash group
sN_n <- noNA_sN[noNA_sN$flash ==0,grepl("CORR",colnames(noNA_sN))]  # noNA_sN non-flash group

sNLL_f <- sNL_f[,1:90]
sNLL_n <- sNL_n[,1:90]
sNLN_f <- sNL_f[,91:180]
sNLN_n <- sNL_n[,91:180]

sNLL_f$PC <- rowSums(sNLL_f)/90   # percent correct
qu <- quantile(sNLL_f$PC,0.05,na.rm=TRUE)
sNLL_f <- sNLL_f[sNLL_f$PC > qu,1:90]

sNLL_n$PC <- rowSums(sNLL_n)/90
qu <- quantile(sNLL_n$PC,0.05,na.rm=TRUE)
sNLL_n <- sNLL_n[sNLL_n$PC > qu,1:90]

sNLN_f$PC <- rowSums(sNLN_f)/90  
qu <- quantile(sNLN_f$PC,0.05,na.rm=TRUE)
sNLN_f <- sNLN_f[sNLN_f$PC > qu,1:90]

sNLN_n$PC <- rowSums(sNLN_n)/90
qu <- quantile(sNLN_n$PC,0.05,na.rm=TRUE)
sNLN_n <- sNLN_n[sNLN_n$PC > qu,1:90]

sN_f$PC <- rowSums(sN_f)/90   
qu <- quantile(sN_f$PC,0.05,na.rm=TRUE)
sN_f <- sN_f[sN_f$PC > qu,1:90]

sN_n$PC <- rowSums(sN_n)/90
qu <- quantile(sN_n$PC,0.05,na.rm=TRUE)
sN_n <- sN_n[sN_n$PC > qu,1:90]

# sNLL (letters, first half) flash
x <- sNLL_f
alpha_snllf <- alpha(x)$total$std.alpha
xcor <- polychoric(x)$rho
fa.parallel(xcor,n.obs=nrow(x))    # Parallel analysis suggests that the number of factors =  32  and the number of components =  25
nfactors(xcor,n.obs=nrow(x))       # 2, 3, 20 factors

sNLLf_mod <- mirt(x,2)
mod <- sNLLf_mod
oblimin_loadings <- fa.sort(irt.fa(x,2)$fa)    
promax_loadings <- fa.sort(irt.fa(x,2,rotate="promax",plot = F)$fa) 
obli_exp_sNLL_f <- data.frame(oblimin_loadings$loadings[1:90,1:2])
pro_exp_sNLL_f <-  data.frame(promax_loadings$loadings[1:90,1:2])
obli_sum_sNLL_f <- data.frame(-summary(mod)$rotF)
pro_sum_sNLL_f <-  data.frame(-summary(mod,rotate="promax")$rotF[1:90,1:2])
ifc_oesNLL_f <- oblimin_loadings$Phi                # 0.249
ifc_pesNLL_f <- promax_loadings$Phi                 # 0.328
ifc_ossNLL_f <- summary(mod)$fcor                   # 0.393
ifc_pssNLL_f <- summary(mod,rotate="promax")$fcor   # 0.480

# sNLL (letters, first half) non-flash
x <- sNLL_n
alpha_snlln <- alpha(x,check.keys=TRUE)$total$std.alpha
xcor <- polychoric(x)$rho
fa.parallel(xcor,n.obs=nrow(x))    # Parallel analysis suggests that the number of factors =  30  and the number of components =  26 
nfactors(xcor,n.obs=nrow(x))       # 2, 4, 13, 20 factors

sNLLn_mod <- mirt(x,2)
mod <- sNLLn_mod
oblimin_loadings <- fa.sort(irt.fa(x,2)$fa)    
promax_loadings <- fa.sort(irt.fa(x,2,rotate="promax",plot = F)$fa) 
obli_exp_sNLL_n <- data.frame(oblimin_loadings$loadings[1:180,1:2])
pro_exp_sNLL_n <-  data.frame(promax_loadings$loadings[1:180,1:2])
obli_sum_sNLL_n <- data.frame(summary(mod)$rotF)
pro_sum_sNLL_n <-  data.frame(summary(mod,rotate="promax")$rotF[1:180,1:2])
ifc_oesNLL_n <- oblimin_loadings$Phi                # 0.190
ifc_pesNLL_n <- promax_loadings$Phi                 # 0.288
ifc_ossNLL_n <- summary(mod)$fcor                   # 0.324
ifc_pssNLL_n <- summary(mod,rotate="promax")$fcor   # 0.470

# (a) CPT (SPCPTNL, letters/first half) flash vs non-flash comparison ----
names(obli_exp_sNLL_f) <- c("FlashF1","FlashF2")
names(obli_exp_sNLL_n) <- c("NFlashF1","NFlashF2")
obli_exp_sNLL <- left_join(rownames_to_column(obli_exp_sNLL_f),rownames_to_column(obli_exp_sNLL_n),by="rowname")
obli_exp_sNLLcorr1 <- cor(obli_exp_sNLL[,c(2,4)])   # 0.861
obli_exp_sNLLcorr2 <- cor(obli_exp_sNLL[,c(3,5)])   # 0.827

names(pro_exp_sNL_f) <- c("FlashF1","FlashF2")
names(pro_exp_sNL_n) <- c("NFlashF1","NFlashF2")
pro_exp_sNL <- left_join(rownames_to_column(pro_exp_sNL_f),rownames_to_column(pro_exp_sNL_n),by="rowname")
pro_exp_sNLcorr1 <- cor(pro_exp_sNL[,c(2,4)])    # 0.865
pro_exp_sNLcorr2 <- cor(pro_exp_sNL[,c(3,5)])    # 0.850

names(obli_sum_sNL_f) <- c("FlashF1","FlashF2")
names(obli_sum_sNL_n) <- c("NFlashF1","NFlashF2")
obli_sum_sNL <- left_join(rownames_to_column(obli_sum_sNL_f),rownames_to_column(obli_sum_sNL_n),by="rowname")
obli_sum_sNLcorr1 <- cor(obli_sum_sNL[,c(2,4)])   # 0.850
obli_sum_sNLcorr2 <- cor(obli_sum_sNL[,c(3,5)])   # 0.920

names(pro_sum_sNL_f) <- c("FlashF1","FlashF2")
names(pro_sum_sNL_n) <- c("NFlashF1","NFlashF2")
pro_sum_sNL <- left_join(rownames_to_column(pro_sum_sNL_f),rownames_to_column(pro_sum_sNL_n),by="rowname")
pro_sum_sNLcorr1 <- cor(pro_sum_sNL[,c(2,4)])     # 0.875
pro_sum_sNLcorr2 <- cor(pro_sum_sNL[,c(3,5)])     # 0.920

# sNLL (letters, first half) flash
x <- sNLN_f
alpha_snlnf <- alpha(x,check.keys = T)$total$std.alpha
xcor <- polychoric(x)$rho
fa.parallel(xcor,n.obs=nrow(x))    # Parallel analysis suggests that the number of factors =  28  and the number of components =  22
nfactors(xcor,n.obs=nrow(x))       # 2, 4, 20 factors

sNLNf_mod <- mirt(x,2)
mod <- sNLNf_mod
oblimin_loadings <- fa.sort(irt.fa(x,2)$fa)    
promax_loadings <- fa.sort(irt.fa(x,2,rotate="promax",plot = F)$fa) 
obli_exp_sNLN_f <- data.frame(oblimin_loadings$loadings[1:90,1:2])
pro_exp_sNLN_f <-  data.frame(promax_loadings$loadings[1:90,1:2])
obli_sum_sNLN_f <- data.frame(-summary(mod)$rotF)
pro_sum_sNLN_f <-  data.frame(-summary(mod,rotate="promax")$rotF[1:90,1:2])
ifc_oesNLN_f <- oblimin_loadings$Phi                # 0.249
ifc_pesNLN_f <- promax_loadings$Phi                 # 0.328
ifc_ossNLN_f <- summary(mod)$fcor                   # 0.393
ifc_pssNLN_f <- summary(mod,rotate="promax")$fcor   # 0.480

# sNLL (letters, first half) non-flash
x <- sNLN_n
alpha_snlnn <- alpha(x,check.keys=TRUE)$total$std.alpha
xcor <- polychoric(x)$rho
fa.parallel(xcor,n.obs=nrow(x))    # Parallel analysis suggests that the number of factors =  30  and the number of components =  26 
nfactors(xcor,n.obs=nrow(x))       # 2, 4, 13, 20 factors

sNLNn_mod <- mirt(x,2)
mod <- sNLNn_mod
oblimin_loadings <- fa.sort(irt.fa(x,2)$fa)    
promax_loadings <- fa.sort(irt.fa(x,2,rotate="promax",plot = F)$fa) 
obli_exp_sNLN_n <- data.frame(oblimin_loadings$loadings[1:180,1:2])
pro_exp_sNLN_n <-  data.frame(promax_loadings$loadings[1:180,1:2])
obli_sum_sNLN_n <- data.frame(summary(mod)$rotF)
pro_sum_sNLN_n <-  data.frame(summary(mod,rotate="promax")$rotF[1:180,1:2])
ifc_oesNLN_n <- oblimin_loadings$Phi                # 0.190
ifc_pesNLN_n <- promax_loadings$Phi                 # 0.288
ifc_ossNLN_n <- summary(mod)$fcor                   # 0.324
ifc_pssNLN_n <- summary(mod,rotate="promax")$fcor   # 0.470

# (b) CPT (SPCPTNL, numbers/last half) flash vs non-flash comparison ----
names(obli_exp_sNL_f) <- c("FlashF1","FlashF2")
names(obli_exp_sNL_n) <- c("NFlashF1","NFlashF2")
obli_exp_sNL <- left_join(rownames_to_column(obli_exp_sNL_f),rownames_to_column(obli_exp_sNL_n),by="rowname")
obli_exp_sNLcorr1 <- cor(obli_exp_sNL[,c(2,4)])   # 0.861
obli_exp_sNLcorr2 <- cor(obli_exp_sNL[,c(3,5)])   # 0.827

names(pro_exp_sNL_f) <- c("FlashF1","FlashF2")
names(pro_exp_sNL_n) <- c("NFlashF1","NFlashF2")
pro_exp_sNL <- left_join(rownames_to_column(pro_exp_sNL_f),rownames_to_column(pro_exp_sNL_n),by="rowname")
pro_exp_sNLcorr1 <- cor(pro_exp_sNL[,c(2,4)])    # 0.865
pro_exp_sNLcorr2 <- cor(pro_exp_sNL[,c(3,5)])    # 0.850

names(obli_sum_sNL_f) <- c("FlashF1","FlashF2")
names(obli_sum_sNL_n) <- c("NFlashF1","NFlashF2")
obli_sum_sNL <- left_join(rownames_to_column(obli_sum_sNL_f),rownames_to_column(obli_sum_sNL_n),by="rowname")
obli_sum_sNLcorr1 <- cor(obli_sum_sNL[,c(2,4)])   # 0.850
obli_sum_sNLcorr2 <- cor(obli_sum_sNL[,c(3,5)])   # 0.920

names(pro_sum_sNL_f) <- c("FlashF1","FlashF2")
names(pro_sum_sNL_n) <- c("NFlashF1","NFlashF2")
pro_sum_sNL <- left_join(rownames_to_column(pro_sum_sNL_f),rownames_to_column(pro_sum_sNL_n),by="rowname")
pro_sum_sNLcorr1 <- cor(pro_sum_sNL[,c(2,4)])     # 0.875
pro_sum_sNLcorr2 <- cor(pro_sum_sNL[,c(3,5)])     # 0.920

# sN flash
x <- sN_f
alpha_snf <- alpha(x)$total$std.alpha
xcor <- polychoric(x)$rho
fa.parallel(xcor,n.obs=nrow(x))    # Parallel analysis suggests that the number of factors =  17  and the number of components =  12
nfactors(xcor,n.obs=nrow(x))       # 2, 3, 15, 20 factors

sNf_mod <- mirt(x,2)
mod <- sNf_mod
oblimin_loadings <- fa.sort(irt.fa(x,2)$fa)   
promax_loadings <- fa.sort(irt.fa(x,2,rotate="promax",plot = F)$fa)  
obli_exp_sN_f <- data.frame(oblimin_loadings$loadings[1:90,1:2])
pro_exp_sN_f <-  data.frame(promax_loadings$loadings[1:90,1:2])
obli_sum_sN_f <- data.frame(summary(mod)$rotF)
pro_sum_sN_f <-  data.frame(summary(mod,rotate="promax")$rotF[1:90,1:2])
ifc_oesN_f <- oblimin_loadings$Phi                # 0.210
ifc_pesN_f <- promax_loadings$Phi                 # 0.307
ifc_ossN_f <- summary(mod)$fcor                   # 0.300
ifc_pssN_f <- summary(mod,rotate="promax")$fcor   # 0.480

mod <- mirt(x,3)
oblimin_loadings <- fa.sort(irt.fa(x,3)$fa)    
promax_loadings <- fa.sort(irt.fa(x,3,rotate="promax")$fa,plot = F) 
obli_exp_sN_f3 <- data.frame(oblimin_loadings$loadings[1:90,1:3])
pro_exp_sN_f3 <-  data.frame(promax_loadings$loadings[1:90,1:3])
obli_sum_sN_f3 <- data.frame(summary(mod)$rotF)
pro_sum_sN_f3 <-  data.frame(summary(mod,rotate="promax")$rotF[1:90,1:3])

# sN non-flash
x <- sN_n
alpha_snn <- alpha(x)$total$std.alpha
xcor <- polychoric(x)$rho
fa.parallel(xcor,n.obs=nrow(x))    # Parallel analysis suggests that the number of factors =  13  and the number of components =  12 
nfactors(xcor,n.obs=nrow(x))       # 2, 3, 12, 20 factors

sNn_mod <- mirt(x,2)
mod <- sNn_mod
oblimin_loadings <- fa.sort(irt.fa(x,2)$fa)    
promax_loadings <- fa.sort(irt.fa(x,2,rotate="promax",plot = F)$fa)
obli_exp_sN_n <- data.frame(oblimin_loadings$loadings[1:90,1:2])
pro_exp_sN_n <-  data.frame(promax_loadings$loadings[1:90,1:2])
obli_sum_sN_n <- data.frame(summary(mod)$rotF)
pro_sum_sN_n <-  data.frame(summary(mod,rotate="promax")$rotF[1:90,1:2])
ifc_oesN_n <- oblimin_loadings$Phi                # 0.207
ifc_pesN_n <- promax_loadings$Phi                 # 0.298
ifc_ossN_n <- summary(mod)$fcor                   # 0.360
ifc_pssN_n <- summary(mod,rotate="promax")$fcor   # 0.467

mod <- mirt(x,6)
oblimin_loadings <- fa.sort(irt.fa(x,6)$fa)    
promax_loadings <- fa.sort(irt.fa(x,6,rotate="promax")$fa,plot = F) 
obli_exp_sN_n6 <- data.frame(oblimin_loadings$loadings[1:90,1:6])
pro_exp_sN_n6 <-  data.frame(promax_loadings$loadings[1:90,1:6])
obli_sum_sN_n6 <- data.frame(summary(mod)$rotF)
pro_sum_sN_n6 <-  data.frame(summary(mod,rotate="promax")$rotF[1:90,1:6])

# (c) CPT (SPCPTN90) flash vs non-flash comparison ----
names(obli_exp_sN_f) <- c("FlashF1","FlashF2")
names(obli_exp_sN_n) <- c("NFlashF1","NFlashF2")
obli_exp_sN <- left_join(rownames_to_column(obli_exp_sN_f),rownames_to_column(obli_exp_sN_n),by="rowname")
obli_exp_sNcorr1 <- cor(obli_exp_sN[,c(2,4)])   # 0.826
obli_exp_sNcorr2 <- cor(obli_exp_sN[,c(3,5)])   # 0.901

names(pro_exp_sN_f) <- c("FlashF1","FlashF2")
names(pro_exp_sN_n) <- c("NFlashF1","NFlashF2")
pro_exp_sN <- left_join(rownames_to_column(pro_exp_sN_f),rownames_to_column(pro_exp_sN_n),by="rowname")
pro_exp_sNcorr1 <- cor(pro_exp_sN[,c(2,4)])    # 0.841
pro_exp_sNcorr2 <- cor(pro_exp_sN[,c(3,5)])    # 0.902

names(obli_sum_sN_f) <- c("FlashF1","FlashF2")
names(obli_sum_sN_n) <- c("NFlashF1","NFlashF2")
obli_sum_sN <- left_join(rownames_to_column(obli_sum_sN_f),rownames_to_column(obli_sum_sN_n),by="rowname")
obli_sum_sNcorr1 <- cor(obli_sum_sN[,c(2,4)])   # 0.800
obli_sum_sNcorr2 <- cor(obli_sum_sN[,c(3,5)])   # 0.837

names(pro_sum_sN_f) <- c("FlashF1","FlashF2")
names(pro_sum_sN_n) <- c("NFlashF1","NFlashF2")
pro_sum_sN <- left_join(rownames_to_column(pro_sum_sN_f),rownames_to_column(pro_sum_sN_n),by="rowname")
pro_sum_sNcorr1 <- cor(pro_sum_sN[,c(2,4)])     # 0.815
pro_sum_sNcorr2 <- cor(pro_sum_sN[,c(3,5)])     # 0.841


# summary table ----
summary <- data.frame(matrix(NA,nrow=4,ncol = 5))
names(summary) <- c("Exp_oblimin","Exp_promax","MIRT_oblimin","MIRT_promax","Alpha")
rownames(summary) <- c("SPCPTNL_flash","SPCPTNL_nonflash","SPCPTN90_flash","SPCPTN90_nonflash")
summary[1,] <- c(ifc_oesNL_f[1,2],ifc_pesNL_f[1,2],ifc_ossNL_f[1,2],ifc_pssNL_f[1,2],alpha_snlf)
summary[2,] <- c(ifc_oesNL_n[1,2],ifc_pesNL_n[1,2],ifc_ossNL_n[1,2],ifc_pssNL_n[1,2],alpha_snln)
summary[3,] <- c(ifc_oesN_f[1,2],ifc_pesN_f[1,2],ifc_ossN_f[1,2],ifc_pssN_f[1,2],alpha_snf)
summary[4,] <- c(ifc_oesN_n[1,2],ifc_pesN_n[1,2],ifc_ossN_n[1,2],ifc_pssN_n[1,2],alpha_snn)
summary <- round(summary,3)
# summary %>% 
#   kbl(caption="Inter-factor correlations of Exploratory Item-Factor and MIRT analylses for SPCPTNL and SPCPTN90") %>% 
#   kable_classic(full_width = F, html_font = "Cambria") %>%
#   save_kable(file = "SPCPT_flashdif_interfactorcorr.html", self_contained = T)
write.csv(summary,"SPCPT_flashdif_interfactorcorr.csv")




# speed differences ----
pcptnl <- cpt_iw[,grepl("PCPTNL.CPT_Q", colnames(cpt_iw))]
pcptn360 <- cpt_iw[,grepl("PCPTN360.CPT_Q", colnames(cpt_iw))]
spcptnl <- cpt_iw[,grepl("SPCPTNL.SCPT_Q", colnames(cpt_iw))]
spcptn90 <- cpt_iw[,grepl("SPCPTN90.SCPT_Q", colnames(cpt_iw))]

pcptnl <- pcptnl[,grepl("TTR", colnames(pcptnl))]
pcptn360 <- pcptn360[,grepl("TTR", colnames(pcptn360))]
spcptnl <- spcptnl[,grepl("TTR", colnames(spcptnl))]
spcptn90 <- spcptn90[,grepl("TTR", colnames(spcptn90))]

pcptnl <- cbind(id, pcptnl)
pcptn360 <- cbind(id, pcptn360)
spcptnl <- cbind(id, spcptnl)
spcptn90 <- cbind(id, spcptn90)

# now keep the ones that don't have NA
noNA_NL <- pcptnl[rowSums(!is.na(pcptnl)) > 5,]        # pcpt[nl]     flash
noNA_N <- pcptn360[rowSums(!is.na(pcptn360)) > 5,]     # pcpt[n]360   flash
noNA_sNL <- spcptnl[rowSums(!is.na(spcptnl)) > 5,]     # spcpt[nl]    both
noNA_sN <- spcptn90[rowSums(!is.na(spcptn90)) > 5,]    # spcpt[n]90   both

colnames(noNA_sNL)[5:184] <- sprintf("item_%03d",1:180)
colnames(noNA_sN)[5:94] <- sprintf("item_%03d",1:90)


# matching ----
# -- making sure that there is a TTR for all CORR == 1, TTR is NA for CORR == 0
spcptnl <- cpt_iw[,grepl("SPCPTNL.SCPT_Q", colnames(cpt_iw))]
spcptn90 <- cpt_iw[,grepl("SPCPTN90.SCPT_Q", colnames(cpt_iw))]
sNL_resp <- spcptnl[,grepl("RESP", colnames(spcptnl))]
sN_resp <- spcptn90[,grepl("RESP", colnames(spcptn90))]
sNL_resp<- cbind(id, sNL_resp)
sN_resp  <- cbind(id, sN_resp)
sNL_resp <- sNL_resp[rowSums(!is.na(sNL_resp)) > 5,] 
sN_resp <- sN_resp[rowSums(!is.na(sN_resp)) > 5,] 

sNL_speed <- noNA_sNL
sN_speed <- noNA_sN
both <- intersect(unique(sNL_speed$test_sessions.datasetid),unique(sNL_resp$test_sessions.datasetid))
sNL_speed <- sNL_speed[sNL_speed$test_sessions.datasetid %in% both,]  
sNL_resp <- sNL_resp[sNL_resp$test_sessions.datasetid %in% both,]
both <- intersect(unique(sN_speed$test_sessions.datasetid),unique(sN_resp$test_sessions.datasetid))
sN_speed <- sN_speed[sN_speed$test_sessions.datasetid %in% both,]
sN_resp <- sN_resp[sN_resp$test_sessions.datasetid %in% both,]

sNL_speed <- sNL_speed[match(sNL_speed$test_sessions.datasetid,sNL_resp$test_sessions.datasetid),5:184]
sN_speed <- sN_speed[match(sN_speed$test_sessions.datasetid,sN_resp$test_sessions.datasetid),5:94]
sNL_resp <- sNL_resp[5:184]
sN_resp <- sN_resp[5:94]

sN_corr <- sN_speed
sN_corr[!is.na(sN_corr)] <- NA
for (i in 1:nrow(sN_speed)){
        spe <- sN_speed[i,]
        resp <- sN_resp[i,]
        sN_corr[i,] <- ifelse(resp==1, ifelse(!is.na(spe),1,0), ifelse(is.na(spe),1,NA))
}
sNL_corr <- sNL_speed
sNL_corr[!is.na(sNL_corr)] <- NA
for (i in 1:nrow(sNL_speed)){
        spe <- sNL_speed[i,]
        resp <- sNL_resp[i,]
        sNL_corr[i,] <- ifelse(resp==1, ifelse(!is.na(spe),1,0), ifelse(is.na(spe),1,NA))
}

sNL_corr$sum <- rowSums(sNL_corr,na.rm=T)
sN_corr$sum <- rowSums(sN_corr,na.rm=T)


# negative <- subset(noNA_sNL, (rowSums(sign(noNA_sNL[,5:184]) < 1,na.rm=T) > 0))
# toolong <- subset(noNA_sNL, rowSums(noNA_sNL[,5:184] > 1000, na.rm=T) > 0)
# 
# write.csv(negative,"SPCPTNL_negative.csv",row.names = F)
# write.csv(toolong,"SPCPTNL_toolong.csv",row.names = F)

noNA_sNL <- subset(noNA_sNL, (rowSums(sign(noNA_sNL[,5:184]) <= 0,na.rm=T) == 0))
noNA_sN <- subset(noNA_sN, (rowSums(sign(noNA_sN[,5:94]) <= 0,na.rm=T) == 0))

temp <- noNA_sNL[,5:184]
temp[temp >= 1200 | 200 > temp] <- NA
noNA_sNL[,5:184] <- temp
temp <- noNA_sN[,5:94]
temp[temp >= 1200 | 200 > temp] <- NA
noNA_sN[,5:94] <- temp

colsums_sNL <- colSums(!is.na(noNA_sNL[,5:184]),na.rm = T)
colsums_sN <- colSums(!is.na(noNA_sN[,5:94]),na.rm = T)

min(colsums_sNL)   # no items with less than 20 respondents
min(colsums_sN)    

noNA_sN <- cbind(noNA_sN[,1:4],noNA_sN[,c(names(colsums_sN[colsums_sN>20]))])  # get rid of items with less than 20 respondents


# only need sNL and sN since these tests were administered both flash and non-flash
sNL_fs <- noNA_sNL[noNA_sNL$flash ==1,grepl("item",colnames(noNA_sNL))]  # noNA_sNL flash group
sNL_ns <- noNA_sNL[noNA_sNL$flash ==0,grepl("item",colnames(noNA_sNL))]  # noNA_sNL non-flash group
sN_fs <- noNA_sN[noNA_sN$flash ==1,grepl("item",colnames(noNA_sN))]  # noNA_sN flash group
sN_ns <- noNA_sN[noNA_sN$flash ==0,grepl("item",colnames(noNA_sN))]  # noNA_sN non-flash group


sNL_fmeans <- data.frame(colMeans(sNL_fs,na.rm = T))
sNL_nmeans <- data.frame(colMeans(sNL_ns,na.rm = T))
sN_fmeans <- data.frame(colMeans(sN_fs,na.rm = T))
sN_nmeans <- data.frame(colMeans(sN_ns,na.rm = T))

sNL_fsd <- data.frame(apply(sNL_fs, 2, sd,na.rm=T))
sNL_nsd <- data.frame(apply(sNL_ns, 2, sd,na.rm=T))
sN_fsd <- data.frame(apply(sN_fs, 2, sd,na.rm=T))
sN_nsd <- data.frame(apply(sN_ns, 2, sd,na.rm=T))

sNL_fmeansd <- cbind(rownames_to_column(sNL_fmeans),sNL_fsd)
names(sNL_fmeansd) <- c("Item","MeanTTR","sd")
sNL_fmeansd$Flash <- 1
sNL_nmeansd <- cbind(rownames_to_column(sNL_nmeans),sNL_nsd)
names(sNL_nmeansd) <- c("Item","MeanTTR","sd")
sNL_nmeansd$Flash <- 0
sN_fmeansd <- cbind(rownames_to_column(sN_fmeans),sN_fsd)
names(sN_fmeansd) <- c("Item","MeanTTR","sd")
sN_fmeansd$Flash <- 1
sN_nmeansd <- cbind(rownames_to_column(sN_nmeans),sN_nsd)
names(sN_nmeansd) <- c("Item","MeanTTR","sd")
sN_nmeansd$Flash <- 0

sNL_meansd1 <- rbind(sNL_fmeansd[1:90,],sNL_nmeansd[1:90,])
sNL_meansd2 <- rbind(sNL_fmeansd[91:180,],sNL_nmeansd[91:180,])
sN_meansd <- rbind(sN_fmeansd,sN_nmeansd)





# plot of means with error bars
spe1_means <- ggplot(sNL_meansd1,aes(x=Item,y=MeanTTR,color=factor(Flash))) +
        geom_point(position=position_dodge(.9)) +
        geom_errorbar(aes(ymin=MeanTTR-sd, ymax=MeanTTR+sd), width=.2,position=position_dodge(.9)) +
        theme_bw() + ylab("Response Time (ms)") +
        scale_color_discrete(name = "Test Administration", labels = c("Non-Flash (n=1511)", "Flash (n=20475)")) +
        labs(title = paste("Flash differences in SPCPTNL Speed (first 90 items)")) +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggsave("myresults/meanTTR_peritem/SPCPTNL_1_means.png",height = 10,width = 30)



spe2_means <- ggplot(sNL_meansd2,aes(x=Item,y=MeanTTR,color=factor(Flash))) +
        geom_point(position=position_dodge(.9)) +
        geom_errorbar(aes(ymin=MeanTTR-sd, ymax=MeanTTR+sd), width=.2,position=position_dodge(.9)) +
        theme_bw() + ylab("Response Time (ms)") +
        scale_color_discrete(name = "Test Administration", labels = c("Non-Flash (n=1511)", "Flash (n=20475)")) +
        labs(title = paste("Flash differences in SPCPTNL Speed (last 90 items)")) +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggsave("myresults/meanTTR_peritem/SPCPTNL_2_means.png",height = 10,width = 30)



spe3_means <- ggplot(sN_meansd,aes(x=Item,y=MeanTTR,color=factor(Flash))) +
        geom_point(position=position_dodge(.9)) +
        geom_errorbar(aes(ymin=MeanTTR-sd, ymax=MeanTTR+sd), width=.2,position=position_dodge(.9)) +
        theme_bw() + ylab("Response Time (ms)") +
        scale_color_discrete(name = "Test Administration", labels = c("Non-Flash (n=133)", "Flash (n=145)")) +
        labs(title = paste("Flash differences in SPCPTN90 Speed")) +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggsave("myresults/meanTTR_peritem/SPCPTN90_means.png",height = 10,width = 30)





# log transform plots

noNA_sNL[,5:184] <- sapply(noNA_sNL[,5:184]+1,log)
noNA_sN[,5:64] <- sapply(noNA_sN[,5:64]+1,log)

sNL_fs <- noNA_sNL[noNA_sNL$flash ==1,grepl("item",colnames(noNA_sNL))]  # noNA_sNL flash group
sNL_ns <- noNA_sNL[noNA_sNL$flash ==0,grepl("item",colnames(noNA_sNL))]  # noNA_sNL non-flash group
sN_fs <- noNA_sN[noNA_sN$flash ==1,grepl("item",colnames(noNA_sN))]  # noNA_sN flash group
sN_ns <- noNA_sN[noNA_sN$flash ==0,grepl("item",colnames(noNA_sN))]  # noNA_sN non-flash group


sNL_fmeans <- data.frame(colMeans(sNL_fs,na.rm = T))
sNL_nmeans <- data.frame(colMeans(sNL_ns,na.rm = T))
sN_fmeans <- data.frame(colMeans(sN_fs,na.rm = T))
sN_nmeans <- data.frame(colMeans(sN_ns,na.rm = T))

sNL_fsd <- data.frame(apply(sNL_fs, 2, sd,na.rm=T))
sNL_nsd <- data.frame(apply(sNL_ns, 2, sd,na.rm=T))
sN_fsd <- data.frame(apply(sN_fs, 2, sd,na.rm=T))
sN_nsd <- data.frame(apply(sN_ns, 2, sd,na.rm=T))

sNL_fmeansd <- cbind(rownames_to_column(sNL_fmeans),sNL_fsd)
names(sNL_fmeansd) <- c("Item","MeanTTR","sd")
sNL_fmeansd$Flash <- 1
sNL_nmeansd <- cbind(rownames_to_column(sNL_nmeans),sNL_nsd)
names(sNL_nmeansd) <- c("Item","MeanTTR","sd")
sNL_nmeansd$Flash <- 0
sN_fmeansd <- cbind(rownames_to_column(sN_fmeans),sN_fsd)
names(sN_fmeansd) <- c("Item","MeanTTR","sd")
sN_fmeansd$Flash <- 1
sN_nmeansd <- cbind(rownames_to_column(sN_nmeans),sN_nsd)
names(sN_nmeansd) <- c("Item","MeanTTR","sd")
sN_nmeansd$Flash <- 0

sNL_meansd1 <- rbind(sNL_fmeansd[1:90,],sNL_nmeansd[1:90,])
sNL_meansd2 <- rbind(sNL_fmeansd[91:180,],sNL_nmeansd[91:180,])
sN_meansd <- rbind(sN_fmeansd,sN_nmeansd)


log_spe1_means <- ggplot(sNL_meansd1,aes(x=Item,y=MeanTTR,color=factor(Flash))) +
        geom_point(position=position_dodge(.9)) +
        geom_errorbar(aes(ymin=MeanTTR-sd, ymax=MeanTTR+sd), width=.2,position=position_dodge(.9)) +
        theme_bw() + ylab("Log transformed Response Time (ms)") +
        scale_color_discrete(name = "Test Administration", labels = c("Non-Flash (n=1511)", "Flash (n=20475)")) +
        labs(title = paste("Flash differences in SPCPTNL Speed (first 90, log transformed)")) +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggsave("myresults/meanTTR_peritem/SPCPTNL_1_logmeans.png",height = 10,width = 30)


log_spe2_means <- ggplot(sNL_meansd2,aes(x=Item,y=MeanTTR,color=factor(Flash))) +
        geom_point(position=position_dodge(.9)) +
        geom_errorbar(aes(ymin=MeanTTR-sd, ymax=MeanTTR+sd), width=.2,position=position_dodge(.9)) +
        theme_bw() + ylab("Log transformed Response Time (ms)") +
        scale_color_discrete(name = "Test Administration", labels = c("Non-Flash (n=1511)", "Flash (n=20475)")) +
        labs(title = paste("Flash differences in SPCPTNL Speed (last 90, log transformed)")) +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggsave("myresults/meanTTR_peritem/SPCPTNL_2_logmeans.png",height = 10,width = 30)


log_spe3_means <- ggplot(sN_meansd,aes(x=Item,y=MeanTTR,color=factor(Flash))) +
        geom_point(position=position_dodge(.9)) +
        geom_errorbar(aes(ymin=MeanTTR-sd, ymax=MeanTTR+sd), width=.2,position=position_dodge(.9)) +
        theme_bw() + ylab("Log transformed Response Time (ms)") +
        scale_color_discrete(name = "Test Administration", labels = c("Non-Flash (n=133)", "Flash (n=145)")) +
        labs(title = paste("Flash differences in SPCPTN90 Speed (Log transformed)")) +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggsave("myresults/meanTTR_peritem/SPCPTN90_logmeans.png",height = 10,width = 30)





















# extra, not using rn ----


# need to convert to long format first
sNL <- gather(noNA_sNL,"item", "ttr", item_1:item_180, factor_key=TRUE)
sN <- gather(noNA_sN, "item", "ttr", item_1:item_90, factor_key=TRUE)

# trying to change x-axis labels to be easier to read
sNLnames <- names(noNA_sNL[,5:184])
sNLnums <- regmatches(sNLnames, gregexpr("[[:digit:]]+", sNLnames))
for (i in 1:180) {
        sNLnames[i] <- paste("item",sNLnums[[i]][1],sNLnums[[i]][2],sep="_")
}
sNLnames <- data.frame(sNLnames)




# graphs
spe_box <- ggplot(sNL,aes(x=item,y=ttr,color=factor(flash))) +
        geom_boxplot() +
        theme_bw() + xlab("Item") + ylab("Response Time (ms)") +
        scale_color_discrete(name = "Test Administration", labels = c("Non-Flash (n=1511)", "Flash (n=20475)")) +
        labs(title = paste("Flash differences in SPCPTNL Speed"))

ggsave("SPCPTNL_box_flash1.png",height = 10,width = 30)

spe2_box <- ggplot(sN,aes(x=item,y=ttr,color=factor(flash))) +
        geom_boxplot() +
        theme_bw() + xlab("Item") +
        scale_color_discrete(name = "Test Administration", labels = c("Non-Flash (n=133)", "Flash (n=145)")) +
        labs(title = paste("Flash differences in SPCPTN90 Speed")) 

ggsave("SPCPTN90_box_flash.png",height = 10,width = 15)



spe_log_box1 <- ggplot(sNL,aes(x=item,y=log(ttr+1),color=factor(flash))) +
        geom_boxplot() +
        theme_bw() + xlab("Item") + ylab("Log Transform of Response Time (ms)") +
        scale_color_discrete(name = "Test Administration", labels = c("Non-Flash (n=1511)", "Flash (n=20475)")) +
        labs(title = paste("Flash differences in SPCPTNL Speed (log transformed)"))

ggsave("SPCPTNL_box_log.png",height = 10,width = 30)

no_out_log_box1 <- ggplot(sNL,aes(x=item,y=log(ttr+1),color=factor(flash))) +
        geom_boxplot(outlier.shape = NA) +
        theme_bw() + xlab("Item") + ylab("Log Transform of Response Time (ms)") +
        scale_color_discrete(name = "Test Administration", labels = c("Non-Flash (n=1511)", "Flash (n=20475)")) +
        labs(title = paste("Flash differences in SPCPTNL Speed (log transformed)"))

ggsave("SPCPTNL_box_log_nooutliers.png",height = 10,width = 30)


spe_log_box2 <- ggplot(sN,aes(x=item,y=log(ttr+1),color=factor(flash))) +
        geom_boxplot() +
        theme_bw() + xlab("Item") + ylab("Log Transform of Response Time (ms)") +
        scale_color_discrete(name = "Test Administration", labels = c("Non-Flash (n=1511)", "Flash (n=20475)")) +
        labs(title = paste("Flash differences in SPCPTN90 Speed (log transformed)"))

ggsave("SPCPTN90_box_log.png",height = 10,width = 15)

no_out_log_box2 <- ggplot(sN,aes(x=item,y=log(ttr+1),color=factor(flash))) +
        geom_boxplot(outlier.shape = NA) +
        theme_bw() + xlab("Item") + ylab("Log Transform of Response Time (ms)") +
        scale_color_discrete(name = "Test Administration", labels = c("Non-Flash (n=1511)", "Flash (n=20475)")) +
        labs(title = paste("Flash differences in SPCPTN90 Speed (log transformed)"))

ggsave("SPCPTN90_box_log_nooutliers.png",height = 10,width = 15)





