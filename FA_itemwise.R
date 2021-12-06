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

sNL_f$PC <- rowSums(sNL_f)/180   # percent correct
qu <- quantile(sNL_f$PC,0.05,na.rm=TRUE)
sNL_f <- sNL_f[sNL_f$PC > qu,1:180]

sNL_n$PC <- rowSums(sNL_n)/180
qu <- quantile(sNL_n$PC,0.05,na.rm=TRUE)
sNL_n <- sNL_n[sNL_n$PC > qu,1:180]

sN_f$PC <- rowSums(sN_f)/90   # percent correct
qu <- quantile(sN_f$PC,0.05,na.rm=TRUE)
sN_f <- sN_f[sN_f$PC > qu,1:90]

sN_n$PC <- rowSums(sN_n)/90
qu <- quantile(sN_n$PC,0.05,na.rm=TRUE)
sN_n <- sN_n[sN_n$PC > qu,1:90]

# sNL flash
x <- sNL_f
alpha_snlf <- alpha(x)$total$std.alpha
xcor <- polychoric(x)$rho
fa.parallel(xcor,n.obs=nrow(x))    # Parallel analysis suggests that the number of factors =  41  and the number of components =  18
nfactors(xcor,n.obs=nrow(x))       # 2, 17, 20 factors

sNLf_mod <- mirt(x,2)
mod <- sNLf_mod
oblimin_loadings <- fa.sort(irt.fa(x,2)$fa)    
promax_loadings <- fa.sort(irt.fa(x,2,rotate="promax",plot = F)$fa) 
obli_exp_sNL_f <- data.frame(oblimin_loadings$loadings[1:180,1:2])
pro_exp_sNL_f <-  data.frame(promax_loadings$loadings[1:180,1:2])
obli_sum_sNL_f <- data.frame(summary(mod)$rotF)
pro_sum_sNL_f <-  data.frame(summary(mod,rotate="promax")$rotF[1:180,1:2])
ifc_oesNL_f <- oblimin_loadings$Phi                # 0.265  # obli_exp_sNL_f [i]nter[f]actor[c]orrelation
ifc_pesNL_f <- promax_loadings$Phi                 # 0.273 used to be 0.300
ifc_ossNL_f <- summary(mod)$fcor                   # 0.441
ifc_pssNL_f <- summary(mod,rotate="promax")$fcor   # 0.435

# sNL non-flash
x <- sNL_n
alpha_snln <- alpha(x,check.keys=TRUE)$total$std.alpha
xcor <- polychoric(x)$rho
fa.parallel(xcor,n.obs=nrow(x))    # Parallel analysis suggests that the number of factors =  50  and the number of components =  39 
nfactors(xcor,n.obs=nrow(x))       # 2, 3, 5, 20 factors

sNLn_mod <- mirt(x,2)
mod <- sNLn_mod
oblimin_loadings <- fa.sort(irt.fa(x,2)$fa)    
promax_loadings <- fa.sort(irt.fa(x,2,rotate="promax",plot = F)$fa) 
obli_exp_sNL_n <- data.frame(oblimin_loadings$loadings[1:180,1:2])
pro_exp_sNL_n <-  data.frame(promax_loadings$loadings[1:180,1:2])
obli_sum_sNL_n <- data.frame(summary(mod)$rotF)
pro_sum_sNL_n <-  data.frame(summary(mod,rotate="promax")$rotF[1:180,1:2])
ifc_oesNL_n <- oblimin_loadings$Phi                # 0.190
ifc_pesNL_n <- promax_loadings$Phi                 # 0.288
ifc_ossNL_n <- summary(mod)$fcor                   # 0.324
ifc_pssNL_n <- summary(mod,rotate="promax")$fcor   # 0.470

mod <- mirt(x,6)
oblimin_loadings <- fa.sort(irt.fa(x,6)$fa)    
promax_loadings <- fa.sort(irt.fa(x,6,rotate="promax")$fa)  
obli_exp_sNL_f6 <- data.frame(oblimin_loadings$loadings[1:180,1:6])
pro_exp_sNL_f6 <-  data.frame(promax_loadings$loadings[1:180,1:6])
obli_sum_sNL_f6 <- data.frame(summary(mod)$rotF)
pro_sum_sNL_f6 <-  data.frame(summary(mod,rotate="promax")$rotF[1:180,1:6])

# (a) CPT (SPCPTNL) flash vs non-flash comparison ----
names(obli_exp_sNL_f) <- c("FlashF1","FlashF2")
names(obli_exp_sNL_n) <- c("NFlashF1","NFlashF2")
obli_exp_sNL <- left_join(rownames_to_column(obli_exp_sNL_f),rownames_to_column(obli_exp_sNL_n),by="rowname")
obli_exp_sNLcorr1 <- cor(obli_exp_sNL[,c(2,4)])   # 0.861, was 0.949 before removing outliers
obli_exp_sNLcorr2 <- cor(obli_exp_sNL[,c(3,5)])   # 0.827

names(pro_exp_sNL_f) <- c("FlashF1","FlashF2")
names(pro_exp_sNL_n) <- c("NFlashF1","NFlashF2")
pro_exp_sNL <- left_join(rownames_to_column(pro_exp_sNL_f),rownames_to_column(pro_exp_sNL_n),by="rowname")
pro_exp_sNLcorr1 <- cor(pro_exp_sNL[,c(2,4)])    # 0.865 used to be 1.000 before adding plot=F in irt.fa()
pro_exp_sNLcorr2 <- cor(pro_exp_sNL[,c(3,5)])    # 0.850 used to be 1.000 before adding plot=F in irt.fa()

names(obli_sum_sNL_f) <- c("FlashF1","FlashF2")
names(obli_sum_sNL_n) <- c("NFlashF1","NFlashF2")
obli_sum_sNL <- left_join(rownames_to_column(obli_sum_sNL_f),rownames_to_column(obli_sum_sNL_n),by="rowname")
obli_sum_sNLcorr1 <- cor(obli_sum_sNL[,c(2,4)])   # 0.850, was 0.939 before removing outliers
obli_sum_sNLcorr2 <- cor(obli_sum_sNL[,c(3,5)])   # 0.920

names(pro_sum_sNL_f) <- c("FlashF1","FlashF2")
names(pro_sum_sNL_n) <- c("NFlashF1","NFlashF2")
pro_sum_sNL <- left_join(rownames_to_column(pro_sum_sNL_f),rownames_to_column(pro_sum_sNL_n),by="rowname")
pro_sum_sNLcorr1 <- cor(pro_sum_sNL[,c(2,4)])     # 0.875, was 0.943 before removing outliers
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

# (b) CPT (SPCPTN90) flash vs non-flash comparison ----
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


# summary table
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




# speed differences
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

noNA_sNL <- subset(noNA_sNL,  (rowSums(sign(noNA_sNL[,5:184]) < 0,na.rm=T) == 0))
noNA_sN <- subset(noNA_sN,  (rowSums(sign(noNA_sN[,5:94]) < 0,na.rm=T) == 0))

# only need sNL and sN since these tests were administered both flash and non-flash
sNL_fs <- noNA_sNL[noNA_sNL$flash ==1,grepl("TTR",colnames(noNA_sNL))]  # noNA_sNL flash group
sNL_ns <- noNA_sNL[noNA_sNL$flash ==0,grepl("TTR",colnames(noNA_sNL))]  # noNA_sNL non-flash group
sN_fs <- noNA_sN[noNA_sN$flash ==1,grepl("TTR",colnames(noNA_sN))]  # noNA_sN flash group
sN_ns <- noNA_sN[noNA_sN$flash ==0,grepl("TTR",colnames(noNA_sN))]  # noNA_sN non-flash group

# not sure how to get rid of outliers for this


# need to convert to long format first
sNL <- gather(noNA_sNL,"item", "ttr", SPCPTNL.SCPT_QID000001_0_TTR:SPCPTNL.SCPT_QID000036_4_TTR, factor_key=TRUE)
sN <- gather(noNA_sN, "item", "ttr", SPCPTN90.SCPT_QID000001_0_TTR:SPCPTN90.SCPT_QID000018_4_TTR, factor_key=TRUE)

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
        theme_bw() + xlab("Item") +
        scale_color_discrete(name = "Test Administration", labels = c("Non-Flash", "Flash")) +
        # scale_x_discrete(name="Item",breaks=colnames(noNA_sNL[,5:184]),limits=factor(sNLnames)) +
        labs(title = paste("Flash differences in SPCPTNL Speed")) +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggsave("SPCPTNL_box_flash.png")


spe2_box <- ggplot(sN,aes(x=item,y=ttr,color=factor(flash))) +
        geom_boxplot() +
        theme_bw() + xlab("Item") +
        scale_color_discrete(name = "Test Administration", labels = c("Non-Flash", "Flash")) +
        # scale_x_discrete(name="Item",breaks=colnames(noNA_sNL[,5:184]),limits=factor(sNLnames)) +
        labs(title = paste("Flash differences in SPCPTN90 Speed")) +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))











