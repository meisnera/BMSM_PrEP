#### Combining results for BMSM analysis

rm(list=ls())

library(ggplot2)
library(ggpubr)

rslt1 <- read.csv("/Users/ameisner/Dropbox/FHCRC/BlackMSM/forcluster/fromcluster/BMSManalysis_pt1.csv")
rslt2 <- read.csv("/Users/ameisner/Dropbox/FHCRC/BlackMSM/forcluster/fromcluster/BMSManalysis_pt2.csv")
rslt3 <- read.csv("/Users/ameisner/Dropbox/FHCRC/BlackMSM/forcluster/fromcluster/BMSManalysis_pt3.csv")
rslt4 <- read.csv("/Users/ameisner/Dropbox/FHCRC/BlackMSM/forcluster/fromcluster/BMSManalysis_pt4.csv")
rslt5 <- read.csv("/Users/ameisner/Dropbox/FHCRC/BlackMSM/forcluster/fromcluster/BMSManalysis_pt5.csv")
rslt6 <- read.csv("/Users/ameisner/Dropbox/FHCRC/BlackMSM/forcluster/fromcluster/BMSManalysis_pt6.csv")
rslt7 <- read.csv("/Users/ameisner/Dropbox/FHCRC/BlackMSM/forcluster/fromcluster/BMSManalysis_pt7.csv")
rslt8 <- read.csv("/Users/ameisner/Dropbox/FHCRC/BlackMSM/forcluster/fromcluster/BMSManalysis_pt8.csv")
rslt9 <- read.csv("/Users/ameisner/Dropbox/FHCRC/BlackMSM/forcluster/fromcluster/BMSManalysis_pt9.csv") 
rslt10 <- read.csv("/Users/ameisner/Dropbox/FHCRC/BlackMSM/forcluster/fromcluster/BMSManalysis_pt10.csv")

allresults <- data.frame(rbind(rslt1, rslt2, rslt3, rslt4, rslt5, rslt6, rslt7, rslt8, rslt9, rslt10))
allresults <- allresults[,-1]

allnames <- c("mean_Awt","sd_Awt","min_Awt","max_Awt","estAwt","Abaltab_unadj1","Abaltab_unadj2","Abaltab_unadj3",
              "Abaltab_unadj4","Abaltab_unadj5","Abaltab_unadj6","Abaltab_unadj7","Abaltab_unadj8","Abaltab_adj1",
              "Abaltab_adj2","Abaltab_adj3","Abaltab_adj4","Abaltab_adj5","Abaltab_adj6","Abaltab_adj7",                      
              "Abaltab_adj8","Asens_rslt1","Asens_rslt2","Asens_rslt3","Asens_rslt4","Asens_rslt5","Asens_rslt6",
              "Asens_rslt7","Asens_rslt8","Asens_rslt9","estAwt_LL","mean_Dwt","sd_Dwt","min_Dwt","max_Dwt",
              "mean_Dwt_t1","sd_Dwt_t1","min_Dwt_t1","max_Dwt_t1","mean_Dwt_t2","sd_Dwt_t2","min_Dwt_t2",
              "max_Dwt_t2","estDwt","hivD","Danalysis_AbalX1_unadj1","Danalysis_AbalX1_unadj2",
              "Danalysis_AbalX1_unadj3","Danalysis_AbalX1_unadj4","Danalysis_AbalX1_unadj5","Danalysis_AbalX1_unadj6",
              "Danalysis_AbalX1_unadj7","Danalysis_AbalX1_unadj8","Danalysis_AbalX1_adj1","Danalysis_AbalX1_adj2",
              "Danalysis_AbalX1_adj3","Danalysis_AbalX1_adj4","Danalysis_AbalX1_adj5","Danalysis_AbalX1_adj6",
              "Danalysis_AbalX1_adj7","Danalysis_AbalX1_adj8","Danalysis_AbalX1y1_unadj1","Danalysis_AbalX1y1_unadj2",
              "Danalysis_AbalX1y1_unadj3","Danalysis_AbalX1y1_unadj4","Danalysis_AbalX1y1_unadj5",
              "Danalysis_AbalX1y1_unadj6","Danalysis_AbalX1y1_unadj7","Danalysis_AbalX1y1_unadj8",
              "Danalysis_AbalX1y1_adj1","Danalysis_AbalX1y1_adj2","Danalysis_AbalX1y1_adj3","Danalysis_AbalX1y1_adj4",
              "Danalysis_AbalX1y1_adj5","Danalysis_AbalX1y1_adj6","Danalysis_AbalX1y1_adj7","Danalysis_AbalX1y1_adj8",
              "Danalysis_Dbal1_unadj1","Danalysis_Dbal1_unadj2","Danalysis_Dbal1_unadj3","Danalysis_Dbal1_unadj4",
              "Danalysis_Dbal1_unadj5","Danalysis_Dbal1_unadj6","Danalysis_Dbal1_unadj7","Danalysis_Dbal1_unadj8",
              "Danalysis_Dbal1_adj1","Danalysis_Dbal1_adj2","Danalysis_Dbal1_adj3","Danalysis_Dbal1_adj4",
              "Danalysis_Dbal1_adj5","Danalysis_Dbal1_adj6","Danalysis_Dbal1_adj7","Danalysis_Dbal1_adj8",
              "Danalysis_Dbal2_unadj1","Danalysis_Dbal2_unadj2","Danalysis_Dbal2_unadj3","Danalysis_Dbal2_unadj4",
              "Danalysis_Dbal2_unadj5","Danalysis_Dbal2_unadj6","Danalysis_Dbal2_unadj7","Danalysis_Dbal2_unadj8",
              "Danalysis_Dbal2_adj1","Danalysis_Dbal2_adj2","Danalysis_Dbal2_adj3","Danalysis_Dbal2_adj4",
              "Danalysis_Dbal2_adj5","Danalysis_Dbal2_adj6","Danalysis_Dbal2_adj7","Danalysis_Dbal2_adj8",
              "Dsens_rslt1","Dsens_rslt2","Dsens_rslt3","Dsens_rslt4","Dsens_rslt5","Dsens_rslt6","Dsens_rslt7",
              "Dsens_rslt8","Dsens_rslt9","Dsens_rslt10","Dsens_rslt11","estDwt_LL","extAprob","extAprob1",
              "extAprob1y1","extDprob1d0","extDprob1d1","extDprob1d2","extDprob2d0","extDprob2d1","extDprob2d2",
              "minAprob","maxAprob","minAprob1","maxAprob1","minAprob1y1","maxAprob1y1","minDprob1d0","maxDprob1d0",
              "minDprob1d1","maxDprob1d1","minDprob1d2","maxDprob1d2","minDprob2d0","maxDprob2d0","minDprob2d1",
              "maxDprob2d1","minDprob2d2","maxDprob2d2","maximbalAunadj","maximbalAadj","maxDanalysis_AbalX1_unadj",
              "maxDanalysis_AbalX1_adj","maxDanalysis_AbalX1y1_unadj","maxDanalysis_AbalX1y1_adj",
              "maxDanalysis_Dbal1_unadj","maxDanalysis_Dbal1_adj","maxDanalysis_Dbal2_unadj","maxDanalysis_Dbal2_adj",
              "maximbalA_unadj1","maximbalA_unadj2","maximbalA_unadj3","maximbalA_unadj4","maximbalA_unadj5",
              "maximbalA_unadj6","maximbalA_unadj7","maximbalA_unadj8","maximbalA_adj1","maximbalA_adj2",
              "maximbalA_adj3","maximbalA_adj4","maximbalA_adj5","maximbalA_adj6","maximbalA_adj7","maximbalA_adj8",
              "maxDanalysis_AbalX1_unadj_indiv1","maxDanalysis_AbalX1_unadj_indiv2","maxDanalysis_AbalX1_unadj_indiv3",
              "maxDanalysis_AbalX1_unadj_indiv4","maxDanalysis_AbalX1_unadj_indiv5","maxDanalysis_AbalX1_unadj_indiv6",
              "maxDanalysis_AbalX1_unadj_indiv7","maxDanalysis_AbalX1_unadj_indiv8","maxDanalysis_AbalX1_adj_indiv1",
              "maxDanalysis_AbalX1_adj_indiv2","maxDanalysis_AbalX1_adj_indiv3","maxDanalysis_AbalX1_adj_indiv4",
              "maxDanalysis_AbalX1_adj_indiv5","maxDanalysis_AbalX1_adj_indiv6","maxDanalysis_AbalX1_adj_indiv7",
              "maxDanalysis_AbalX1_adj_indiv8","maxDanalysis_AbalX1y1_unadj_indiv1","maxDanalysis_AbalX1y1_unadj_indiv2",
              "maxDanalysis_AbalX1y1_unadj_indiv3","maxDanalysis_AbalX1y1_unadj_indiv4","maxDanalysis_AbalX1y1_unadj_indiv5",
              "maxDanalysis_AbalX1y1_unadj_indiv6","maxDanalysis_AbalX1y1_unadj_indiv7","maxDanalysis_AbalX1y1_unadj_indiv8",
              "maxDanalysis_AbalX1y1_adj_indiv1","maxDanalysis_AbalX1y1_adj_indiv2","maxDanalysis_AbalX1y1_adj_indiv3",
              "maxDanalysis_AbalX1y1_adj_indiv4","maxDanalysis_AbalX1y1_adj_indiv5","maxDanalysis_AbalX1y1_adj_indiv6",
              "maxDanalysis_AbalX1y1_adj_indiv7","maxDanalysis_AbalX1y1_adj_indiv8","maxDanalysis_Dbal1_unadj_indiv1",
              "maxDanalysis_Dbal1_unadj_indiv2","maxDanalysis_Dbal1_unadj_indiv3","maxDanalysis_Dbal1_unadj_indiv4",
              "maxDanalysis_Dbal1_unadj_indiv5","maxDanalysis_Dbal1_unadj_indiv6","maxDanalysis_Dbal1_unadj_indiv7",
              "maxDanalysis_Dbal1_unadj_indiv8","maxDanalysis_Dbal1_adj_indiv1","maxDanalysis_Dbal1_adj_indiv2",
              "maxDanalysis_Dbal1_adj_indiv3","maxDanalysis_Dbal1_adj_indiv4","maxDanalysis_Dbal1_adj_indiv5",
              "maxDanalysis_Dbal1_adj_indiv6","maxDanalysis_Dbal1_adj_indiv7","maxDanalysis_Dbal1_adj_indiv8",
              "maxDanalysis_Dbal2_unadj_indiv1","maxDanalysis_Dbal2_unadj_indiv2","maxDanalysis_Dbal2_unadj_indiv3",
              "maxDanalysis_Dbal2_unadj_indiv4","maxDanalysis_Dbal2_unadj_indiv5","maxDanalysis_Dbal2_unadj_indiv6",
              "maxDanalysis_Dbal2_unadj_indiv7","maxDanalysis_Dbal2_unadj_indiv8","maxDanalysis_Dbal2_adj_indiv1",
              "maxDanalysis_Dbal2_adj_indiv2","maxDanalysis_Dbal2_adj_indiv3","maxDanalysis_Dbal2_adj_indiv4",
              "maxDanalysis_Dbal2_adj_indiv5","maxDanalysis_Dbal2_adj_indiv6","maxDanalysis_Dbal2_adj_indiv7",
              "maxDanalysis_Dbal2_adj_indiv8","maxAwt","maxDwt","minAprob_allimp","maxAprob_allimp","minAprob1_allimp",
              "maxAprob1_allimp","minAprob1y1_allimp","maxAprob1y1_allimp","minDprob1d0_allimp","maxDprob1d0_allimp",
              "minDprob1d1_allimp","maxDprob1d1_allimp","minDprob1d2_allimp","maxDprob1d2_allimp","minDprob2d0_allimp",
              "maxDprob2d0_allimp","minDprob2d1_allimp","maxDprob2d1_allimp","minDprob2d2_allimp","maxDprob2d2_allimp",
              "max_extAprob","max_extAprob1","max_extAprob1y1","max_extDprob1d0","max_extDprob1d1","max_extDprob1d2",
              "max_extDprob2d0","max_extDprob2d1","max_extDprob2d2")  

colnames(allresults) <- allnames


table(allresults$hivD, useNA="always") ## looks like we have to rm 4 samples
allresults <- allresults[which(allresults$hivD != 0),]

exp(-6.905494e-01) 
exp(quantile(allresults[,"estAwt"],c(0.025,0.975), type=8, na.rm=T))
exp(-7.626911e-01) 
exp(quantile(allresults[,"estDwt"],c(0.025,0.975), type=8, na.rm=T))
exp(quantile(allresults[which(allresults$maxDanalysis_Dbal2_adj <= 0.1 & allresults$maxDanalysis_Dbal1_adj <= 0.1),"estDwt"],c(0.025,0.975), type=8, na.rm=T))
exp(2*-7.626911e-01) 
exp(2*quantile(allresults[,"estDwt"],c(0.025,0.975), type=8, na.rm=T))
exp(2*quantile(allresults[which(allresults$maxDanalysis_Dbal2_adj <= 0.1 & allresults$maxDanalysis_Dbal1_adj <= 0.1),"estDwt"],c(0.025,0.975), type=8, na.rm=T))

## log-linear results
exp(-6.535770e-01) 
exp(quantile(allresults[,"estAwt_LL"],c(0.025,0.975), type=8, na.rm=T))
exp(-7.407256e-01) 
exp(quantile(allresults[,"estDwt_LL"],c(0.025,0.975), type=8, na.rm=T))
exp(quantile(allresults[which(allresults$maxDanalysis_Dbal2_adj <= 0.1 & allresults$maxDanalysis_Dbal1_adj <= 0.1),"estDwt_LL"],c(0.025,0.975), type=8, na.rm=T))
exp(2*-7.407256e-01) 
exp(2*quantile(allresults[,"estDwt_LL"],c(0.025,0.975), type=8, na.rm=T))
exp(2*quantile(allresults[which(allresults$maxDanalysis_Dbal2_adj <= 0.1 & allresults$maxDanalysis_Dbal1_adj <= 0.1),"estDwt_LL"],c(0.025,0.975), type=8, na.rm=T))


####################################
####################################
####################################

###### Sensitivity analysis  
Asensrslt <- c(1.595093e-01, -9.864607e-02, -2.548982e-01, -3.884129e-01, -6.535770e-01, -9.210580e-01, 
               -1.057500e+00, -1.218543e+00, -1.487723e+00)
                   
Asensdf <- data.frame("RRval"=c(0.5, 0.63, 0.71, 0.8, 1, 1.25, 1.4, 1.6, 2), "result"=exp(Asensrslt))
Asensdf$lower <- c(exp(quantile(allresults[,"Asens_rslt1"],c(0.025), type=8, na.rm=T)),
  exp(quantile(allresults[,"Asens_rslt2"],c(0.025), type=8, na.rm=T)),
  exp(quantile(allresults[,"Asens_rslt3"],c(0.025), type=8, na.rm=T)),
  exp(quantile(allresults[,"Asens_rslt4"],c(0.025), type=8, na.rm=T)),
  exp(quantile(allresults[,"Asens_rslt5"],c(0.025), type=8, na.rm=T)),
  exp(quantile(allresults[,"Asens_rslt6"],c(0.025), type=8, na.rm=T)),
  exp(quantile(allresults[,"Asens_rslt7"],c(0.025), type=8, na.rm=T)),
  exp(quantile(allresults[,"Asens_rslt8"],c(0.025), type=8, na.rm=T)),
  exp(quantile(allresults[,"Asens_rslt9"],c(0.025), type=8, na.rm=T)))
Asensdf$upper <- c(exp(quantile(allresults[,"Asens_rslt1"],c(0.975), type=8, na.rm=T)),
                   exp(quantile(allresults[,"Asens_rslt2"],c(0.975), type=8, na.rm=T)),
                   exp(quantile(allresults[,"Asens_rslt3"],c(0.975), type=8, na.rm=T)),
                   exp(quantile(allresults[,"Asens_rslt4"],c(0.975), type=8, na.rm=T)),
                   exp(quantile(allresults[,"Asens_rslt5"],c(0.975), type=8, na.rm=T)),
                   exp(quantile(allresults[,"Asens_rslt6"],c(0.975), type=8, na.rm=T)),
                   exp(quantile(allresults[,"Asens_rslt7"],c(0.975), type=8, na.rm=T)),
                   exp(quantile(allresults[,"Asens_rslt8"],c(0.975), type=8, na.rm=T)),
                   exp(quantile(allresults[,"Asens_rslt9"],c(0.975), type=8, na.rm=T)))



Dsensrslt <- c(3.725770e-01, 1.178196e-01, -8.509084e-02, -2.924411e-01, -4.184391e-01, -5.262937e-01, -7.407256e-01, 
               -9.569530e-01, -1.067129e+00, -1.197057e+00, -1.414091e+00)  

Dsensdf <- data.frame("RRval"=c(RR_Dsens <- c(0.3, 0.4, 0.5, 0.63, 0.71, 0.8, 1, 1.25, 1.4, 1.6, 2)), "result"=exp(Dsensrslt))
Dsensdf$lower <- c(exp(quantile(allresults[,"Dsens_rslt1"],c(0.025), type=8, na.rm=T)),
                   exp(quantile(allresults[,"Dsens_rslt2"],c(0.025), type=8, na.rm=T)),
                   exp(quantile(allresults[,"Dsens_rslt3"],c(0.025), type=8, na.rm=T)),
                   exp(quantile(allresults[,"Dsens_rslt4"],c(0.025), type=8, na.rm=T)),
                   exp(quantile(allresults[,"Dsens_rslt5"],c(0.025), type=8, na.rm=T)),
                   exp(quantile(allresults[,"Dsens_rslt6"],c(0.025), type=8, na.rm=T)),
                   exp(quantile(allresults[,"Dsens_rslt7"],c(0.025), type=8, na.rm=T)),
                   exp(quantile(allresults[,"Dsens_rslt8"],c(0.025), type=8, na.rm=T)),
                   exp(quantile(allresults[,"Dsens_rslt9"],c(0.025), type=8, na.rm=T)),
                   exp(quantile(allresults[,"Dsens_rslt10"],c(0.025), type=8, na.rm=T)),
                   exp(quantile(allresults[,"Dsens_rslt11"],c(0.025), type=8, na.rm=T)))
Dsensdf$upper <- c(exp(quantile(allresults[,"Dsens_rslt1"],c(0.975), type=8, na.rm=T)),
                   exp(quantile(allresults[,"Dsens_rslt2"],c(0.975), type=8, na.rm=T)),
                   exp(quantile(allresults[,"Dsens_rslt3"],c(0.975), type=8, na.rm=T)),
                   exp(quantile(allresults[,"Dsens_rslt4"],c(0.975), type=8, na.rm=T)),
                   exp(quantile(allresults[,"Dsens_rslt5"],c(0.975), type=8, na.rm=T)),
                   exp(quantile(allresults[,"Dsens_rslt6"],c(0.975), type=8, na.rm=T)),
                   exp(quantile(allresults[,"Dsens_rslt7"],c(0.975), type=8, na.rm=T)),
                   exp(quantile(allresults[,"Dsens_rslt8"],c(0.975), type=8, na.rm=T)),
                   exp(quantile(allresults[,"Dsens_rslt9"],c(0.975), type=8, na.rm=T)),
                   exp(quantile(allresults[,"Dsens_rslt10"],c(0.975), type=8, na.rm=T)),
                   exp(quantile(allresults[,"Dsens_rslt11"],c(0.975), type=8, na.rm=T)))
  
Dsensdf2 <- data.frame("RRval"=Dsensdf$RRval,exp(2*log(Dsensdf[,2:4])))


Asensplot_band <- ggplot(Asensdf, aes(x=RRval, y=result, color="#7CAE00", fill="#7CAE00")) + 
  scale_x_continuous(trans='log', labels=as.character(Asensdf$RRval), breaks=Asensdf$RRval) + 
  geom_hline(yintercept = 1, linetype = 1, size = 0.35, color="gray65") +
  geom_vline(xintercept = 1, linetype = 1, size = 0.35, color="gray65") +
  geom_ribbon(aes(ymin=lower, ymax=upper, colour=NULL), alpha=0.2) +
  geom_line(size=1.25) +
  xlab(bquote(alpha ~ " (bias factor)")) +
  ylab("Relative risk for exposure") +
  ggtitle("Unmeasured Confounding: A") + 
  theme_bw() + theme(axis.text=element_text(size=11),axis.title=element_text(size=13),plot.title = element_text(size=14),legend.position = "none") + 
  scale_fill_manual(values=c("#7CAE00")) + 
  scale_color_manual(values=c("#7CAE00"))

Dsens_all <- data.frame(rbind(Dsensdf, Dsensdf2))
Dsens_all$Comparison <- c(rep("D = 1 vs. 0",11),rep("D = 2 vs. 0",11))
Dsens_all <- Dsens_all[which(Dsens_all$RRval >= 0.5),]

Dsensplot_band <- ggplot(Dsens_all, aes(x=RRval, y=result, group=Comparison, fill=Comparison, colour=Comparison)) + 
  scale_x_continuous(trans='log', labels=as.character(Dsens_all$RRval), breaks=Dsens_all$RRval) + 
  geom_hline(yintercept = 1, linetype = 1, size = 0.35, color="gray65") +
  geom_vline(xintercept = 1, linetype = 1, size = 0.35, color="gray65") +
  geom_ribbon(aes(ymin=lower, ymax=upper, colour=NULL), alpha=0.2) +
  geom_line(size=1.25) + 
  xlab(bquote(alpha(0,1) ~ " (bias factor)")) +
  ylab("Relative risk for exposure") +
  ggtitle("Unmeasured Confounding: D") + 
  theme_bw() + theme(axis.text=element_text(size=11),axis.title=element_text(size=13),plot.title = element_text(size=14),
                     legend.title=element_text(size=13), legend.text=element_text(size=11), legend.position = c(0.85, 0.85)) + 
  scale_fill_manual(values=c("#00BFC4", "#C77CFF")) + 
  scale_color_manual(values=c("#00BFC4", "#C77CFF"))


ggsave("/Users/ameisner/Dropbox/FHCRC/BlackMSM/forcluster/fromcluster/sensanalysis_band.png", 
       plot=ggarrange(Asensplot_band, Dsensplot_band, ncol=2, nrow=1), width = 12.5,height = 5, units="in",bg="white")



####################################
####################################
####################################

#### positivity

## In the non-BS imputed sample, maxAwt = 3.837, maxDwt = 4.715
## Across BS:
summary(allresults$maxAwt)
summary(allresults$maxDwt)
quantile(allresults$maxDwt, 0.95)

## In the non-BS imputed sample, pos violations (i.e., extreme values) were observed in:
## up to 3.2% of participants in each imputed dataset for Pr(D2 = 0) --> few pos violations in each imputed dataset
##
## Also, min Pr(D2 = 0) = 0.0003 across all imps, so pos violations weren't super severe
##
## Across BS: 

summary(allresults$minAprob_allimp) ## not an indication of a pos violation
summary(allresults$maxAprob_allimp)
summary(allresults$minAprob1_allimp) ## not an indication of a pos violation
summary(allresults$maxAprob1_allimp)
summary(allresults$minAprob1y1_allimp) ## not an indication of a pos violation
summary(allresults$maxAprob1y1_allimp)

summary(allresults$minDprob1d0_allimp)
min(allresults$minDprob1d0_allimp)
quantile(allresults$minDprob1d0_allimp, 0.063)
summary(allresults$maxDprob1d0_allimp)
summary(allresults$minDprob1d1_allimp)
min(allresults$minDprob1d1_allimp)
quantile(allresults$minDprob1d1_allimp, 0.014)
summary(allresults$maxDprob1d1_allimp)
summary(allresults$minDprob1d2_allimp)
min(allresults$minDprob1d2_allimp)
quantile(allresults$minDprob1d2_allimp, 0.0007)
summary(allresults$maxDprob1d2_allimp)

summary(allresults$minDprob2d0_allimp)
quantile(allresults$minDprob2d0_allimp, 0.95) ## we know this is an issue from the non-BS data above
summary(allresults$maxDprob2d0_allimp)
quantile(allresults$maxDprob2d0_allimp, 0.96) ## 96% of bootstrap samples have a max prob (across all imputations) of < 0.999 
summary(allresults$minDprob2d1_allimp)
quantile(allresults$minDprob2d1_allimp, 0.055) 
summary(allresults$maxDprob2d1_allimp)
summary(allresults$minDprob2d2_allimp)
quantile(allresults$minDprob2d2_allimp, 0.42) 
summary(allresults$maxDprob2d2_allimp)
quantile(allresults$maxDprob2d2_allimp, 0.998)

# summary(allresults$max_extAprob)
# summary(allresults$max_extAprob1)
# summary(allresults$max_extAprob1y1)
# summary(allresults$max_extDprob1d0) ## up to 18.1% of participants in any imputed dataset across all bootstrap samples had a pos violation for Pr(D1 = 0)
#   ## i.e., in one bootstrap sample, in at least one imputed dataset, 18.1% of ppts had a pos violation
# quantile(allresults$max_extDprob1d0,c(0.946, 0.982)) ## in 94.6% of boostrap samples, in each imputed dataset, there were pos violations in at most 1% of ppts 
# summary(allresults$max_extDprob1d1)
# quantile(allresults$max_extDprob1d1,c(0.993)) ## max = 9.3%
# summary(allresults$max_extDprob1d2)
# summary(allresults$max_extDprob2d0)
# quantile(allresults$max_extDprob2d0,c(0.077, 0.37)) ## again, we knew this was a problem from above (this gives 10% of ppts) - give 1% and 10% #s
# summary(allresults$max_extDprob2d1)
# quantile(allresults$max_extDprob2d1,c(0.976)) ## max = 9.2%
# summary(allresults$max_extDprob2d2)
# quantile(allresults$max_extDprob2d2,c(0.67, 0.991))

summary(allresults$max_extAprob)
summary(allresults$max_extAprob1)
summary(allresults$max_extAprob1y1)
summary(allresults$max_extDprob1d0) 
quantile(allresults$max_extDprob1d0,0.95) 
summary(allresults$max_extDprob1d1)
quantile(allresults$max_extDprob1d1,0.95) 
summary(allresults$max_extDprob1d2)
quantile(allresults$max_extDprob1d2,0.95)
summary(allresults$max_extDprob2d0)
quantile(allresults$max_extDprob2d0,0.95) 
summary(allresults$max_extDprob2d1)
quantile(allresults$max_extDprob2d1,0.95)
summary(allresults$max_extDprob2d2)
quantile(allresults$max_extDprob2d2,0.95)

###


summary(allresults$minAprob_allimp) ## not an indication of a pos violation
quantile(allresults$minAprob_allimp, 0.05)
summary(allresults$maxAprob_allimp)
quantile(allresults$maxAprob_allimp, 0.95)
summary(allresults$minAprob1_allimp) ## not an indication of a pos violation
summary(allresults$maxAprob1_allimp)
summary(allresults$minAprob1y1_allimp) ## not an indication of a pos violation
summary(allresults$maxAprob1y1_allimp)

summary(allresults$minDprob1d0_allimp)
quantile(allresults$minDprob1d0_allimp, 0.05)
summary(allresults$maxDprob1d0_allimp)
quantile(allresults$maxDprob1d0_allimp, 0.95)
summary(allresults$minDprob1d1_allimp)
quantile(allresults$minDprob1d1_allimp, 0.05)
summary(allresults$maxDprob1d1_allimp)
quantile(allresults$maxDprob1d1_allimp, 0.95)
summary(allresults$minDprob1d2_allimp)
quantile(allresults$minDprob1d2_allimp, 0.05)
summary(allresults$maxDprob1d2_allimp)
quantile(allresults$maxDprob1d2_allimp, 0.95)

summary(allresults$minDprob2d0_allimp)
quantile(allresults$minDprob2d0_allimp, 0.05)
summary(allresults$maxDprob2d0_allimp)
quantile(allresults$maxDprob2d0_allimp, 0.95)
summary(allresults$minDprob2d1_allimp)
quantile(allresults$minDprob2d1_allimp, 0.05)
summary(allresults$maxDprob2d1_allimp)
quantile(allresults$maxDprob2d1_allimp, 0.95)
summary(allresults$minDprob2d2_allimp)
quantile(allresults$minDprob2d2_allimp, 0.05)
summary(allresults$maxDprob2d2_allimp)
quantile(allresults$maxDprob2d2_allimp, 0.95)


### make a figure to summarize extreme prob values
extprobs <- data.frame("exposure"=c(rep("A=0",6),rep("D1=0",6),rep("D1=1",6),rep("D1=2",6),
                                    rep("D2=0",6),rep("D2=1",6),rep("D2=2",6)),
                       "type"=rep(c("min_imp","min_BS","0.05_BS","max_imp","max_BS","0.95_BS"),7),
                       "value"=c(1-8.099078e-01, 0.05031, 0.1187862, NA, NA, NA,
                                 3.359629e-02, 1.650e-06, 0.0006335887, 7.776638e-01, 0.9949, 0.9539924,
                                 5.528704e-02, 0.000001, 0.004516555, 7.432565e-01, 0.9873, 0.9195719, 
                                 1.393765e-01, 0.0004729, 0.01362398, 8.196935e-01, 0.9803, 0.9399594,
                                 3.206794e-04, 1.000e-06, 9.99999e-07, 9.196628e-01, 0.999998, 0.998039, 
                                 4.333725e-02, 0.000001, 0.0009101974, 7.844907e-01, 0.9971, 0.9753233,
                                 1.876920e-02, 0.0000010, 1.332463e-05, 9.193725e-01, 0.9996, 0.9920133))

extprobs_new <- reshape(extprobs, idvar = "exposure", timevar = "type", direction = "wide")

my_colors <- RColorBrewer::brewer.pal(8, "Spectral")[c(7,6,8)]

posplot1 <- ggplot(extprobs_new) + 
  geom_point(aes(x = value.min_BS, y = exposure, color=my_colors[2]), size = 3) + 
  geom_point(aes(x = value.0.05_BS, y = exposure, color=my_colors[1]), size = 3) + 
  geom_point(aes(x = value.min_imp, y = exposure, color=my_colors[3]), size = 3) +
  geom_vline(xintercept=0.001, linetype="dotted") + 
  scale_y_discrete(labels=rev(c(bquote(Pr("A=0|"*bold(X)[0])),bquote(Pr(D[1]*"=0|"*bold(X)[0])),
                                bquote(Pr(D[1]*"=1|"*bold(X)[0])),bquote(Pr(D[1]*"=2|"*bold(X)[0])),
                                bquote(Pr(D[2]*"=0|"*bold(X)[1])),bquote(Pr(D[2]*"=1|"*bold(X)[1])),
                                bquote(Pr(D[2]*"=2|"*bold(X)[1])))), limits=rev) +
  scale_color_manual(values=c(my_colors[3],my_colors[1],my_colors[2]), labels=c("Min. imputation","5th percentile BS","Min. BS")) + 
  theme_bw() + labs(x="Exposure Probability", y=" ", color=" ") + ggtitle("Minimum Probabilities") + 
  theme(legend.text=element_text(size=9),legend.margin=margin(0,0,0,0))


my_colors2 <- RColorBrewer::brewer.pal(8, "Spectral")[c(2,3,1)]

posplot2 <- ggplot(extprobs_new) + 
  geom_point(aes(x = value.max_BS, y = exposure, color=my_colors2[2]), size = 3) + 
  geom_point(aes(x = value.0.95_BS, y = exposure, color=my_colors2[1]), size = 3) + 
  geom_point(aes(x = value.max_imp, y = exposure, color=my_colors2[3]), size = 3) +
  geom_vline(xintercept=0.999, linetype="dotted") + 
  scale_y_discrete(labels=rev(c(bquote(Pr("A=0|"*bold(X)[0])),bquote(Pr(D[1]*"=0|"*bold(X)[0])),
                                bquote(Pr(D[1]*"=1|"*bold(X)[0])),bquote(Pr(D[1]*"=2|"*bold(X)[0])),
                                bquote(Pr(D[2]*"=0|"*bold(X)[1])),bquote(Pr(D[2]*"=1|"*bold(X)[1])),
                                bquote(Pr(D[2]*"=2|"*bold(X)[1])))), limits=rev) +
  scale_color_manual(values=c(my_colors2[3],my_colors2[1],my_colors2[2]), labels=c("Max. imputation","95th percentile BS","Max. BS")) + 
  theme_bw() + labs(x="Exposure Probability", y=" ", color=" ") + ggtitle("Maximum Probabilities") + 
  theme(legend.text=element_text(size=9),legend.margin=margin(0,0,0,0))

ggsave("/Users/ameisner/Dropbox/FHCRC/BlackMSM/forcluster/fromcluster/positivity.png", 
       plot=ggarrange(posplot1, posplot2, ncol=2, nrow=1), width = 10.5,height = 4.5, units="in",bg="white")

##################
##################

summary(allresults[which(allresults$maxDanalysis_Dbal2_adj <= 0.1 & allresults$maxDanalysis_Dbal1_adj <= 0.1),]$minAprob_allimp) ## not an indication of a pos violation
summary(allresults[which(allresults$maxDanalysis_Dbal2_adj <= 0.1 & allresults$maxDanalysis_Dbal1_adj <= 0.1),]$maxAprob_allimp)
summary(allresults[which(allresults$maxDanalysis_Dbal2_adj <= 0.1 & allresults$maxDanalysis_Dbal1_adj <= 0.1),]$minAprob1_allimp) ## not an indication of a pos violation
summary(allresults[which(allresults$maxDanalysis_Dbal2_adj <= 0.1 & allresults$maxDanalysis_Dbal1_adj <= 0.1),]$maxAprob1_allimp)
summary(allresults[which(allresults$maxDanalysis_Dbal2_adj <= 0.1 & allresults$maxDanalysis_Dbal1_adj <= 0.1),]$minAprob1y1_allimp) ## not an indication of a pos violation
summary(allresults[which(allresults$maxDanalysis_Dbal2_adj <= 0.1 & allresults$maxDanalysis_Dbal1_adj <= 0.1),]$maxAprob1y1_allimp)

summary(allresults[which(allresults$maxDanalysis_Dbal2_adj <= 0.1 & allresults$maxDanalysis_Dbal1_adj <= 0.1),]$minDprob1d0_allimp)
min(allresults[which(allresults$maxDanalysis_Dbal2_adj <= 0.1 & allresults$maxDanalysis_Dbal1_adj <= 0.1),]$minDprob1d0_allimp)
quantile(allresults[which(allresults$maxDanalysis_Dbal2_adj <= 0.1 & allresults$maxDanalysis_Dbal1_adj <= 0.1),]$minDprob1d0_allimp, 0.044)
summary(allresults[which(allresults$maxDanalysis_Dbal2_adj <= 0.1 & allresults$maxDanalysis_Dbal1_adj <= 0.1),]$maxDprob1d0_allimp)
summary(allresults[which(allresults$maxDanalysis_Dbal2_adj <= 0.1 & allresults$maxDanalysis_Dbal1_adj <= 0.1),]$minDprob1d1_allimp)
min(allresults[which(allresults$maxDanalysis_Dbal2_adj <= 0.1 & allresults$maxDanalysis_Dbal1_adj <= 0.1),]$minDprob1d1_allimp)
quantile(allresults[which(allresults$maxDanalysis_Dbal2_adj <= 0.1 & allresults$maxDanalysis_Dbal1_adj <= 0.1),]$minDprob1d1_allimp, 0.013)
summary(allresults[which(allresults$maxDanalysis_Dbal2_adj <= 0.1 & allresults$maxDanalysis_Dbal1_adj <= 0.1),]$maxDprob1d1_allimp)
summary(allresults[which(allresults$maxDanalysis_Dbal2_adj <= 0.1 & allresults$maxDanalysis_Dbal1_adj <= 0.1),]$minDprob1d2_allimp)
min(allresults[which(allresults$maxDanalysis_Dbal2_adj <= 0.1 & allresults$maxDanalysis_Dbal1_adj <= 0.1),]$minDprob1d2_allimp)
quantile(allresults[which(allresults$maxDanalysis_Dbal2_adj <= 0.1 & allresults$maxDanalysis_Dbal1_adj <= 0.1),]$minDprob1d2_allimp, 0.0008)
summary(allresults[which(allresults$maxDanalysis_Dbal2_adj <= 0.1 & allresults$maxDanalysis_Dbal1_adj <= 0.1),]$maxDprob1d2_allimp)

summary(allresults[which(allresults$maxDanalysis_Dbal2_adj <= 0.1 & allresults$maxDanalysis_Dbal1_adj <= 0.1),]$minDprob2d0_allimp)
quantile(allresults[which(allresults$maxDanalysis_Dbal2_adj <= 0.1 & allresults$maxDanalysis_Dbal1_adj <= 0.1),]$minDprob2d0_allimp, 0.943) ## we know this is an issue from the non-BS data above
summary(allresults[which(allresults$maxDanalysis_Dbal2_adj <= 0.1 & allresults$maxDanalysis_Dbal1_adj <= 0.1),]$maxDprob2d0_allimp)
quantile(allresults[which(allresults$maxDanalysis_Dbal2_adj <= 0.1 & allresults$maxDanalysis_Dbal1_adj <= 0.1),]$maxDprob2d0_allimp, 0.975) ## 96% of bootstrap samples have a max prob (across all imputations) of < 0.999 
summary(allresults[which(allresults$maxDanalysis_Dbal2_adj <= 0.1 & allresults$maxDanalysis_Dbal1_adj <= 0.1),]$minDprob2d1_allimp)
quantile(allresults[which(allresults$maxDanalysis_Dbal2_adj <= 0.1 & allresults$maxDanalysis_Dbal1_adj <= 0.1),]$minDprob2d1_allimp, 0.051) 
summary(allresults[which(allresults$maxDanalysis_Dbal2_adj <= 0.1 & allresults$maxDanalysis_Dbal1_adj <= 0.1),]$maxDprob2d1_allimp)
summary(allresults[which(allresults$maxDanalysis_Dbal2_adj <= 0.1 & allresults$maxDanalysis_Dbal1_adj <= 0.1),]$minDprob2d2_allimp)
quantile(allresults[which(allresults$maxDanalysis_Dbal2_adj <= 0.1 & allresults$maxDanalysis_Dbal1_adj <= 0.1),]$minDprob2d2_allimp, 0.38) 
summary(allresults[which(allresults$maxDanalysis_Dbal2_adj <= 0.1 & allresults$maxDanalysis_Dbal1_adj <= 0.1),]$maxDprob2d2_allimp)
quantile(allresults[which(allresults$maxDanalysis_Dbal2_adj <= 0.1 & allresults$maxDanalysis_Dbal1_adj <= 0.1),]$maxDprob2d2_allimp, 0.998)

summary(allresults[which(allresults$maxDanalysis_Dbal2_adj <= 0.1 & allresults$maxDanalysis_Dbal1_adj <= 0.1),]$max_extAprob)
summary(allresults[which(allresults$maxDanalysis_Dbal2_adj <= 0.1 & allresults$maxDanalysis_Dbal1_adj <= 0.1),]$max_extAprob1)
summary(allresults[which(allresults$maxDanalysis_Dbal2_adj <= 0.1 & allresults$maxDanalysis_Dbal1_adj <= 0.1),]$max_extAprob1y1)
summary(allresults[which(allresults$maxDanalysis_Dbal2_adj <= 0.1 & allresults$maxDanalysis_Dbal1_adj <= 0.1),]$max_extDprob1d0) ## up to 13.7% of participants in any imputed dataset across all bootstrap samples had a pos violation for Pr(D1 = 0)
## i.e., in one bootstrap sample, in at least one imputed dataset, 13.7% of ppts had a pos violation
quantile(allresults[which(allresults$maxDanalysis_Dbal2_adj <= 0.1 & allresults$maxDanalysis_Dbal1_adj <= 0.1),]$max_extDprob1d0,c(0.964, 0.9993)) ## in 94.6% of boostrap samples, in each imputed dataset, there were pos violations in at most 1% of ppts 
summary(allresults[which(allresults$maxDanalysis_Dbal2_adj <= 0.1 & allresults$maxDanalysis_Dbal1_adj <= 0.1),]$max_extDprob1d1)
quantile(allresults[which(allresults$maxDanalysis_Dbal2_adj <= 0.1 & allresults$maxDanalysis_Dbal1_adj <= 0.1),]$max_extDprob1d1,c(0.993)) ## max = 9.3%
summary(allresults[which(allresults$maxDanalysis_Dbal2_adj <= 0.1 & allresults$maxDanalysis_Dbal1_adj <= 0.1),]$max_extDprob1d2)
summary(allresults[which(allresults$maxDanalysis_Dbal2_adj <= 0.1 & allresults$maxDanalysis_Dbal1_adj <= 0.1),]$max_extDprob2d0)
quantile(allresults[which(allresults$maxDanalysis_Dbal2_adj <= 0.1 & allresults$maxDanalysis_Dbal1_adj <= 0.1),]$max_extDprob2d0,c(0.088, 0.42)) ## again, we knew this was a problem from above (this gives 10% of ppts) - give 1% and 10% #s
summary(allresults[which(allresults$maxDanalysis_Dbal2_adj <= 0.1 & allresults$maxDanalysis_Dbal1_adj <= 0.1),]$max_extDprob2d1)
quantile(allresults[which(allresults$maxDanalysis_Dbal2_adj <= 0.1 & allresults$maxDanalysis_Dbal1_adj <= 0.1),]$max_extDprob2d1,c(0.978)) ## max = 9.2%
summary(allresults[which(allresults$maxDanalysis_Dbal2_adj <= 0.1 & allresults$maxDanalysis_Dbal1_adj <= 0.1),]$max_extDprob2d2)
quantile(allresults[which(allresults$maxDanalysis_Dbal2_adj <= 0.1 & allresults$maxDanalysis_Dbal1_adj <= 0.1),]$max_extDprob2d2,c(0.72, 0.995))



##################
#### imbalance
summary(allresults$maximbalAunadj)
summary(allresults$maximbalAadj)
summary(allresults$maxDanalysis_AbalX1_unadj)
summary(allresults$maxDanalysis_AbalX1_adj)
summary(allresults$maxDanalysis_AbalX1y1_unadj)
summary(allresults$maxDanalysis_AbalX1y1_adj)
summary(allresults$maxDanalysis_Dbal1_unadj)
summary(allresults$maxDanalysis_Dbal1_adj)
summary(allresults$maxDanalysis_Dbal2_unadj)
summary(allresults$maxDanalysis_Dbal2_adj)

### some imbalance (even after weighting) for D1 and D2
summary(allresults$maximbalA_adj1)
summary(allresults$maximbalA_adj2)
summary(allresults$maximbalA_adj3)
summary(allresults$maximbalA_adj4)
summary(allresults$maximbalA_adj5)
summary(allresults$maximbalA_adj6)
summary(allresults$maximbalA_adj7)
summary(allresults$maximbalA_adj8)

summary(allresults$maxDanalysis_Dbal1_adj_indiv1)
summary(allresults$maxDanalysis_Dbal1_adj_indiv2)
summary(allresults$maxDanalysis_Dbal1_adj_indiv3)
summary(allresults$maxDanalysis_Dbal1_adj_indiv4)
summary(allresults$maxDanalysis_Dbal1_adj_indiv5)
summary(allresults$maxDanalysis_Dbal1_adj_indiv6)
summary(allresults$maxDanalysis_Dbal1_adj_indiv7)
summary(allresults$maxDanalysis_Dbal1_adj_indiv8)

summary(allresults$maxDanalysis_Dbal2_adj_indiv1)
summary(allresults$maxDanalysis_Dbal2_adj_indiv2)
summary(allresults$maxDanalysis_Dbal2_adj_indiv3)
summary(allresults$maxDanalysis_Dbal2_adj_indiv4)
summary(allresults$maxDanalysis_Dbal2_adj_indiv5)
summary(allresults$maxDanalysis_Dbal2_adj_indiv6)
summary(allresults$maxDanalysis_Dbal2_adj_indiv7)
summary(allresults$maxDanalysis_Dbal2_adj_indiv8)


### A balance

Abal_max <- c(7.650636e-01, 3.533337e-01, 2.983780e-01, 5.477458e-02, 9.274993e-02, 3.858850e-01, 3.511590e-02, 8.398302e-01, 
              7.152536e-04, 3.542523e-04, 3.497606e-04, 1.753668e-04, 1.080740e-04, 3.699808e-04, 5.493635e-05, 7.929108e-04 ) 
Abal_allmax <- rbind(Abal_max[1:8],Abal_max[9:16]) 
colnames(Abal_allmax) <- c("Age","Northeast","South","West","STI","Number of \n partners",
                           "URAI","Gay")
Abal_allmax <- data.frame(t(Abal_allmax))
colnames(Abal_allmax) <- c("Unadjusted","Adjusted")
Abal_allmax$vars <- rownames(Abal_allmax)

Abal_allmax$vars <- factor(Abal_allmax$vars, levels = Abal_allmax$vars)
Abal_max_unadj <- Abal_allmax[,c("vars","Unadjusted")]
Abal_max_adj <- Abal_allmax[,c("vars","Adjusted")]
names(Abal_max_unadj) <- c("vars","mean")
names(Abal_max_adj) <- c("vars","mean")
Abal_allmax2 <- data.frame(rbind(Abal_max_unadj,Abal_max_adj))
Abal_allmax2$adj <- c(rep("Unweighted",8),rep("Weighted",8))
Abal_allmax2$vars <- factor(Abal_allmax2$vars, levels=c("Age","Northeast","South","West",
                                                        "Gay","Number of \n partners","STI","URAI"))
Abal_max_unadj$vars <- factor(Abal_max_unadj$vars, levels=c("Age","Northeast","South","West",
                                                            "Gay","Number of \n partners","STI","URAI"))

p1 <- ggplot(Abal_allmax2) +
  geom_segment(data = Abal_max_unadj,
               aes(x = mean, y = vars, yend = Abal_max_adj$vars, xend = Abal_max_adj$mean), 
               color = "#aeb6bf", linewidth = 3, alpha = .5) + 
  scale_y_discrete(limits=rev) + geom_vline(xintercept=0.10, linetype="dotted") +
  geom_point(aes(x = mean, y = vars, color = adj), size = 2.5, show.legend = TRUE) + 
  annotate("text", x = -0.425, y = 6, label = "Region",angle=90, size=3.6, color="gray32") +
  annotate("segment", x = -0.375, xend = -0.375, y = 5, yend=7, size=0.5, color="gray32") +
  coord_cartesian(xlim = c(0, 1),  clip = 'off')  + theme(axis.title.y = element_text(margin = margin(t = 0, r = 25, b = 0, l = 0))) + 
  labs(title="Covariate Balance: A", x="Standardized Difference", y="") + theme(legend.title=element_blank(), axis.text=element_text(size=10), legend.text=element_text(size=10.5)) 

### D balance
Dbal_maxv1 <- c(5.539920e-01, 2.254382e-01, 5.054490e-01, 4.855660e-01, 3.907401e-01, 5.837279e-01, 1.892438e-01, 2.814029e-01, 
                6.150230e-06, 4.734463e-06, 6.639162e-06, 7.582000e-06, 6.149476e-06, 5.361672e-06, 4.689676e-06, 7.103400e-06) 

Dbal_allmaxv1 <- rbind(Dbal_maxv1[1:8],Dbal_maxv1[9:16])
colnames(Dbal_allmaxv1) <- c("Age","Northeast","South","West","STI","Number of \n partners",
                             "URAI","Gay")
Dbal_allmaxv1 <- data.frame(t(Dbal_allmaxv1))
colnames(Dbal_allmaxv1) <- c("Unadjusted","Adjusted")
Dbal_allmaxv1$vars <- rownames(Dbal_allmaxv1)

Dbal_allmaxv1$vars <- factor(Dbal_allmaxv1$vars, levels = Dbal_allmaxv1$vars)
Dbal_max_unadjv1 <- Dbal_allmaxv1[,c("vars","Unadjusted")]
Dbal_max_adjv1 <- Dbal_allmaxv1[,c("vars","Adjusted")]
names(Dbal_max_unadjv1) <- c("vars","mean")
names(Dbal_max_adjv1) <- c("vars","mean")
Dbal_allmax2v1 <- data.frame(rbind(Dbal_max_unadjv1,Dbal_max_adjv1))
Dbal_allmax2v1$adj <- c(rep("Unweighted",8),rep("Weighted",8))
Dbal_allmax2v1$vars <- factor(Dbal_allmax2v1$vars, levels=c("Age","Northeast","South","West",
                                                            "Gay","Number of \n partners","STI","URAI"))
Dbal_max_unadjv1$vars <- factor(Dbal_max_unadjv1$vars, levels=c("Age","Northeast","South","West",
                                                                "Gay","Number of \n partners","STI","URAI"))

p2 <- ggplot(Dbal_allmax2v1) +
  geom_segment(data = Dbal_max_unadjv1,
               aes(x = mean, y = vars, yend = Dbal_max_adjv1$vars, xend = Dbal_max_adjv1$mean), 
               color = "#aeb6bf", linewidth = 3, alpha = .5) + 
  scale_y_discrete(limits=rev) + geom_vline(xintercept=0.10, linetype="dotted") +
  geom_point(aes(x = mean, y = vars, color = adj), size = 2.5, show.legend = TRUE) + 
  annotate("text", x = -0.425, y = 6, label = "Region",angle=90, size=3.6, color="gray32") +
  annotate("segment", x = -0.375, xend = -0.375, y = 5, yend=7, size=0.5, color="gray32") +
  coord_cartesian(xlim = c(0, 1),  clip = 'off')  + theme(axis.title.y = element_text(margin = margin(t = 0, r = 25, b = 0, l = 0))) + 
  labs(title="Covariate Balance: D (time = 1)", x="Standardized Difference", y="") + theme(legend.title=element_blank(), axis.text=element_text(size=10), legend.text=element_text(size=10.5))


### D balance - v2
Dbal_maxv2 <- c(5.539920e-01, 2.254382e-01, 5.054490e-01, 4.855660e-01, 3.907401e-01, 5.837279e-01, 1.892438e-01, 2.814029e-01, 
                7.970441e-06, 5.537492e-06, 9.299147e-06, 7.654162e-06, 8.452761e-06, 8.462794e-06, 5.875480e-06, 1.163268e-05) 

Dbal_allmaxv2 <- rbind(Dbal_maxv2[1:8],Dbal_maxv2[9:16])
colnames(Dbal_allmaxv2) <- c("Age","Northeast","South","West","STI","Number of \n partners",
                             "URAI","Gay")
Dbal_allmaxv2 <- data.frame(t(Dbal_allmaxv2))
colnames(Dbal_allmaxv2) <- c("Unadjusted","Adjusted")
Dbal_allmaxv2$vars <- rownames(Dbal_allmaxv2)

Dbal_allmaxv2$vars <- factor(Dbal_allmaxv2$vars, levels = Dbal_allmaxv2$vars)
Dbal_max_unadjv2 <- Dbal_allmaxv2[,c("vars","Unadjusted")]
Dbal_max_adjv2 <- Dbal_allmaxv2[,c("vars","Adjusted")]
names(Dbal_max_unadjv2) <- c("vars","mean")
names(Dbal_max_adjv2) <- c("vars","mean")
Dbal_allmax2v2 <- data.frame(rbind(Dbal_max_unadjv2,Dbal_max_adjv2))
Dbal_allmax2v2$adj <- c(rep("Unweighted",8),rep("Weighted",8))
Dbal_allmax2v2$vars <- factor(Dbal_allmax2v2$vars, levels=c("Age","Northeast","South","West",
                                                            "Gay","Number of \n partners","STI","URAI"))
Dbal_max_unadjv2$vars <- factor(Dbal_max_unadjv2$vars, levels=c("Age","Northeast","South","West",
                                                                "Gay","Number of \n partners","STI","URAI"))

p3 <- ggplot(Dbal_allmax2v2) +
  geom_segment(data = Dbal_max_unadjv2,
               aes(x = mean, y = vars, yend = Dbal_max_adjv2$vars, xend = Dbal_max_adjv2$mean), 
               color = "#aeb6bf", linewidth = 3, alpha = .5) + 
  scale_y_discrete(limits=rev) + geom_vline(xintercept=0.10, linetype="dotted") +
  geom_point(aes(x = mean, y = vars, color = adj), size = 2.5, show.legend = TRUE) + 
  annotate("text", x = -0.425, y = 6, label = "Region",angle=90, size=3.6, color="gray32") +
  annotate("segment", x = -0.375, xend = -0.375, y = 5, yend=7, size=0.5, color="gray32") +
  coord_cartesian(xlim = c(0, 1),  clip = 'off')  + theme(axis.title.y = element_text(margin = margin(t = 0, r = 25, b = 0, l = 0))) +  
  labs(title="Covariate Balance: D (time = 2)", x="Standardized Difference", y="") + 
  theme(legend.title=element_blank(), axis.text=element_text(size=10), legend.text=element_text(size=10.5)) 

library(ggpubr)

ggsave("/Users/ameisner/Dropbox/FHCRC/BlackMSM/forcluster/covbal_max.png", 
       plot=ggarrange(ggarrange(p1, p2, p3, ncol=3,common.legend = TRUE, legend="bottom")), width = 12,
       height = 5.5, units="in",bg="white")


######
## Table 1
######
load("/Users/ameisner/Dropbox/FHCRC/BlackMSM/alldata_wide20230413.Rda")

`%!in%` = Negate(`%in%`)

#### Remove transgender people from 061 (exclusion factor in 073)
alldata_wide <- alldata_wide[which(alldata_wide$gender=="male"),]
#### Remove ppts from 061 who reported only TGM partners at baseline (this would have made them not eligible for HPTN 073)
alldata_wide <- alldata_wide[-which(alldata_wide$study=="HPTN061" & 
                                      alldata_wide$id %in% c(119,308,875,529,1453,368,1122,999,666)), ]

#### Some variables should be excluded from dataset (variables that have been categorized - keep categorical version)
alldata_wide$newid <- 1:nrow(alldata_wide) ## mice doesn't like having character variables (including 'studyid') but we need a unique ID for each ppt
alldata_wide$totalMPcat_0 <- ifelse(alldata_wide$totalMP_0==0 | alldata_wide$totalMP_0==1, 0, 
                                    ifelse(alldata_wide$totalMP_0==2 | alldata_wide$totalMP_0==3, 1,
                                           ifelse(alldata_wide$totalMP_0>=4, 2, NA)))
alldata_wide$totalMPcat_1 <- ifelse(alldata_wide$totalMP_1==0 | alldata_wide$totalMP_1==1, 0, 
                                    ifelse(alldata_wide$totalMP_1==2 | alldata_wide$totalMP_1==3, 1,
                                           ifelse(alldata_wide$totalMP_1>=4, 2, NA)))
alldata_wide$totalMPcat_2 <- ifelse(alldata_wide$totalMP_2==0 | alldata_wide$totalMP_2==1, 0, 
                                    ifelse(alldata_wide$totalMP_2==2 | alldata_wide$totalMP_2==3, 1,
                                           ifelse(alldata_wide$totalMP_2>=4, 2, NA)))
toimpute <- alldata_wide[,c("study","multisti_2","totalMPcat_2","primaryMP_2","gavemoneyforsex_2","gotmoneyforsex_2",
                            "HIVpuMP_2","UIAI_2","URAI_2","UIAIHIVpu_2","URAIHIVpu_2","stimulants_2",     
                            "poppers_2","marijuana_2","heroin_2","vicodin_2","alcohol_unpro_2","alcohol_pro_2","stimulant_unpro_2",
                            "stimulant_pro_2","popper_unpro_2","popper_pro_2","marijuana_unpro_2","marijuana_pro_2",
                            "trusthcp1_2","trusthcp2_2","trusthcp3_2","trusthcp4_2",     
                            "multisti_1","totalMPcat_1","primaryMP_1","gavemoneyforsex_1","gotmoneyforsex_1","HIVpuMP_1","UIAI_1",           
                            "URAI_1","UIAIHIVpu_1","URAIHIVpu_1","stimulants_1","poppers_1","marijuana_1",      
                            "heroin_1","vicodin_1","alcohol_unpro_1","alcohol_pro_1","stimulant_unpro_1","stimulant_pro_1","popper_unpro_1",   
                            "popper_pro_1","marijuana_unpro_1","marijuana_pro_1",       
                            "trusthcp1_1","trusthcp2_1","trusthcp3_1","trusthcp4_1","multisti_0","totalMPcat_0",        
                            "primaryMP_0","gavemoneyforsex_0","gotmoneyforsex_0","HIVpuMP_0","UIAI_0","URAI_0","UIAIHIVpu_0",   
                            "URAIHIVpu_0","stimulants_0","poppers_0","marijuana_0","heroin_0","vicodin_0",    
                            "alcohol_unpro_0","alcohol_pro_0","stimulant_unpro_0","stimulant_pro_0","popper_unpro_0","popper_pro_0",
                            "marijuana_unpro_0","marijuana_pro_0",
                            "trusthcp1_0","trusthcp2_0","trusthcp3_0","trusthcp4_0","age","region","education",          
                            "work","sexorient","hispanic","healthins","house",            
                            "marital","prepinit1","prepinit2","adherence_1","adherence_2",      
                            "hiv1","hiv2","hiv1yr","audit_0","audit_1","audit_2","depression_0","depression_1",
                            "depression_2","conspiracy_0","conspiracy_1","conspiracy_2","inthomoph_0",   
                            "inthomoph_1","inthomoph_2","ssupport_0","ssupport_1","ssupport_2","hivstigma_0","hivstigma_1",   
                            "hivstigma_2","incomecat","nomoneycat","nightinjailcat","newid")]
## should usedpep be included in imputation?? NO 
toimpute$study <- ifelse(toimpute$study=="HPTN061",0,1)
### Change to factor variables (only non-binary variables that should be treated as nominal vars)
categoricalvars <- c("region","house","totalMPcat_2","totalMPcat_1","totalMPcat_0","sexorient","incomecat","education","work")
## totalMPcat will not be modeled as an ordinal variable in IPW models
toimpute[,categoricalvars] <- lapply(toimpute[,categoricalvars], factor)
## Converting some variables from character to numeric (ordinal)
## NB: missRanger pmm algorithm will ensure the imputed variable takes values observed in data
toimpute$incomecat <- relevel(toimpute$incomecat, ref="less than $10K")
toimpute$incomecat <- as.numeric(toimpute$incomecat)
toimpute$education <- factor(toimpute$education, levels = c("HS or less", "some college", "college graduate"))
toimpute$education <- as.numeric(toimpute$education)
toimpute$work <- factor(toimpute$work, levels = c("unemployed", "parttime", "fulltime")) 
toimpute$work <- as.numeric(toimpute$work)
## re-code marital status
toimpute$marital <- ifelse(toimpute$marital=="single",0,1) ## partner/no partner
## change some vars from character to numeric
toimpute$hispanic <- ifelse(toimpute$hispanic=="Yes",1,0)
toimpute$healthins <- ifelse(toimpute$healthins=="Yes",1,0)
## change prep init vars to be 1 if ever initiated
toimpute$prepinit2 <- ifelse(toimpute$prepinit1==1 | toimpute$prepinit2==1, 1, 0)

table(toimpute$study)

table(toimpute$prepinit1, useNA="always")
table(toimpute$prepinit2, useNA="always")
table(toimpute$adherence_1, useNA="always")
table(toimpute$adherence_2, useNA="always")

mean(toimpute[which(toimpute$study==0),"age"])
sd(toimpute[which(toimpute$study==0),"age"])
table(is.na(toimpute[which(toimpute$study==0),"age"]))
mean(toimpute[which(toimpute$study==1),"age"])
sd(toimpute[which(toimpute$study==1),"age"])
table(is.na(toimpute[which(toimpute$study==1),"age"]))

table(toimpute[which(toimpute$study==0),"region"],useNA="always")
table(toimpute[which(toimpute$study==1),"region"],useNA="always")

table(toimpute[which(toimpute$study==0),"sexorient"],useNA="always")
table(toimpute[which(toimpute$study==1),"sexorient"],useNA="always")

table(toimpute[which(toimpute$study==0),"totalMPcat_0"],useNA="always")
table(toimpute[which(toimpute$study==1),"totalMPcat_0"],useNA="always")

table(toimpute[which(toimpute$study==0),"totalMPcat_1"],useNA="always")
table(toimpute[which(toimpute$study==1),"totalMPcat_1"],useNA="always")


table(toimpute[which(toimpute$study==0),"URAI_0"],useNA="always")
table(toimpute[which(toimpute$study==1),"URAI_0"],useNA="always")

table(toimpute[which(toimpute$study==0),"URAI_1"],useNA="always")
table(toimpute[which(toimpute$study==1),"URAI_1"],useNA="always")


table(toimpute[which(toimpute$study==0),"multisti_0"],useNA="always")
table(toimpute[which(toimpute$study==1),"multisti_0"],useNA="always")

table(toimpute[which(toimpute$study==0),"multisti_1"],useNA="always")
table(toimpute[which(toimpute$study==1),"multisti_1"],useNA="always")


table(toimpute[which(toimpute$study==0),"hiv1"],useNA="always")
table(toimpute[which(toimpute$study==1),"hiv1"],useNA="always")

table(toimpute[which(toimpute$study==0),"hiv2"],useNA="always")
table(toimpute[which(toimpute$study==1),"hiv2"],useNA="always")
