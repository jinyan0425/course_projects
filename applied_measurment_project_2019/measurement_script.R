#INSTALL LIBRARIES
library(careless)
library(psych)
library(GPArotation)
library(lavaan)

setwd(dir) ##set the working directory where the file and script locate

#LOADING DATA
path = 'MTG_measurement_data.csv'
MTG <- read.csv(file = path)
View(MTG)
describe(MTG)

#others-signaling 17
MTG_OS <- MTG[c(3:19)]
#self-signaling 17
MTG_SS <- MTG[c(20:36)]
#SS-OS
MTG_SSOS <- MTG[c(3:36)]
#brand attachment 5
MTG_BA <- MTG[c(37:41)]
#self-esteem 10
MTG_SE <- MTG[c(42:51)]
#Status con 5
MTG_SC <- MTG[c(52:56)]
#Presitige Sensativity 9
MTG_PS <- MTG[c(57:65)]
#Material Value 9
MTG_MV <- MTG[c(66:74)]
#Demo
MTG_Demo <- MTG[c(75:83)]

#IDENTIFY CARELESS RESPONSE
OSmax <- cor(MTG[c(3:19)],use="pairwise.complete.obs")
SSmax <- cor(MTG[c(20:36)],use="pairwise.complete.obs")
View(OSmax)
BAmax <- cor(MTG[c(37:41)],use="pairwise.complete.obs")
SCmax <- cor(MTG[c(52:56)],use="pairwise.complete.obs")
PSmax <- cor(MTG[c(57:65)],use="pairwise.complete.obs")
MVmax <- cor(MTG[c(66:74)],use="pairwise.complete.obs")

MTG$MVCr <- 10-MTG$MVC1
MTG$SCr <- 10-MTG$SC4
View(MTG)

MTG_sub <- MTG[c(3:41,52:54,85,56:65,66:68,84,70:74)]
MTG_sub2 <- MTG[c(3:19)]
MTG_sub3 <- MTG[c(20:36)]
MTG_sub <- as.data.frame(MTG_sub)
MTG_sub2 <- as.data.frame(MTG_sub2)
MTG_sub3 <- as.data.frame(MTG_sub2)
View(MTG_sub)
describe(MTG_sub)

SLengths <- c(17,17,5,5,9,9)
SLengths2 <- c(17,17)

MTG_sub$evenodd <- evenodd(MTG_sub,SLengths)
MTG_sub$irv <- irv(MTG_sub)
MTG_sub$lstring <- longstring(MTG_sub,avg=T)
MTG_sub$MD <- mahad(MTG_sub)

MTG_sub2$evenodd2 <- evenodd(MTG_sub,SLengths2)
MTG_sub2$irv <- irv(MTG_sub2)
MTG_sub2$lstring <- longstring(MTG_sub2,avg=T)
MTG_sub2$MD <- mahad(MTG_sub2)
View(MTG_sub2)
write.csv(MTG_sub[c(62:66)],"project careless responses.csv")
write.csv(MTG_sub2[c(40:43)],"project careless responses2.csv")
CRtable <- project.careless.responses
describe(CRtable)
CRtable2 <- project.careless.responses2
describe(CRtable2)

#EFA
MTG2 <- MTG_SP_Cleaned
View(MTG2)
MTG2_OS <- MTG2[c(3:19)]
MTG2_SS <- MTG2[c(20:36)]
MTG2_SSOS<- MTG2[c(3:36)]

#OS-ONE FACTOR
Para_OS <- fa.parallel(MTG2_OS,fm="pa",fa="fa",error.bars=T)
Para_SS <- fa.parallel(MTG2_SS,fm="pa",fa="fa",error.bars=T)
Para_SS <- fa.parallel(MTG2_SSOS,fm="pa",fa="fa",error.bars=T)
EFA_SSOS_4 <- fa(MTG2_SSOS,fm="pa",nfactors=4,SMC=T)
print(EFA_SSOS_4,sort=T,cut=.4)

#OS-ONE FACTOR
EFA_OS_1 <- fa(MTG2_OS,fm="pa",nfactors=1,SMC=T)
print(EFA_OS_1,sort=T,cut=.4)

#R1
MTG2_OS2 <- MTG2_OS[c(10,11,5,4,12,2,13)]
EFA_OS_12 <- fa(MTG2_OS2,fm="pa",nfactors=1,SMC=T)
print(EFA_OS_12,sort=T,cut=.4)

#Final 1
MTG2_OS3 <- MTG2_OS[c(10,11,5,12,2)]
EFA_OS_13 <- fa(MTG2_OS3,fm="pa",nfactors=1,SMC=T)
print(EFA_OS_13,sort=T,cut=.4)
OS_alpha <- alpha(MTG2_OS3)
OS_alpha

#Final 2
MTG2_OS4 <- MTG2_OS[c(10,11,5,2)]
EFA_OS_14 <- fa(MTG2_OS4,fm="pa",nfactors=1,SMC=T)
print(EFA_OS_14,sort=T,cut=.4)
OS_alpha2 <- alpha(MTG2_OS4)
OS_alpha2
OSFinalmax <- cor(MTG2_OS4,use="pairwise.complete.obs")
View(OSFinalmax)
write.csv(OSFinalmax,"OSO EFA Matrix.csv")

#OS-TWO FACTOR

EFA_OS_2 <- fa(MTG2_OS,fm="pa",nfactors=2,SMC=T)
print(EFA_OS_2,sort=T,cut=.2)
#Revise
MTG_OS5 <- MTG2_OS[c(10,15,2,16,8)]
EFA_OS_21 <- fa(MTG_OS5,fm="pa",nfactors=2,SMC=T)
print(EFA_OS_21,sort=T,cut=.2)

#SS-TWO FACTOR
EFA_SS_2 <- fa(MTG2_SS,fm="pa",nfactors=2,SMC=T)
print(EFA_SS_2,sort=T,cut=.2)
MTG2_SS2 <- MTG2_SS[c(9,4,7,8,11,15)]
#EFA_SS_22 <- fa(MTG2_SS2,fm="pa",nfactors=2,SMC=T)
print(EFA_SS_22,sort=T,cut=.2)
#SS-THREE FACTOR
EFA_SS_3 <- fa(MTG2_SS,fm="pa",nfactors=3,SMC=T)
print(EFA_SS_3,sort=T,cut=.2)
#Revise
MTG2_SS3 <- MTG2_SS[c(9,4,7,8,2,11,10,16,14)]
EFA_SS_32 <- fa(MTG2_SS3,fm="pa",nfactors=3,SMC=T)
print(EFA_SS_32,sort=T,cut=.2)
SS_alpha <- alpha(MTG2_SS3)
SS_alpha

#Final
MTG2_SS4 <- MTG2_SS[c(9,4,7,11,2,10,16,14)]
EFA_SS_33 <- fa(MTG2_SS4,fm="pa",nfactors=3,SMC=T)
print(EFA_SS_33,sort=T,cut=.2)
SS_alpha2 <- alpha(MTG2_SS4)
SS_alpha2
SSFinalmax <- cor(MTG2_SS4,use="pairwise.complete.obs")
View(SSFinalmax)
write.csv(SSFinalmax,"SSO EFA Matrix.csv")

#Vadility
MTG_SP_Cleaned$MVCr <- 10-MTG_SP_Cleaned$MVC1
MTG_SP_Cleaned$SCr <- 10-MTG_SP_Cleaned$SC4
View(MTG_SP_Cleaned)
MTG_sub3 <- MTG_SP_Cleaned[c(28,26,23,30,21,29,35,33,12,13,4,7,52:54,86,56:65,66:68,85,70:74,79,82)]
MTG_sub4 <- MTG_SP_Cleaned[c(28,26,23,30,21,29,35,33,12,13,4,7,79,82)]
View(MTG_sub3)

MTG_sub3$SSO <-(MTG_sub3$Se9+MTG_sub3$Se7+MTG_sub3$Se4+MTG_sub3$Se11+MTG_sub3$Se2+MTG_sub3$Se10+MTG_sub3$Se16+MTG_sub3$Se14)/8
MTG_sub3$SSOf1 <-(MTG_sub3$Se9+MTG_sub3$Se7+MTG_sub3$Se4)/3
MTG_sub3$SSOf2 <-(MTG_sub3$Se11+MTG_sub3$Se2+MTG_sub3$Se10)/3
MTG_sub3$SSOf3 <-(MTG_sub3$Se16+MTG_sub3$Se14)/2
MTG_sub3$OSO <-(MTG_sub3$So10+MTG_sub3$So11+MTG_sub3$So2+MTG_sub3$So5)/4
MTG_sub3$PS <- (MTG_sub3$PS1+MTG_sub3$PS2+MTG_sub3$PS3+MTG_sub3$PS4+MTG_sub3$PS5+MTG_sub3$PS6+MTG_sub3$PS7+MTG_sub3$PS8+MTG_sub3$PS9)/9
MTG_sub3$MV <- (MTG_sub3$MVCr+MTG_sub3$MVC2+MTG_sub3$MVC3+MTG_sub3$MVH1+MTG_sub3$MVH2+MTG_sub3$MVH3+MTG_sub3$MVS1+MTG_sub3$MVS2+MTG_sub3$MVS3)/9
MTG_sub3$MVC <-(MTG_sub3$MVCr+MTG_sub3$MVC2)/3
MTG_sub3$MVH <- (MTG_sub3$MVH1+MTG_sub3$MVH2+MTG_sub3$MVH3)/3
MTG_sub3$MVS <- (MTG_sub3$MVS1+MTG_sub3$MVS2+MTG_sub3$MVS3)/3
MTG_sub3$SC <- (MTG_sub3$SC1+MTG_sub3$SC2+MTG_sub3$SC3+MTG_sub3$SCr+MTG_sub3$SC5)/5
View(MTG_sub3)

SSOmax <- cor(MTG_sub3[c(36,38:43)],use="pairwise.complete.obs")
SSOmax2 <-cor(MTG_sub3[c(36,44:46,38:43)],use="pairwise.complete.obs")
OSOmax <- cor(MTG_sub3[c(37:43)],use="pairwise.complete.obs")
View(SSOmax)
View(SSOmax2)
View(OSOmax)
write.csv(SSOmax,"SSO vadility Matrix.csv")
write.csv(SSOmax2,"SSO vadility Matrix2.csv")
write.csv(OSOmax,"OSO vadility Matrix.csv")

#CFA
MTG_SSO <- MTG_sub3[c(1:8,36,37)]
View(MTG_SSO)
MTG_OSO <- MTG_sub3[c(9:12,36,37)]
View(MTG_OSO)
describe(MTG_SSO)
mardia(MTG_SSO)
describe(MTG_OSO)
mardia(MTG_OSO)
MTG_nt <- MTG_sub3[c(1:35)]
describe(MTG_nt)
mardia(MTG_nt)

SSOMV <- MTG_sub3[c(1:8,27:35)]
OSOMV <- MTG_sub3[c(9:12,27:35)]
SSOPS <- MTG_sub3[c(1:8,18:26)]
OSOPS <- MTG_sub3[c(9:12,18:26)]
SSOSC <- MTG_sub3[c(1:8,13:17)]
OSOSC <- MTG_sub3[c(9:17)]
describe(OSOSC)
mardia(OSOSC)
describe(SSOSC)
mardia(SSOSC)
SSO <- 'Se=~Se9+Se7+Se4
Ss=~Se11+Se2+Se10
Pu=~Se16+Se14'
OSO <- 'OS=~So10+So11+So2+So5'
SSO.SBfit <-cfa(SSO,data=MTG_SSO,test="Satorra.Bentler")
summary(SSO.SBfit,fit.measures=T,standardized=T)
SSO.DWLSfit <-cfa(SSO,data=MTG_SSO,estimator="WLSMV")
summary(SSO.DWLSfit,fit.measures=T,standardized=T)
anova(SSO.SBfit,SSO.DWLSfit)

SO <-'SS=~Se9+Se7+Se4+Se11+Se2+Se10+Se16+Se14
OS=~So10+So11+So2+So5'
SO.SBfit <-cfa(SO,data=MTG_sub3,test="Satorra.Bentler")
summary(SO.SBfit,fit.measures=T,standardized=T)
SO1 <-'SS=~Se9+Se7+Se4+Se11+Se2+Se10+Se16+Se14
OS=~So10+So11+So2+So5
SS~~1*OS'
SO.SBfit1 <-cfa(SO1,data=MTG_sub3,test="Satorra.Bentler")
summary(SO.SBfit1,fit.measures=T,standardized=T)
anova(SO.SBfit,SO.SBfit1)


OSO.SBfit <-cfa(OSO,data=MTG_OSO,test="Satorra.Bentler")
summary(OSO.SBfit,fit.measures=T,standardized=T)
OSO.DWLSfit <-cfa(OSO,data=MTG_OSO,estimator="WLSMV")
summary(OSO.DWLSfit,fit.measures=T,standardized=T)
anova(OSO.SBfit,OSO.DWLSfit)

SSO.YBfit <-cfa(SSO,data=MTG_SSO,test="Yuan.Bentler")
summary(SSO.SBfit,fit.measures=T,standardized=T)
OSO.YBfit <-cfa(OSO,data=MTG_OSO,test="Yuan.Bentler")
summary(OSO.SBfit,fit.measures=T,standardized=T)

SSOPS <- 'SSO=~Se9+Se7+Se4+Se11+Se2+Se10+Se16+Se14
PS=~PS1+PS2+PS3+PS4+PS5+PS6+PS7+PS8+PS9'
SSOPS.SBfit <-cfa(SSOPS,data=MTG_sub3,test="Satorra.Bentler",std.lv=T)
summary(SSOPS.SBfit,fit.measures=T,standardized=T)

SSOPS1 <- 'SSO=~Se9+Se7+Se4+Se11+Se2+Se10+Se16+Se14
PS=~PS1+PS2+PS3+PS4+PS5+PS6+PS7+PS8+PS9
SSO~~1*PS'
SSOPS.SBfit1 <-cfa(SSOPS1,data=MTG_sub3,test="Satorra.Bentler",std.lv=T)
summary(SSOPS.SBfit1,fit.measures=T,standardized=T)
anova(SSOPS.SBfit,SSOPS.SBfit1)

OSOPS <- 'OSO=~So10+So11+So2+So5
PS=~PS1+PS2+PS3+PS4+PS5+PS6+PS7+PS8+PS9'
OSOPS.SBfit <-cfa(OSOPS,data=MTG_sub3,test="Satorra.Bentler",std.lv=T)
summary(OSOPS.SBfit,fit.measures=T,standardized=T)
OSOPS1 <- 'OSO=~So10+So11+So2+So5
PS=~PS1+PS2+PS3+PS4+PS5+PS6+PS7+PS8+PS9
OSO~~1*PS'
OSOPS.SBfit1 <-cfa(OSOPS1,data=MTG_sub3,test="Satorra.Bentler",std.lv=T)
summary(OSOPS.SBfit1,fit.measures=T,standardized=T)
anova(OSOPS.SBfit,OSOPS.SBfit1)

SSOSC<- 'SSO=~Se9+Se7+Se4+Se11+Se2+Se10+Se16+Se14
SC=~SC1+SC2+SC3+SCr+SC5'
SSOSC.SBfit <-cfa(SSOSC,data=MTG_sub3,test="Satorra.Bentler")
summary(SSOSC.SBfit,fit.measures=T,standardized=T)
SSOSC1<- 'SSO=~Se9+Se7+Se4+Se11+Se2+Se10+Se16+Se14
SC=~SC1+SC2+SC3+SCr+SC5
SSO~~1*SC'
SSOSC.SBfit1 <-cfa(SSOSC1,data=MTG_sub3,test="Satorra.Bentler")
summary(SSOSC.SBfit1,fit.measures=T,standardized=T)
anova(SSOSC.SBfit,SSOSC.SBfit1)

OSOSC<- 'OSO=~So10+So11+So2+So5
SC=~SC1+SC2+SC3+SCr+SC5'
OSOSC.SBfit <-cfa(OSOSC,data=MTG_sub3,test="Satorra.Bentler",std.lv=T)
summary(OSOSC.SBfit,fit.measures=T,standardized=T)
OSOSC1<- 'OSO=~So10+So11+So2+So5
SC=~SC1+SC2+SC3+SCr+SC5
OSO~~1*SC'
OSOSC.SBfit1 <-cfa(OSOSC1,data=MTG_sub3,test="Satorra.Bentler",std.lv=T)
summary(OSOSC.SBfit1,fit.measures=T,standardized=T)

SSOMV<- 'SSO=~Se9+Se7+Se4+Se11+Se2+Se10+Se16+Se14
MV=~MVCr+MVC2+MVC3+MVH1+MVH2+MVH3+MVS1+MVS2+MVS3'
SSOMV.SBfit <-cfa(SSOMV,data=MTG_sub3,test="Satorra.Bentler",std.lv=T)
summary(SSOMV.SBfit,fit.measures=T,standardized=T)
SSOMV1<- 'SSO=~Se9+Se7+Se4+Se11+Se2+Se10+Se16+Se14
MV=~MVCr+MVC2+MVC3+MVH1+MVH2+MVH3+MVS1+MVS2+MVS3
SSO~~1*MV'
SSOMV.SBfit1 <-cfa(SSOMV1,data=MTG_sub3,test="Satorra.Bentler",std.lv=T)
summary(SSOMV.SBfit1,fit.measures=T,standardized=T)
anova(SSOMV.SBfit,SSOMV.SBfit1)

OSOMV<- 'OSO=~So10+So11+So2+So5
MV=~MVCr+MVC2+MVC3+MVH1+MVH2+MVH3+MVS1+MVS2+MVS3'
OSOMV.SBfit <-cfa(OSOMV,data=MTG_sub3,test="Satorra.Bentler",std.lv=T)
summary(OSOMV.SBfit,fit.measures=T,standardized=T)
OSOMV1<- 'OSO=~So10+So11+So2+So5
MV=~MVCr+MVC2+MVC3+MVH1+MVH2+MVH3+MVS1+MVS2+MVS3
OSO~~1*MV'
OSOMV.SBfit1 <-cfa(OSOMV1,data=MTG_sub3,test="Satorra.Bentler",std.lv=T)
summary(OSOMV.SBfit1,fit.measures=T,standardized=T)
anova(OSOMV.SBfit,OSOMV.SBfit1)

SSO.SBfit <-cfa(SSO,data=MTG_sub3)
summary(SSO.SBfit,fit.measures=T,standardized=T,test="Satorra.Bentler")
SSO.DWLSfit <-cfa(SSO,data=MTG_sub3,estimator="WLSMV")
summary(SSO.DWLSfit,fit.measures=T,standardized=T)
anova(SSO.SBfit,SSO.DWLSfit)
OSO.DWLSfit <-cfa(OSO,data=MTG_sub3,estimator="WLSMV")
summary(OSO.DWLSfit,fit.measures=T,standardized=T)

SSO2 <- 'Se=~Se9+Se7+Se4
Ss=~Se11+Se2+Se10
Pu=~Se16'
SSO2.DWLSfit <-cfa(SSO2,data=MTG_SSO,estimator="WLSMV")
summary(SSO2.DWLSfit,fit.measures=T,standardized=T)

#HIHER ORDER MODEL
SSOH <- 'Se=~Se9+Se7+Se4
Ss=~Se11+Se2+Se10
Pu=~Se16+Se14'
SSOH.DWLSfit <-cfa(SSOH,data=MTG_SSO,estimator="WLSMV")
SSOH.SBfit <-cfa(SSOH,data=MTG_sub3,test="Satorra.Bentler")
summary(SSOH.DWLSfit,fit.measures=T,standardized=T)
summary(SSOH.SBfit,fit.measures=T,standardized=T)

SH <-' sso=~Se9+Se7+Se4+Se11+Se2+Se10+Se16+Se14
oso=~So10+So11+So2+So5'
SH.SBfit <-cfa(SH,data=MTG_sub3,test="Satorra.Bentler")
summary(SH.SBfit,fit.measures=T,standardized=T)

SHM <-  'Se=~Se9+Se7+Se4
Ss=~Se11+Se2+Se10
Pu=~Se16+Se14
Os=~So10+So11+So2+So5'
SHM.SBfit <-cfa(SHM,data=MTG_sub3,test="Satorra.Bentler")
summary(SHM.SBfit,fit.measures=T,standardized=T)

MTG_fs <-MTG_nt[c(1:12)]
SH.SBfit <-cfa(SHM,data=MTG_fs,test="Satorra.Bentler")
summary(SH.SBfit,fit.measures=T,standardized=T)
SH.DWLSfit <-cfa(SH,data=MTG_fs,estimator="WLSMV")
summary(SH.DWLSfit,fit.measures=T,standardized=T)

#GENDER EQUIVELANCE-SSO
Mod0_SSO.fit <-cfa(SSO,data=MTG_SSO,estimator="WLSMV",group="Gender")
summary(Mod0_SSO.fit,fit.measures=T,standardized=T)
Mod1_SSO.fit <-cfa(SSO,data=MTG_SSO,estimator="WLSMV",group="Gender",group.equal=c("loadings"))
summary(Mod1_SSO.fit,fit.measures=T,standardized=T)
anova(Mod0_SSO.fit,Mod1_SSO.fit)

Mod2_SSO.fit <-cfa(SSO,data=MTG_SSO,estimator="WLSMV",group="Gender",group.equal=c("loadings", "intercepts"))
summary(Mod2_SSO.fit,fit.measures=T,standardized=T)
anova(Mod1_SSO.fit,Mod2_SSO.fit)

Mod3_SSO.fit <-cfa(SSO,data=MTG_SSO,estimator="WLSMV",group="Gender",group.equal=c("loadings", "intercepts","residuals"))
summary(Mod3_SSO.fit,fit.measures=T,standardized=T)
anova(Mod2_SSO.fit,Mod3_SSO.fit)

Mod4_SSO.fit <- cfa(SSO,data=MTG_SSO,estimator="WLSMV",group="Gender",
                          group.equal=c("loadings","intercepts", "residuals","lv.variances",
                                        "lv.covariances","means"))
summary(Mod4_SSO.fit,fit.measures=T,standardized=T)
anova(Mod3_SSO.fit,Mod4_SSO.fit)

#GENDER EQUIVELANCE-0SO
Mod0_OSO.fit <-cfa(OSO,data=MTG_OSO,estimator="WLSMV",group="Gender")
summary(Mod0_OSO.fit,fit.measures=T,standardized=T)
Mod1_OSO.fit <-cfa(OSO,data=MTG_OSO,estimator="WLSMV",group="Gender",group.equal=c("loadings"))
summary(Mod1_OSO.fit,fit.measures=T,standardized=T)
anova(Mod0_OSO.fit,Mod1_OSO.fit)

Mod2_OSO.fit <-cfa(OSO,data=MTG_OSO,estimator="WLSMV",group="Gender",group.equal=c("loadings", "intercepts"))
summary(Mod2_OSO.fit,fit.measures=T,standardized=T)
anova(Mod1_OSO.fit,Mod2_OSO.fit)

Mod3_OSO.fit <-cfa(OSO,data=MTG_OSO,estimator="WLSMV",group="Gender",group.equal=c("loadings", "intercepts","residuals"))
summary(Mod3_OSO.fit,fit.measures=T,standardized=T)
anova(Mod2_OSO.fit,Mod3_OSO.fit)

Mod4_OSO.fit <- cfa(OSO,data=MTG_OSO,estimator="WLSMV",group="Gender",
                    group.equal=c("loadings","intercepts", "residuals","lv.variances",
                                  "lv.covariances","means"))
summary(Mod4_OSO.fit,fit.measures=T,standardized=T)
anova(Mod3_OSO.fit,Mod4_OSO.fit)

write.csv(MTG_nt[c(1:12)],"mplus.csv")


#CLASS EQUIVELANCE-SSO
Mod0_SSO.fit <-cfa(SSO,data=MTG_SSO,estimator="WLSMV",group="Class")
summary(Mod0_SSO.fit,fit.measures=T,standardized=T)
Mod1_SSO.fit <-cfa(SSO,data=MTG_SSO,estimator="WLSMV",group="Class",group.equal=c("loadings"))
summary(Mod1_SSO.fit,fit.measures=T,standardized=T)
anova(Mod0_SSO.fit,Mod1_SSO.fit)

Mod2_SSO.fit <-cfa(SSO,data=MTG_SSO,estimator="WLSMV",group="Class",group.equal=c("loadings", "intercepts"))
summary(Mod2_SSO.fit,fit.measures=T,standardized=T)
anova(Mod1_SSO.fit,Mod2_SSO.fit)

Mod3_SSO.fit <-cfa(SSO,data=MTG_SSO,estimator="WLSMV",group="Class",group.equal=c("loadings", "intercepts","residuals"))
summary(Mod3_SSO.fit,fit.measures=T,standardized=T)
anova(Mod2_SSO.fit,Mod3_SSO.fit)

Mod4_SSO.fit <- cfa(SSO,data=MTG_SSO,estimator="WLSMV",group="Class",
                    group.equal=c("loadings","intercepts", "residuals","lv.variances",
                                  "lv.covariances","means"))
summary(Mod4_SSO.fit,fit.measures=T,standardized=T)
anova(Mod3_SSO.fit,Mod4_SSO.fit)

#CLASS EQUIVELANCE-0SO
Mod0_OSO.fit <-cfa(OSO,data=MTG_OSO,estimator="WLSMV",group="Class")
summary(Mod0_OSO.fit,fit.measures=T,standardized=T)
Mod1_OSO.fit <-cfa(OSO,data=MTG_OSO,estimator="WLSMV",group="Class",group.equal=c("loadings"))
summary(Mod1_OSO.fit,fit.measures=T,standardized=T)
anova(Mod0_OSO.fit,Mod1_OSO.fit)

Mod2_OSO.fit <-cfa(OSO,data=MTG_OSO,estimator="WLSMV",group="Class",group.equal=c("loadings", "intercepts"))
summary(Mod2_OSO.fit,fit.measures=T,standardized=T)
anova(Mod1_OSO.fit,Mod2_OSO.fit)

Mod3_OSO.fit <-cfa(OSO,data=MTG_OSO,estimator="WLSMV",group="Class",group.equal=c("loadings", "intercepts","residuals"))
summary(Mod3_OSO.fit,fit.measures=T,standardized=T)
anova(Mod2_OSO.fit.mean,Mod3_OSO.fit,mean)

Mod4_OSO.fit <- cfa(OSO,data=MTG_OSO,estimator="WLSMV",group="Class",
                    group.equal=c("loadings","intercepts", "residuals","lv.variances",
                                  "lv.covariances","means"))
summary(Mod4_OSO.fit,fit.measures=T,standardized=T)
anova(Mod3_OSO.fit,Mod4_OSO.fit)

MV<- 'MVC=~MVCr+MVC2+MVC3
MVH=~MVH1+MVH2+MVH3
MVS=~MVS1+MVS2+MVS3'
MV.SBfit <-cfa(MV,data=MTG_sub3,test="Satorra.Bentler",std.lv=T)
summary(MV.SBfit,fit.measures=T,standardized=T)

