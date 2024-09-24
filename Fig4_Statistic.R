remove(list=ls()) # Remove all variables from memory
require(gplots)   # Package with special plot functions
require(nlme)     # Package for Non-Linear mixed-effects models
library(car)      # Anova, levene.test, outlier.test
library(pwr)
library(nortest)  # ad.test, cvm.test
library(ARTool)
library(lsmeans)
library(phia)

OpenWindow = function (Width,Height) {
  if (Sys.info()["sysname"]=="Darwin"){  # (Darwin stands for a Mac computer)
    quartz(width=Width, height=Height)
  } else {
    windows(width = Width, height = Height)}
}

path=getwd()
setwd(path)
PlotData=read.csv(file='Root_Shoot_RAW.csv',header = T)
# Custom build standard error function
stderr <-function(x) sqrt(var(x,na.rm=T)/length(which(!is.na(x))))

PlotData$Exposure[PlotData$Exposure==1]<-"Unexposed"
PlotData$Exposure[PlotData$Exposure==2]<-"Exposed"

PlotData$Density[PlotData$Density==1]<-"High density"
PlotData$Density[PlotData$Density==2]<-"Low density"

PlotData$exposure=as.factor(PlotData$Exposure)
PlotData$groups=as.factor(PlotData$Density)

data1=PlotData[,15:17]
data1$MgBioInd <- PlotData$MgBioInd
data1$SRratio <- PlotData$Srratio

# --- Statistics --------------------------------------------------------------
# MgBioInd
cat("\n--- Testing for normality and homogeneity - Untransformed data\n")
m4.1<-lm(MgBioInd~groups*exposure, data=data1)
print(shapiro.test(resid(m4.1)))
OpenWindow(Width=7,Height=5); qqPlot(m4.1, main = "Untransformed data")
print(leveneTest(m4.1))
print(Anova(m4.1))

Values=data1$MgBioInd
Exposure=as.factor(PlotData$Exposure)
Density=as.factor(PlotData$Density)
M1=aov(Values~Exposure*Density)
summary(M1)
print(leveneTest(M1))  # if not significant, than variance is homogeneous (underlying assumption of ANOVA)
print(TukeyHSD(M1))


# --- Statistics --------------------------------------------------------------
# Shoot to root ratio
cat("\n--- Testing for normality and homogeneity - Untransformed data\n")
m4.2<-lm(SRratio~groups*exposure, data=data1)
print(shapiro.test(resid(m4.2)))
OpenWindow(Width=7,Height=5); qqPlot(m4.2, main = "Untransformed data")
print(leveneTest(m4.2))
print(Anova(m4.2))

# Value=data1$SRratio
# Exposure=as.factor(PlotData$Exposure)
# Density=as.factor(PlotData$Density)
# M2=aov(Value~Exposure*Density)
# summary(M2)
# print(leveneTest(M2))  # if not significant, than variance is homogeneous (underlying assumption of ANOVA)
# print(TukeyHSD(M2))


# N = art(SRratio ~ groups * exposure, data = data1)
# anova(N)
# lsmeans(artlm(N,"groups"), pairwise ~ groups)
# lsmeans(artlm(N,"exposure"), pairwise ~ exposure)
# 
# testInteractions(artlm(N, "groups:exposure"), pairwise=c("groups", "exposure"), adjustment="holm")
