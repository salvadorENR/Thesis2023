
install.packages("lavaan")                   
install.packages("semPlot")  
#loading packages
library(datos)
library(ggplot2)
library(reshape2)
library(ggsci)
library(readr)
library(scales)
library(stringr)
library(utf8)
library(devtools)
library(ggpubr)
library(nlme)                   
library(lmerTest)              
library(lme4)                 
library(ggeffects)               
library(stargazer)               
library(texreg)  
library(dplyr)
library("Hmisc")
library(lavaan)
library(semPlot)

y=read.csv("TEACHER_DATA.csv",sep = ";")
y=as.data.frame(y)
colnames(y)[1]="CCK_1"
attach(y)

#Modelo 1 (6 factores correlacionados)#
Modelo_1<-'
F1 =~ CCK_1 + CCK_2 + CCK_3 + CCK_4 + CCK_5
F2 =~ SCK_1 + SCK_2 + SCK_3 + SCK_4 + SCK_5 
F3 =~ HCK_1 + HCK_2 
F4 =~ KCS_1 + KCS_2
F5 =~ KCT_1 + KCT_2
F6 =~ KC_1 + KC_2
'

#Modelo 1 (2 factores correlacionados)#
Modelo_1<-'
F1 =~ Total_CCK + Total_SCK + Total_HCK
F2 =~ Total_KCS + Total_KCT + Total_KC 
'

#Modelo 2 (estructura Unifactorial)#
Modelo_2<-'
Bienestar =~ RP_1	+ RP_2 + RP_3	+ RP_4
            + C_1	+ C_2 + C_3	+ C_4
            + EP_1	+ EP_2 + EP_3	+ EP_4
'

#Análisis de una estructura con 2 factores
AFC_Modelo_1 <- cfa(Modelo_1,orthogonal=F, data=y, estimator="WLSMV", ordered ="FALSE")
summary(AFC_Modelo_1, fit.measures=TRUE, standardized=T, rsquare=T)
fitMeasures(AFC_Modelo_1, fit.measures = c("chisq", "df","srmr", "rmsea", "tli", "cfi"))
semPaths(AFC_Modelo_1, nCharNodes = 0,intercepts = FALSE, edge.label.cex=1.3, optimizeLatRes = T, groups = "lat",pastel = T, sizeInt=5,edge.color ="black",esize = 5, label.prop=0,sizeLat = 11,"std",layout="circle3", exoVar = F)



#Análisis de una estructura con 3 factores#
AFC_Modelo_1 <- cfa(Modelo_1,orthogonal=F, data=Base_AFC, estimator="ULS", ordered =names(Base_AFC))
summary(AFC_Modelo_1, fit.measures=TRUE, standardized=T, rsquare=T)
fitMeasures(AFC_Modelo_1, fit.measures = c("chisq", "df","srmr", "rmsea", "tli", "cfi"))
semPaths(AFC_Modelo_1, nCharNodes = 0,intercepts = FALSE, edge.label.cex=1.3, optimizeLatRes = T, groups = "lat",pastel = T, sizeInt=5,edge.color ="black",esize = 5, label.prop=0,sizeLat = 11,"std",layout="circle3", exoVar = F)

#Análisis de una estructura Unifactorial#
AFC_Modelo_2 <- cfa(Modelo_2,orthogonal=F, data=Base_AFC, estimator="uls", ordered =names(Base_AFC))
summary(AFC_Modelo_2, fit.measures=TRUE, standardized=T, rsquare=T)
fitMeasures(AFC_Modelo_2, fit.measures = c("chisq", "df","srmr", "rmsea", "tli", "cfi"))
semPaths(AFC_Modelo_2, nCharNodes = 0,intercepts = FALSE, edge.label.cex=1.3, optimizeLatRes = T, groups = "lat",pastel = T, sizeInt=5,edge.color ="blue",esize = 5, label.prop=0,sizeLat = 11,"std",layout="circle3", exoVar = F)

#Resumen de los índices de ajuste de los 2 modelos competidores#
resumen_fit <- rbind(fitmeasures(AFC_Modelo_1,fit.measures = c("chisq", "df","srmr", "tli", "cfi","rmsea","rmsea.ci.lower","rmsea.ci.upper")),
                     fitmeasures(AFC_Modelo_2, fit.measures = c("chisq", "df","srmr", "tli", "cfi","rmsea","rmsea.ci.lower","rmsea.ci.upper")))
rownames(resumen_fit) <- c("3 factores oblicuos", "Unifactorial")
round(resumen_fit, digits = 3)


disc <- vector("numeric")
i <- 1
for (i in 1:ncol(myScore$scored)){
  gU <- subset(myScore$scored, myScore$scored[,i] == 1)
  gL <- subset(myScore$scored, myScore$scored[,i] == 0 )
  disc[i] <- round((sum(gU)/nrow(gU) - sum(gL)/nrow(gL))/sd(myScore$score)*sqrt(diff[i]*(1-diff[i])), 3)
}



