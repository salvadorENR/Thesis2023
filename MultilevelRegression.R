#Loading packages
install.packages("nlme")                   
install.packages("lmerTest")              
install.packages("lme4")                 
install.packages("ggeffects")               
install.packages("stargazer")               
install.packages("texreg") 

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



#Loading data base
  DBT=read.csv("Data_set.csv",sep = ";")
  DBT=as.data.frame(DBT)
  attach(DBT)
  
#******************************* Analysis *******************************************************************************************************
  
  
  m7<-lme(STUDENTSCORE~SAR_SCORE+HMG_SCORE+TEACHERSCORE, random = ~1|ID_TEACHER, method = "ML", data = DBT)
  summary(m7)
  
  DBT %>% 
    group_by(ID_SCHOOL) %>% 
    summarise("mean HMG_SCORE" = mean(HMG_SCORE))
  
  DBT %>% 
  group_by(ID_SCHOOL) %>% 
    summarise("mean SAR_SCORE" = mean(SAR_SCORE))
  
  
  data <- data.frame(values = c(HMG_SCORE,SAR_SCORE,STUDENTSCORE,TEACHERSCORE),group = c(rep("HMG SCORE", 140),rep("SAR SCORE", 140),rep("STUDENT SCORE", 140),rep("TEACHER SCORE",140)))
  ggplot(data, aes(x = values, fill = group))+geom_histogram(breaks=c(0,1,2,3,4,5,6,7,8,9,10),position = "identity", alpha = 1,colour="Black")+ scale_fill_manual(values = c("#62FE96","#FE206B","#62278E","#0000FF"))
  
  data <- data.frame(values = c(HMG_SCORE,SAR_SCORE),group = c(rep("HMG SCORE", 140),rep("SAR SCORE", 140)))
  ggplot(data, aes(x = values, fill = group))+geom_histogram(breaks=c(0,1,2,3,4,5,6,7,8,9,10),position = "identity", alpha = 0.2,colour="Black")+ scale_fill_manual(values = c("#62FE96","#FE206B","#62278E","#0000FF"))
  
  
  data <- data.frame(values = c(STUDENTSCORE,TEACHERSCORE),group = c(rep("STUDENT SCORE", 140),rep("TEACHER SCORE",140)))
  ggplot(data, aes(x = values, fill = group))+geom_histogram(breaks=c(0,1,2,3,4,5,6,7,8,9,10),position = "identity", alpha = 1,colour="Black")+ scale_fill_manual(values = c("#62FE96","#FE206B","#62278E","#0000FF"))
  
  data <- data.frame(values = c(HMG_SCORE,SAR_SCORE,STUDENTSCORE),group = c(rep("HMG SCORE", 140),rep("SAR SCORE", 140),rep("STUDENT SCORE", 140)))
  ggplot(data, aes(x = values, fill = group))+geom_histogram(breaks=c(0,1,2,3,4,5,6,7,8,9,10),position = "identity", alpha = 1,colour="Black")+ scale_fill_manual(values = c("#62FE96","#FE206B","#62278E","#0000FF"))
  
  
  hist(HMG_SCORE,breaks=c(0,1,2,3,4,5,6,7,8,9,10))
  hist(SAR_SCORE,breaks=c(0,1,2,3,4,5,6,7,8,9,10))
  hist(STUDENTSCORE,breaks=c(0,1,2,3,4,5,6,7,8,9,10))
  hist(TEACHERSCORE,breaks=c(0,1,2,3,4,5,6,7,8,9,10))
  hist(HMG_TS1,breaks=c(0,1,2,3,4,5,6,7,8,9,10))
  hist(HMG_TS2,breaks=c(0,1,2,3,4,5,6,7,8,9,10))
  hist(HMG_TS3,breaks=c(0,1,2,3,4,5,6,7,8,9,10))
  hist(HMG_TS4,breaks=c(0,1,2,3,4,5,6,7,8,9,10))
  hist(HMG_TS5,breaks=c(0,1,2,3,4,5,6,7,8,9,10))
  hist(SCORE_PCK,breaks=c(0,2,4,6,8,10))
  hist(SCORE_SMK,breaks=c(0,2,4,6,8,10))
  
  
  
  
  plot(SCORE_SMK,SCORE_PCK)
  wilcox.test(SCORE_SMK,SCORE_PCK,alternative = "greater")
  df <- data.frame(HMG_TS1,HMG_TS2,HMG_TS3,HMG_TS4,HMG_TS5)
  colMeans(df)
  t.test(HMG_TS1,HMG_TS2)
  t.test(HMG_TS2,HMG_TS4)
  t.test(HMG_TS1,HMG_TS2)
  t.test(HMG_TS1,HMG_TS2)
  
  
  
  
  
  
#**************************************************************************************************************************************************************

  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
#Model1
m1 <- lme(STUDENTSCORE ~ 1, random = ~1|ID_TEACHER, na.action = "na.omit", method = "ML", data = DBT)
summary(m1)

#Model2
m2 <- lme(STUDENTSCORE ~TEACHERSCORE , random = ~1|ID_TEACHER, na.action = "na.omit", method = "ML", data = DBT)
summary(m2)

#Comparison between models 1 and 2
anova(m1, m2)

#Model3
m3 <- lme(STUDENTSCORE ~ TEACHERSCORE, random = ~TEACHERSCORE|ID_TEACHER, na.action = "na.omit", method = "ML", data =DBT,)
summary(m3)
#Comparison between models 2 and 3
anova(m2,m3)

#Model4
m4 <- lme(STUDENTSCORE ~TEACHERSCORE+SAR_SCORE + HMG_SCORE, random = ~TEACHERSCORE|ID_TEACHER, na.action = "na.omit", method = "ML", data = DBT)
summary(m4)
anova(m3,m4)

#model5
m5 <- lme(STUDENTSCORE ~ TEACHERSCORE + SAR_SCORE + HMG_SCORE + SAR_SCORE*TEACHERSCORE + HMG_SCORE*TEACHERSCORE, random = ~TEACHERSCORE|ID_TEACHER, method = "ML", data = DBT)
summary(m5)
anova(m4,m5)

#Final_Model
m6<- lme(STUDENTSCORE ~SCORE_PCK, random = ~1|ID_TEACHER, method = "ML", data = DBT)
m7<-lme(STUDENTSCORE~SCORE_SMK, random = ~1|ID_TEACHER, method = "ML", data = DBT)
summary(m6)
summary(m7)

ID_TEACHER




