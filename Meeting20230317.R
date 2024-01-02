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
library(plyr)

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
library(Hmisc)
library(plyr)
library(ggridges)
library(lavaan)
library(semPlot)
library(pastecs)
library(tidyverse)
library(insight)
library(shiny)
library(car)
library(fmsb)
library(gapminder)

#************************************ Loading the data *****************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************
data1=read.csv("DBS_SALVADOR_20230330.csv",sep=";")
data1=as.data.frame(data1)
attach(data1)
res2 <- rcorr(cbind(HMG_SCORE,SAR_SCORE,HMG_TS1,HMG_TS2,HMG_TS3,HMG_TS4,HMG_TS5,SAR_TS1,SAR_TS2,SAR_TS3,SAR_TS4), type = c("pearson","spearman"))
res2

rest<-rcorr(cbind(TEACHINGEXPERIENCE,STUDENTSCORE,TOTAL_KD,TOTAL_RAD,TOTAL_NRAD,TOTAL_RD,TOTAL_KD,TOTAL_RAD,TOTAL_NRAD,TOTAL_RD,TEACHERSCORE,SCORE_PCK,SCORE_SMK,HMG_SCORE,SAR_SCORE,HMG_TS1,HMG_TS2,HMG_TS3,HMG_TS4,HMG_TS5,SAR_TS1,SAR_TS2,SAR_TS3,SAR_TS4), type = c("pearson","spearman"))

model1<-lm(STUDENTSCORE ~ SAR_TS3, data=data1)
summary(model1)


#ggfreqScatter(STUDENTSCORE,HMG_SCORE)
#ggfreqScatter(STUDENTSCORE,SAR_SCORE)
#ggfreqScatter(STUDENTSCORE,TEACHERSCORE)
data2=read.csv("TEACHER_DATA.csv",sep=";")
data2=as.data.frame(data2)
colnames(data2)[1]="TEACHINGEXPERIENCE"


#********************************** Descriptive statistics *****************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************
theme_set(theme_ridges())
theme_set(theme_bw())
chart1 <- data.frame(Scores = c(HMG_SCORE,STUDENTSCORE,SAR_SCORE),Frequency= c(rep("HMS", length(STUDENTSCORE)),rep("TEST", length(STUDENTSCORE)),rep("SRQ-A", length(STUDENTSCORE))))
ggplot(chart1, aes(x = Scores, y = Frequency)) + geom_density_ridges(quantile_lines = TRUE, quantiles = 2,fill ="#F1948A",alpha = .5,)+coord_cartesian(clip = "off") + labs(title = 'Distribution of students scores')+theme_ridges(font_size = 30,grid =TRUE, line_size = 0.75, center_axis_labels = TRUE)+scale_x_continuous(breaks = c(0:10), limits = c(-.5, 13),expand = c(0, 0), name = "Scores")
estadistics=data.frame(STUDENTSCORE,SAR_SCORE,HMG_SCORE)
stat.desc(estadistics)
ggplot(chart1, aes(x = Scores, y = Frequency)) + geom_density_ridges(quantile_lines = TRUE, quantiles = 2,alpha = .5,)+coord_cartesian(clip = "off") + labs(title = 'Distribution of students scores')+theme_ridges(font_size = 30,grid =TRUE, line_size = 0.75, center_axis_labels = TRUE)+scale_x_continuous(breaks = c(0:10), limits = c(-.5, 13),expand = c(0, 0), name = "Scores")


chart2 <- data.frame(Scores = c(data2$TotalTestScore,data2$SCORE_SMK,data2$SCORE_PCK),Frequency = c(rep("CKTM", length(data2$TotalTestScore)),rep("SMK", length(data2$TotalTestScore)),rep("PCK", length(data2$TotalTestScore))))
ggplot(chart2, aes(x = Scores, y = Frequency)) + geom_density_ridges(quantile_lines = TRUE, quantiles = 2,fill ="#48C9B0",alpha = .5,)+coord_cartesian(clip = "off") + labs(title = 'Distribution of teachers scores')+theme_ridges(font_size = 30,grid =TRUE, line_size = 0.75, center_axis_labels = TRUE)+scale_x_continuous(breaks = c(0:10), limits = c(-.5, 13),expand = c(0, 0), name = "Scores")
estadistics1=data.frame(data2$TotalTestScore,data2$SCORE_SMK,data2$SCORE_PCK)
stat.desc(estadistics1)
ggplot(chart2, aes(x = Scores, y = Frequency)) + geom_density_ridges(quantile_lines = TRUE, quantiles = 2,alpha = .5,)+coord_cartesian(clip = "off") + labs(title = 'Distribution of teachers scores')+theme_ridges(font_size = 30,grid =TRUE, line_size = 0.75, center_axis_labels = TRUE)+scale_x_continuous(breaks = c(0:10), limits = c(-.5, 13),expand = c(0, 0), name = "Scores")

hist(data2$TEACHINGEXPERIENCE)
#Scatterplots
#SAR VS HMG
#English
d=data.frame(SAR_SCORE,HMG_SCORE)
d$pc <- predict(prcomp(~SAR_SCORE+HMG_SCORE, d))[,1]
ggplot(d, aes(SAR_SCORE, HMG_SCORE, color = pc)) +
  geom_point(shape = 16,size = 5, show.legend = FALSE) + ggtitle("Academic Self-Regulation Questionnaire Scores VS The Homework Management Scale Scores") +
  xlab("Academic Self-Regulation Questionnaire Scores") + ylab("The Homework Managament Scores")+geom_abline(intercept = 4.8672 , slope = 0.3355, color="red", linetype="dashed", size=1.5)+
  theme_minimal() +scale_color_gradient(low = "#D7E3FB", high = "#092C71")


#Spanish
d=data.frame(SAR_SCORE,HMG_SCORE)
d$pc <- predict(prcomp(~SAR_SCORE+HMG_SCORE, d))[,1]
ggplot(d, aes(SAR_SCORE, HMG_SCORE, color = pc)) +
  geom_point(shape = 16,size = 5, show.legend = FALSE) + ggtitle("Autorregulación académica VS Gestión de la tarea") +
  xlab("Autorregulación académica") + ylab("Gestión de la tarea")+geom_abline(intercept = 4.8672 , slope = 0.3355, color="red", linetype="dashed", size=1.5)+
  theme_minimal() +scale_color_gradient(low = "#D7E3FB", high = "#092C71")






ggplot(d, aes(SAR_SCORE, HMG_SCORE)) +
  geom_point(pch = 21,fill="gray",size = 5, show.legend = FALSE,color="black") + ggtitle("Academic Self-Regulation Questionnaire Scores VS The Homework Management Scale Scores") +
  xlab("Academic Self-Regulation Questionnaire scores") + ylab("The Homework Management Scale")+geom_abline(intercept = 4.8672 , slope = 0.3355, color="black", linetype="dashed", size=1.5)+
  theme_minimal() 


reg<-lm(HMG_SCORE~SAR_SCORE, data = mtcars)
reg
plot(TEACHINGEXPERIENCE,TOTAL_KD)
plot(TEACHINGEXPERIENCE,STUDENTSCORE)
res1 <- rcorr(cbind(SAR_SCORE,SAR_TS1,SAR_TS2,SAR_TS3,SAR_TS4,HMG_SCORE,HMG_TS1,HMG_TS2,HMG_TS3,HMG_TS4,HMG_TS5))
res1
res2 <- rcorr(cbind(TEACHINGEXPERIENCE,TOTAL_KD))
res2
res3 <- rcorr(cbind(TEACHINGEXPERIENCE,TEACHERSCORE,SCORE_PCK,SCORE_SMK))
res3
res4 <- rcorr(cbind(TEACHERSCORE,SCORE_PCK,SCORE_SMK,STUDENTSCORE,TOTAL_KD,TOTAL_RAD,TOTAL_NRAD,TOTAL_RD))
res4
res4 <- rcorr(cbind(TEACHERSCORE,STUDENTSCORE,HMG_SCORE,SAR_SCORE))
res4



plot(TEACHINGEXPERIENCE,SCORE_PCK)
plot(TEACHINGEXPERIENCE,SCORE_SMK)
plot(TEACHINGEXPERIENCE,TEACHERSCORE)
plot(SCORE_SMK,SAR_TS4)
plot(STUDENTSCORE,HMG_SCORE)

examinar=data.frame(STUDENTSCORE,HMG_SCORE,SAR_SCORE)


#SCORE VS SAR
  d=data.frame(SAR_SCORE,STUDENTSCORE)
  d$pc <- predict(prcomp(~SAR_SCORE+STUDENTSCORE, d))[,1]
  ggplot(d, aes(SAR_SCORE, STUDENTSCORE, color = pc)) +
    geom_point(shape = 16, size = 5, show.legend = FALSE) + ggtitle("SRQ-A VS TEST") +
    xlab("SRQ-A Scores") + ylab("Test scores")
  theme_minimal()
#SCORE VS HMG
  d=data.frame(HMG_SCORE,STUDENTSCORE)
  d$pc <- predict(prcomp(~HMG_SCORE+STUDENTSCORE, d))[,1]
  ggplot(d, aes(HMG_SCORE, STUDENTSCORE, color = pc)) +
    geom_point(shape = 16, size = 5, show.legend = FALSE) + ggtitle("HMS VS TEST") +
    xlab("HMS Scores") + ylab("Test scores")
  theme_minimal()  
  
  
#Teacher experience
  c2=c(2,7,3,0,3)
  c1=c("[1,6]","[7,12]","[13,18]","[19,24]","[25,30]")
  k=data.frame(c1,c2)
  par(las=1) #That represents the style of axis labels. (0=parallel, 1=all horizontal, 2=all perpendicular to axis, 3=all vertical)
  par(mar= c(5, 12, 4, 2) + 0.1) #Just increase the size of the left margin before you plot: par(mar= c(5, 10, 4, 2) + 0.1) (change the 10 value to suit your taste) 
  barra=barplot(k$c2,names.arg = c("[1,6]","[7,12]","[13,18]","[19,24]","[25,30]"),ylim=c(0,8),col = "#48C9B0",ylab = "Number of teachers",xlab="Years of teaching experience",cex.names=1.5,cex.axis=1.5,cex.lab=1.5)
  text(x=barra, y = k$c2, label = k$c2, pos = 3, cex = 1.5, col = "black") 
  
  #Radarchart
  Tapply(STUDENTSCORE ~ID_SCHOOL,mean,data=data1)
  Tapply(STUDENTSCORE ~ID_SCHOOL,median,data=data1)
  Tapply(STUDENTSCORE ~ID_SCHOOL,sd,data=data1)
  Tapply(TOTAL_KD ~ID_SCHOOL,median,data=data1)
  Tapply(TOTAL_RAD ~ID_SCHOOL,median,data=data1)
  Tapply(TOTAL_NRAD ~ID_SCHOOL,median,data=data1)
  Tapply(TOTAL_RD ~ID_SCHOOL,median,data=data1)
school1=c(3.8,0,0,0)
school2=c(2.5,0,0,0)
school3=c(0,0,0,0)
school4=c(5,0,0,0)
school5=c(2.5,0,0,0)
school6=c(2.5,0,0,0)
school7=c(0,0,0,0)
school8=c(2.5,0,0,0)
school9=c(7.5,5,0,0)
school10=c(2.5,0,0,0)
school11=c(2.5,0,0,0)
school12=c(10,5,0,0)
school13=c(3.8,0,0,0)
school14=c(2.5,0,0,0)
school15=c(5,0,0,0)

schools=data.frame(school1,school2,school3,school4,school5)
rownames(schools) <- c("Knowledge domain","Routine application","No routine application","Reasoning")
schools=t(schools)
schools=rbind(rep(10,4),rep(0,4),schools)
schools=as.data.frame(schools)

colors_border=c("#ef3a3a","#337755","#fecc4d","#0f2137","#c856a5")
colors_in=c("#ef3a3a","#337755","#fecc4d","#0f2137","#c856a5")

radarchart( schools  , axistype=1 , 
            #custom polygon
            pcol=colors_border, plwd=4 , plty=1,
            #custom the grid
            cglcol="black", cglty=4, axislabcol="black", caxislabels=seq(0,10,2.5), cglwd=0.8,
            #custom labels
            vlcex=1
)
legend(x=1.5, y=1, legend = c("School 1","School 2","School 3","School 4","School 5"), bty = "n", pch=20 , col=colors_in , text.col = "Black", cex=1.2, pt.cex=3)

schools=data.frame(school6,school7,school8,school9,school10)
rownames(schools) <- c("Knowledge domain","Routine application","No routine application","Reasoning")
schools=t(schools)
schools=rbind(rep(10,4),rep(0,4),schools)
schools=as.data.frame(schools)

colors_border=c("#ef3a3a","#337755","#fecc4d","#0f2137","#c856a5")
colors_in=c("#ef3a3a","#337755","#fecc4d","#0f2137","#c856a5")

radarchart( schools  , axistype=1 , 
            #custom polygon
            pcol=colors_border, plwd=4 , plty=1,
            #custom the grid
            cglcol="black", cglty=4, axislabcol="black", caxislabels=seq(0,10,2.5), cglwd=0.8,
            #custom labels
            vlcex=1
)
legend(x=1.5, y=1, legend = c("School 6","School 7","School 8","School 9","School 10"), bty = "n", pch=20 , col=colors_in , text.col = "Black", cex=1.2, pt.cex=3)

schools=data.frame(school11,school12,school13,school14,school15)
rownames(schools) <- c("Knowledge domain","Routine application","No routine application","Reasoning")
schools=t(schools)
schools=rbind(rep(10,4),rep(0,4),schools)
schools=as.data.frame(schools)

colors_border=c("#ef3a3a","#337755","#fecc4d","#0f2137","#c856a5")
colors_in=c("#ef3a3a","#337755","#fecc4d","#0f2137","#c856a5")

radarchart( schools  , axistype=1 , 
            #custom polygon
            pcol=colors_border, plwd=4 , plty=1,
            #custom the grid
            cglcol="black", cglty=4, axislabcol="black", caxislabels=seq(0,10,2.5), cglwd=0.8,
            #custom labels
            vlcex=1
)
legend(x=1.5, y=1, legend = c("School 11","School 12","School 13","School 14","School 15"), bty = "n", pch=20 , col=colors_in , text.col = "Black", cex=1.2, pt.cex=3)










#********************************Radar chart by teachers*********************************
Teacher1=c(10,0,6,5,6.3,5)
Teacher2=c(10,0,10,2.5,2.5,5)
Teacher3=c(10,5,10,2.5,1.3,5)
Teacher4=c(8,7.5,8,10,1.3,7.5)
Teacher5=c(10,10,10,7.5,10,10)
Teacher6=c(10,5,10,7.5,6.3,0)
Teacher7=c(10,10,10,2.5,10,5)
Teacher8=c(10,0,8,5,7.5,10)
Teacher9=c(10,0,10,2.5,3.75,10)
Teacher10=c(10,0,10,7.5,8.75,10)
Teacher11=c(10,10,9.2,7.5,10,10)
Teacher12=c(10,10,8,10,6.25,0)
Teacher13=c(10,10,9.2,5,8.75,10)
Teacher14=c(10,0,9.2,0,7.5,5)
Teacher15=c(10,0,8,0,6.25,5)

schools=data.frame(Teacher1)
schools=data.frame(Teacher2)
schools=data.frame(Teacher3)
schools=data.frame(Teacher4)
schools=data.frame(Teacher5)
schools=data.frame(Teacher6)
schools=data.frame(Teacher7)
schools=data.frame(Teacher8)
schools=data.frame(Teacher9)
schools=data.frame(Teacher10)
schools=data.frame(Teacher11)
schools=data.frame(Teacher12)
schools=data.frame(Teacher13)
schools=data.frame(Teacher14)
schools=data.frame(Teacher15)

rownames(schools) <- c("Common Content Knowledge","Knowledge at the Mathematical Horizon","Specialized Content Knowledge","Knowledge of Content and Students","Knowledge of Curriculum","Knowledge of Content and Teaching")
schools=t(schools)
schools=rbind(rep(10,6),rep(0,6),schools)
schools=as.data.frame(schools)

radarchart( schools  , axistype=1 , 
            #custom polygon
            pcol="#48C9B0",pfcol = scales::alpha("#48C9B0", 0.5),plwd=4 , plty=1,
            #custom the grid
            cglcol="black", cglty=4, axislabcol="black", caxislabels=seq(0,10,2.5), cglwd=0.8,
            #custom labels
            vlcex=1
)
#*********************************************************************************************************************************************************************************************************************************

#********************************Radar chart by schools*********************************
school1=c(3.8,0,0,0)
school2=c(2.5,0,0,0)
school3=c(0,0,0,0)
school4=c(5,0,0,0)
school5=c(2.5,0,0,0)
school6=c(2.5,0,0,0)
school7=c(0,0,0,0)
school8=c(2.5,0,0,0)
school9=c(7.5,5,0,0)
school10=c(2.5,0,0,0)
school11=c(2.5,0,0,0)
school12=c(10,5,0,0)
school13=c(3.8,0,0,0)
school14=c(2.5,0,0,0)
school15=c(5,0,0,0)
schools=data.frame(school1)
schools=data.frame(school2)
schools=data.frame(school3)
schools=data.frame(school4)
schools=data.frame(school5)
schools=data.frame(school6)
schools=data.frame(school7)
schools=data.frame(school8)
schools=data.frame(school9)
schools=data.frame(school10)
schools=data.frame(school11)
schools=data.frame(school12)
schools=data.frame(school13)
schools=data.frame(school14)
schools=data.frame(school15)

rownames(schools) <- c("Knowledge domain","Routine application","No routine application","Reasoning")
schools=t(schools)
schools=rbind(rep(10,4),rep(0,4),schools)
schools=as.data.frame(schools)

radarchart( schools  , axistype=1 , 
            #custom polygon
            pcol="#F1948A",pfcol = scales::alpha("#F1948A", 0.5),plwd=4 , plty=1,
            #custom the grid
            cglcol="black", cglty=4, axislabcol="black", caxislabels=seq(0,10,2.5), cglwd=0.8,
            #custom labels
            vlcex=1
)

#*********************************************************************************************************************************************************************************************************************************


#*********************************************General students*************************************************************************************************************************
KD=median(TOTAL_KD)
RAD=median(TOTAL_RAD)
NRAD=median(TOTAL_NRAD)
RD=median(TOTAL_RD)


subscales=data.frame(KD,RAD,NRAD,RD)
colnames(subscales) <- c("Knowledge Domain","Routine Application Domain","No Routine Application Domain","Reasoning Domain")
subscales=rbind(rep(10,4),rep(0,4),subscales)
subscales=as.data.frame(subscales)


radarchart( schools  , axistype=1 , 
            #custom polygon
            pcol="#2C64DD",pfcol = scales::alpha("#2C64DD", 0.5),plwd=4 , plty=1,
            #custom the grid
            cglcol="black", cglty=4, axislabcol="black", caxislabels=seq(0,10,2.5), cglwd=0.8,
            #custom labels
            vlcex=1
)

#*************************************************************************************************************************************************************************************************


#******************************************** General teachers ************************************************************************************************************************************

CCK=median(TOTAL_CCK)
HCK=median(TOTAL_HCK)
SCK=median(TOTAL_SCK)
KCS=median(TOTAL_KCS)
KC=median(TOTAL_KC)
KCT=median(TOTAL_KCT)
subscales=data.frame(CCK,HCK,SCK,KCS,KC,KCT)
colnames(subscales) <- c("Common Content Knowledge","Knowledge at the Mathematical Horizon","Specialized Content Knowledge","Knowledge of Content and Students","Knowledge of Curriculum","Knowledge of Content and Teaching")
subscales=rbind(rep(10,6),rep(0,6),subscales)
subscales=as.data.frame(subscales)
radarchart( subscales  , axistype=1 , 
            #custom polygon
            pcol="#2C64DD",pfcol = scales::alpha("#2C64DD", 0.5),plwd=4 , plty=1,
            #custom the grid
            cglcol="black", cglty=4, axislabcol="black", caxislabels=seq(0,10,2.5), cglwd=0.8,
            #custom labels
            vlcex=1
)
#**************************************************************************************************************************************************************************************************











#General radar plot of students' scores

KD=median(TOTAL_KD)
RAD=median(TOTAL_RAD)
NRAD=median(TOTAL_NRAD)
RD=median(TOTAL_RD)

kD=mean(TOTAL_KD)
RAD=mean(TOTAL_RAD)
NRAD=mean(TOTAL_NRAD)
RD=mean(TOTAL_RD)



subscales=data.frame(KD,RAD,NRAD,RD)
colnames(subscales) <- c("Knowledge Domain","Routine Application Domain","No Routine Application Domain","Reasoning Domain")
subscales=rbind(rep(10,4),rep(0,4),subscales)
subscales=as.data.frame(subscales)

radarchart( subscales  , axistype=1 , 
            #custom polygon
            pcol="black" , plwd=4 , plty=1,
            #custom the grid
            cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,10,2.5), cglwd=0.8,
            #custom labels
            vlcex=0.8 
)


Tapply(HMG_TS1,median,data=data1)
Organizing_the_Enviroment=median(HMG_TS1)
Time_management=median(HMG_TS2)
Motivation_control=median(HMG_TS3)
Emotional_control=median(HMG_TS4)
Distraction_management=median(HMG_TS5)

subscales=data.frame(Organizing_the_Enviroment,Time_management,Motivation_control,Emotional_control,Distraction_management)
colnames(subscales) <- c("Organizing the Enviroment,","Time management","Motivation control","Emotional control","Distraction management")
subscales=rbind(rep(10,5),rep(0,5),subscales)
subscales=as.data.frame(subscales)

radarchart( subscales  , axistype=1 , 
            #custom polygon
            pcol="#F1948A",pfcol = scales::alpha("#F1948A", 0.5),plwd=4 , plty=1,
            #custom the grid
            cglcol="black", cglty=4, axislabcol="black", caxislabels=seq(0,10,2.5), cglwd=0.8,
            #custom labels
            vlcex=1
)



external_regulation=median(SAR_TS1)
introjected_regulation=median(SAR_TS2)
identified_regulation=median(SAR_TS3)
instrinsic_motivation=median(SAR_TS4)

subscales2=data.frame(external_regulation,introjected_regulation,identified_regulation,instrinsic_motivation)
colnames(subscales2) <- c("External Regulation","Introjected Regulation","Identified Regulation","Instrinsic Motivation")
subscales2=rbind(rep(10,5),rep(0,5),subscales2)
subscales2=as.data.frame(subscales2)

radarchart( subscales2  , axistype=1 , 
            #custom polygon
            pcol="#F1948A",pfcol = scales::alpha("#F1948A", 0.5),plwd=4 , plty=1,
            #custom the grid
            cglcol="black", cglty=4, axislabcol="black", caxislabels=seq(0,10,2.5), cglwd=0.8,
            #custom labels
            vlcex=1
)


#Radar chart of teachers
#Radarchart
Tapply(TOTAL_CCK ~ID_SCHOOL,median,data=data1)
Tapply(TOTAL_HCK~ID_SCHOOL,median,data=data1)
Tapply(TOTAL_SCK ~ID_SCHOOL,median,data=data1)
Tapply(TOTAL_KCS ~ID_SCHOOL,median,data=data1)
Tapply(TOTAL_KC ~ID_SCHOOL,median,data=data1)
Tapply(TOTAL_KCT ~ID_SCHOOL,median,data=data1)
Teacher1=c(10,0,6,5,6.3,5)
Teacher2=c(10,0,10,2.5,2.5,5)
Teacher3=c(10,5,10,2.5,1.3,5)
Teacher4=c(8,7.5,8,10,1.3,7.5)
Teacher5=c(10,10,10,7.5,10,10)
Teacher6=c(10,5,10,7.5,6.3,0)
Teacher7=c(10,10,10,2.5,10,5)
Teacher8=c(10,0,8,5,7.5,10)
Teacher9=c(10,0,10,2.5,3.75,10)
Teacher10=c(10,0,10,7.5,8.75,10)
Teacher11=c(10,10,9.2,7.5,10,10)
Teacher12=c(10,10,8,10,6.25,0)
Teacher13=c(10,10,9.2,5,8.75,10)
Teacher14=c(10,0,9.2,0,7.5,5)
Teacher15=c(10,0,8,0,6.25,5)

schools=data.frame(Teacher1,Teacher2,Teacher3,Teacher4,Teacher5)
rownames(schools) <- c("Common Content Knowledge","Knowledge at the Mathematical Horizon","Specialized Content Knowledge","Knowledge of Content and Students","Knowledge of Curriculum","Knowledge of Content and Teaching")
schools=t(schools)
schools=rbind(rep(10,6),rep(0,6),schools)
schools=as.data.frame(schools)

colors_border=c("#ef3a3a","#337755","#fecc4d","#0f2137","#c856a5")
colors_in=c("#ef3a3a","#337755","#fecc4d","#0f2137","#c856a5")

radarchart( schools  , axistype=1 , 
            #custom polygon
            pcol=colors_border, plwd=4 , plty=1,
            #custom the grid
            cglcol="black", cglty=4, axislabcol="black", caxislabels=seq(0,10,2.5), cglwd=0.8,
            #custom labels
            vlcex=1
)

legend(x=1.75, y=1, legend = c("Teacher 1","Teacher 2","Teacher 3","Teacher 4","Teacher 5"), bty = "n", pch=20 , col=colors_in , text.col = "black", cex=1.2, pt.cex=3)



schools=data.frame(Teacher6,Teacher7,Teacher8,Teacher9,Teacher10)
rownames(schools) <- c("Common Content Knowledge","Knowledge at the Mathematical Horizon","Specialized Content Knowledge","Knowledge of Content and Students","Knowledge of Curriculum","Knowledge of Content and Teaching")
schools=t(schools)
schools=rbind(rep(10,6),rep(0,6),schools)
schools=as.data.frame(schools)

colors_border=c("#ef3a3a","#337755","#fecc4d","#0f2137","#c856a5")
colors_in=c("#ef3a3a","#337755","#fecc4d","#0f2137","#c856a5")

radarchart( schools  , axistype=1 , 
            #custom polygon
            pcol=colors_border, plwd=4 , plty=1,
            #custom the grid
            cglcol="black", cglty=4, axislabcol="black", caxislabels=seq(0,10,2.5), cglwd=0.8,
            #custom labels
            vlcex=1
)
legend(x=1.75, y=1, legend = c("Teacher 6","Teacher 7","Teacher 8","Teacher 9","Teacher 10"), bty = "n", pch=20 , col=colors_in , text.col = "black", cex=1.2, pt.cex=3)


schools=data.frame(Teacher11,Teacher12,Teacher13,Teacher14,Teacher15)
rownames(schools) <- c("Common Content Knowledge","Knowledge at the Mathematical Horizon","Specialized Content Knowledge","Knowledge of Content and Students","Knowledge of Curriculum","Knowledge of Content and Teaching")
schools=t(schools)
schools=rbind(rep(10,6),rep(0,6),schools)
schools=as.data.frame(schools)

colors_border=c("#ef3a3a","#337755","#fecc4d","#0f2137","#c856a5")
colors_in=c("#ef3a3a","#337755","#fecc4d","#0f2137","#c856a5")

radarchart( schools  , axistype=1 , 
            #custom polygon
            pcol=colors_border, plwd=4 , plty=1,
            #custom the grid
            cglcol="black", cglty=4, axislabcol="black", caxislabels=seq(0,10,2.5), cglwd=0.8,
            #custom labels
            vlcex=1
)
legend(x=1.75, y=1, legend = c("Teacher 11","Teacher 12","Teacher 13","Teacher 14","Teacher 15"), bty = "n", pch=20 , col=colors_in , text.col = "black", cex=1.2, pt.cex=3)



#General radar plot of teachers' scores

CCK=median(TOTAL_CCK)
HCK=median(TOTAL_HCK)
SCK=median(TOTAL_SCK)
KCS=median(TOTAL_KCS)
KC=median(TOTAL_KC)
KCT=median(TOTAL_KCT)

CCK=mean(TOTAL_CCK)
HCK=mean(TOTAL_HCK)
SCK=mean(TOTAL_SCK)
KCS=mean(TOTAL_KCS)
KCS=mean(TOTAL_KC)
KCS=mean(TOTAL_KCT)



subscales=data.frame(CCK,HCK,SCK,KCS,KC,KCT)
colnames(subscales) <- c("Common Content Knowledge","Knowledge at the Mathematical Horizon","Specialized Content Knowledge","Knowledge of Content and Students","Knowledge of Curriculum","Knowledge of Content and Teaching")
subscales=rbind(rep(10,6),rep(0,6),subscales)
subscales=as.data.frame(subscales)

radarchart( subscales  , axistype=1 , 
            #custom polygon
            pcol="black" , plwd=4 , plty=1,
            #custom the grid
            cglcol="black", cglty=3, axislabcol="black", caxislabels=seq(0,10,2.5), cglwd=0.8,
            #custom labels
            vlcex=0.8 
)


#Radar chart, MINED PLOTS
M=56.3
E=58.3
C=61.1
L=49.6



subscales=data.frame(M,E,C,L)
colnames(subscales) <- c("Mathematics","Social Study","Natural Science","Lenguage \n and Literature")
subscales=rbind(rep(100,4),rep(0,4),subscales)
subscales=as.data.frame(subscales)

radarchart( subscales  , axistype=1 , 
            #custom polygon
            pcol="black" , plwd=4 , plty=1,
            #custom the grid
            cglcol="black", cglty=3, axislabcol="black", caxislabels=seq(0,100,25), cglwd=0.8,
            #custom labels
            vlcex=0.8 
)

Pub=c(55.8,57.0,60.0,48.6)
Priv=c(59.5,65.4,67.1,55.4)

schools=data.frame(Pub,Priv)
rownames(schools) <- c("Mathematics","Social Study","Natural Science","Lenguage \n and Literature")
schools=t(schools)
schools=rbind(rep(100,4),rep(0,4),schools)
schools=as.data.frame(schools)

colors_border=c("#b2312b","#000080")
colors_in=c("#b2312b","#000080")

radarchart(schools, axistype=1 , 
            #custom polygon
            pcol=colors_border, plwd=4 , plty=1,
            #custom the grid
            cglcol="black", cglty=4, axislabcol="black", caxislabels=seq(0,100,25), cglwd=0.8,
            #custom labels
            vlcex=1
)

legend(x=1, y=1, legend = c("Public","Private"), bty = "n", pch=20 , col=colors_in , text.col = "black", cex=1.2, pt.cex=3)


#Line chart of Minstry of Education
Year=c(2015,2016,2017,2018,2019)
Scores=c(4.44,4.85,4.8,5.22,5.31)
ES=data.frame(Year,Scores)
ggplot(ES, aes(x = Year, y = Scores)) +
  geom_line(color = "#0099f9", size= 2) +
  geom_point(color = "#0099f9", size = 5) +
  geom_label(
    aes(label = Scores,),
    nudge_x = 0.15,
    nudge_y = 0.15,
    check_overlap = TRUE,
    size=7
  )+xlab("Years") + ylab("National average score")+ylim(4,6)+theme(text=element_text(size = 15),axis.text.y = element_text(size = 12))


#*********************************** Hypothesis testing *****************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************
#Wilcoxon test by school zone
#Hypothesis test1 and 2 
Tapply(SAR_SCORE ~ SCHOOL_ZONE,median,data=data1)
test <- wilcox.test(SAR_SCORE ~ SCHOOL_ZONE,data=data1,alternative = "greater",paired = FALSE)
test
Tapply(HMG_SCORE ~ SCHOOL_ZONE,median,data=data1)
test <- wilcox.test(HMG_SCORE ~ SCHOOL_ZONE,data=data1,alternative = "greater",paired = FALSE)
test
#Wilcoxon test by gender
#Hypothesis test 3 and 4 
Tapply(SAR_SCORE ~ GENDER,median,data=data1)
test <- wilcox.test(SAR_SCORE ~ GENDER,data=data1,alternative = "two.sided",paired = FALSE)
test
Tapply(HMG_SCORE ~ GENDER,median,data=data1)
test <- wilcox.test(HMG_SCORE ~ GENDER,data=data1,alternative = "greater",paired = FALSE)
test
#Wilcoxon test by school zone
#Hypothesis test 5
Tapply(STUDENTSCORE ~ SCHOOL_ZONE,median,data=data1)
test <- wilcox.test(STUDENTSCORE ~ SCHOOL_ZONE,data=data1,alternative = "less",paired = FALSE)
test
#Wilcoxon test by  gender
#Hypothesis test 6
Tapply(STUDENTSCORE ~ GENDER,median,data=data1)
test <- wilcox.test(STUDENTSCORE~GENDER,data=data1,alternative = "two.sided",paired = FALSE)
test
#Wilcoxon test by teacher PCK performance
#Hypothesis test 7

teacherPCK=function(var1){
  group=numeric()
  m=0
  for (i in 1:length(var1)) {
    if(var1[i]<=9){
      m=m+1
      group[m]=1
    }
    else{
      m=m+1
      group[m]=2
    }
  }
  group
}
groupV1=teacherPCK(SCORE_PCK)
Tapply(STUDENTSCORE ~ groupV1,median,data=data1)
test <- wilcox.test(STUDENTSCORE ~groupV1,data=data1,alternative = "less",paired = FALSE)
test
#Wilcoxon test by teacher experience
#Hypothesis test 8
teacherEXP=function(var1){
  group=numeric()
  m=0
  for (i in 1:length(var1)) {
    if(var1[i]<=15){
      m=m+1
      group[m]=1
    }
    else{
      m=m+1
      group[m]=2
    }
    
  }
  group
}
groupV2=teacherEXP(TEACHINGEXPERIENCE)
Tapply(STUDENTSCORE ~ groupV2,median,data=data1)
Tapply(SCORE_SMK ~ groupV2,median,data=data1)
Tapply(SCORE_PCK ~ groupV2,median,data=data1)
Tapply(SAR_SCORE ~ groupV2,median,data=data1)
Tapply(HMG_SCORE ~ groupV2,median,data=data1)

test <- wilcox.test(STUDENTSCORE~groupV2,data=data1,alternative = "less",paired = FALSE)
test
test <- wilcox.test(SCORE_SMK~groupV2,data=data1,alternative = "less",paired = FALSE)
test
test <- wilcox.test(SCORE_PCK~groupV2,data=data1,alternative = "less",paired = FALSE)
test
test <- wilcox.test(SAR_SCORE~groupV2,data=data1,alternative = "greater",paired = FALSE)
test
test <- wilcox.test(HMG_SCORE~groupV2,data=data1,alternative = "less",paired = FALSE)
test
#test score, SMK, PCK, homework, Self-regulation, homework management










#Wilcoxon test by teacher SMK performance
#Hypothesis test 9

teacherSMK=function(var1){
  group=numeric()
  m=0
  for (i in 1:length(var1)) {
    if(var1[i]<=8.3){
      m=m+1
      group[m]=1
    }
    else{
      m=m+1
      group[m]=2
    }
  }
  group
}
groupV1=teacherSMK(SCORE_SMK)
Tapply(STUDENTSCORE ~ groupV1,median,data=data1)
test <- wilcox.test(STUDENTSCORE ~groupV1,data=data1,alternative = "greater",paired = FALSE)
test

table(SCORE_SMK)

#Linear regression model
table(SCORE_SMK)
table(SCORE_PCK)



plot(SCORE_SMK,SCORE_PCK)





#8












#Seydou

Pret=c(14.5,8,8.5,8,4,9.25,10.5,12,10,12.5,13,12.5,9,11,10.5,12,7.5,5,13,10)
Post=c(17,18.25,13.5,17,15.25,16.5,18.5,15.5,18,18.5,14.75,18.5,17,15,16.5,14.5,14.5,11,16.75,15.5)


theme_set(theme_ridges())
data <- data.frame(Scores = c(Pret,Post),Frequency= c(rep("Pret", length(Post)),rep("Post", length(Post))))
ggplot(data, aes(x = Scores, y = Frequency)) + geom_density_ridges(fill ="lightblue",alpha = .5) +scale_x_continuous(expand = c(0, 0)) +scale_y_discrete(expand = expand_scale(mult = c(0.01, .7)))+coord_cartesian(clip = "off") + labs(title = 'Distribution of trainee teachers scores')+theme_ridges(font_size = 50,grid = FALSE, center_axis_labels = TRUE)#scale_fill_manual(values = c("#00AFBB", "#E7B800", "#FC4E07"))+ 

estadistics=data.frame(Pret,Post)
stat.desc(estadistics)


test <- wilcox.test(FataFrameFemale$studentscoref,FataFrameMale$studentscoref,alternative = "two.sided",paired = FALSE)
test

#Test1
PreT1=c(3,2,2.5,2,0,2,2.5,3,2.5,3,2.5,3,3.5,2.5,2,3,1,1,3.5,2)
PosT1=c(4,4.5,3.5,4,3,4,4.5,4,4.5,4,4.5,4.5,4,4.5,4.5,4,3.5,3,4.5,3.5)

test1 <- t.test(PosT1, PreT1,alternative = "greater",paired = TRUE)
test1
#Test2
PreT2=c(8.5,3.5,4,5,2.5,5,5,6,5.75,6,5.75,7.5,4.5,5,6.75,6.5,4,2.75,7,5.75)
PosT2=c(9,8.75,6,9,9,8.25,9,7.75,9.25,9,6.5,9.25,8.5,7,8.75,7,7,5,8.25,8.75)

test2 <- t.test(PosT2, PreT2,alternative = "greater",paired = TRUE)
test2
#Test3
PreT3=c(3,2.5,2,1,1.5,2.25,3,3,1.75,3.5,3.25,2.5,2.5,3.5,1.75,2.5,2.5,1.25,3,2.25)
PosT3=c(5,5,4,4,3.25,4.25,5,4,4.25,5,4.25,4.75,4,4,3.25,3.5,4,3,4,3.5)

test3 <- t.test(PosT3, PreT3,alternative = "greater",paired = TRUE)
test3




#Separación de los puntajes

separar1=function(var1,var2){
  MSLow=numeric()
  MSHig=numeric()
  SARLow=numeric()
  SARHig=numeric()
  m=0
  n=0
  for (i in 1:length(var1)) {
    if(var1[i]<6){
      m=m+1
      MSLow[m]=var1[i]
      SARLow[m]=var2[i]
    }
    else{
      n=n+1
      MSHig[n]=var1[i]
      SARHig[n]=var2[i]
    }
  }
  framelow=data.frame(MSLow,SARLow)
  #framehig=data.frame(MSHig,SARHig)
  #show(framelow)
  #show(framehig)
  #corrLow=cor(MSLow,SARLow)
  #corrHig=cor(MSHig,SARHig)
  #Coeficientes=c(corrLow,corrHig)
  }

Prueba1=separar1(STUDENTSCORE,SAR_SCORE)
Prueba1
res1 <- rcorr(Prueba1$MSLow,Prueba1$SARLow)
res1

separar2=function(var1,var2){
  MSLow=numeric()
  MSHig=numeric()
  SARLow=numeric()
  SARHig=numeric()
  m=0
  n=0
  for (i in 1:length(var1)) {
    if(var1[i]<6){
      m=m+1
      MSLow[m]=var1[i]
      SARLow[m]=var2[i]
    }
    else{
      n=n+1
      MSHig[n]=var1[i]
      SARHig[n]=var2[i]
    }
  }
  framelow=data.frame(MSHig,SARHig)
  #framehig=data.frame(MSHig,SARHig)
  #show(framelow)
  #show(framehig)
  #corrLow=cor(MSLow,SARLow)
  #corrHig=cor(MSHig,SARHig)
  #Coeficientes=c(corrLow,corrHig)
}
Prueba2=separar2(STUDENTSCORE,SAR_SCORE)
Prueba2
res2 <- rcorr(Prueba2$MSHig,Prueba2$SARHig)
res2

#Factorial analysis
#Modelo p (2 factores correlacionados)#
Model_1<-'
F1 =~ SCORE_PCK+SCORE_SMK
F2 =~ TOTAL_KD+TOTAL_RAD+TOTAL_NRAD+TOTAL_RD
F2~F1
'

AFC_Model_1 <- cfa(Model_1,orthogonal=F, data=data1, estimator="WLSMV", ordered ="FALSE")
summary(AFC_Model_1, fit.measures=TRUE, standardized=T, rsquare=T)
fitMeasures(AFC_Model_1, fit.measures = c("chisq", "df","srmr", "rmsea", "tli", "cfi"))
semPaths(AFC_Model_1, nCharNodes = 0,intercepts = FALSE, edge.label.cex=1.3, optimizeLatRes = T, groups = "lat",pastel = T, sizeInt=5,edge.color ="black",esize = 5, label.prop=0,sizeLat = 11,"std",layout="circle3", exoVar = F)


Model_1<-'
F1 =~ HMG_TS1 + HMG_TS2 + HMG_TS3 + HMG_TS4 + HMG_TS5
'

AFC_Model_1 <- cfa(Model_1,orthogonal=F, data=data1, estimator="WLSMV", ordered ="FALSE")
summary(AFC_Model_1, fit.measures=TRUE, standardized=T, rsquare=T)
fitMeasures(AFC_Model_1, fit.measures = c("chisq", "df","srmr", "rmsea", "tli", "cfi"))
semPaths(AFC_Model_1, nCharNodes = 0,intercepts = FALSE, edge.label.cex=1.3, optimizeLatRes = T, groups = "lat",pastel = T, sizeInt=5,edge.color ="black",esize = 5, label.prop=0,sizeLat = 11,"std",layout="circle3", exoVar = F)



Model_2<-'
F1 =~ HMG_TS1 + HMG_TS2 + HMG_TS3 + HMG_TS4 + HMG_TS5
F2 =~ SAR_TS1 + SAR_TS2 + SAR_TS3 + SAR_TS4
'

AFC_Model_2 <- cfa(Model_2,orthogonal=F, data=data1, estimator="WLSMV", ordered ="FALSE")
summary(AFC_Model_2, fit.measures=TRUE, standardized=T, rsquare=T)
fitMeasures(AFC_Model_2, fit.measures = c("chisq", "df","srmr", "rmsea", "tli", "cfi"))
semPaths(AFC_Model_2, nCharNodes = 0,intercepts = FALSE, edge.label.cex=1.3, optimizeLatRes = T, groups = "lat",pastel = T, sizeInt=5,edge.color ="black",esize = 5, label.prop=0,sizeLat = 11,"std",layout="circle3", exoVar = F)

Tapply(STUDENTSCORE ~ ID_SCHOOL,median,data=data1)
Tapply(STUDENTSCORE ~ ID_SCHOOL,mean,data=data1)
Tapply(STUDENTSCORE ~ ID_SCHOOL,var,data=data1)


head(data1)


Matemedible=STUDENTSCORE
HMGRAW=HMG_1+HMG_2+HMG_3+HMG_4+HMG_5+HMG_6+HMG_7+HMG_8+HMG_9+HMG_10+HMG_11+HMG_12+HMG_13+HMG_14+HMG_15+HMG_16+HMG_17+HMG_18R+HMG_19R+HMG_20R+HMG_21R+HMG_22R
SARRAW=SAR_1+SAR_2+SAR_3+SAR_4+SAR_5+SAR_6+SAR_7+SAR_8+SAR_9+SAR_10+SAR_11+SAR_12+SAR_13+SAR_14+SAR_15+SAR_16+SAR_17+SAR_18+SAR_19+SAR_20+SAR_21+SAR_22+SAR_23+SAR_24+SAR_25+SAR_26+SAR_27+SAR_28+SAR_29+SAR_30+SAR_31+SAR_32
Profespecial=HCK_1 +HCK_2 +KCS_1+ KCS_2 +KCT_1 +KCT_2+ KC_1 +KC_2
Tarea=HMGRAW
Regulation=SARRAW
data1$Profespecial=Profespecial
data1$Tarea=Tarea
data1$Regulacion=Regulacion


Matemedible=STUDENTSCORE
Tarea=HMG_1+HMG_2+HMG_3+HMG_4+HMG_5+HMG_6+HMG_7+HMG_8+HMG_9+HMG_14+HMG_15+HMG_16+HMG_17
Regulacion=SAR_1+SAR_4+SAR_10+SAR_12+SAR_17+SAR_18+SAR_26+SAR_29+SAR_31+SAR_5+SAR_8+SAR_11+SAR_16+SAR_21+SAR_23+SAR_30
Profespecial=KCT_1 +KCT_2+CCK_1+CCK_2+CCK_3+CCK_4+CCK_5+SCK_1+SCK_2+SCK_3+SCK_4+SCK_5
#Profespecial=KCS_1+ KCS_2 +KCT_1 +KCT_2+ KC_1 +KC_2+HCK_2+HCK_1
data1$Profespecial=Profespecial
data1$Tarea=Tarea
data1$Regulacion=Regulacion


Gstudents=function(var1,var2,var3,var4){
  GSCORE=numeric()
  GHMG=numeric()
  GSAR=numeric()
  GTEACH=numeric()
  m=0
  for (i in 1:length(var1)) {
    if(var1[i]>=0){
      m=m+1
      GSCORE[m]=var1[i]
      GHMG[m]=var2[i]
      GSAR[m]=var3[i]
      GTEACH[m]=var4[i]
    }
    
  }
  
  x <- cbind(GSCORE,GHMG, GSAR, GTEACH)
  x
}
GstudentsFrame=as.data.frame(Gstudents(STUDENTSCORE,Tarea,Regulacion,Profespecial))

Matemedible=GstudentsFrame$GSCORE
Tarea=GstudentsFrame$GHMG
Regulacion=GstudentsFrame$GSAR
Profesores=GstudentsFrame$GTEACH

res5=rcorr(cbind(Matemedible,Tarea,Regulacion,Profesores))
res5

Matemedible=STUDENTSCORE
OrgEnviroment=HMG_1+HMG_2+HMG_3+HMG_4+HMG_5
TimeManagement=HMG_6+HMG_7+HMG_8+HMG_9
EmotionalControl=HMG_14+HMG_15+HMG_16+HMG_17
IntrojectedRegulation=SAR_1+SAR_4+SAR_10+SAR_12+SAR_17+SAR_18+SAR_26+SAR_29+SAR_31
IdentifiedRegulation=SAR_5+SAR_8+SAR_11+SAR_16+SAR_21+SAR_23+SAR_30
CommonContentK=CCK_1+CCK_2+CCK_3+CCK_4+CCK_5
SpecializedContentK=SCK_1+SCK_2+SCK_3+SCK_4+SCK_5
KContentTeaching=KCT_1 +KCT_2
#Profespecial=KCS_1+ KCS_2 +KCT_1 +KCT_2+ KC_1 +KC_2+HCK_2+HCK_1
data1$OrgEnviroment=OrgEnviroment
data1$TimeManagement=TimeManagement
data1$EmotionalControl=EmotionalControl
data1$IntrojectedRegulation=IntrojectedRegulation
data1$IdentifiedRegulation=IdentifiedRegulation
data1$CommonContentK=CommonContentK
data1$SpecializedContentK=SpecializedContentK
data1$KContentTeaching=KContentTeaching


var2=data1$OrgEnviroment
var3=data1$TimeManagement
var4=data1$EmotionalControl
var5=data1$IntrojectedRegulation
var6=data1$IdentifiedRegulation
var7=data1$CommonContentK
var8=data1$SpecializedContentK
var9=data1$KContentTeaching
res5=rcorr(cbind(Matemedible,var2,var3,var4,var5,var6,var7,var8,var9))
res5



var(SAR_TS4)
var(SAR_TS3)
var(SAR_TS2)
var(SAR_TS1)

var(HMG_TS1)
var(HMG_TS2)
var(HMG_TS3)
var(HMG_TS4)
var(HMG_TS5)



res5=rcorr(cbind(Matemedible,SAR_TS1,SAR_TS2,SAR_TS3,SAR_TS4,HMG_TS1,HMG_TS2,HMG_TS3,HMG_TS4,HMG_TS5))
res5

plot(HMG_SCORE,SAR_SCORE)






#SCORE VS SAR
p=data.frame(STUDENTSCORE,SAR_SCORE)
ggplot(p, aes(STUDENTSCORE,SAR_SCORE)) +
  geom_point(pch = 21,fill="gray",size = 5, show.legend = FALSE,color="black") + ggtitle("Mathematics test scores vs Academic Self-Regulation Questionnaire Scores") +
  xlab("Mathematics test scores") + ylab("Academic Self-Regulation Questionnaire Scores")+
  ylim(0,10)+annotate("text", x=4.7, y=6.5, label="Low scores", angle=90, size=7, color="blue") +
  annotate("text", x=5.3, y=6.5, label="High scores", angle=90, size=7, color="red")+annotate("text", x=1.25, y=4.7, label="Low scores", angle=0, size=7, color="blue") +
  annotate("text", x=1.25, y=5.3, label="High scores", angle=0, size=7, color="red")+geom_vline(xintercept = 5,colour="black", linetype = "longdash",size=1)+geom_hline(yintercept = 5,colour="black", linetype = "longdash",size=1)+theme_minimal() 

#SCORE VS HMG
p=data.frame(STUDENTSCORE,HMG_SCORE)
ggplot(p, aes(STUDENTSCORE,HMG_SCORE)) +
  geom_point(pch = 21,fill="gray",size = 5, show.legend = FALSE,color="black") + ggtitle("Mathematics test scores vs The Homework Management Scale Scores") +
  xlab("Mathematics test scores vs ") + ylab("The Homework Management Scale Scores")+
  ylim(0,10)+annotate("text", x=4.7, y=6.5, label="Low scores", angle=90, size=7, color="blue") +
  annotate("text", x=5.3, y=6.5, label="High scores", angle=90, size=7, color="red")+annotate("text", x=1.25, y=4.7, label="Low scores", angle=0, size=7, color="blue") +
  annotate("text", x=1.25, y=5.3, label="High scores", angle=0, size=7, color="red")+geom_vline(xintercept = 5,colour="black", linetype = "longdash",size=1)+geom_hline(yintercept = 5,colour="black", linetype = "longdash",size=1)+theme_minimal() 



#SCORE VS SAR
d=data.frame(SAR_SCORE,STUDENTSCORE)
d$pc <- predict(prcomp(~SAR_SCORE+STUDENTSCORE, d))[,1]
ggplot(d, aes(SAR_SCORE, STUDENTSCORE, color = pc)) +
  geom_point(shape = 16, size = 5, show.legend = FALSE) + ggtitle("SRQ-A VS TEST") +
  xlab("SRQ-A Scores") + ylab("Test scores")
theme_minimal()
#SCORE VS HMG
d=data.frame(HMG_SCORE,STUDENTSCORE)
d$pc <- predict(prcomp(~HMG_SCORE+STUDENTSCORE, d))[,1]
ggplot(d, aes(HMG_SCORE, STUDENTSCORE, color = pc)) +
  geom_point(shape = 16, size = 5, show.legend = FALSE) + ggtitle("HMS VS TEST") +
  xlab("HMS Scores") + ylab("Test scores")
theme_minimal()  

estadistics1=data.frame(HMG_TS1,HMG_TS2,HMG_TS3,HMG_TS4,HMG_TS5,SAR_TS1,SAR_TS2,SAR_TS3,SAR_TS4)
stat.desc(estadistics1)




df <- data.frame(Years=c("2018", "2019", "2020", "2021", "2022"),Per=c(28.57,24.96,21.42,39.23,20.43))
head(df)
# Basic line plot with points
ggplot(data=df, aes(x=Years, y=Per, group=1))+geom_line(size=1)+geom_point(size=2.5)+ggtitle("BAC GÉNÉRAL- REUSSITE TOTAL")+  xlab("Years") + ylab("Percentages")+
  ylim(0, 100)+theme(text=element_text(size = 20),axis.text.y = element_text(size = 20),panel.background = element_rect(fill = "lightblue",colour = "white",size = 0.5, linetype = "solid"),panel.grid.major = element_line(size = 0.0, linetype = 'solid',colour = "black"), panel.grid.minor = element_line(size = 0.25, linetype = 'solid',colour = "lightblue"),panel.grid.major.x = element_blank())


Year=c(2018,2019,2020,2021,2022)
Scores=c(28.57,24.96,21.42,39.23,20.43)
ES=data.frame(Year,Scores)
ggplot(ES, aes(x = Year, y = Scores)) +
  geom_line(color = "#0099f9", size= 1.5) +
  geom_point(color = "#0099f9", size = 3.5) +
  geom_label(
    aes(label = Scores,),
    nudge_x = 0,
    nudge_y = 4,
    check_overlap = TRUE,
    size=3
  )+ggtitle("BAC GÉNÉRAL- REUSSITE TOTAL")+xlab("Years") + ylab("Percentages")+ylim(0,50)+theme(text=element_text(size = 12),axis.text.y = element_text(size = 12),axis.text.x = element_text(size = 12))




#Bah
Q1=c("Always","Often","Always","Always","Often","Never","Occasionally","Sometimes","Sometimes",
     "Often","Often","Sometimes","Always",
     "Often","Often","Often","Often","Occasionally","Sometimes","Often","Often")
k=data.frame(table(Q1))
par(las=1) #That represents the style of axis labels. (0=parallel, 1=all horizontal, 2=all perpendicular to axis, 3=all vertical)
par(mar= c(5, 12, 4, 2) + 0.1) #Just increase the size of the left margin before you plot: par(mar= c(5, 10, 4, 2) + 0.1) (change the 10 value to suit your taste) 
barra=barplot(k$Freq,names.arg = c("Never","Occasionally","Sometimes","Often","always"),ylim=c(0,12),col = "lightblue",ylab = "Number of teachers",xlab = "Frequency of application",border = TRUE,cex.names=1.2,cex.axis=1.2,cex.lab=1.2)
text(x=barra, y = k$Freq, label = k$Freq, pos = 3, cex = 1, col = "black") 

Q2=c("Mixed-ability",
     "Desk location",
     "Mixed-ability",
     "Mixed-ability",
     "Desk location",
     "Mixed-ability",
     "Mixed-ability",
     "Class list and gender",
     "Desk location",
     "Mixed-ability",
     "Desk location",
     "Class list and gender",
     "Class list and gender",
     "Mixed-ability",
     "Mixed-ability",
     "Desk location",
     "Mixed-ability",
     "Class list and gender",
     "Mixed-ability",
     "Desk location",
     "Mixed-ability"
)
k1=data.frame(table(Q2))
extras=data.frame(Q2=c("High-performing students","Proficiency"),Freq=c(0,0))
k=rbind(k1,extras)
par(las=1) #That represents the style of axis labels. (0=parallel, 1=all horizontal, 2=all perpendicular to axis, 3=all vertical)
par(mar= c(5, 12, 4, 2) + 0.1) #bottom margin, left margin, top margin, right margin.Just increase the size of the left margin before you plot: par(mar= c(5, 10, 4, 2) + 0.1) (change the 10 value to suit your taste) 
barra=barplot(k$Freq,names.arg = c("High-performing students"," Proficiency","Desk location","Mixed-ability","Class list and gender"),ylim=c(0,12),col = "lightblue",ylab = "Number of teachers",xlab = "Grouping Strategies",border = TRUE,cex.names=1.5,cex.axis=1.5,cex.lab=1.5)
text(x=barra, y = k$Freq, label = k$Freq, pos = 3, cex = 1.5, col = "black") 


Q3=c("Very effective",
     "Very effective",
     "Very effective",
     "Very effective",
     "Effective",
     "Fairly effective",
     "Fairly effective",
     "Effective",
     "Effective",
     "Very effective",
     "Effective",
     "Effective",
     "Effective",
     "Very effective",
     "Fairly effective",
     "Fairly effective",
     "Fairly effective",
     "Fairly effective",
     "Effective",
     "Fairly effective",
     "Very effective"
)

k1=data.frame(table(Q3))
extras=data.frame(Q3=c("Not effective"),Freq=c(0))
k=rbind(k1,extras)
par(las=1) #That represents the style of axis labels. (0=parallel, 1=all horizontal, 2=all perpendicular to axis, 3=all vertical)
par(mar= c(5, 12, 4, 2) + 0.1) #bottom margin, left margin, top margin, right margin.Just increase the size of the left margin before you plot: par(mar= c(5, 10, 4, 2) + 0.1) (change the 10 value to suit your taste) 
barra=barplot(k$Freq,names.arg = c("Not effective","Fairly effective","Effective","Very effective"),ylim=c(0,8),col = "lightblue",ylab = "Number of teachers",xlab = "Effectiveness of the group work activities",border = TRUE,cex.names=1.5,cex.axis=1.5,cex.lab=1.5)
text(x=barra, y = k$Freq, label = k$Freq, pos = 3, cex = 1.5, col = "black") 


Q4=c("Very motivated",
    "Very motivated",
    "Motivated",
    "Motivated",
    "Motivated",
    "Fairly motivated",
    "Motivated",
    "Fairly motivated",
    "Motivated",
    "Motivated",
    "Motivated",
    "Motivated",
    "Motivated",
    "Motivated",
    "Motivated",
    "Fairly motivated",
    "Fairly motivated",
    "Fairly motivated",
    "Motivated",
    "Fairly motivated",
    "Motivated"
)

k=data.frame(table(Q4))
par(las=1) #That represents the style of axis labels. (0=parallel, 1=all horizontal, 2=all perpendicular to axis, 3=all vertical)
par(mar= c(5, 12, 4, 2) + 0.1) #bottom margin, left margin, top margin, right margin.Just increase the size of the left margin before you plot: par(mar= c(5, 10, 4, 2) + 0.1) (change the 10 value to suit your taste) 
barra=barplot(k$Freq,names.arg = c("Fairly motivated","Motivated","Very motivated"),ylim=c(0,15),col = "lightblue",ylab = "Number of teachers",xlab = "Students' motivation during group work activies",border = TRUE,cex.names=1.5,cex.axis=1.5,cex.lab=1.5)
text(x=barra, y = k$Freq, label = k$Freq, pos = 3, cex = 1.5, col = "black") 


c2=c(0,2,22,2,0)
c1=c("Under 25","26-35","36-45","46-55","Over 55")
k=data.frame(c1,c2)
par(las=1) #That represents the style of axis labels. (0=parallel, 1=all horizontal, 2=all perpendicular to axis, 3=all vertical)
par(mar= c(5, 12, 4, 2) + 0.1) #Just increase the size of the left margin before you plot: par(mar= c(5, 10, 4, 2) + 0.1) (change the 10 value to suit your taste) 
barra=barplot(k$c2,names.arg = c("Under 25","26-35","36-45","46-55","Over 55"),ylim=c(0,25),col = "lightblue",xlab="Age of teachers",ylab = "Number of teachers",border = TRUE,cex.names=1.5,cex.axis=1.5,cex.lab=1.5)
text(x=barra, y = k$c2, label = k$c2, pos = 3, cex = 1, col = "black")


#Graph 2
c2=c(3,19,4)
c1=c("Civil servant","Community official","Contractual")
k=data.frame(c1,c2)
par(las=1) #That represents the style of axis labels. (0=parallel, 1=all horizontal, 2=all perpendicular to axis, 3=all vertical)
par(mar= c(5, 12, 4, 2) + 0.1) #Just increase the size of the left margin before you plot: par(mar= c(5, 10, 4, 2) + 0.1) (change the 10 value to suit your taste) 
barra=barplot(k$c2,names.arg = c("Civil servant","Community official","Contractual"),ylim=c(0,20),col = "lightblue",xlab="Employment status",ylab = "Number of teachers",cex.names=1.5,cex.axis=1.5,cex.lab=1.5)
text(x=barra, y = k$c2, label = k$c2, pos = 3, cex = 1, col = "black") 




c2=c(1,18,7)
c1=c("Bachelor degree","Maítrise","Master degree")
k=data.frame(c1,c2)
par(las=1) #That represents the style of axis labels. (0=parallel, 1=all horizontal, 2=all perpendicular to axis, 3=all vertical)
par(mar= c(5, 12, 4, 2) + 0.1) #Just increase the size of the left margin before you plot: par(mar= c(5, 10, 4, 2) + 0.1) (change the 10 value to suit your taste) 
barra=barplot(k$c2,names.arg = c("Bachelor degree","Maítrise","Master degree"),ylim=c(0,20),col = "lightblue",xlab="English teachers' level of formal education",ylab = "Number of teachers",cex.names=1.5,cex.axis=1.5,cex.lab=1.5)
text(x=barra, y = k$c2, label = k$c2, pos = 3, cex = 1, col = "black") 


c2=c(3,1,3,7,8,0,0,0,3)
c1=c("[5,7]","[8,9]","[10,11]","[12,13]","[14,15]","[16,17]","[18,19]","[20,21]","[22,23]")
k=data.frame(c1,c2)
par(las=1) #That represents the style of axis labels. (0=parallel, 1=all horizontal, 2=all perpendicular to axis, 3=all vertical)
par(mar= c(5, 12, 4, 2) + 0.1) #Just increase the size of the left margin before you plot: par(mar= c(5, 10, 4, 2) + 0.1) (change the 10 value to suit your taste) 
barra=barplot(k$c2,names.arg = c("[5,7]","[8,9]","[10,11]","[12,13]","[14,15]","[16,17]","[18,19]","[20,21]","[22,23]"),ylim=c(0,10),col = "lightblue",xlab="Years of teaching experience",ylab = "Number of teachers",cex.names=1.5,cex.axis=1.5,cex.lab=1.5)
text(x=barra, y = k$c2, label = k$c2, pos = 3, cex = 1, col = "black") 


c2=c(4,8,11)
c1=c("1 grade","2 grades","3 grades")
k=data.frame(c1,c2)
par(las=1) #That represents the style of axis labels. (0=parallel, 1=all horizontal, 2=all perpendicular to axis, 3=all vertical)
par(mar= c(5, 12, 4, 2) + 0.1) #Just increase the size of the left margin before you plot: par(mar= c(5, 10, 4, 2) + 0.1) (change the 10 value to suit your taste) 
barra=barplot(k$c2,names.arg = c("1 grade","2 grades","3 grades"),ylim=c(0,12),col = "lightblue",ylab = "Number of teachers",xlab="Number of grades attended this year",cex.names=1.5,cex.axis=1.5,cex.lab=1.5)
text(x=barra, y = k$c2, label = k$c2, pos = 3, cex = 1, col = "black") 

c2=c(5,9,4,3,3,1)
c1=c("1","2","3","4","5","6")
k=data.frame(c1,c2)
par(las=1) #That represents the style of axis labels. (0=parallel, 1=all horizontal, 2=all perpendicular to axis, 3=all vertical)
par(mar= c(5, 12, 4, 2) + 0.1) #Just increase the size of the left margin before you plot: par(mar= c(5, 10, 4, 2) + 0.1) (change the 10 value to suit your taste) 
barra=barplot(k$c2,names.arg = c("1","2","3","4","5","6"),ylim=c(0,12),col = "lightblue",ylab = "Number of teachers",xlab="Number of classes conducted per week",cex.names=1.5,cex.axis=1.5,cex.lab=1.5)
text(x=barra, y = k$c2, label = k$c2, pos = 3, cex = 1, col = "black") 


c2=c(12,6,6,2,0)
c1=c("Never","Once","Twice","Three times","More than three times")
k=data.frame(c1,c2)
par(las=1) #That represents the style of axis labels. (0=parallel, 1=all horizontal, 2=all perpendicular to axis, 3=all vertical)
par(mar= c(5, 12, 4, 2) + 0.1) #Just increase the size of the left margin before you plot: par(mar= c(5, 10, 4, 2) + 0.1) (change the 10 value to suit your taste) 
barra=barplot(k$c2,names.arg = c("Never","Once","Twice","Three times","More than three times"),ylim=c(0,14),col = "lightblue",xlab="Frequency of receiving an inspector in the last two years",ylab = "Number of teachers",cex.names=1.5,cex.axis=1.5,cex.lab=1.5)
text(x=barra, y = k$c2, label = k$c2, pos = 3, cex = 1, col = "black")

c2=c(5,12,8,1)
c1=c("Strongly agree","Agree","Disagree","Strongly disagree")
k=data.frame(c1,c2)
par(las=1) #That represents the style of axis labels. (0=parallel, 1=all horizontal, 2=all perpendicular to axis, 3=all vertical)
par(mar= c(5, 12, 4, 2) + 0.1) #Just increase the size of the left margin before you plot: par(mar= c(5, 10, 4, 2) + 0.1) (change the 10 value to suit your taste) 
barra=barplot(k$c2,names.arg = c("Strongly agree","Agree","Disagree","Strongly disagree"),ylim=c(0,14),col = "lightblue",xlab="Levels of agreement",ylab = "Number of teachers",cex.names=1.5,cex.axis=1.5,cex.lab=1.5)
text(x=barra, y = k$c2, label = k$c2, pos = 3, cex = 1, col = "black")

c2=c(3,4,5,5,6,2)
c1=c("[33,40]","[41,48]","[49,56]","[57,64]","[65,73]","[74,81]")
k=data.frame(c1,c2)
par(las=1) #That represents the style of axis labels. (0=parallel, 1=all horizontal, 2=all perpendicular to axis, 3=all vertical)
par(mar= c(5, 12, 4, 2) + 0.1) #Just increase the size of the left margin before you plot: par(mar= c(5, 10, 4, 2) + 0.1) (change the 10 value to suit your taste) 
barra=barplot(k$c2,names.arg = c("[33,40]","[41,48]","[49,56]","[57,64]","[65,73]","[74,81]"),ylim=c(0,7),col = "lightblue",ylab = "Number of teachers",xlab="Maximum number of students in a classroom",cex.names=1.5,cex.axis=1.5,cex.lab=1.5)
text(x=barra, y = k$c2, label = k$c2, pos = 3, cex = 1, col = "black") 

c2=c(19,16,6,6,3,2)
c1=c("Lack of material","Large class size","Lack of INSET","Lack of time","Lack of performance","Lack of motivation")
k=data.frame(c1,c2)
par(las=1) #That represents the style of axis labels. (0=parallel, 1=all horizontal, 2=all perpendicular to axis, 3=all vertical)
par(mar= c(5, 12, 4, 2) + 0.1) #Just increase the size of the left margin before you plot: par(mar= c(5, 10, 4, 2) + 0.1) (change the 10 value to suit your taste) 
barra=barplot(k$c2,names.arg = c("Lack of material","Large class size","Lack of INSET","Lack of time","Lack of performance","Lack of motivation"),ylim=c(0,20),col = "lightblue",ylab = "Number of teachers",xlab="Difficulties related to lesson plan development for CBA",cex.names=1.5,cex.axis=1.5,cex.lab=1.5)
text(x=barra, y = k$c2, label = k$c2, pos = 3, cex = 1, col = "black")




c2=c(1,3,4,9,3,1,1)
c1=c("Never","Occasionally","Sometimes","Often","Always","Generally","When there're group activities")
k=data.frame(c1,c2)
par(las=1) #That represents the style of axis labels. (0=parallel, 1=all horizontal, 2=all perpendicular to axis, 3=all vertical)
par(mar= c(5, 12, 4, 2) + 0.1) #Just increase the size of the left margin before you plot: par(mar= c(5, 10, 4, 2) + 0.1) (change the 10 value to suit your taste) 
barra=barplot(k$c2,names.arg = c("Never","Occasionally","Sometimes","Often","Always","Generally","When there're group activities"),main="How often do you apply group work activities during your lessons/classes",ylim=c(0,10),col = "lightblue",xlab="Levels of agreement",ylab = "Number of teachers",cex.names=1,cex.axis=1,cex.lab=1)
text(x=barra, y = k$c2, label = k$c2, pos = 3, cex = 1, col = "black")


#************************** Keita, Hypothesis test ***************************************************************************************************************************
data1=read.csv("Salvador_file.csv",sep=";")
data1=as.data.frame(data1)
colnames(data1)[1]="V1"
attach(data1)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++ Stacked bar plot ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
clasificar1=function(var1){
  Factor1=character()
  m=0
  for (i in 1:length(var1)) {
    m=m+1
    Factor1[m]=switch(var1[i],"1"="25-35","2"="36-45","3"="46-55")
  }
  Factor1
}
  
Factor1=clasificar1(V1)  
legend_title <- "Employment status as teacher"
ggplot(data1, aes(x = factor(V4), y =V1 , fill = Factor1)) + ggtitle("Highest level of formal education of respondents")+
geom_bar(stat = "identity")+labs(x="Levels of formal education", y="Number of respondents")+scale_fill_manual(legend_title,values=c("#A3E4D7","#F5B7B1","#AED6F1"))+theme(plot.title = element_text(size = 20,hjust = 0.5),axis.title.x = element_text(size = 15),axis.text.x = element_text(size = 15),axis.title.y = element_text(size = 15),axis.text.y = element_text(size = 15),legend.text = element_text(size = 15),legend.title = element_text(size = 15,hjust = 0.5))+
scale_x_discrete(labels=c("Bachelor degree", "Maitrise","Master degree "))

clasificar1=function(var1){
  Factor1=character()
  m=0
  for (i in 1:length(var1)) {
    m=m+1
    Factor1[m]=switch(var1[i],"1"="25-35","2"="36-45","3"="46-55")
  }
  Factor1
}

Factor1=clasificar1(V1)

source("http://pcwww.liv.ac.uk/~william/R/crosstab.r")
crosstab(data1, row.vars = "V1", col.vars = "V4", type = "f")
col1=c(rep("Bachelor degree",3),rep("Maitrise",3),rep("Master degree",3))
col2=c(rep(c("25-35","36-45","46-55"),3))
col3=c(0,1,0,1,15,2,1,6,0)
col4=round(col3/sum(col3),2)
col5=paste0(sprintf("%.0f",col4*100),"%")
framePru=data.frame(col1,col2,col3,col4,col5) 
framePru=framePru[-c(1,3,9),] 

legend_title <- "Employment status as teacher"
ggplot(data = framePru, mapping = aes(x=col1, y=col3, fill=col2)) + ggtitle("Highest level of formal education of respondents")+
geom_bar(stat="identity",color="black") +scale_fill_manual(legend_title,values=c("#A3E4D7","#F5B7B1","#AED6F1"))+
geom_text(aes(label=col5), position = position_stack(vjust= 0.5),
colour = "Black", size = 5)+labs(x="Levels of formal education", y="Number of respondents")+scale_fill_manual(legend_title,values=c("#A3E4D7","#F5B7B1","#AED6F1"))+theme(plot.title = element_text(size = 20,hjust = 0.5),axis.title.x = element_text(size = 15),axis.text.x = element_text(size = 15),axis.title.y = element_text(size = 15),axis.text.y = element_text(size = 15),legend.text = element_text(size = 15),legend.title = element_text(size = 15,hjust = 0.5))



clasificar2=function(var1){
  Factor2=character()
  m=0
  for (i in 1:length(var1)) {
    m=m+1
    Factor2[m]=switch(var1[i],"1"="25-35","2"="36-45","3"="46-55")
  }
  Factor2
}

Factor2=clasificar2(V1)

source("http://pcwww.liv.ac.uk/~william/R/crosstab.r")
crosstab(data1, row.vars = "V4", col.vars = "V5", type = "f")
col1=c(rep("Bachelor degree",5),rep("Maitrise",5),rep("Master degree",5))
col2=c(rep(c("1-5","6-10","11-15","16-20","21-25"),3))
col3=c(0,0,1,0,0,1,4,11,0,2,0,1,5,0,1)
col4=round(col3/sum(col3),2)
col5=paste0(sprintf("%.0f",col4*100),"%")
framePru=data.frame(col1,col2,col3,col4,col5) 
framePru=framePru[-c(1,2,4,5,9,11,14),] 
framePru$col1=factor(framePru$col1,levels = c('Bachelor degree','Maitrise','Master degree'))
framePru$col2=factor(framePru$col2,levels = c('1-5','6-10','11-15','16-20','21-25'))

legend_title <- "Year of teaching experience"
ggplot(data = framePru, mapping = aes(x=col1, y=col3, fill=col2)) + ggtitle("Highest level of formal education of respondents")+
geom_bar(stat="identity",color="black") +scale_fill_manual(legend_title,values=c("#A3E4D7","#F5B7B1","#AED6F1","#F9E79F","#D2B4DE"))+
geom_text(aes(label=col5), position = position_stack(vjust= 0.5),colour = "Black", size = 5)+labs(x="Levels of formal education", y="Number of respondents")+theme(plot.title = element_text(size = 20,hjust = 0.5),axis.title.x = element_text(size = 15),axis.text.x = element_text(size = 15),axis.title.y = element_text(size = 15),axis.text.y = element_text(size = 15),legend.text = element_text(size = 15),legend.title = element_text(size = 15,hjust = 0.5))

source("http://pcwww.liv.ac.uk/~william/R/crosstab.r")
crosstab(data1, row.vars = "V3", col.vars = "V1", type = "f")
col1=c(rep("Part time (less than 50%\n of full time)",3),rep("Part time (50%-90%\n of full time)",3),rep("Full time",3))
col2=c(rep(c("25-35","36-45","46-55"),3))
col3=c(1,3,2,0,5,0,1,14,0)
col4=round(col3/sum(col3),2)
col5=paste0(sprintf("%.0f",col4*100),"%")
framePru=data.frame(col1,col2,col3,col4,col5) 
framePru=framePru[-c(4,6,9),] 
framePru$col1=factor(framePru$col1,levels = c('Part time (less than 50%\n of full time)','Part time (50%-90%\n of full time)','Full time'))
framePru$col2=factor(framePru$col2,levels = c('25-35','36-45','46-55'))

legend_title <- "Age of respondents"
ggplot(data = framePru, mapping = aes(x=col1, y=col3, fill=col2)) + ggtitle("Employment status as teacher of respondents")+
  geom_bar(stat="identity",color="black") +scale_fill_manual(legend_title,values=c("#A3E4D7","#F5B7B1","#AED6F1","#F9E79F","#D2B4DE"))+
  geom_text(aes(label=col5), position = position_stack(vjust= 0.5),colour = "Black", size = 5)+labs(x="Employment status as a teacher", y="Number of respondents")+theme(plot.title = element_text(size = 20,hjust = 0.5),axis.title.x = element_text(size = 15),axis.text.x = element_text(size = 15),axis.title.y = element_text(size = 15),axis.text.y = element_text(size = 15),legend.text = element_text(size = 15),legend.title = element_text(size = 15,hjust = 0.5))

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ Hypothesis tests +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
corr <- cor.test(x=V4, y=V9, method = 'spearman')
corr
corr <- cor.test(x=V1, y=V10, method = 'spearman')
corr
clasificar2=function(var1){
  Factor2=character()
  m=0
  for (i in 1:length(var1)) {
      m=m+1
      Factor2[m]=switch(var1[i],"1"="Quarterly","2"="Monthly","3"="Bi-weekly","4"="Weekly","5"="Daily")
  }
  Factor2
}
Factor2=clasificar2(V10)  
legend_title <- "Adequate frequency of the teacher's\n development of the lesson plan."
ggplot(data1, aes(x = factor(V1), y =V10 , fill = Factor2)) + ggtitle("Respondents' opinion about the adequate frequency\n of the teacher's development of the lesson plan",)+
  geom_bar(stat = "identity")+labs(x="Age of respondents", y="Number of respondents")+scale_fill_manual(legend_title,values=c("#A3E4D7","#F5B7B1","#AED6F1","#D7BDE2"))+theme(plot.title = element_text(size = 20,hjust = 0.5),axis.title.x = element_text(size = 15),axis.text.x = element_text(size = 15),axis.title.y = element_text(size = 15),axis.text.y = element_text(size = 15),legend.text = element_text(size = 15),legend.title = element_text(size = 15,hjust = 0.5))+
  scale_x_discrete(labels=c("25-35", "36-45","46-55"))

corr <- cor.test(x=V1, y=V10, method = 'spearman')
corr

corr <- cor.test(x=V5, y=V11, method = 'spearman')
corr

corr <- cor.test(x=V3, y=V7, method = 'spearman')
corr

corr <- cor.test(x=V5, y=V8, method = 'spearman')
corr

corr <- cor.test(x=V4, y=V10, method = 'spearman')
corr


#*
plot(V4,V9)
x=data.frame(V4,V9)
#f requency - frequency count
#r ow.pct - proprotion within row
#c ol.pct - proportion within column
#j oint.pct - proportion within final 2 dimensions of table
#t otal.pct - proportion of entire table
source("http://pcwww.liv.ac.uk/~william/R/crosstab.r")
crosstab(x, row.vars = "V4", col.vars = "V9", type = "f")
crosstab(x, row.vars = "V4", col.vars = "V9", type = "r")
x=data.frame(V1,V10)
crosstab(x, row.vars = "V1", col.vars = "V10", type = "r")
x=data.frame(V5,V6)
crosstab(x, row.vars = "V5", col.vars = "V6", type = "r")
x=data.frame(V3,V7)
crosstab(x, row.vars = "V3", col.vars = "V7", type = "r")
x=data.frame(V4,V10)
crosstab(x, row.vars = "V4", col.vars = "V10", type = "r")


ControlGroup1Pre=c(17,13,12,11,15,11,6,3,10,5,6,11,8,18,16,7,3,3,18,14,5)
ControlGroup1Pos=c(14,4,13,2,15,13,7,1,4,2,9,17,3,16,4,6,2,0,15,10,3)
mean(ControlGroup1Pre)
mean(ControlGroup1Pos)

mean(ControlGroup2Pre)
mean(ControlGroup2Pos)



ControlGroup2Pre=c(18,7,19,18,21,4,18,17,21,21,12,21,20,21,17,17)
ControlGroup2Pos=c(10,6,16,7,16,3,8,18,17,10,5,16,16,16,8,12)
mean(ControlGroup2Pre)
mean(ControlGroup2Pos)

boxplot(ControlGroup1Pre)
boxplot(ControlGroup1Pos)

boxplot(ControlGroup2Pre)
boxplot(ControlGroup2Pos)

boxplot(ControlGroup1Pos)
boxplot(ControlGroup2Pos)


#For the presentation in Spanish

#Conocimiento matemático \n para la enseñanza (MKT)

theme_set(theme_ridges())
theme_set(theme_bw())
chart1 <- data.frame(Scores = c(HMG_SCORE,STUDENTSCORE,SAR_SCORE),Frequency= c(length(STUDENTSCORE)),rep("Examen de \n matemática",rep("Gestión de \n la tarea",  length(STUDENTSCORE)),rep("Autorregulación académica", length(STUDENTSCORE))))
ggplot(chart1, aes(x = Scores, y = Frequency)) + geom_density_ridges(quantile_lines = TRUE, quantiles = 2,fill ="#2C64DD",alpha = .5,)+coord_cartesian(clip = "off") + labs(title = 'Distribución de los puntajes de los estudiantes')+theme_ridges(font_size = 30,grid =TRUE, line_size = 0.75, center_axis_labels = TRUE)+scale_x_continuous(breaks = c(0:10), limits = c(-.5, 13),expand = c(0, 0), name = "Puntajes")+
  scale_y_discrete(limits=c("Examen de \n matemática","Gestión de \n la tarea","Autorregulación académica"))+theme(axis.text.y = element_text(hjust = 0.5))

chart2 <- data.frame(Scores = c(data2$TotalTestScore,data2$SCORE_SMK,data2$SCORE_PCK),Frequency = c(rep("Conocimiento matemático \n para la enseñanza", length(data2$TotalTestScore)),rep("Conocimiento \n del contenido", length(data2$TotalTestScore)),rep("Conocimiento Pedagógico \n del contenido", length(data2$TotalTestScore))))
ggplot(chart2, aes(x = Scores, y = Frequency)) + geom_density_ridges(quantile_lines = TRUE, quantiles = 2,fill ="#2C64DD",alpha = .5,)+coord_cartesian(clip = "off") + labs(title = 'Distribución de los puntajes de los profesores',xlab("a"))+theme_ridges(font_size = 30,grid =TRUE, line_size = 0.75, center_axis_labels = TRUE)+scale_x_continuous(breaks = c(0:10), limits = c(-.5, 13),expand = c(0, 0), name = "Puntajes")+
scale_y_discrete(labels=c("Conocimiento matemático \n para la enseñanza", "Conocimiento \n del contenido", "Conocimiento Pedagógico \n del contenido"))+theme(axis.text.y = element_text(hjust = 0.5))
estadistics1=data.frame(data2$TotalTestScore,data2$SCORE_SMK,data2$SCORE_PCK)



#*********************************************General students*************************************************************************************************************************
KD=median(TOTAL_KD)
RAD=median(TOTAL_RAD)
NRAD=median(TOTAL_NRAD)
RD=median(TOTAL_RD)


subscales=data.frame(KD,RAD,NRAD,RD)
colnames(subscales) <- c("Conocimiento","Aplicación rutinaria","Aplicación no rutinaria","Razonamiento")
subscales=rbind(rep(10,4),rep(0,4),subscales)
subscales=as.data.frame(subscales)


radarchart(subscales, axistype=1 , 
            #custom polygon
            pcol="#2C64DD",pfcol = scales::alpha("#2C64DD", 0.5),plwd=4 , plty=1,
            #custom the grid
            cglcol="black", cglty=4, axislabcol="black", caxislabels=seq(0,10,2.5), cglwd=0.8,paxislabels=c("1","2","3","4"),
            #custom labels
            vlcex=1
)

#*************************************************************************************************************************************************************************************************


#******************************************** General teachers ************************************************************************************************************************************
#default value for mar is c(5.1, 4.1, 4.1, 2.1)
par(mar = c(5.1, 5, 4.1, 3))
CCK=median(TOTAL_CCK)
HCK=median(TOTAL_HCK)
SCK=median(TOTAL_SCK)
KCS=median(TOTAL_KCS)
KC=median(TOTAL_KC)
KCT=median(TOTAL_KCT)
subscales=data.frame(CCK,HCK,SCK,KCS,KC,KCT)
colnames(subscales) <-c("Conocimiento  común \n del contenido","Conocimiento del horizonte \n matemático  del contenido","Conocimiento especializado \n del contenido","Conocimiento del contenido \n y de los alumnos","Conocimiento del \n contenido curricular","Conocimiento del contenido \n y la enseñanza")
subscales=rbind(rep(10,6),rep(0,6),subscales)
subscales=as.data.frame(subscales)
radarchart( subscales  , axistype=1 , 
            #custom polygon
            pcol="#2C64DD",pfcol = scales::alpha("#2C64DD", 0.5),plwd=4 , plty=1,
            #custom the grid
            cglcol="black", cglty=4, axislabcol="black", caxislabels=seq(0,10,2.5), cglwd=0.8,
            #custom labels
            vlcex=1
)
#**************************************************************************************************************************************************************************************************

c2=c(2,7,3,0,3)
c1=c("[1,6]","[7,12]","[13,18]","[19,24]","[25,30]")
k=data.frame(c1,c2)
par(las=1) #That represents the style of axis labels. (0=parallel, 1=all horizontal, 2=all perpendicular to axis, 3=all vertical)
par(mar= c(5, 12, 4, 2) + 0.1) #Just increase the size of the left margin before you plot: par(mar= c(5, 10, 4, 2) + 0.1) (change the 10 value to suit your taste) 
barra=barplot(k$c2,names.arg = c("[1,6]","[7,12]","[13,18]","[19,24]","[25,30]"),ylim=c(0,8),col = "#2C64DD",ylab = "Cantidad de profesores",xlab="Años de experiencia",cex.names=1.5,cex.axis=1.5,cex.lab=1.5)
text(x=barra, y = k$c2, label = k$c2, pos = 3, cex = 1.5, col = "black") 


#SAR VS HMG
d=data.frame(SAR_SCORE,HMG_SCORE)
d$pc <- predict(prcomp(~SAR_SCORE+HMG_SCORE, d))[,1]
ggplot(d, aes(SAR_SCORE, HMG_SCORE, color = pc)) +
  geom_point(shape = 16,size = 5, show.legend = FALSE) + ggtitle("Autorregulación académica VS Gestión de la tarea") +
  xlab("Autorregulación académica") + ylab("Gestión de la tarea")+geom_abline(intercept = 4.8672 , slope = 0.3355, color="red", linetype="dashed", size=1.5)+
  theme_minimal() +scale_color_gradient(low = "#D7E3FB", high = "#092C71")



