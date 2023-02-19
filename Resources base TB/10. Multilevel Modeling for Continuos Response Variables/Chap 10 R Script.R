# Chap 10 R Script

# Remove all objects
rm(list = ls(all=TRUE))

# The following user-written packages need to be installed first 
# Use install.packages(" ") and then loadit with library()
# library(nlme)                    # It is part of R base distribution 
# library(lmerTest)              
# library(lme4)                 
# library(ggeffects)               # It is already installed for Chapter 2
# library(stargazer)               # It is already installed for Chapter 2
# library(texreg)                  # It is already installed for Chapter 4

# Import the dataset
library(foreign)
chp10 <- read.dta("C:/CDA/els2002.dta")
chp10 <- chp10[!is.na(chp10$mathach)&!is.na(chp10$efficacy)&!is.na(chp10$public)&!is.na(chp10$sclimate), ]
chp10$csclimat <- chp10$sclimate-mean(chp10$sclimate, na.rm=TRUE)
chp10$gceffic <- chp10$efficacy-mean(chp10$efficacy, na.rm=TRUE)
attach(chp10)

# install.packages("nlme")
library(nlme)

# Null model with lme()
m1 <- lme(mathach ~ 1, random = ~1|SCH_ID, na.action="na.omit", method="ML", data=chp10)
summary(m1)
intervals(m1)
VarCorr(m1)

# Random-intercept model
m2 <- lme(mathach ~ gceffic, random = ~1|SCH_ID, na.action="na.omit", method="ML", data=chp10)
summary(m2)
intervals(m2)
VarCorr(m2)

# Model comparison
anova(m1, m2)

# Random-coefficient model
m3 <- lme(mathach ~ gceffic, random = ~gceffic|SCH_ID, na.action="na.omit", method="ML", data=chp10)
summary(m3)
intervals(m3, which="fixed")
VarCorr(m3)
anova(m2, m3)

# Contextual model with predictor variables in both levels
m4 <- lme(mathach ~ gceffic + public + csclimat, random = ~gceffic|SCH_ID, 
        na.action="na.omit", method="ML", data=chp10)
summary(m4)
intervals(m4, which="fixed")
VarCorr(m4)
anova(m3, m4)

# Contextual model with cross-level interactions
m5 <- lme(mathach ~ gceffic + public + csclimat + public*gceffic + csclimat*gceffic,
        random = ~gceffic|SCH_ID, method="ML", data=chp10)
summary(m5)
intervals(m5, which="fixed")
VarCorr(m5)

anova(m4, m5)

# Presenting the results with stargazer()
library(stargazer)
stargazer(m1, m4, type="text", align=TRUE, out="mul.txt")
stargazer(m1, m4, type="html", align=TRUE, out="mul.htm")

# Presenting the results with texreg()
library(texreg)
screenreg(list(m1, m4))
htmlreg(list(m1, m4), file="mul.doc", doctype=TRUE, html.tag=TRUE, head.tag=TRUE)

# Contextual model with lmer()
library(lmerTest)
library(lme4)
m4.b <- lmer(mathach ~ gceffic + public + csclimat + (gceffic|SCH_ID), data=chp10, REML = FALSE)
summary(m4.b)
confint(m4.b)
confint(m4.b, method="Wald")

# Marginal effects/Predicted values with ggpredict()
library(ggeffects)
m4.b.pub <- ggpredict(m4.b, terms="public")
m4.b.pub
as.data.frame(m4.b.pub)
plot(m4.b.pub)

m4.efpub <- ggpredict(m4.b, terms=c("gceffic", "public"))
m4.efpub
plot(m4.efpub)

m4.efpub.r <- ggpredict(m4.b, terms=c("gceffic", "public"), type="re")
m4.efpub.r
plot(m4.efpub.r)

detach(chp10)
