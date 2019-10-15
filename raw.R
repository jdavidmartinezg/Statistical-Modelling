# Setup ---------

library(knitr)
library(formatR)
library(ggplot2)
library(ggfortify)
library(quantreg)
library(gridExtra)
library(Hmisc)
library(corrplot)
library(GGally)
library(caret)
library(psych)
library(car)
library(huxtable)
library(stargazer)
library(DataExplorer)
library(GGally)
library(MASS)
library(data.table)
library(e1071)
library(pROC)
library(tidyverse)
library(papeR)
library(xtable)
library(arm)
library(pROC)
library(e1071)
library(caret)
library(ggpubr)



options(scipen=999)

lalonde <- read.csv('lalondedata.csv')

str(lalonde)

# Drop 

lalonde$X <- NULL

# Feature engineering ---------


# Logit Dependent variable

lalonde$non.zero.wage78 <- factor(as.numeric(lalonde$re78 > 0), levels = c(0,1), labels = c("Zero", "Non zero"))

# Recode as factors categorical predictors

lalonde$treat    <- factor(lalonde$treat, levels = c(0,1), labels = c("Not received job training","Received job training"))
lalonde$black    <- factor(lalonde$black, levels = c(0,1), labels = c("Not black","Black"))
lalonde$hispan   <- factor(lalonde$hispan, levels = c(0,1), labels = c("Not hispanic","Hispanic"))
lalonde$married  <- factor(lalonde$married, levels = c(0,1), labels = c("Not married","Married"))
lalonde$nodegree <- factor(lalonde$nodegree, levels = c(0,1), labels = c("Not dropped out","Dropped out"))

# Center variables

lalonde$ageC  <- lalonde$age - mean(lalonde$age)
lalonde$educC <- lalonde$educ - mean(lalonde$educ)
lalonde$re74C <- lalonde$re74 - mean(lalonde$re74)
lalonde$re75C <- lalonde$re75 - mean(lalonde$re75)
lalonde$re78C <- lalonde$re78 - mean(lalonde$re78)

# Square terms

lalonde$ageCsq  <- lalonde$ageC*lalonde$ageC
lalonde$educCsq <- lalonde$educC*lalonde$educC
lalonde$re74Csq <- lalonde$re74C*lalonde$re74C
lalonde$re75Csq <- lalonde$re75C*lalonde$re75C
lalonde$re78Csq <- lalonde$re78C*lalonde$re78C

# Log transformations

lalonde$log.re74  <- log(lalonde$re74)
lalonde$log.re75  <- log(lalonde$re75)
lalonde$log.re78  <- log(lalonde$re78)

# Log trasnformation to avoid inf due to zeros

lalonde$advlog.re74 <- log(1 + lalonde$re74)
lalonde$advlog.re75 <- log(1 + lalonde$re75)
lalonde$advlog.re78 <- log(1 + lalonde$re78)

# SQRT Transformation
 
lalonde$sqrt.re78 <- sqrt(lalonde$re78)

# ArcSin Transformation

asinTransform <- function(p) { asin(sqrt(p)) }

lalonde$arcsin.re78 <- asinTransform(lalonde$re78)

# More-complex log trasnformation

lalonde$complexlog.re78 <- (0.5)*log((lalonde$re78)/(1 - lalonde$re78))

# Removing zeros from response variable

lalonde_log <- lalonde[which(lalonde$re78 > 0),]

# Which re78 trasnformation is more normaly distributed?

ggqqplot(lalonde_log$log.re78)

ggqqplot(lalonde_log$sqrt.re78)

ggqqplot(lalonde_log$arcsin.re78)

ggqqplot(lalonde_log$complexlog.re78)


# Shapiro test for normality

shapiro.test(lalonde_log$log.re78)

shapiro.test(lalonde_log$sqrt.re78)

shapiro.test(lalonde_log$complexlog.re78)


# Regression ----------


# Full model

baseline_lalonde_lm <- lm(sqrt.re78 ~ ageC + ageCsq + educC + re74C + treat + black + hispan + married + nodegree + # Main predictors
                   treat:black + treat:hispan + treat:married + treat:nodegree +  # Categorical interactions of treat
                   ageC:treat + educC:treat + re74C:treat, # Mixed interactions of treat
                   data = lalonde_log)


#                   black:married + black:nodegree + 
#                   hispan:married + hispan:nodegree +
#                   married:nodegree +
#                   ageC:treat + ageC:black + ageC:hispan + ageC:married + ageC:nodegree + # mixed interactions
#                   educC:treat + educC:black + educC:hispan + educC:married + educC:nodegree + 
#                   re74C:treat + re74C:black + re74C:hispan + re74C:married + re74C:nodegree,
#                   data = lalonde_log)

# lalonde_step.model <- stepAIC(baseline_lalonde_lm, direction = "both", trace = FALSE)

# summary(lalonde_step.model)

# ******************************************************************************

# Null model

null_lalonde_lm <- lm(sqrt.re78 ~ 1, data = lalonde_log)
n = nrow(lalonde_log)

# Step BIC



baseline_lalonde_lm_BIC <- step(null_lalonde_lm, scope = formula(baseline_lalonde_lm),
                                direction = "both",
                                k = log(n))

summary(baseline_lalonde_lm_BIC)

# STEP AIC

baseline_lalonde_lm_AIC <- step(null_lalonde_lm, scope = formula(baseline_lalonde_lm),
                                direction = "both", k = 2)

summary(baseline_lalonde_lm_AIC)

# Backward BIC

baseline_lalonde_lm_BIC_backward <- step(baseline_lalonde_lm, direction = "backward", trace = 0, k = log(n))

summary(baseline_lalonde_lm_BIC_backward)

vif(baseline_lalonde_lm_BIC_backward)


# Backward AIC -- Final model picked by R-squared

baseline_lalonde_lm_AIC_backward <- step(baseline_lalonde_lm, direction = "backward", trace = 0)

summary(baseline_lalonde_lm_AIC_backward)

# Model evaluation

AIC(baseline_lalonde_lm_BIC_backward)

AIC(baseline_lalonde_lm_AIC_backward)

vif(baseline_lalonde_lm_AIC_backward)

# plot(baseline_lalonde_lm_AIC_backward)

baseline_lalonde_lm_AIC_backward.stdres = rstandard(baseline_lalonde_lm_AIC_backward)

# Helps to have converted to response variable to SQRT

ggqqplot(baseline_lalonde_lm_AIC_backward.stdres)
ggqqplot(lalonde_log$re78)


autoplot(baseline_lalonde_lm_AIC_backward, label.size = 3, which = c(1:6), 
         label.colour = "Red", label.hjust = 1.33, size = 0.8,
         smooth.linetype = "blank")



# Include treat interactions and TEST

baseline_lalonde_lm_AIC_backward_treat_interactions <- lm(sqrt.re78 ~ ageC +
                                                            educC + re74C +
                                                            treat + black +
                                                            treat:re74C + 
                                                            hispan + married + nodegree + # other demographics
                                                            treat:ageC + treat:re74C + treat:black + treat:hispan + treat:married + treat:nodegree,
                                                            data = lalonde_log)
                                                           
summary(baseline_lalonde_lm_AIC_backward_treat_interactions)

anova(baseline_lalonde_lm_AIC_backward_treat_interactions, baseline_lalonde_lm_AIC_backward)

AIC(baseline_lalonde_lm_AIC_backward)
AIC(baseline_lalonde_lm_AIC_backward_treat_interactions)

# The interactions of treatment between race are not significant

confint(baseline_lalonde_lm_AIC_backward)

# INCLUDE TABLE

crPlots(lm(sqrt.re78 ~ ageC + educC + re74C + treat + black, data = lalonde_log))

# residuals against predictors

plot(aseline_lalonde_lm_AIC_backward.stdres, lalonde_log$ageC)
plot(aseline_lalonde_lm_AIC_backward.stdres, lalonde_log$educC)
plot(aseline_lalonde_lm_AIC_backward.stdres, lalonde_log$re74C)
boxplot(aseline_lalonde_lm_AIC_backward.stdres, lalonde_log$treat)
boxplot(aseline_lalonde_lm_AIC_backward.stdres, lalonde_log$black)

baseline_lalonde_lm_AIC_backward_fulldata <- lm(sqrt.re78 ~ ageC +
                                                            educC + re74C +
                                                            treat + black +
                                                            treat:re74C,
                                                          data = lalonde)

summary(baseline_lalonde_lm_AIC_backward_fulldata)
