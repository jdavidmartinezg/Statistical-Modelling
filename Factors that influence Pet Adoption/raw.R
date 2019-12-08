# Setup ----------------


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
library(arm)
library(lme4)
library(lmerTest)
library(ggpubr)
library(readxl)
library(readr)
library(stringr)
library(tm)
library(tidytext)
library(RWeka)
library(randomForest)
library(mlbench)
library(gbm)
library(fastAdaboost)
library(ada)
library(C50)
library(SparkR)
library(dplyr)
library(sjPlot)
library(sjlabelled)
library(sjmisc)
library(lubridate)
library(tidyverse)
library(plyr)

setwd("C:/Users/Juan David Martínez/Desktop/IDS 702 Final Project Data/Data")

# Data processing & Cleaning ------------

adoption_train <- read.csv("train.csv", sep = ",")

# adoption_test <- read.csv("test.csv", sep = ",") <---- does not conatin true labels (Kaggle competition)

breeds <- read.csv("breed_labels.csv", sep = ",")

colors <- read.csv("color_labels.csv", sep = ",")

states <- read.csv("state_labels.csv", sep = ",")






# Merge data (Breeds)

breeds$Breed1Name <- breeds$BreedName

breeds$Breed2Name <- breeds$BreedName

breeds$Breed.Fixed.Name <- breeds$BreedName

adoption_train <- merge(adoption_train, breeds[,c(1,2,4)], by.x = c("Breed1", "Type"), by.y = c("BreedID", "Type"), all.x = T)

adoption_train <- merge(adoption_train, breeds[,c(1,2,5)], by.x = c("Breed2", "Type"), by.y = c("BreedID", "Type"), all.x = T)

# When the pet has a zero in the breed it means that does not have that breed

# Breed1 is the primary breed of the pet

adoption_train[which(adoption_train$Breed1 == 0 & adoption_train$Breed2 == 0),]

# There are no pets with no breeds

# Create a variable called abs breed to fix the pets that do not have primary breed but do have secondary breed

adoption_train$Breed_fixed <- adoption_train$Breed1

adoption_train$Breed_fixed[adoption_train$Breed_fixed == 0] <- adoption_train$Breed2[adoption_train$Breed_fixed == 0]

adoption_train <- merge(adoption_train, breeds[,c(1,2,6)], by.x = c("Breed_fixed", "Type"), by.y = c("BreedID", "Type"), all.x = T)



# Merge data (Color)

colnames(colors)[2] <- "Primary.ColorName"

colors$Color2Name <- colors$Primary.ColorName

colors$Color3Name <- colors$Primary.ColorName

adoption_train <- merge(adoption_train, colors[,c(1,2)], by.x = c("Color1"), by.y = c("ColorID"), all.x = T)

adoption_train <- merge(adoption_train, colors[,c(1,3)], by.x = c("Color2"), by.y = c("ColorID"), all.x = T)

adoption_train <- merge(adoption_train, colors[,c(1,4)], by.x = c("Color3"), by.y = c("ColorID"), all.x = T)



# Merge data (States)

adoption_train <- merge(adoption_train, states, by.x = c("State"), by.y = c("StateID"), all.x = T)



# True labels:

str(adoption_train)

adoption_train$AdoptionSpeed. <- factor(adoption_train$AdoptionSpeed, levels = c(0,1,2,3,4), 
                                         labels = c("Same day","1st Week", "1st Month", "2nd & 3rd Month", "No adoption (After 100 days)"))

adoption_train$Type. <- factor(adoption_train$Type, levels = c(1,2), labels = c("Dog", "Cat"))

adoption_train$Gender. <- factor(adoption_train$Gender, levels = c(1,2,3), labels = c("Male", "Female", "Mixed"))

adoption_train$MaturitySize. <- factor(adoption_train$MaturitySize, levels = c(1,2,3,4,0), 
                                        labels = c("Small", "Medium", "Large", "Extra Large", "Not Specified"))

adoption_train$FurLength. <- factor(adoption_train$FurLength, levels = c(1,2,3,0), 
                                        labels = c("Short", "Medium", "Long", "Not Specified"))
  
adoption_train$Vaccinated. <- factor(adoption_train$Vaccinated, levels = c(1,2,3), 
                                     labels = c("Yes", "No", "Not Sure"))

adoption_train$Dewormed. <- factor(adoption_train$Dewormed, levels = c(1,2,3), 
                                      labels = c("Yes", "No", "Not Sure"))

adoption_train$Sterilized. <- factor(adoption_train$Sterilized, levels = c(1,2,3), 
                                    labels = c("Yes", "No", "Not Sure"))

adoption_train$Health. <- factor(adoption_train$Health, levels = c(1,2,3,0), 
                                     labels = c("Healthy", "Minor Injury", "Serious Injury", "Not Specified"))



# Feature engineering --------------

# Dummy on whether the pet is mixed breed or not

adoption_train$MixedBreed <- factor((adoption_train$Breed1 != 0 & adoption_train$Breed2 != 0) & !(is.na(adoption_train$Breed1) & is.na(adoption_train$Breed2)))

# Dummy on wheter the pet is multicolor

adoption_train$Multicolor <- factor((adoption_train$Color2 != 0 | adoption_train$Color3 != 0))

# Dummy on whether the pet was adopted within the first month or not (Check threshold with EDA) - Most balanced partition

adoption_train$Adopted.FirstMonth <- factor(adoption_train$AdoptionSpeed <= 2)

adoption_train$Adopted.FirstMonth.num <- as.integer(adoption_train$AdoptionSpeed <= 2)

# Dummy on whether the pet was adopted within the first 100 days

adoption_train$Adopted.First100 <- factor(adoption_train$AdoptionSpeed <= 3)

adoption_train$Adopted.First100.num <- as.integer(adoption_train$AdoptionSpeed <= 3)

# Dummy on whether the dog has a name or not

adoption_train$HasName <- factor(adoption_train$Name != "")

# Length of the name in characters (careful with this variable, some names are dirty due to encoding of characters)

adoption_train$NameLength <- str_count(adoption_train$Name)

# Categorical variable of age: Cats 0-6 m (Kitten), 7-35 (Junior), 36-83 (Prime), 84-131 (Mature), 132-179 (Senior), >=180 (Geriatric)
# Dogs: Same, Check EDA to see if groups are balanced

adoption_train$LifeStage. <- cut(adoption_train$Age, breaks = c(0,7,36,84,132,180,10000000), right = F,
                                labels = c("Puppy/Kitten", "Junior", "Prime", "Mature", "Senior" , "Geriatric")) # Too few Geriatric and Seniors, this may be a good variable to try Hierarchical model

# Dummy on whether the profile is for a group of pets

adoption_train$Group <- factor(adoption_train$Quantity > 1)

# Whether the adoption is free or not

adoption_train$FreeAdoption <- factor(adoption_train$Fee == 0)

# Dummy on wether the pet has videos in profile

adoption_train$HasVideos <- factor(adoption_train$VideoAmt > 0) 

# Dummy on wether the pet has images in profile

adoption_train$HasPhotos <- factor(adoption_train$PhotoAmt > 0) 

# Dummy on wether the pet has a description in profile

adoption_train$HasDescription <- factor(adoption_train$Description != "") # All of them have descriptions

# Length of description in characters

adoption_train$DescriptionLength <- str_count(adoption_train$Description)

# Fee per animal in listing

adoption_train$FeePercapita <- adoption_train$Fee/adoption_train$Quantity

# Videos to Phtoso ratio

adoption_train$Videos2PhotosRatio <- adoption_train$VideoAmt/adoption_train$PhotoAmt

# Rescuer experience

rescuer_exp <- as.data.frame(table(adoption_train$RescuerID))

colnames(rescuer_exp) <- c("RescuerID", "NumRescues")

adoption_train <- merge(adoption_train, rescuer_exp, by = c("RescuerID"), all.x = T)

# Race uniqueness (By Type)

race_count <- as.data.frame(table(adoption_train$Breed_fixed, adoption_train$Type))

colnames(race_count) <- c("Breed_fixed", "Type", "Breed_Frequency")

race_count$Breed_Rarity <- 1/race_count$Breed_Frequency

race_count <- race_count[race_count$Breed_Frequency > 0,]

adoption_train <- merge(adoption_train, race_count, by = c("Type", "Breed_fixed"), all.x = T)

# Color uniqueness (By Type)

color_count <- as.data.frame(table(adoption_train$Color1, adoption_train$Type))

colnames(color_count) <- c("Color1", "Type", "Color_Frequency")

color_count$Color_Rarity <- 1/color_count$Color_Frequency

color_count <- color_count[color_count$Color_Frequency > 0,]

adoption_train <- merge(adoption_train, color_count, by = c("Type", "Color1"), all.x = T)

# Color uniqueness (By Type and Breed)

color_breed_count <- as.data.frame(table(adoption_train$Color1, adoption_train$Type, adoption_train$Breed_fixed))

colnames(color_breed_count) <- c("Color1", "Type", "Breed_fixed", "Color_Breed_Frequency")

color_breed_count$Color_Breed_Rarity <- 1/color_breed_count$Color_Breed_Frequency

color_breed_count <- color_breed_count[color_breed_count$Color_Breed_Frequency > 0,]

adoption_train <- merge(adoption_train, color_breed_count, by = c("Type", "Color1", "Breed_fixed"), all.x = T)



# EDA ----------------


# Type

options(repr.plot.width=4, repr.plot.height=3)
ggplot(adoption_train, aes(x=Type.,y=..count..)) + 
  geom_bar(width=0.5,fill="#00529B")+ theme_classic() + 
  labs(title = "Number of observations by Type", x = "Type")

# Age Density

options(repr.plot.width=8, repr.plot.height=3)
ggplot(adoption_train, aes(x=Age,y=..count..)) + geom_density(fill="#00529B") +  theme_classic() + 
  labs(title = "Density of Age", x = "Age")

# Age density - Zoom

ggplot(adoption_train[adoption_train$Age<25,], aes(x=Age,y=..count..)) + geom_density(fill="#00529B") +  theme_classic() +
  labs(title = "Density of Age (Pets under 25 months old)", x = "Age")


# Adoption Speed

ggplot(adoption_train, aes(x=AdoptionSpeed.))+geom_bar(fill="#00529B") + 
  theme_classic() +coord_flip() +
  labs(title = "Adoption Speed", y = "count", x = "Adoption Speed")


# Modeling dataset

adoption <- adoption_train[,c(23,11,19,20,21,24,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,
                              47,48,49,50,51,52,53,54,56,57,58,59,61,63,65)]



# Adoption vs life stage

p1 <- ggplot(adoption, aes(x = Adopted.First100, y = Age, fill = Adopted.First100)) +
  geom_boxplot(col = "black", outlier.colour = NA) +
  coord_cartesian(ylim = range(boxplot(adoption$Age, plot=FALSE)$stats)*c(.9, 1.1)) + labs(title="Age vs Adoption", 
                                            x = "Adopted within the first 100 days",
                                            y = "Age in months",
                                            fill = "Adopted First 100 days")

# Color|Breed rarity vs Adoption

p2 <- ggplot(adoption, aes(x = Adopted.First100, y = Color_Breed_Rarity, fill = Adopted.First100)) +
  geom_boxplot(col = "black", outlier.colour = NA) + coord_cartesian(ylim = range(boxplot(adoption$Color_Breed_Rarity, plot=FALSE)$stats)*c(.9, 1.1)) +
  labs(title="Color|Breed rarity vs Adoption", x = "Adopted within the first 100 days",
                                                                                           y = "Color|Breed rarity",
                                                                                           fill = "Adopted First 100 days")

# Description Length vs Adoption

p3 <- ggplot(adoption, aes(x = Adopted.First100, y = DescriptionLength, fill = Adopted.First100)) +
  geom_boxplot(col = "black", outlier.colour = NA) + coord_cartesian(ylim = range(boxplot(adoption$DescriptionLength, plot=FALSE)$stats)*c(.9, 1.1)) + 
  labs(title="Description Length vs Adoption", x = "Adopted within the first 100 days", y = "Number of characters", fill = "Adopted First 100 days")

# Quantity of pets in listing vs Adoption

ggplot(adoption, aes(x = Adopted.First100, y = Quantity, fill = Adopted.First100)) +
  geom_boxplot(col = "black", outlier.colour = NA) + 
  labs(title="Quantity of pets in listing vs Adoption", x = "Adopted within the first 100 days", y = "Number of pets", fill = "Adopted First 100 days")

# Group

ggplot(adoption, aes(Group, ..count..)) + geom_bar(aes(fill = Adopted.First100), position = "dodge")

ggplot(data=adoption) + geom_bar(aes(x=Adopted.First100, fill=Group), position = "fill") +
  labs(title="Group Listing vs Adoption", x = "Adopted within the first 100 days", y = "count", fill = "Group")

ggplot(data=adoption) + geom_bar(aes(x=Group, fill=Adopted.First100), position = "fill") +
  labs(title="Group Listing vs Adoption", x = "Group", y = "count", fill = "Adopted within the first 100 days")


ggplot(data=adoption) + geom_bar(aes(x=FreeAdoption, fill=Adopted.First100), position = "fill") +
  labs(title="Free Adoption vs Adoption", x = "Free Adoption", y = "count", fill = "Adopted within the first 100 days")

# Photos

p4 <- ggplot(data=adoption) + geom_bar(aes(x=HasPhotos, fill=Adopted.First100), position = "fill") +
  labs(title="Listing Has Photos vs Adoption", x = "Has Photos", y = "Count - Proportion", fill = "Adopted First 100 days")

# Video

ggplot(data=adoption) + geom_bar(aes(x=HasVideos, fill=Adopted.First100), position = "fill") +
  labs(title="Listing Has Video vs Adoption", x = "Has Video", y = "count", fill = "Adopted within the first 100 days")

# Color

ggplot(data=adoption) + geom_bar(aes(x=Primary.ColorName, fill=Adopted.First100), position = "fill") +
  labs(title="Primary Color vs Adoption", x = "Primary Color", y = "count", fill = "Adopted within the first 100 days")

# Type

ggplot(data=adoption) + geom_bar(aes(x=Type., fill=Adopted.First100), position = "fill") +
  labs(title="Type vs Adoption", x = "Type", y = "count", fill = "Adopted First 100 days")

# Gender

ggplot(data=adoption) + geom_bar(aes(x=Gender., fill=Adopted.First100), position = "fill") +
  labs(title="Gender vs Adoption", x = "Gender", y = "count", fill = "Adopted within the first 100 days")

# Vaccination

ggplot(data=adoption) + geom_bar(aes(x=Vaccinated., fill=Adopted.First100), position = "fill") +
  labs(title="Vaccinated vs Adoption", x = "Vaccinated", y = "count", fill = "Adopted within the first 100 days")

# 415 pixels, {height="71%" width="71%"}

grid.arrange(p1, p2, p3, p4, nrow = 2)


# Logistic regression ----------------


# Model whether a pet is adopted within 1 month or not

# Basic, no interactions

oneMonth.logit <- glm(Adopted.FirstMonth.num ~ LifeStage. + Group + FreeAdoption + HasVideos + HasPhotos + 
                        Primary.ColorName + Type. + Gender. + MaturitySize. + FurLength. + Vaccinated. + Dewormed. +
                        Sterilized. + Health. + MixedBreed + Multicolor + HasName + NameLength + NumRescues + DescriptionLength + 
                        NumRescues + Breed_Rarity + Color_Breed_Rarity + StateName, 
                      data = adoption[,-c(1)], 
                      family = binomial(link = "logit"))

summary(oneMonth.logit)

vif(oneMonth.logit)



# Adopted or not in less than 100 days

oneHundred.logit <- glm(Adopted.First100.num ~ Age + Group + FreeAdoption +  
                          HasVideos + HasPhotos + Primary.ColorName + Type. + Gender. + MaturitySize. + FurLength. + Vaccinated. +
                          Dewormed. + Sterilized. + Health. + MixedBreed + Multicolor + HasName + NameLength + DescriptionLength +
                          NumRescues + Breed_Rarity + Color_Breed_Rarity + StateName, 
                      data = adoption[,-c(1)], 
                      family = binomial(link = "logit"))

summary(oneHundred.logit)

vif(oneHundred.logit)

# Vaccinated. and Dewormed. seem to be very higly correlated

table(adoption$Vaccinated.,adoption$Dewormed.)

# Backward AIC

oneHundred.logit_AIC_backward <- step(oneHundred.logit, direction = "backward", trace = 0)

summary(oneHundred.logit_AIC_backward)

vif(oneHundred.logit_AIC_backward)

# The variables that were taken out are NameLength and Breed_Rarity

# Both AIC

null_oneHundred <- glm(Adopted.First100.num ~ 1, data = adoption, 
                       family = binomial(link = "logit"))

oneHundred.logit_AIC_both <- step(null_oneHundred, scope = formula(oneHundred.logit),
                                direction = "both", k = 2, trace = 0)


summary(oneHundred.logit_AIC_both)

# Both BIC

n = nrow(adoption)

oneHundred.logit_BIC_both <- step(null_oneHundred, scope = formula(oneHundred.logit),
                                direction = "both",
                                k = log(n), trace = 0)

summary(oneHundred.logit_BIC_both)

# Backward BIC

oneHundred.logit_BIC_backward <- step(oneHundred.logit, direction = "backward", trace = 0, k = log(n))

summary(oneHundred.logit_BIC_backward)

# Mc Fadden R squared

# 1-logLik(oneHundred.logit_AIC_both)/logLik(null_oneHundred)


BIC(oneHundred.logit_BIC_backward)
BIC(oneHundred.logit_BIC_both)
BIC(oneHundred.logit_AIC_both)
BIC(oneHundred.logit_AIC_backward)

# best: BIC_both

AIC(oneHundred.logit_BIC_backward)
AIC(oneHundred.logit_BIC_both)
AIC(oneHundred.logit_AIC_both)
AIC(oneHundred.logit_AIC_backward)

# best: AIC_both

# picked BIC_backward

vif(oneHundred.logit_BIC_backward)

# No multicolinearity problems

rawresid1 <- residuals(oneHundred.logit_BIC_backward, type="response")

##binned residual plots

binnedplot(x=fitted(oneHundred.logit_BIC_backward),y=rawresid1,xlab="Pred. probabilities",
           col.int="red4",ylab="Avg. residuals",main="Residuals vs Pred probabilities",col.pts="navy")


summary(oneHundred.logit_BIC_backward)

# Confussion Matrix and ROC

cutoff = 0.85

Conf_mat <- confusionMatrix(factor(fitted(oneHundred.logit_BIC_backward) >= cutoff),
                            adoption$Adopted.First100, positive = "TRUE")

Conf_mat

invisible(roc(adoption$Adopted.First100,fitted(oneHundred.logit_BIC_backward), plot=T,print.thres="best",legacy.axes=T,
              print.auc =T,col="red"))



# Multilevel logistic regression ----------------

# Random intercept by Primary Breed

oneHundred_multilevel <- glmer(formula = Adopted.First100.num ~  + (1|Breed.Fixed.Name) + LifeStage. + Group + FreeAdoption + 
      HasPhotos + Primary.ColorName + Type. + Gender. + MaturitySize. + 
      FurLength. + Sterilized. + Multicolor + DescriptionLength.scaled + 
      NumRescues.scaled, family = binomial, data = adoption, 
      control = glmerControl(optimizer = "Nelder_Mead", optCtrl=list(maxfun=10000e20))) # bobyqa | Nelder_Mead

summary(oneHundred_multilevel)

# AIC: 15490, BIC: 15711

# Both the BIC and the AIC decrease. It is valuable to use a hierarchical model.

length(fixef(oneHundred_multilevel))

adoption$DescriptionLength.scaled <- scale(adoption$DescriptionLength)

adoption$NumRescues.scaled <- scale(adoption$NumRescues)

# Re run the model to see if it converges

tt <- getME(oneHundred_multilevel,"theta")
ll <- getME(oneHundred_multilevel,"lower")
min(tt[ll==0])

# Apparently, the model should be simplified  

oneHundred_multilevel_simplified <- glmer(formula = Adopted.First100.num ~  + (1|Breed.Fixed.Name) + LifeStage. + Group + FreeAdoption + 
                                 HasPhotos + Gender. + Sterilized. + Multicolor + DescriptionLength.scaled + 
                                 NumRescues.scaled, family = binomial, data = adoption, 
                               control = glmerControl(optimizer = "Nelder_Mead", optCtrl=list(maxfun=10000e20))) # bobyqa | Nelder_Mead

summary(oneHundred_multilevel_simplified)

# Proportional odds model --------------------


# Random Forest and Boosting trees ------------

control <- trainControl(method="repeatedcv", number=10, repeats=3)

adoption_rf <- adoption[,-c(1,7,8,11,12,14,25,26,28,38,43,44)]

adoption_rf <- adoption_rf[complete.cases(adoption_rf),]

rf_full <- train(Adopted.First100~ LifeStage. + Group + FreeAdoption + HasVideos + HasPhotos + 
                   Primary.ColorName + Type. + Gender. + MaturitySize. + FurLength. + Vaccinated. + Dewormed. +
                   Sterilized. + Health. + MixedBreed + Multicolor + HasName + NameLength + DescriptionLength + 
                   NumRescues + Breed_Rarity + Color_Breed_Rarity + StateName, data=adoption_rf, method="rf", trControl=control)
rf_full

plot(rf_full)

confusionMatrix(rf_full, "none")


summary(rf_full)






