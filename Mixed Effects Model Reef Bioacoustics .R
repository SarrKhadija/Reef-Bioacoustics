#Install packages, including Tim Newbold's Statistical Modelling package for generalized linear models
install.packages("lme4")
library(devtools)
install_github("timnewbold/StatisticalModels")

# Load the libraries we need
library(ggplot2)
library(tidyverse)
library(dplyr)
library(lme4)
library(StatisticalModels)
library(MASS)

#We will load several mixed effects model with the UMAP results to assess the best candidate
#Set working directory
setwd("~/Documents/UCL Courses/UCLCourses terms 1 2 3/Term 3/Recordings_Experiment B/Results")

#Load csv with the metadata 
features_metadata_umap <- read.csv(file="features_metadata_umap.csv",
                              fill = TRUE,
                              header = TRUE)
view(features_metadata_umap)
summary(features_metadata_umap)


#Transform UMAP variables into numeric values
str(features_metadata_umap$UMAP.1)
str(features_metadata_umap$UMAP.2)

#Transform Recorder, Class, and Site variables as factors since they are categorical 
features_metadata_umap$Site <- as.factor(features_metadata_umap$Site)
features_metadata_umap$Recorder_ID <- as.factor(features_metadata_umap$Recorder_ID)
features_metadata_umap$Class <- as.factor(features_metadata_umap$Class)

#Group features under one mean features variable, figure out how to do mean across columns instead of rows 
#meanfeatures <- features_metadata_umap %>% group_by(feature_1) %>%
 # summarise (mean_features=mean(feature_),
#             .groups ='drop')
#meanfeatures

#Let's plot the relationship between health status and UMAP 1 and 2
ggplot(features_metadata_umap,aes(x=Class,y=UMAP.1))+
  geom_point()+
  theme_classic()


#0 Linear mixed effects model for UMAP1 which reflects the hypothesis the best:
model0_umap1 <- lmer(UMAP.1 ~ Class + (1 | Site / Recorder_ID), data = features_metadata_umap)


#1 Linear mixed effects model for UMAP1, I should use a mean features value cause with 
model1_umap1 <- lmer(UMAP.1 ~ Class + (1 | Site / Recorder_ID), data = features_metadata_umap)

model1B_umap1 <- lmer(UMAP.1 ~ 1 + (1 | Site), data = features_metadata_umap)

#2 Linear mixed effect model for UMAP1, Nested  + crossed structure 
#FE:Class RE: Site w intercept based on class
# A:   Model failed to converge: degenerate  Hessian with 1 negative eigenvalues
model2_umap1 <- lmer(UMAP.1 ~ Class + (Class | Site), data = features_metadata_umap)


#3 Linear mixed effect model for UMAP1, Nested + crossed structure 
#FE: Class, RE: Site, Microsite, recording
model3_umap1 <-lmer(UMAP.1 ~ Class + (1 | Recorder_ID/Site) + (1 | filename), data = features_metadata_umap)
model2_umap1 <-lmer(UMAP.2 ~ Class + (1 | Recorder_ID/Site) + (1 | filename), data = features_metadata_umap)


#Test for equality of variance and normality of errors and of random effects
#One main outlier in variance at extremity of x axis
plot(model1_umap1,which=1)


#Test for outliers with cook's distance plot

 
cooks.distance(model1_umap1)


#Linear downloard trend around 0 between 0 and 10
plot(model3_umap1, which=1)

#Test for equality of variance and normality of errors and of random effects
#Both models1 and 3 have a linear negative relationship between errors and fitted
plot(fitted(model1_umap1),residuals(model1_umap1))
plot(fitted(model3_umap1),residuals(model3_umap1))

model1_umap1
plot(model1_umap1)
print(summary(model1_umap1))

#Pseudo R2 values
R2GLMER(model1_umap1)
#Checkout random effects
print(ranef(model1_umap1))

#Compare model that doesn't use class as a predictor to one that does 
anova(model1_umap1,model1B_umap1)

# Linear mixed effects model for UMAP2
model1_umap2 <- lmer(UMAP2 ~ Class + (1 | Site / Recorder_ID), data = features_metadata)

#Assessing collinearity between predictor variables

#Assess interaction effects

#Remove interaction if necessary 

#Center or standardize the model esp with independent effects at large values. 