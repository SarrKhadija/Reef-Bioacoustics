#Install packages, including Tim Newbold's Statistical Modelling package for generalized linear models
install.packages("lme4")
library(devtools)
install_github("timnewbold/StatisticalModels")
install.packages("multilevel")
install.packages("purrr")
install.packages("nlme")
install.packages("kableExtra")
install.packages("psych",dependencies=TRUE)

#Load the libraries we need
library(ggplot2)
library(tidyverse)
library(dplyr)
library(lme4)
library(StatisticalModels)
library(MASS)
library(tidyr)
library(nlme) #For the multivariate mixed effects modelling
library(car) #For correlationmatrix
library(knitr) #For aesthetics
library(kableExtra) #For tables
library(broom) #For aesthetic plots
library(lattice)
library(grid)

library(purrr) #For nested effects quantification
library(nlme) #For nested effects quantification
library(kableExtra) #For nested effects quantification

#Colors are from Colorblind color palette by David MAth Logic https://davidmathlogic.com/colorblind/#%23D81B60-%231E88E5-%23FFC107-%23004D0C

#We will load several mixed effects model with the PCA results to assess the best candidate
#Set working directory
setwd("~/Documents/UCL Courses/UCLCourses terms 1 2 3/Term 3/Recordings_Experiment B/Results")

#Load csv with the metadata 
features_metadata_pca <- read.csv(file="features_metadata_pca.csv",
                                   fill = TRUE,
                                   header = TRUE)
view(features_metadata_pca)
summary(features_metadata_pca)

#Transform PC.1 and PC.2 variables into numeric values + log transform??
str(features_metadata_pca$PC.1)
str(features_metadata_pca$PC.2)

#Transform Recorder, Class, and Site variables as factors since they are categorical 
features_metadata_pca$Site <- as.factor(features_metadata_pca$Site)
features_metadata_pca$Recorder_ID <- as.factor(features_metadata_pca$Recorder_ID)
features_metadata_pca$Class <- as.factor(features_metadata_pca$Class)

#Transform the dataframe to evaluate whether PC1 or PC2 should 
features_metadata_pca_mme <- features_metadata_pca %>%
  pivot_longer(cols = c(PC.1, PC.2), names_to = "PC", values_to = "dependent")

#Plot histograms of PC.1 and PC.2, not best representation of coordinates but let's see
#PC.1 is slightly right skewed and PC.2 is mostly normal!
hist(features_metadata_pca$PC.1)
hist(features_metadata_pca$PC.2)



#Candidate Models: (from Model 1 onwards)
#0 Linear mixed effects model for PCA which reflects the hypothesis the best:
#FE: Class, RE: Site and Recorder
#model0_pc1 <- lmer(PC.1 ~ Class + (1 | Site / Recorder_ID), data = features_metadata_pca)
#model0_pc2 <- lmer(PC.2 ~ Class + (1 | Site / Recorder_ID), data = features_metadata_pca)
model1_pc1_2 <- lme(dependent ~ Class,
             random = ~ 1 | Site / Recorder_ID,
             data = features_metadata_pca_mme,
             weights = varIdent(form = ~ 1 | PC))

model1B_pc1_2 <- lmer(dependent ~ Class + (1|Site) + (1|Recorder_ID),
                     data = features_metadata_pca_mme,
                     weights = varIdent(form = ~ 1 | PC))

model1C_pc1 <- lme(PC.1 ~ Class,
                   random = ~ 1 | Site / Recorder_ID,
                     data = features_metadata_pca)

model1D_pc2 <- lme(PC.2 ~ Class,
                  random = ~ 1 | Site / Recorder_ID,
                  data = features_metadata_pca)

#2 Multivariate mixed effect model for PCA, 
#FE:Class RE: Site 
model2_pc1_2 <- lme(dependent ~ Class,
                    random = ~ 1 | Site,
                    data = features_metadata_pca_mme,
                    weights = varIdent(form = ~ 1 | PC))

#3 FE:Class RE: Recorder 
model3_pc1_2 <- lme(dependent ~ Class,
                    random = ~ 1 | Recorder_ID,
                    data = features_metadata_pca_mme,
                    weights = varIdent(form = ~ 1 | PC))

#4 FE: Class, Site RE:Recorder
#Get error" singularity in backsolve at level 0, block 1" which can signal multicollinearity
model4_pc1_2 <- lme(dependent ~ Class + Site,
                    random = ~ 1 | Recorder_ID,
                    data = features_metadata_pca_mme,
                    weights = varIdent(form = ~ 1 | PC))

#Correlation matrix won't work on categorical variables
corr_Class_Site<- cor(features_metadata_pca_mme[, c("Class", "Site")])
vif(model4_pc1_2)

#5FE: Class, Recorder RE: Site
model5_pc1_2 <- lme(dependent ~ Class + Recorder_ID,
                    random = ~ 1 | Site,
                    data = features_metadata_pca_mme,
                    weights = varIdent(form = ~ 1 | PC))

#6FE: Class * Site, RE: Recorder
#Interaction term creates error "Singularity in backsolve at level 0, block 1"
model6_pc1_2 <- lme(dependent ~ Class * Site,
                    random = ~ 1 | Recorder_ID,
                    data = features_metadata_pca_mme,
                    weights = varIdent(form = ~ 1 | PC))

#7FE: Recorder , RE: Site
model7_pc1_2 <- lme(dependent ~ Recorder_ID,
                     random = ~ 1 | Site,
                     data = features_metadata_pca_mme,
                     weights = varIdent(form = ~ 1 | PC))

#8FE: Class , RE: Site, Recorder (Crossed random effects)
model8_pc1_2 <- lmer(dependent ~ Class + (1 |Site) + (1 |Recorder_ID),
                   data = features_metadata_pca_mme)


#Test for independence of errors
#Results: 
#Residuals not independent for model0 on PC1
plot(model0_pc1,which=1)
#Residuals not independent for model0 on PC2
plot(model0_pc2,which=1)
#Residuals independent for model 1 on both PC1 and PC2
plot(model1_pc1_2)
#Residuals seem to have increasing variance compared to fitted values
plot(model1C_pc1)
#Residuals independent though some gaps 
plot(model1D_pc2)
#Very spread out residuals
plot(model2_pc1_2, which=1)
#Very spread out residuals
plot(model3_pc1_2)
#Non-independent residuals (empty space between -3 and 0) 
plot(model5_pc1_2)
#Model did not converge
plot(model7_pc1_2)
#Residuals are very spread out  
plot(model8_pc1_2)


#Test for normality of errors
#Errors are pretty normal, despite slightly long right tail
hist(residuals(model1_pc1_2))
#normal residuals for both PC.1 and PC.2
hist(residuals(model1C_pc1))
hist(residuals(model1D_pc2))

              
#Test for normality of variance
qqnorm(residuals(model0_pc1),pch=16); qqline(residuals(model0_pc1))
qqnorm(residuals(model0_pc2),pch=16); qqline(residuals(model0_pc2))
#Though errors are constant, qqplot is very right skewed
qqnorm(residuals(model1_pc1_2),pch=16); qqline(residuals(model1_pc1_2))
#PC1 shows slightly unequal variance while PC2 shows relatively unequal variance 
qqnorm(residuals(model1C_pc1),pch=16); qqline(residuals(model1C_pc1))
qqnorm(residuals(model1D_pc2),pch=16); qqline(residuals(model1D_pc2))
#qqplot is very right skewed, a little left skewed
qqnorm(residuals(model2_pc1_2),pch=16); qqline(residuals(model2_pc1_2))
#qqplot is very right skewed 
qqnorm(residuals(model3_pc1_2),pch=16); qqline(residuals(model3_pc1_2))
#qqplot is kinda left skewed and very right skewed 
qqnorm(residuals(model5_pc1_2),pch=16); qqline(residuals(model5_pc1_2))
#qqplot is very right skewed 
qqnorm(residuals(model8_pc1_2),pch=16); qqline(residuals(model8_pc1_2))

#Fixed effects estimates
#Model1
fixed_effects1 <- fixef(model1_pc1_2)
fixed_effects1
#Model2
fixed_effects2 <- fixef(model2_pc1_2)
fixed_effects2
#Model 3
fixed_effects3 <- fixef(model3_pc1_2)
fixed_effects3
#Model 4
fixed_effects4 <- fixef(model4_pc1_2)
fixed_effects4

#Testing for homoscedasticity in model 1
residuals1 <- residuals(model1_pc1_2)
fitted_values1 <- fitted(model1_pc1_2)

#Create a scatterplot of residuals against fitted values
plot(fitted_values1, residuals1,
     xlab = "Fitted Values",
     ylab = "Residuals",
     main = "Residuals vs. Fitted Values",
     abline(h = 0, col = "red")
)

#Testing for homoscedasticity in models 1C and 1D
residuals1C <- residuals(model1C_pc1)
fitted_values1C <- fitted(model1C_pc1)
residuals1D <- residuals(model1D_pc2)
fitted_values1D <- fitted(model1D_pc2)


#Create 3 scatterplot of residuals against the three predictors 
#Because the multivariate model requires an elongated dataset, this doesn't let us
#Visualize the effect of given predictors on residuals. We will use model 1C and 1B to create scatter plots for PC.1 and PC.2
# Create scatter plots for model1C_pc1
par(mfrow = c(1, 3))

plot(features_metadata_pca$Class, residuals1C,
     xlab = "Class",
     ylab = "Residuals",
     main = "Residuals vs. Class (PC.1)")

plot(features_metadata_pca$Site, residuals1C,
     xlab = "Site",
     ylab = "Residuals",
     main = "Residuals vs. Site (PC.1)")

plot(features_metadata_pca$Recorder_ID, residuals1C,
     xlab = "Recorder_ID",
     ylab = "Residuals",
     main = "Residuals vs. Recorder_ID (PC.1)")

#Create scatter plots for model1D_pc2
par(mfrow = c(1, 3))

plot(features_metadata_pca$Class, residuals1D,
     xlab = "Class",
     ylab = "Residuals",
     main = "Residuals vs. Class (PC.2)")

plot(features_metadata_pca$Site, residuals1D,
     xlab = "Site",
     ylab = "Residuals",
     main = "Residuals vs. Site (PC.2)")

plot(features_metadata_pca$Recorder_ID, residuals1D,
     xlab = "Recorder_ID",
     ylab = "Residuals",
     main = "Residuals vs. Recorder_ID (PC.2)")

#We will sensitivity check this to assess which of the three varibales contributed most to residual sprad for both PC.1 and PC.2
residual1_df <- data.frame(
  Residuals = residuals1C,
  Class = features_metadata_pca$Class,
  Site = features_metadata_pca$Site,
  Recorder_ID = features_metadata_pca$Recorder_ID
)

residual2_df <- data.frame(
  Residuals = residuals1D,
  Class = features_metadata_pca$Class,
  Site = features_metadata_pca$Site,
  Recorder_ID = features_metadata_pca$Recorder_ID
)
view(residual1_df)

#Perform ANOVA of predictors on PC.1 and PC.2 from original dataframe
#PC.1
anova_class <- aov(Residuals ~ Class, data = residual1_df)
anova_site <- aov(Residuals ~ Site, data = residual1_df)
anova_rec <- aov(Residuals ~ Recorder_ID, data = residual1_df)

#PC.2
anova_class2 <- aov(Residuals ~ Class, data = residual2_df)
anova_site2 <- aov(Residuals ~ Site, data = residual2_df)
anova_rec2 <- aov(Residuals ~ Recorder_ID, data = residual2_df)

#Print results
print(summary(anova_class))
print(summary(anova_site))
print(summary(anova_rec))
print(summary(anova_class2))
print(summary(anova_site2))
print(summary(anova_rec2))
#Cook's distance 
#cooks.distance(model0_pc1)
# Calculate Cook's distance
#influence_1 <- influence(model1_pc1_2)
#cooksd1 <- cooks.distance(influence_data_1)

#Assess explanatory Power of model  
#Variance terms of Site, and Recorder
varr1 <- VarCorr(model1_pc1_2)
VarCorr(model2_pc1_2)
VarCorr(model3_pc1_2)
VarCorr(model5_pc1_2)

summary(model8_pc1_2)

#Compare models using AIC criterion
AIC1 <- AIC(model1_pc1_2)
AIC2 <- AIC(model2_pc1_2)
AIC3 <- AIC(model3_pc1_2)
AIC5 <- AIC(model5_pc1_2)
AIC8 <- AIC(model8_pc1_2)

#Create a data frame for AIC comparison
AIC_comparison <- data.frame(
  Model = c("model1_pc1_2","model2_pc1_2", "model3_pc1_2","model5_pc1_2","model8_pc1_2"),
  AIC = c(AIC1, AIC2, AIC3, AIC5, AIC8)
)

#Sort by AIC in ascending order
AIC_comparison <- AIC_comparison[order(AIC_comparison$AIC), ]

#Print the comparison table
print(AIC_comparison)

#Let's make a results table using our selected model outputs
results_table <- data.frame(
  `Fixed/Random effects` = c("Intercept", "Healthy", "Site", "Recorder", "Residual"),
  Estimate = c(3.174357, -6.348714, 2.874488, 4.881971, 18.36665),
  `Standard deviation` = c(1.615488, 2.284645, NA, NA, NA),
  `T-value` = c(1.964953, -2.778863, NA, NA, NA),
  `P-value` = c(0.0494, 0.0240, NA, NA, NA)
)

#Create the results table
results_table <- knitr::kable(results_table, align = "lccrr", caption = "Results Table", format = "latex", digits = 4, booktabs = TRUE)
print(results_table)

#Now that we select candidate 1B as the one model to rule them all, let's test significance of our predictor variable
summary(model1B_pc1_2)
model1_pc1_2

#Calculate confidence intervals
#Calculate confidence intervals for fixed and random effects
intervals(model1_pc1_2, which = "fixed")
intervals(model1_pc1_2, which = "var-cov")

#Let's analyse intra-site variance more deeply
#Create and compute intraclass correlation coefficients. Extract variance components
varcomp <- VarCorr(model1_pc1_2)
varcomp
#Extract random effects estimates
random_effects1 <- ranef(model1_pc1_2)
random_effects1


#ICC plot. ICC was computed manually by dividing the individual site variance with the total (pooled) variance
icc_values <- c(0.61, 0.31, 0.26, 0.06, 1.23, 0.29, 0.09, 0.86, 0.23, 0.24)
sites <- c("D1", "D2", "D3", "D4", "D5", "H1", "H2", "H3", "H4", "H5")

#Create the ICC and site dataframe
icc_df <- data.frame(
  Class = c(rep("Degraded", 5), rep("Healthy", 5)),
  Site_ID = sites,
  ICC = icc_values
)

view (icc_df)

ggplot(icc_df, aes(x = ICC, y = reorder(Site_ID, ICC), fill = Class)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values=c("#4D0000", "#004D0C")) +
  labs(title = "ICC by Site and Class",
       x = "Intraclass Correlation Coefficient (ICC)",
       y = "Site") +
  facet_wrap(~ Class) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

#Boxplots
#Let's rename the recorder and sites (something messed up plot labels in site mapping redo)
 site_mapping <- c(
   "GoodControl3" = "H1", 
   "GoodControl2" = "H2", 
   "KecilHealthy" = "H3", 
   "GosongHealthyNorth" = "H4", 
   "GosongHealthy2" = "H5", 
   "KecilDegraded" = "D1", 
   " NorthCentralWestSector" = "D2", 
   "SailisiBDegradedCES" = "D3", 
   "BesarSES" = "D4", 
   "DegradedGosong" = "D5"
)


levels(features_metadata_pca_mme$Recorder_ID) <- c("1", "2", "3", "4", "5")

#Visualizations
Boxplot1 <- ggplot(features_metadata_pca_mme, aes(x = Site, y = dependent, color = Class)) +
  geom_boxplot()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
Boxplot1

Boxplot2 <- ggplot(features_metadata_pca_mme, aes(x = Site, y = dependent, fill = Class)) +
  geom_boxplot() +
  facet_wrap(~ Class) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
Boxplot2

#violin plot
violin_site <- ggplot(features_metadata_pca_mme, aes(x = Site, y = dependent, fill = Class)) +
  geom_violin() +
  facet_wrap(~ Class) +
  labs(x = "",
       y = " Panel A ") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

violin_site

violin_site2 <- ggplot(features_metadata_pca_mme, aes(x = Site, y = dependent, fill = Class)) +
  geom_violin() +
  labs(x = "Site",
       y = "PCA") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

violin_site2


violin_rec_facet_site <- ggplot(features_metadata_pca_mme, aes(x = Recorder_ID, y = dependent, fill = Recorder_ID)) +
  geom_violin() +
  facet_wrap(~ Site) +
  labs(
       x = "Recorder ID",
       y = "A")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
violin_rec_facet_site 

violin_site_facet_rec <- ggplot(features_metadata_pca_mme, aes(x = Site, y = dependent, fill = Recorder_ID)) +
  geom_violin() +
  facet_wrap(~ Recorder_ID) +
  labs(x = "Recorder ID",
       y = "B")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
violin_site_facet_rec 


