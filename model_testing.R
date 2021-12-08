# tests assumptions for logistic regression
# date: 01/31/2021

library(tidyverse)
library(lattice)
library(caret)
library(olsrr)
library(RCurl)
library(Matrix)
library(lme4)

setwd("C:/Users/Eliza Fink/Desktop/WfG 2021")

load("model.Rdata") #missing data already removed

# outliers - mahalanobis
# first, drop all the factor columns, mahalanobis only runs on continuous variables
# continuous variables ~ time_between, dist_office, sum_past_broken, 
#                        percent_past_broken, urban_center, population, average_nn

# screen for outliers -----------------------------------------------------


# create mahalanobis scores (multidimensional distance from mean)
mahal <- mahalanobis(model[ , c("time_between", "sum_past_broken", "percent_past_broken",
                                "dist_office", "urban_center", "average_nn", "Population_1k",
                                "Population_3k")], 
                     colMeans(model[ , c("time_between", "sum_past_broken", "percent_past_broken",
                                         "dist_office", "urban_center", "average_nn", "Population_1k",
                                         "Population_3k")], na.rm = TRUE),
                     cov(model[ , c("time_between", "sum_past_broken", "percent_past_broken",
                                    "dist_office", "urban_center", "average_nn", "Population_1k",
                                    "Population_3k")], use="pairwise.complete.obs"))


# find cutoff for bad scores

cutoff = qchisq(1 - .001,ncol(model[ , c("time_between", "sum_past_broken", "percent_past_broken",
                                         "dist_office", "urban_center", "average_nn", "Population_1k",
                                         "Population_3k")]))

ncol(model[ , c("time_between", "sum_past_broken", "percent_past_broken",
                "dist_office", "urban_center", "average_nn", "Population_1k",
                "Population_3k")]) ##this is degrees of freedom (for chi squared not analysis)
cutoff

#find out how many observations are outliers
summary(mahal < cutoff)
#647 observations are outliers out of 16399, ~4%
#check later if removing these pumps changes things dramatically

# remove outliers
no_outliers <- subset(model, mahal < cutoff)
# no outliers, 15752 observations 83.17% of original data
# 2829 / 15752 not working, ~ 17.96%
# 1582 / 15752 not fixed, ~ 10.04%

# look at outliers
outliers <- subset(model, mahal > cutoff)
# 237 / 647 pumps not working ~ 37.20%
# 172 / 647 pumps not fixed ~ 26.58%

#full data 
# 3066 / 16399 pumps not working, 10.70%
# 1754 / 16399 pumps not fixed, 18.70%

# check for multiple colinearity ------------------------------------------

#functionality factors
lm_fxn <- lm(pump_working ~ last_funct + time_between + follow_up + activity_date +
               pump_type + sum_past_broken + percent_past_broken + eep_moto + rapid +
               payment_YN + dist_office + urban_center +
               Population_3k + average_nn + conflict, data = model)

# without prefecture and sous-prefecture VIF scores are okay
# prefecture and sous-prefecture removed from model dataframe in stats_dataframe.R file
# after initially testing VIF scores. Still present in full_data.Rdata df
# can't have both year and activity date, or activity date and month

VIF_fxn <- data.frame(ols_vif_tol(lm_fxn))

VIF_fxn

lm_pmt <- lm(payment_YN ~ activity_date + last_funct + time_between +
               pump_type + sum_past_broken + percent_past_broken + eep_moto + rapid +
               + dist_office + urban_center + pump_fixed + pump_working +
               Population_3k + average_nn + conflict + follow_up, data = model)

VIF_pmt <- data.frame(ols_vif_tol(lm_pmt))

VIF_pmt

#fine if you remove sous-prefecture and prefecture, only activity date or year


# DV Ratios ---------------------------------------------------------------
# dependent variable ratios

summary(model$pump_working) #4.4:1 working : broken
summary(model$pump_fixed) #8.3:1 working : broken, very different from last time... check to see what is wrong`  `
summary(model$payment_YN) #5.4:1 no payment : payment

summary(no_outliers$pump_working) #4.6:1 working : broken
summary(no_outliers$pump_fixed) #9.1:1 working : broken, very different from last time... check to see what is wrong`  `
summary(no_outliers$payment_YN) #5.7:1 no payment : payment


#############################################################################################
#there are three categories for models:
# 1) pump working, which is the value for whether or not the pump was functional at the time wfg mechanics arrived
# 2) pump fixed, which is the value for whether the pump was working at the time the wfg mechanics left
# 3) payment, is whether the mechanics recieved a payment from the community

# the below models were compared using AIC value and the P - value for [ACC > NIR]
# the following alternatives were considered:
#       data containing outliers and those without (outliers vs model)
#       logistic vs multilevel (glm vs glmer)
#       different combinations of covariables (examples: Year or activity date, excluding conflict info for functionality measures)



#
#
#         general linear models
#
#


# working on arrival --------------------------------------------

glm_working <- glm(pump_working ~ Year + last_funct + time_between +
                     pump_type + sum_past_broken + percent_past_broken + eep_moto + 
                     rapid + dist_office + urban_center +
                     average_nn + conflict + follow_up,
                   data = model, family="binomial" )

summary(glm_working)

pred_w <- glm_working$fitted.values >= 0.05
sum(pred_w == model$pump_working)/length(pred_w)

confusionMatrix(as.factor(pred_w), as.factor(model$pump_working))

# working on departure ------------------------------------------


glm_fixed <- glm(pump_fixed ~ Year + month + last_funct + time_between + follow_up +
                   sum_past_broken + percent_past_broken + eep_moto +
                   rapid + payment_YN + dist_office + urban_center + Population_3k +
                   average_nn + pump_type,
                 data = no_outliers, family="binomial")

summary(glm_fixed)

pred_f <- glm_fixed$fitted.values >= 0.5
sum(pred_f == no_outliers$pump_fixed)/length(pred_f)

confusionMatrix(as.factor(pred_f), as.factor(no_outliers$pump_fixed))


# payment -------------------------------------------------------------

glm_pmt <- glm(payment_YN ~ activity_date + last_funct + time_between +
                 sum_past_broken + percent_past_broken + eep_moto + pump_working +
                 rapid  + dist_office + urban_center + Population_3k +
                 average_nn + conflict + pump_fixed + follow_up,
               data = no_outliers, family="binomial")

summary(glm_pmt)

pred_p <- glm_pmt$fitted.values >= 0.5
confusionMatrix(as.factor(pred_p), as.factor(no_outliers$payment_YN))

#
#
#         multi-level general linear models
#
#
# accounts that maintenance visits are clustered into wells (correlated error)

# fixed on departure ----------------------------------------------

model$Year <- factor(model$Year, ordered = FALSE)

glmer_fixed <- glmer(pump_fixed ~ Year + last_funct + pump_type + month +
                       time_between + sum_past_broken + conflict +
                       percent_past_broken + eep_moto + follow_up +
                       rapid + payment_YN + dist_office + urban_center + Population_1k +
                       average_nn + (1 | well_ID),
                     data = model, 
                     family = binomial(link = 'logit'),
                     control = glmerControl(optimizer = "bobyqa"),
                     nAGQ = 0)

summary(glmer_fixed)

pred_mf <- predict(glmer_fixed, type="response") >= 0.5

as_factor(model$pump_fixed)

mean(pred_mf == model$pump_fixed)

confusionMatrix(as_factor((pred_mf)), as_factor(model$pump_fixed))


# working on arrival ------------------------------------------------

glmer_working <- glmer(pump_working ~ activity_date + last_funct +
                         time_between + pump_type + sum_past_broken + 
                         percent_past_broken + eep_moto + follow_up +
                         rapid + dist_office + urban_center + Population_1k +
                         average_nn + (1 | well_ID),
                       data = model, 
                       family = binomial(link = 'logit'),
                       control = glmerControl(optimizer = "bobyqa"),
                       nAGQ = 0)

summary(glmer_working)

pred_mw <- predict(glmer_working, type="response") >= 0.5

mean(pred_mw == model$pump_working)

confusionMatrix(as_factor((pred_mw)), as_factor(model$pump_working))


# payment -----------------------------------------------------------

model$Year <- factor(model$Year, ordered = FALSE)

glmer_payment <- glmer(payment_YN ~ Year + last_funct +
                         time_between + conflict + follow_up +
                         eep_moto + pump_working + pump_fixed +
                         rapid + dist_office + urban_center + Population_1k +
                         average_nn + (1 | well_ID),
                       data = model, 
                       family = binomial(link = 'logit'),
                       control = glmerControl(optimizer = "bobyqa"),
                       nAGQ = 0)

summary(glmer_payment)

pred_mp <- predict(glmer_payment, type="response") >= 0.5

mean(pred_mp == model$payment_YN)

confusionMatrix(as_factor((pred_mp)), as_factor(model$payment_YN))

################################################################################################
####tables#### # R^2

tab_model(glmer_payment)
