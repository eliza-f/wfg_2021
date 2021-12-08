# final model
# 2/1/2021

setwd("C:/Users/Eliza Fink/Desktop/WfG 2021")

library(sjPlot)
library(tidyverse)
library(lattice)
library(caret)
library(olsrr)
library(RCurl)
library(Matrix)
library(lme4)

# load data ---------------------------------------------------------------

load("model.Rdata")

# models ------------------------------------------------------------------

glmer_working <- glmer(pump_working ~ last_funct + activity_date +
                         time_between + pump_type + sum_past_broken + 
                         percent_past_broken + eep_moto + follow_up +
                         rapid + dist_office + urban_center + Population_1k +
                         average_nn + (1 | well_ID),
                       data = model, 
                       family = binomial(link = 'logit'),
                       control = glmerControl(optimizer = "bobyqa"),
                       nAGQ = 0)


working.glmm <- lmer(pump_working ~ last_funct + activity_date +
                       time_between + pump_type + sum_past_broken + 
                       percent_past_broken + eep_moto + follow_up +
                       rapid + dist_office + urban_center + Population_1k +
                       average_nn + (1 | well_ID), 
                     data = model ,family="binomial", method="Laplace")


nms <- rownames(ranef(glmer_working)$well_ID)
intercepts <- ranef(glmer_working)$well_ID[,1]
support <- tapply(model$well_ID,model$well_ID,length)
labels <- paste(nms,support)
barplot(intercepts[order(intercepts)],names.arg=labels[order(intercepts)],
          las=2,mgp=c(3,-0.5,0),ylim=c(-0.5,.5)) # mgp fix to give room for verb names

shapiro.test(ranef(glmer_working)$well_ID[,1])
shapiro.test(ranef(glmer_fixed)$well_ID[,1])
shapiro.test(ranef(glmer_payment)$well_ID[,1])
# random effects are not normally distributed...


#year to factor 
model$Year <- factor(model$Year, ordered = FALSE)
# by including years as factors, you automatically account for conflict years
# (conflict and year are colinear), also account for differences in years operations?

glmer_fixed <- glmer(pump_fixed ~ last_funct + pump_type +
                       time_between + sum_past_broken + conflict +
                       percent_past_broken + eep_moto + follow_up +
                       rapid + payment_YN + dist_office + urban_center + Population_1k +
                       average_nn + (1 | well_ID),
                     data = model, 
                     family = binomial(link = 'logit'),
                     control = glmerControl(optimizer = "bobyqa"),
                     nAGQ = 0)

glmer_payment <- glmer(payment_YN ~ last_funct + Year + month +
                         time_between + follow_up +
                         eep_moto + pump_working + pump_fixed +
                         rapid + dist_office + urban_center + Population_1k +
                         average_nn + (1 | well_ID),
                       data = model, 
                       family = binomial(link = 'logit'),
                       control = glmerControl(optimizer = "bobyqa"),
                       nAGQ = 0)

# summaries ---------------------------------------------------------------

summary(glmer_working)
summary(glmer_fixed)
summary(glmer_payment)

# confusion matrix --------------------------------------------------------

confusionMatrix(as_factor(predict(glmer_working, type="response") >= 0.5), 
                as_factor(model$pump_working))
confusionMatrix(as_factor(predict(glm_working, type="response") >= 0.5), 
                as_factor(model$pump_working))


confusionMatrix(as_factor(predict(glmer_fixed, type="response") >= 0.5), 
                as_factor(model$pump_fixed))
confusionMatrix(as_factor(predict(glmer_payment, type="response") >= 0.5), 
                as_factor(model$payment_YN))

# confidence intervals ----------------------------------------------------

cc_working <- confint(glmer_working, parm = 'beta_', method = 'Wald')
cc_fixed <- confint(glmer_fixed, parm = 'beta_', method = 'Wald')
cc_payment <- confint(glmer_payment, parm = 'beta_', method = 'Wald')




# R^2 tables --------------------------------------------------------------

tab_model(glmer_working)
tab_model(glmer_fixed)
tab_model(glmer_payment)


# output tables -----------------------------------------------------------

working_table <- cbind(coef(summary(glmer_working)), cc_working)
fixed_table <- cbind(coef(summary(glmer_fixed)), cc_fixed)
payment_table <- cbind(coef(summary(glmer_payment)), cc_payment)

write.csv(working_table, file = "model_working.csv")
write.csv(fixed_table, file = "model_fixed.csv")
write.csv(payment_table, file = "model_payment.csv")
