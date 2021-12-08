# final figures including 2020 data
# date: 2/9/2021

setwd("C:/Users/Eliza Fink/Desktop/WfG 2021")

library(tidyverse)
library(lattice)
library(caret)
library(olsrr)
library(RCurl)
library(Matrix)
library(lme4)
library(ggpubr)
library(extrafont)
library(sjPlot)

load("model.Rdata")

# fix model types ---------------------------------------------------------

# set reference level for pump type
model$pump_type <- relevel(model$pump_type, "hpv_60")
#model$dist_office <- as.vector(model$dist_office)
model$activity_date <- as.numeric(model$activity_date)

#no logical
model$last_funct <- as.factor(model$last_funct)
model$eep_moto <- as.factor(model$eep_moto)
model$rapid <- as.factor(model$rapid)
model$payment_YN <- as.factor(model$payment_YN)
model$conflict <- as.factor(model$conflict)
model$pump_working <- as.factor(model$pump_working)
model$pump_fixed <- as.factor(model$pump_fixed)
model$follow_up <- as.factor(model$follow_up)
model$Year <- factor(model$Year, ordered = FALSE)

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




# working plots -----------------------------------------------------------

# significant, hypothesis variables:

# Days since last visit

w_11 <- plot_model(glmer_working, type = "eff", terms = "time_between [all]",
                   show.values = TRUE) +
  theme_classic() +
  theme(text=element_text(family="Times New Roman", face="bold", size=22),
        axis.text.x = element_text(angle=30, size = 20)) +
  scale_y_continuous(limits = c(0,1),
                     labels = scales::percent) +
  scale_x_continuous(limits = c(0,2500),
                     breaks = c(0, 500, 1000, 1500, 2000)) +
  ggtitle("") +
  labs(y = "P(pump working)", x = "(a) \n ")

# WfG office

w_23 <- plot_model(glmer_working, type = "eff", terms = "dist_office [all]",
                   show.values = TURE) +
  theme_classic() + 
  theme(text=element_text(family="Times New Roman", face = "bold", size=22),
        axis.text.x = element_text(angle=30, size=20)) +
  scale_y_continuous(limits = c(.75,1),
                     labels = scales::percent) +
  ggtitle("") +
  labs(y = "P(pump working)", x = "Distance to Nearest \n WFG Office \n (km)")

# Urban Center

w_22 <- plot_model(glmer_working, type = "eff", terms = "urban_center [all]",
                   show.values = TURE) +
  theme_classic() + 
  theme(text=element_text(family="Times New Roman", face = "bold", size=22),
        axis.text.x = element_text(angle=30, size=20)) +
  scale_y_continuous(limits = c(0,1),
                     labels = scales::percent) +
  ggtitle("") +
  labs(y = "P(pump working)", x = "Distance to \n Urban Center \n (km)")

# Nearest Neighbor

w_21 <- plot_model(glmer_working, type = "eff", terms = "average_nn [all]",
                   show.values = TRUE) +
  theme_classic() +
  theme(text=element_text(family="Times New Roman", face="bold", size=22),
        axis.text.x = element_text(angle=30, size = 20)) +
  scale_y_continuous(limits = c(0,1),
                     labels = scales::percent) +
  ggtitle("") +
  labs(y = "P(pump working)", x = "Average Distance to \n 5 Nearest Wells \n (km)")

# Hybrid Motorcycle

w_12 <- plot_model(glmer_working, type = "eff", terms = "eep_moto",
                   show.values = TRUE) +
  theme_classic() +
  theme(text=element_text(family="Times New Roman", face="bold", size=22),
        axis.text.x = element_text(angle=30, size = 20)) +
  scale_y_continuous(limits = c(0.75,1),
                     labels = scales::percent) +
  ggtitle("") +
  labs(y = "P(pump working)", x = "Hybrid Motorcycle Program")

# Rapid Response Program

w_13 <- plot_model(glmer_working, type = "eff", terms = "rapid",
                   show.values = TRUE) +
  theme_classic() +
  theme(text=element_text(family="Times New Roman", face="bold", size=22),
        axis.text.x = element_text(angle=30, size = 20)) +
  scale_y_continuous(limits = c(.75,1),
                     labels = scales::percent) +
  ggtitle("") +
  labs(y = "P(pump working)", x = "Rapid Response Program")



# fixed plots -------------------------------------------------------------

# significant, hypothesis variables:

# days since last visit
f_11 <- plot_model(glmer_fixed, type = "eff", terms = "time_between [all]",
                   show.values = TRUE) +
  theme_classic() +
  theme(text=element_text(family="Times New Roman", face="bold", size=22),
        axis.text.x = element_text(angle=30, size = 20)) +
  scale_y_continuous(limits = c(0,1),
                     labels = scales::percent) +
  scale_x_continuous(limits = c(0,2500),
                     breaks = c(0, 500, 1000, 1500, 2000)) +
  ggtitle("") +
  labs(y = "P(pump fixed)", x = "(b) \n Time Since Last Visit (days)")

# hybrid motorcycle
f_21 <- plot_model(glmer_fixed, type = "eff", terms = "eep_moto",
                   show.values = TRUE) +
  theme_classic() +
  theme(text=element_text(family="Times New Roman", face="bold", size=22),
        axis.text.x = element_text(angle=30, size = 20)) +
  scale_y_continuous(limits = c(.90,1),
                     labels = scales::percent) +
  ggtitle("") +
  labs(y = "P(pump fixed)", x = "Hybrid Motorcycle Program")

# Payment 
f_22 <- plot_model(glmer_fixed, type = "eff", terms = "payment_YN",
                   show.values = TRUE) +
  theme_classic() +
  theme(text=element_text(family="Times New Roman", face="bold", size=22),
        axis.text.x = element_text(angle=30, size = 20)) +
  scale_y_continuous(limits = c(.90,1),
                     labels = scales::percent) +
  ggtitle("") +
  labs(y = "P(pump fixed)", x = "Community Payment \n (a)")

# distance to office
f_12 <- plot_model(glmer_fixed, type = "eff", terms = "urban_center [all]",
                   show.values = TURE) +
  theme_classic() + 
  theme(text=element_text(family="Times New Roman", face = "bold", size=22),
        axis.text.x = element_text(angle=30, size=20)) +
  scale_y_continuous(limits = c(0,1),
                     labels = scales::percent) +
  ggtitle("") +
  labs(y = "P(pump fixed)", x = "Distance to \n Urban Center \n (km)")


# urban center
f_13 <- plot_model(glmer_fixed, type = "eff", terms = "dist_office [all]",
                   show.values = TURE) +
  theme_classic() + 
  theme(text=element_text(family="Times New Roman", face = "bold", size=22),
        axis.text.x = element_text(angle=30, size=20)) +
  scale_y_continuous(limits = c(0,1),
                     labels = scales::percent) +
  ggtitle("") +
  labs(y = "P(pump fixed)", x = "Distance to Nearest \n WFG Office \n (km)")



# payment -----------------------------------------------------------------

# significant, hypothesis variables

# days since last visit
p_11 <- plot_model(glmer_payment, type = "eff", terms = "time_between [all]",
                   show.values = TRUE) +
  theme_classic() +
  theme(text=element_text(family="Times New Roman", face="bold", size=22),
        axis.text.x = element_text(angle=30, size = 20)) +
  scale_y_continuous(limits = c(0,.18),
                     labels = scales::percent) +
  scale_x_continuous(limits = c(0,2500),
                     breaks = c(0, 500, 1000, 1500, 2000)) +
  ggtitle("") +
  labs(y = "P(payment)", x = "(c) \n")


# distance to office
p_12 <- plot_model(glmer_payment, type = "eff", terms = "dist_office [all]",
                   show.values = TURE) +
  theme_classic() + 
  theme(text=element_text(family="Times New Roman", face = "bold", size=22),
        axis.text.x = element_text(angle=30, size=20)) +
  scale_y_continuous(limits = c(0,.18),
                     labels = scales::percent) +
  ggtitle("") +
  labs(y = "P(payment)", x = "Distance to Nearest \n WFG Office \n (km)")


# hybrid motorcycle
p_13 <- plot_model(glmer_payment, type = "eff", terms = "eep_moto",
                   show.values = TRUE) +
  theme_classic() +
  theme(text=element_text(family="Times New Roman", face="bold", size=22),
        axis.text.x = element_text(angle=30, size = 20)) +
  scale_y_continuous(limits = c(0,.18),
                     labels = scales::percent) +
  ggtitle("") +
  labs(y = "P(payment)", x = "Hybrid Motorcycle Program")

# rapid response
p_14 <- plot_model(glmer_payment, type = "eff", terms = "rapid",
                   show.values = TRUE) +
  theme_classic() +
  theme(text=element_text(family="Times New Roman", face="bold", size=22),
        axis.text.x = element_text(angle=30, size = 20)) +
  scale_y_continuous(limits = c(0,.18),
                     labels = scales::percent) +
  ggtitle("") +
  labs(y = "P(payment)", x = "Rapid Response Program")


# functionality on last visit
p_21 <- plot_model(glmer_payment, type = "eff", terms = "last_funct",
                   show.values = TRUE) +
  theme_classic() +
  theme(text=element_text(family="Times New Roman", face="bold", size=22),
        axis.text.x = element_text(angle=30, size = 20)) +
  scale_y_continuous(limits = c(0,.18),
                     labels = scales::percent) +
  ggtitle("") +
  labs(y = "P(payment)", x = "Functionality at Previous \n Visit \n (b)")



# pump working
p_22 <- plot_model(glmer_payment, type = "eff", terms = "pump_working",
                   show.values = TRUE) +
  theme_classic() +
  theme(text=element_text(family="Times New Roman", face="bold", size=22),
        axis.text.x = element_text(angle=30, size = 20)) +
  scale_y_continuous(limits = c(0,.18),
                     labels = scales::percent) +
  ggtitle("") +
  labs(y = "P(payment)", x = "Pump Working \n on Arrival \n (c)")

# pump fixed
p_23 <- plot_model(glmer_payment, type = "eff", terms = "pump_fixed",
                   show.values = TRUE) +
  theme_classic() +
  theme(text=element_text(family="Times New Roman", face="bold", size=22),
        axis.text.x = element_text(angle=30, size = 20)) +
  scale_y_continuous(limits = c(0,.18),
                     labels = scales::percent) +
  ggtitle("") +
  labs(y = "P(payment)", x = "Pump Fixed \n on Departure \n (d)")

#################################

mf1 <- ggarrange(f_22, p_21, p_22, p_23,
                           nrow = 2, ncol = 2, align = 'hv')

mf1


mf2 <- ggarrange(w_11, f_11, p_11,
                 nrow = 1, ncol = 3, align = 'hv')
mf2
