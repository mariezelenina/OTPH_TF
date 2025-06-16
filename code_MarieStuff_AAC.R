library(nlme)
library(CorrMixed)
library(lme4)
library(lmerTest)
library(geepack)
library(Zelig)
library(ggplot2)
library(car)
library(emmeans)
library(effectsize)
library(simr)

names(data)[1] <- "Id"

# Load the cross-frequency coupling data. 
# Change the EC to EO and vice versa in the filename to 
# obtain results for Eyes open and eyes closed, respecitvely

data <- read.table("/Users/zeleninam2/Documents/Oxytocin_final_2024/dat_Rho_lmm_EO_Pearson.csv", header=T, sep=",")

data$Drug <- factor(data$Drug)
data$TP <- factor(data$TP)
data$id <- factor(data$id)

# run the model

# ------------------------------------------------------------------------------
# theta-alpha
t = lmer(theta_alpha ~ theta_alpha_base + TP*Drug + (1|id), data=data)
t
summary(t)
anova(t, type=3)

# do pairwise comparisons
emm_t = emmeans(t, specs = pairwise ~ Drug|TP, type = "response", adjust = "bonferroni")
emm_t

# ------------------------------------------------------------------------------
# theta-beta
a = lmer(theta_beta ~ theta_beta_base + TP*Drug + (1|id), data=data)
a
summary(a)
anova(a, type=3)

# do pairwise comparisons
emm_a = emmeans(a, specs = pairwise ~ Drug|TP, type = "response", adjust = "bonferroni")
emm_a

# ------------------------------------------------------------------------------
# alpha-beta
b = lmer(alpha_beta ~ alpha_beta_base + TP*Drug + (1|id), data=data)
b
summary(b)
anova(b, type=3)

# do pairwise comparisons
emm_b = emmeans(b, specs = pairwise ~ Drug|TP, type = "response", adjust = "bonferroni")
emm_b

# TP1
t_to_d(
  t = 2.688,
  df = 190
)

# TP6
t_to_d(
  t = 2.142,
  df = 189
)
# ------------------------------------------------------------------------------

