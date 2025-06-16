library(nlme)
library(CorrMixed)
library(lme4)
library(lmerTest)
library(geepack)
library(ggplot2)
library(car)
library(emmeans)
library(effectsize)
library(simr)

names(data)[1] <- "Id"

# Load the cross-frequency coupling data. 
# Change the EC to EO and vice versa in the filename to 
# obtain results for Eyes open and eyes closed, respecitvely

data <- read.table("/Users/zeleninam2/Documents/projects/Oxytocin_final_2024/dat_EO_relPower.csv", header=T, sep=",")

data$Drug <- factor(data$Drug)
data$TP <- factor(data$TP)
data$id <- factor(data$id)

# run the model

# ------------------------------------------------------------------------------
#THETA
t = lmer(Theta ~ Theta_base + TP*Drug + (1|id), data=data)
t
summary(t)
anova(t, type=3)


# do pairwise comparisons
emm_t = emmeans(t, specs = pairwise ~ Drug|TP, type = "response", adjust = "none")
emm_t

t_to_d(
  t = 1.605,
  df_error = 190
)
# ------------------------------------------------------------------------------

# ALPHA
a = lmer(Alpha ~ Alpha_base + TP*Drug + (1|id), data=data)
a
summary(a)
anova(a, type=3)

# do pairwise comparisons
emm_a = emmeans(a, specs = pairwise ~ Drug|TP, type = "response", adjust = "none")
emm_a

# We only see sig in TP1
t_to_d(
   t = -2.167,
   df = 190
)

lapply('Alpha', function(r) {
  f <- formula(paste(r, "~TP*Drug+",paste(r,'_base',sep=''),"+(1|id)", sep = ""))
  m <- lmer(f, data=data)
  m.emm <- emmeans(m, specs = pairwise ~ Drug|TP, type = "response")
  
  # edf = degree of freedom from the specific timewindow
  #eff_size(m.emm, sigma = sigma(m), edf = 158.2382) # edf - the df of the contrast in that specific time window
  
  contraststable <- data.frame(m.emm$contrasts)
  
  contraststable$p.value
  #p.adjust(contraststable$p.value, method='fdr')
})

# ------------------------------------------------------------------------------

# BETA
b = lmer(Beta ~ Beta_base + TP*Drug + (1|id), data=data)
b
summary(b)
anova(b, type=3)

# do pairwise comparisons
emm_b = emmeans(b, specs = pairwise ~ Drug|TP, type = "response", adjust = "bonferroni")
emm_b
# ------------------------------------------------------------------------------

