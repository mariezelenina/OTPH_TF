library(nlme)
library(CorrMixed)
library(lme4)
library(lmerTest)
library(geepack)
#library(Zelig)
library(ggplot2)
library(geomtextpath)
library(car)
library(emmeans)
library(effectsize)
library(simr)
library(matrixStats)
library(tidyverse)

names(data)[1] <- "Id"

# ----------------------------------------------
# Load the  data. 
# ----------------------------------------------

data_eo <- read.table("/Users/zeleninam2/Documents/1_projects/Oxytocin_final_2024/behav_lmm/dat_EO_relPower_withbehav_missval.csv", header=T, sep=",")
data_ec <- read.table("/Users/zeleninam2/Documents/1_projects/Oxytocin_final_2024/behav_lmm/dat_EC_relPower_withbehav_missval.csv", header=T, sep=",")

data_eo$Drug <- factor(data_eo$Drug)
data_eo$TP <- factor(data_eo$TP)
data_eo$id <- factor(data_eo$id)

data_ec$Drug <- factor(data_ec$Drug)
data_ec$TP <- factor(data_ec$TP)
data_ec$id <- factor(data_ec$id)

# NOT coding behav scales as factors for slopes

#data_ec$Alertness_reversed <- factor(data_ec$Alertness_reversed)
#data_ec$Sociability_reversed <- factor(data_ec$Sociability_reversed)
#data_ec$Excitement_reversed <- factor(data_ec$Excitement_reversed)

#data_eo$Alertness_reversed <- factor(data_eo$Alertness_reversed)
#data_eo$Sociability_reversed <- factor(data_eo$Sociability_reversed)
#data_eo$Excitement_reversed <- factor(data_eo$Excitement_reversed)

# SPLIT DATA between ot and pl

data_eo_ot_all<-data_eo[data_eo$Drug==1,]
data_eo_pl_all<-data_eo[data_eo$Drug==2,]

data_ec_ot_all<-data_ec[data_ec$Drug==1,]
data_ec_pl_all<-data_ec[data_ec$Drug==2,]

# ----------------------------------------------
# ----------------------------------------------
# run the model
# ----------------------------------------------
# ----------------------------------------------

# ----------------------------------------------
# ---------- MIN WORKING EXAMPLE ---------------
# ---------- EC, THETA, ALERTNESS --------------
# --------------- NO LOOPS ---------------------
# ----------------------------------------------

# EXAMPLE: THETA - Alertness

cat("\n===== Anova LMM results =====\n")
model = lmer(Theta ~ Theta_base + TP*Drug*Alertness_reversed + (1|id), data=data_ec)
anova(model, type=3)

cat("\n===== Slopes estimation =====\n")
trend <- emtrends(model,  as.formula("~1"),var = "Alertness_reversed")
summary_trend_df<- as.data.frame(test(trend))
summary_trend_df <- summary_trend_df %>%
  mutate(
    t = Alertness_reversed.trend / SE
  )
r_trend <- t_to_r(summary_trend_df$t, summary_trend_df$df)
summary_trend_df$r <- r_trend$r
print(r_trend)
print(summary_trend_df)
print(as.matrix(summary_trend_df))

# ------> alertness, theta, ALL time points
cat("\n===== Correlations - all time points =====\n")
print(cor.test(data_eo_ot_all$Alertness_reversed, data_eo_ot_all$Theta,use="pairwise.complete.obs",method = "spearman"))
sum(complete.cases(data_eo_ot_all$Alertness_reversed,data_eo_ot_all$Theta))-2

print(cor.test(data_eo_pl_all$Alertness_reversed, data_eo_pl_all$Theta,use="pairwise.complete.obs",method = "spearman"))
sum(complete.cases(data_ec_pl_all$Alertness_reversed,data_eo_pl_all$Theta))-2

# ------> alertness, theta by time point
cat("\n===== Correlations - by time point =====\n")
for (timep in list(1,2,3,4,5,6)) {
  data_my_tp_ot<-data_eo_ot_all[data_eo_ot_all$TP==timep,]
  data_my_tp_pl<-data_eo_pl_all[data_eo_pl_all$TP==timep,]
  
  print("---------------------------------------")
  cat("\nTP =", timep, "; Oxytocin\n")
  print(cor.test(data_my_tp_ot$Alertness_reversed, data_my_tp_ot$Theta,use="pairwise.complete.obs",method = "spearman"))
  df_ot <- sum(complete.cases(data_my_tp_ot$Alertness_reversed, data_my_tp_ot$Theta)) - 2
  cat("Degrees of freedom =", df_ot, "\n")
  
  cat("\nTP =", timep, "; Placebo\n")
  print(cor.test(data_my_tp_pl$Alertness_reversed, data_my_tp_pl$Theta,use="pairwise.complete.obs",method = "spearman"))
  df_pl <- sum(complete.cases(data_my_tp_pl$Alertness_reversed, data_my_tp_pl$Theta)) - 2
  cat("Degrees of freedom =", df_pl, "\n")
}

# ----------------------------------------------
# ----------------- LOOP -----------------------
# ------- FOR EO/EC, BAND, MEASURE -------------
# ----------------------------------------------

# put all output into a text file
# that will just dump everything we output into a file
path_to_savefile = "/Users/zeleninam2/Documents/1_projects/Oxytocin_final_2024/all_code/OTPH_TF/OTPH_results_behav_allresults.txt"
sink(path_to_savefile)

# LOOP THROUGH EYE OPEN/CLOSED
for (eyes_cond in c("eyes_open", "eyes_closed")){
  
  # chose the right data depending on eyes cond
  if (eyes_cond == "eyes_open") {
    mydata_main   <- data_eo
    mydata_ot_all <- data_eo_ot_all
    mydata_pl_all <- data_eo_pl_all
  } else {
    mydata_main   <- data_ec
    mydata_ot_all <- data_ec_ot_all
    mydata_pl_all <- data_ec_pl_all
  } 

  # LOOP THROUGH BANDS
  bands <- c("Theta", "Alpha", "Beta")
  for (band in bands) {
  
    # LOOP THROUGH BEHAV MEASURES
    behav_vars <- c("Alertness_reversed", "Excitement_reversed", "Sociability_reversed")
    for (behav_var in behav_vars) {
      
      # do the analysis for this eyes condition, band, measure
      cat("\n-------------------------------------------------------------------------------------------------------------\n")
      cat("\n-------------------------------------------EYES = ", eyes_cond, "--------------------------------------------\n")
      cat("\n----------------------------------------------BAND = ", band, "----------------------------------------------\n")
      cat("\n------------------------------------------MEASURE = ", behav_var, "------------------------------------------\n")
      cat("\n-------------------------------------------------------------------------------------------------------------\n")
      
      cat("\n===== Anova results, after fitting lmm =====\n\n")
      
      formula_str <- paste0(band, " ~ ", band, "_base + TP*Drug*", behav_var, " + (1|id)")
      model <- lmer(as.formula(formula_str), data = mydata_main)  
      print(anova(model, type=3))
      
      cat("\n===== Slopes estimation =====\n\n")
      
      trend <- emtrends(model,  as.formula("~1"),var = behav_var)
      summary_trend_df<- as.data.frame(test(trend))
      summary_trend_df <- summary_trend_df %>%
        mutate(
          t = .data[[paste0(behav_var, ".trend")]] / SE
        )
      r_trend <- t_to_r(summary_trend_df$t, summary_trend_df$df)
      summary_trend_df$r <- r_trend$r
      print(as.matrix(summary_trend_df))
      
      cat("\n===== Correlations (", eyes_cond, ": ", band, " × ", behav_var, ") - all time points =====\n", sep="")
      
      cat("\nOxytocin\n")
      x <- mydata_ot_all[[behav_var]]
      y <- mydata_ot_all[[band]]
      # Remove NA rows
      idx <- complete.cases(x, y)
      # calculate corr
      cor_res <- cor.test(x[idx], y[idx], method="spearman")
      print(cor_res)
      # df
      df_ot_all = sum(complete.cases(mydata_ot_all[[behav_var]],mydata_ot_all[[band]]))-2
      cat("Degrees of freedom =", df_ot_all, "\n")
      
      cat("\nPlacebo\n")
      x <- mydata_pl_all[[behav_var]]
      y <- mydata_pl_all[[band]]
      # Remove NA rows
      idx <- complete.cases(x, y)
      # calculate corr
      cor_res <- cor.test(x[idx], y[idx], method="spearman")
      print(cor_res)
      # df
      df_pl_all = sum(complete.cases(mydata_pl_all[[behav_var]],mydata_pl_all[[band]]))-2
      cat("Degrees of freedom =", df_pl_all, "\n")
      
      cat("\n===== Correlations (", band, " × ", behav_var, ") - by time point =====\n", sep="")
      
      for (timep in 1:6) {
        # Subset data for Oxytocin
        data_my_tp_ot<-mydata_ot_all[mydata_ot_all$TP==timep,]
    
        # Remove NAs
        x_ot <- data_my_tp_ot[[behav_var]]
        y_ot <- data_my_tp_ot[[band]]
        idx_ot <- complete.cases(x_ot, y_ot)
        xx_ot <- x_ot[idx_ot]
        yy_ot <- y_ot[idx_ot]
        
        cat("\nTP =", timep, "; Oxytocin\n")
        cor_res_ot <- cor.test(xx_ot, yy_ot, method = "spearman")
        print(cor_res_ot)
        df_ot <- length(xx_ot) - 2
        cat("Degrees of freedom =", df_ot, "\n")
        
        # Subset data for Placebo
        data_my_tp_pl<-mydata_pl_all[mydata_pl_all$TP==timep,]
    
        # Remove NAs
        x_pl <- data_my_tp_pl[[behav_var]]
        y_pl <- data_my_tp_pl[[band]]
        idx_pl <- complete.cases(x_pl, y_pl)
        xx_pl <- x_pl[idx_pl]
        yy_pl <- y_pl[idx_pl]
        
        cat("\nTP =", timep, "; Placebo\n")
        cor_res_pl <- cor.test(xx_pl, yy_pl, method = "spearman")
        print(cor_res_pl)
        df_pl <- length(xx_pl) - 2
        cat("Degrees of freedom =", df_pl, "\n")
        cat("\n---------------------------------------\n")
      }
    }
  }
}
#return control back to console
sink()


# ----------------------------------------------
# ----------------------------------------------

# PLOT THE PLOTS

# ----------------------------------------------
# ----------------------------------------------

# LOOP function - all tp

# ----------------------------------------------

# define options to iterate
moods <- c(
  Alertness   = "Alertness_reversed",
  Excitement  = "Excitement_reversed",
  Sociability = "Sociability_reversed"
)

bands <- c("Theta", "Alpha", "Beta")

eyes_list <- list(
  "Eyes open"   = data_eo,
  "Eyes closed" = data_ec
)

plots <- list()

# function for plotting
plot_band_vs_mood <- function(data, band, mood_col, mood_label, eyes_label) {
  ggplot(
    data,
    aes(
      x = .data[[band]],
      y = .data[[mood_col]],
      color = Drug,
      shape = Drug
    )
  ) +
    geom_point() +
    geom_smooth(method = "lm", se = TRUE) +
    scale_color_manual(
      labels = c("Oxytocin", "Placebo"),
      values = c("blue", "red")
    ) +
    scale_shape_manual(
      labels = c("Oxytocin", "Placebo"),
      values = c(15, 17)
    ) +
    theme_bw(base_size = 10) +
    ggtitle(paste(eyes_label, ",", mood_label, "against", band))
}

# iterate and plot 
for (eyes in names(eyes_list)) {
  for (band in bands) {
    for (mood_label in names(moods)) {
      
      mood_col <- moods[[mood_label]]
      
      p <- plot_band_vs_mood(
        data       = eyes_list[[eyes]],
        band       = band,
        mood_col   = mood_col,
        mood_label = mood_label,
        eyes_label = eyes
      )
      
      plots[[paste(eyes, band, mood_label, sep = "_")]] <- p
    }
  }
}

names(plots)

for (p in plots) {
  print(p)
}

dir.create("/Users/zeleninam2/Documents/1_projects/Oxytocin_final_2024/plots/all_plots/", showWarnings = FALSE)

for (nm in names(plots)) {
  ggsave(
    filename = paste0("/Users/zeleninam2/Documents/1_projects/Oxytocin_final_2024/plots/all_plots/", nm, ".png"),
    plot     = plots[[nm]],
    width    = 4,
    height   = 4,
    dpi      = 300
  )
}

# ----------------------------------------------

# LOOP function - individual tp

# ----------------------------------------------

# function for plotting for specific tp
plot_band_vs_mood_tp <- function(data, band, mood_col, mood_label, eyes_label, tp) {
  data_tp <- data[data$TP == tp, ]
  ggplot(
    data_tp,
    aes(
      x = .data[[band]],
      y = .data[[mood_col]],
      color = Drug,
      shape = Drug
    )
  ) +
    geom_point() +
    geom_smooth(method = "lm", se = TRUE) +
    scale_color_manual(
      labels = c("Oxytocin", "Placebo"),
      values = c("blue", "red")
    ) +
    scale_shape_manual(
      labels = c("Oxytocin", "Placebo"),
      values = c(15, 17)
    ) +
    theme_bw(base_size = 10) +
    ggtitle(
      paste(
        eyes_label, ",",
        mood_label, "against", band,
        ", Time point", tp
      )
    )
}

plots_tp <- list()

for (tp in 1:6) {
  for (eyes in names(eyes_list)) {
    for (band in bands) {
      for (mood_label in names(moods)) {
        
        mood_col <- moods[[mood_label]]
        
        p <- plot_band_vs_mood_tp(
          data       = eyes_list[[eyes]],
          band       = band,
          mood_col   = mood_col,
          mood_label = mood_label,
          eyes_label = eyes,
          tp         = tp
        )
        
        if (is.null(p)) next
        
        plots_tp[[paste(eyes, band, mood_label, paste0("TP", tp), sep = " ")]] <- p
      }
    }
  }
}

names(plots_tp)

for (p in plots_tp) {
  print(p)
}

dir.create("/Users/zeleninam2/Documents/1_projects/Oxytocin_final_2024/plots/all_plots/separate_tps/", showWarnings = FALSE)

for (nm in names(plots_tp)) {
  ggsave(
    filename = paste0("/Users/zeleninam2/Documents/1_projects/Oxytocin_final_2024/plots/all_plots/separate_tps/", nm, ".png"),
    plot     = plots_tp[[nm]],
    width    = 5,
    height   = 4,
    dpi      = 300
  )
}
# ----------------------------------------------

# INDIVIDUAL plots


# example - I found significance in omnibus test for EO, Alertness, Theta. 
# In posthoc correlations, there was a significant correlation between Theta and Alertness in tp5 and tp6, only in the PL group.

timep <- 5
data_eo_tp5<-data_eo[data_eo$TP==timep,]

# plotting Theta against Alertness

ggplot(data_eo_tp5, aes(x = Theta, y = Alertness_reversed, color = Drug, shape=Drug)) +
  geom_point() +
  geom_smooth(aes(label = Drug),
                   method = "lm", se = TRUE) +
  scale_color_manual(labels = c("Oxytocin", "Placebo"), values = c("blue", "red")) +
  scale_shape_manual(labels = c("Oxytocin", "Placebo"), values=c(15, 17))+
  theme_bw(base_size = 20) + 
  ggtitle("Eyes open, Alertness against Theta, Time point 5")

# -------

# alertness, theta, tp6

timep <- 6
data_eo_tp5<-data_eo[data_eo$TP==timep,]

ggplot(data_eo_tp5, aes(x = Theta, y = Alertness_reversed, color = Drug, shape=Drug)) +
  geom_point() +
  geom_smooth(aes(label = Drug),
              method = "lm", se = TRUE) +
  scale_color_manual(labels = c("Oxytocin", "Placebo"), values = c("blue", "red")) +
  scale_shape_manual(labels = c("Oxytocin", "Placebo"), values=c(15, 17))+
  theme_bw(base_size = 20) + 
  ggtitle("Eyes open, Alertness against Theta, Time point 6")

# -------

# sociability, beta, tp3

timep <- 3
data_eo_tp5<-data_eo[data_eo$TP==timep,]

ggplot(data_eo_tp5, aes(x = Beta, y = Sociability_reversed, color = Drug, shape=Drug)) +
  geom_point() +
  geom_smooth(aes(label = Drug),
              method = "lm", se = TRUE) +
  scale_color_manual(labels = c("Oxytocin", "Placebo"), values = c("blue", "red")) +
  scale_shape_manual(labels = c("Oxytocin", "Placebo"), values=c(15, 17))+
  theme_bw(base_size = 20) + 
  ggtitle("Eyes open, Sociability against Beta, Time point 3")

# -------

# sociability, beta, tp5

timep <- 5
data_eo_tp5<-data_eo[data_eo$TP==timep,]

ggplot(data_eo_tp5, aes(x = Beta, y = Sociability_reversed, color = Drug, shape=Drug)) +
  geom_point() +
  geom_smooth(aes(label = Drug),
              method = "lm", se = TRUE) +
  scale_color_manual(labels = c("Oxytocin", "Placebo"), values = c("blue", "red")) +
  scale_shape_manual(labels = c("Oxytocin", "Placebo"), values=c(15, 17))+
  theme_bw(base_size = 20) + 
  ggtitle("Eyes open, Sociability against Beta, Time point 5")

# -------

# EYES CLOSED

# Alertnerss, theta, tp1

timep <- 1
data_ec_tp5<-data_ec[data_ec$TP==timep,]

ggplot(data_ec_tp5, aes(x = Theta, y = Alertness_reversed, color = Drug, shape=Drug)) +
  geom_point() +
  geom_smooth(aes(label = Drug),
              method = "lm", se = TRUE) +
  scale_color_manual(labels = c("Oxytocin", "Placebo"), values = c("blue", "red")) +
  scale_shape_manual(labels = c("Oxytocin", "Placebo"), values=c(15, 17))+
  theme_bw(base_size = 20) + 
  ggtitle("Eyes closed, Alertness against Theta, Time point 1")

# -------

# Sociability, beta, tp3

timep <- 3
data_ec_tp5<-data_ec[data_ec$TP==timep,]

ggplot(data_ec_tp5, aes(x = Beta, y = Sociability_reversed, color = Drug, shape=Drug)) +
  geom_point() +
  geom_smooth(aes(label = Drug),
              method = "lm", se = TRUE) +
  scale_color_manual(labels = c("Oxytocin", "Placebo"), values = c("blue", "red")) +
  scale_shape_manual(labels = c("Oxytocin", "Placebo"), values=c(15, 17))+
  theme_bw(base_size = 20) + 
  ggtitle("Eyes closed, Sociability against Beta, Time point 3")

# ----------------------------------------------
# ----------------------------------------------

# all timepoints

# EO, alertness, theta
ggplot(data_eo, aes(x = Theta, y = Alertness_reversed, color = Drug, shape=Drug)) +
  geom_point() +
  geom_smooth(aes(label = Drug),
              method = "lm", se = TRUE) +
  scale_color_manual(labels = c("Oxytocin", "Placebo"), values = c("blue", "red")) +
  scale_shape_manual(labels = c("Oxytocin", "Placebo"), values=c(15, 17))+
  theme_bw(base_size = 10) + 
  ggtitle("Eyes open, Alertness against Theta")

# EO, alertness, beta
ggplot(data_eo, aes(x = Beta, y = Alertness_reversed, color = Drug, shape=Drug)) +
  geom_point() +
  geom_smooth(aes(label = Drug),
              method = "lm", se = TRUE) +
  scale_color_manual(labels = c("Oxytocin", "Placebo"), values = c("blue", "red")) +
  scale_shape_manual(labels = c("Oxytocin", "Placebo"), values=c(15, 17))+
  theme_bw(base_size = 10) + 
  ggtitle("Eyes open, Alertness against Beta")

# EO, excitement, theta
ggplot(data_eo, aes(x = Theta, y = Excitement_reversed, color = Drug, shape=Drug)) +
  geom_point() +
  geom_smooth(aes(label = Drug),
              method = "lm", se = TRUE) +
  scale_color_manual(labels = c("Oxytocin", "Placebo"), values = c("blue", "red")) +
  scale_shape_manual(labels = c("Oxytocin", "Placebo"), values=c(15, 17))+
  theme_bw(base_size = 10) + 
  ggtitle("Eyes open, Excitement against Theta")

# EO, excitement, alpha
ggplot(data_eo, aes(x = Alpha, y = Excitement_reversed, color = Drug, shape=Drug)) +
  geom_point() +
  geom_smooth(aes(label = Drug),
              method = "lm", se = TRUE) +
  scale_color_manual(labels = c("Oxytocin", "Placebo"), values = c("blue", "red")) +
  scale_shape_manual(labels = c("Oxytocin", "Placebo"), values=c(15, 17))+
  theme_bw(base_size = 10) + 
  ggtitle("Eyes open, Excitement against Alpha")

# EO, excitement, beta
ggplot(data_eo, aes(x = Beta, y = Excitement_reversed, color = Drug, shape=Drug)) +
  geom_point() +
  geom_smooth(aes(label = Drug),
              method = "lm", se = TRUE) +
  scale_color_manual(labels = c("Oxytocin", "Placebo"), values = c("blue", "red")) +
  scale_shape_manual(labels = c("Oxytocin", "Placebo"), values=c(15, 17))+
  theme_bw(base_size = 10) + 
  ggtitle("Eyes open, Excitement against Beta")

# EO, sociability, beta
ggplot(data_eo, aes(x = Beta, y = Sociability_reversed, color = Drug, shape=Drug)) +
  geom_point() +
  geom_smooth(aes(label = Drug),
              method = "lm", se = TRUE) +
  scale_color_manual(labels = c("Oxytocin", "Placebo"), values = c("blue", "red")) +
  scale_shape_manual(labels = c("Oxytocin", "Placebo"), values=c(15, 17))+
  theme_bw(base_size = 10) + 
  ggtitle("Eyes open, Sociability against Beta")

# EO, sociability, alpha
ggplot(data_eo, aes(x = Alpha, y = Sociability_reversed, color = Drug, shape=Drug)) +
  geom_point() +
  geom_smooth(aes(label = Drug),
              method = "lm", se = TRUE) +
  scale_color_manual(labels = c("Oxytocin", "Placebo"), values = c("blue", "red")) +
  scale_shape_manual(labels = c("Oxytocin", "Placebo"), values=c(15, 17))+
  theme_bw(base_size = 10) + 
  ggtitle("Eyes open, Sociability against Alpha")

# EC, alertness, theta
ggplot(data_ec, aes(x = Theta, y = Alertness_reversed, color = Drug, shape=Drug)) +
  geom_point() +
  geom_smooth(aes(label = Drug),
              method = "lm", se = TRUE) +
  scale_color_manual(labels = c("Oxytocin", "Placebo"), values = c("blue", "red")) +
  scale_shape_manual(labels = c("Oxytocin", "Placebo"), values=c(15, 17))+
  theme_bw(base_size = 10) + 
  ggtitle("Eyes closed, Alertness against Theta")

# EC, sociability, beta
ggplot(data_ec, aes(x = Beta, y = Sociability_reversed, color = Drug, shape=Drug)) +
  geom_point() +
  geom_smooth(aes(label = Drug),
              method = "lm", se = TRUE) +
  scale_color_manual(labels = c("Oxytocin", "Placebo"), values = c("blue", "red")) +
  scale_shape_manual(labels = c("Oxytocin", "Placebo"), values=c(15, 17))+
  theme_bw(base_size = 10) + 
  ggtitle("Eyes closed, Sociability against Beta")

# EC, sociability, alpha
ggplot(data_ec, aes(x = Alpha, y = Sociability_reversed, color = Drug, shape=Drug)) +
  geom_point() +
  geom_smooth(aes(label = Drug),
              method = "lm", se = TRUE) +
  scale_color_manual(labels = c("Oxytocin", "Placebo"), values = c("blue", "red")) +
  scale_shape_manual(labels = c("Oxytocin", "Placebo"), values=c(15, 17))+
  theme_bw(base_size = 10) + 
  ggtitle("Eyes closed, Sociability against Alpha")

# ----------------------------------------------
# ----------------------------------------------
# ----------------------------------------------
# Rough comparison of Sociability

df_clean <- data_eo[!is.na(data_eo$Sociability_reversed), ]

aggregate(Sociability_reversed ~ Drug, data = df_clean, 
          FUN = function(x) c(mean = mean(x), sd = sd(x), n = length(x)))

wilcox.test(Sociability_reversed ~ Drug, data = df_clean)
t.test(Sociability_reversed ~ Drug, data = df_clean)

boxplot(
  Sociability_reversed ~ Drug,
  data = df_clean,
  xlab = "Drug",
  ylab = "Sociability (reversed)",
  main = "Sociability by Drug"
)

stripchart(
  Sociability_reversed ~ Drug,
  data = df_clean,
  vertical = TRUE,
  method = "jitter",
  add = TRUE,
  pch = 16,
  col = rgb(0, 0, 0, 0.5)
)

s = lmer(Sociability_reversed ~ Drug * TP + (1 | id), data = df_clean)
anova(s, type=3)

a = lmer(Alertness_reversed ~ Drug * TP + (1 | id), data = df_clean)
anova(a, type=3)

e = lmer(Excitement_reversed ~ Drug * TP + (1 | id), data = df_clean)
anova(e, type=3)
# ----------------------------------------------
# ----------------------------------------------


# Code by Marie Zelenina
# last updated 2026-Jan-14
# marie.zelenina@gmail.com