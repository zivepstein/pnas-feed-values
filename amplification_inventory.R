library(parameters)
library(lme4)
library(car)
library(multcomp)
library(tidyverse)

local({
  script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  root <- if (length(script_arg) > 0) {
    dirname(normalizePath(sub("^--file=", "", script_arg[1])))
  } else {
    getwd()
  }
  source(file.path(root, "R", "utils.R"))
})

options(timeout = 3000)
ampdata_clean <- read.csv("https://www.dropbox.com/scl/fi/fdtwe774awauh2ecsnf6o/amplification_data_disaggregated.csv?rlkey=160c58kzxiheybwn00qe94ae6&st=vt0w7sv4&dl=1")

d <- ampdata_clean %>% group_by(user_id) %>% summarize(amp = mean(is_amplified), g= sum(is_amplified), n=n())
  
#facet 1: base rates
means_inventory<-unlist(lapply(ampdata_clean %>% dplyr::select(val3_face_yhat:val3_tolerance_yhat,user_id) %>% group_by(user_id)%>%summarize_all(mean), mean))
se_inventory<-unlist(lapply(ampdata_clean %>% dplyr::select(val3_face_yhat:val3_tolerance_yhat,user_id) %>% group_by(user_id)%>%summarize_all(mean), se))

pca <- prcomp(ampdata_clean %>% dplyr::select(val3_face_yhat:val3_tolerance_yhat))
summary(pca); plot(pca)
pca$x[,1:4]


pcadat <- data.frame(pca$x[,1:5],is_amplified=ampdata_clean$is_amplified, user_id= ampdata_clean$user_id)
pc_model <- (glmer(as.formula("is_amplified ~PC1+(PC2)+(PC3)+(PC4)+ (1 | user_id)"), data=pcadat, family='binomial'))
pca$rotation[,1:4] %*% as.matrix(unlist(summary(pc_model)$coef[2:5,1]))


#alternative methods of measuring amplification
model <- glmer(as.formula("is_amplified ~ (val3_face_yhat) + val3_dominance_yhat + val3_resources_yhat + val3_achievement_yhat + val3_hedonism_yhat + val3_selfdir_thoughts_yhat + val3_selfdir_actions_yhat + val3_stimulation_yhat + val3_personal_sec_yhat + val3_socsec_yhat + val3_tradition_yhat + val3_rule_conformity_yhat + val3_interpersonal_conformity_yhat + val3_humility_yhat + val3_dependability_yhat + val3_caring_yhat+val3_univ_concern_yhat + val3_nature_yhat + val3_tolerance_yhat +(1 | user_id)"), data=ampdata_clean, family='binomial')
model2 <- glmer(as.formula("is_amplified ~ val2_selftran+val2_selfenhance+val2_conservation+val2_o2c+(1 | user_id)"), data=ampdata_clean, family='binomial')
model3 <- glmer(as.formula("is_amplified ~ val1_selfm + val1_othersm + (1 | user_id)"), data=ampdata_clean, family='binomial')

linearHypothesis(
  model3,
  "val1_selfm = val1_othersm"
)

model <- glmer(as.formula("is_amplified ~val2_conservation +val2_selfenhance  +val2_selftran+val2_o2c+ (1 | user_id)"), data=ampdata_clean, family='binomial')
summary(model); vif(model)
model <- glmer(as.formula("is_amplified ~val1_selfm+val1_othersm+ (1 | user_id)"), data=ampdata_clean, family='binomial')
summary(model); vif(model)

print("begin modeling for value amplificaton using marginal method...")
model <- glmer(as.formula("is_amplified ~val2_conservation +val2_selfenhance  +val2_selftran+val2_o2c+ val3_face_marginal+ (1 | user_id)"), data=ampdata_clean, family='binomial')
summary(glht(model, linfct = c("(2*val3_face_marginal+val2_conservation +val2_selfenhance) /2 >=0")))

model <- glmer(as.formula("is_amplified ~val2_conservation +val2_selfenhance  +val2_selftran+val2_o2c+ val3_dominance_marginal +(1 | user_id)"), data=ampdata_clean, family='binomial')
print(summary(model))
summary(glht(model, linfct = c("val2_selfenhance + val3_dominance_marginal>=0")))

model <- glmer(as.formula("is_amplified ~val2_conservation +val2_selfenhance  +val2_selftran+val2_o2c+  val3_resources_marginal+ (1 | user_id)"), data=ampdata_clean, family='binomial')
print(summary(model))
summary(glht(model, linfct = c("val2_selfenhance + val3_resources_marginal>=0")))

model <- glmer(as.formula("is_amplified ~val2_conservation +val2_selfenhance  +val2_selftran+val2_o2c+  val3_achievement_marginal+(1 | user_id)"), data=ampdata_clean, family='binomial')
print(summary(model))
summary(glht(model, linfct = c("val2_selfenhance + val3_achievement_marginal>=0")))

model <- glmer(as.formula("is_amplified ~val2_conservation +val2_selfenhance  +val2_selftran+val2_o2c+val3_hedonism_marginal+  (1 | user_id)"), data=ampdata_clean, family='binomial')
print(summary(model))
summary(glht(model, linfct = c("(2*val3_hedonism_marginal+val2_o2c +val2_selfenhance) /2 >=0")))

model <- glmer(as.formula("is_amplified ~val2_conservation +val2_selfenhance  +val2_selftran+val2_o2c+ val3_selfdir_thoughts_marginal + (1 | user_id)"), data=ampdata_clean, family='binomial')
print(summary(model))
summary(glht(model, linfct = c("val2_o2c + val3_selfdir_thoughts_marginal>=0")))

model <- glmer(as.formula("is_amplified ~val2_conservation +val2_selfenhance  +val2_selftran+val2_o2c+ val3_selfdir_actions_marginal + (1 | user_id)"), data=ampdata_clean, family='binomial')
print(summary(model))
summary(glht(model, linfct = c("val2_o2c + val3_selfdir_actions_marginal>=0")))

model <- glmer(as.formula("is_amplified ~val2_conservation +val2_selfenhance  +val2_selftran+val2_o2c+  val3_stimulation_marginal +(1 | user_id)"), data=ampdata_clean, family='binomial')
print(summary(model))
summary(glht(model, linfct = c("val2_o2c + val3_stimulation_marginal>=0")))

model <- glmer(as.formula("is_amplified ~val2_conservation +val2_selfenhance  +val2_selftran+val2_o2c+  val3_personal_sec_marginal +(1 | user_id)"), data=ampdata_clean, family='binomial')
print(summary(model))
summary(glht(model, linfct = c("val2_conservation + val3_personal_sec_marginal>=0")))

model <- glmer(as.formula("is_amplified ~val2_conservation +val2_selfenhance  +val2_selftran+val2_o2c+val3_socsec_marginal +  (1 | user_id)"), data=ampdata_clean, family='binomial')
print(summary(model))
summary(glht(model, linfct = c("val2_conservation + val3_socsec_marginal>=0")))

model <- glmer(as.formula("is_amplified ~val2_conservation +val2_selfenhance  +val2_selftran+val2_o2c+ val3_tradition_marginal + (1 | user_id)"), data=ampdata_clean, family='binomial')
print(summary(model))
summary(glht(model, linfct = c("val2_conservation + val3_tradition_marginal>=0")))

model <- glmer(as.formula("is_amplified ~val2_conservation +val2_selfenhance  +val2_selftran+val2_o2c+ val3_rule_conformity_marginal + (1 | user_id)"), data=ampdata_clean, family='binomial')
print(summary(model))
summary(glht(model, linfct = c("val2_conservation + val3_rule_conformity_marginal>=0")))

model <- glmer(as.formula("is_amplified ~val2_conservation +val2_selfenhance  +val2_selftran+val2_o2c+ val3_interpersonal_conformity_marginal + (1 | user_id)"), data=ampdata_clean, family='binomial')
print(summary(model))
summary(glht(model, linfct = c("val2_conservation + val3_interpersonal_conformity_marginal>=0")))

model <- glmer(as.formula("is_amplified ~val2_conservation +val2_selfenhance  +val2_selftran+val2_o2c+ val3_humility_marginal + (1 | user_id)"), data=ampdata_clean, family='binomial')
print(summary(model))
summary(glht(model, linfct = c("(2*val3_humility_marginal+val2_conservation +val2_selftran) /2 >=0")))

model <- glmer(as.formula("is_amplified ~val2_conservation +val2_selfenhance  +val2_selftran+val2_o2c+ val3_dependability_marginal + (1 | user_id)"), data=ampdata_clean, family='binomial')
print(summary(model))
summary(glht(model, linfct = c("val2_selftran + val3_dependability_marginal>=0")))

model <- glmer(as.formula("is_amplified ~val2_conservation +val2_selfenhance  +val2_selftran+val2_o2c+ val3_caring_marginal+ (1 | user_id)"), data=ampdata_clean, family='binomial')
print(summary(model))
summary(glht(model, linfct = c("val2_selftran + val3_caring_marginal>=0")))

model <- glmer(as.formula("is_amplified ~val2_conservation +val2_selfenhance  +val2_selftran+val2_o2c+ val3_univ_concern_marginal + (1 | user_id)"), data=ampdata_clean, family='binomial')
print(summary(model))
summary(glht(model, linfct = c("val2_selftran + val3_univ_concern_marginal>=0")))

model <- glmer(as.formula("is_amplified ~val2_conservation +val2_selfenhance  +val2_selftran+val2_o2c+val3_nature_marginal +   (1 | user_id)"), data=ampdata_clean, family='binomial')
print(summary(model))
summary(glht(model, linfct = c("val2_selftran + val3_nature_marginal>=0")))

model <- glmer(as.formula("is_amplified ~val2_conservation +val2_selfenhance  +val2_selftran+val2_o2c+  val3_tolerance_marginal+ (1 | user_id)"), data=ampdata_clean, family='binomial')
print(summary(model))
summary(glht(model, linfct = c("val2_selftran + val3_tolerance_marginal>=0")))
