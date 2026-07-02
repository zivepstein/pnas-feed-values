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

post_clean <- read.csv(data_path("postfeed_clean.csv"))

value_cols <- dplyr::select(post_clean, Thought:Dependability)
vnorm <- (value_cols / 3) - rowMeans(value_cols / 3, na.rm = TRUE)
colnames(vnorm) <- paste(colnames(vnorm), "Norm", sep = "")
vvnorm <- vnorm[, value_norm_reorder]

personal_values_tojoin <- cbind(vvnorm, InterfaceID = post_clean$InterfaceID, DemRep = post_clean$DemRep_C)
personal_values_tojoin <- personal_values_tojoin[complete.cases(vvnorm), ]
personal_values_tojoin <- personal_values_tojoin[rowSums(abs(personal_values_tojoin[, 1:19])) > 0, ]

perceptions <- post_clean %>%
  dplyr::select(value_perceptions_1:value_perceptions_19, InterfaceID) %>%
  mutate(across(value_perceptions_1:value_perceptions_19, as.numeric))
perceptions <- perceptions[, perception_col_reorder]

personal_values <- personal_values_tojoin %>% left_join(perceptions, by = "InterfaceID")

amp_results <- read_amp_results()

engagement_output <- read.csv(data_path("individual_engagement_results.csv")) %>%
  pivot_wider(
    id_cols = values,
    names_from = c(dvs, datasets, type),
    values_from = c(coef, se),
    names_sep = "_"
  )
engagement_output <- engagement_output[c(19, 1:18), ]

metrics_results <- read.csv(data_path("global_engagement_results.csv"))[1:19, ]
metrics_results <- metrics_results[amp_value_reorder, ]

personal_matrix <- as.matrix(personal_values[, 1:19])
perceived_matrix <- as.matrix(personal_values[, 22:40])

alignment_df <- data.frame(
  alignment_inventory = spearman_alignment(personal_matrix, amp_results$base_rate),
  alignment_amplification = spearman_alignment(personal_matrix, amp_results$ampcoef_pca),
  alignment_perceived_amplification = rowwise_spearman_alignment(personal_matrix, perceived_matrix),
  alignment_engage_reply = spearman_alignment(
    personal_matrix, engagement_output$`coef_did_reply>0_full_data_pca`
  ),
  alignment_engage_favorite = spearman_alignment(
    personal_matrix, engagement_output$`coef_did_favorite>0_full_data_pca`
  ),
  alignment_engage_retweet = spearman_alignment(
    personal_matrix, engagement_output$`coef_did_retweet>0_full_data_pca`
  ),
  InterfaceID = personal_values$InterfaceID,
  DemRep = personal_values$DemRep
)

metrics_df <- data.frame(
  metrics_favorites_pca = spearman_alignment(personal_matrix, metrics_results$FavoriteCountNorm_pca),
  metrics_retweets_pca = spearman_alignment(personal_matrix, metrics_results$RetweetCountNorm_pca),
  metrics_replies_pca = spearman_alignment(personal_matrix, metrics_results$ReplyCountNorm_pca),
  InterfaceID = personal_values$InterfaceID,
  DemRep = personal_values$DemRep
)

t.test(alignment_df[, "alignment_inventory"])
t.test(alignment_df[alignment_df$DemRep > 3, "alignment_inventory"], alignment_df[alignment_df$DemRep < 4, "alignment_inventory"])
t.test(alignment_df[, "alignment_amplification"])
t.test(alignment_df[alignment_df$DemRep > 3, "alignment_amplification"], alignment_df[alignment_df$DemRep < 4, "alignment_amplification"])
t.test(alignment_df[, "alignment_perceived_amplification"])
t.test(alignment_df[alignment_df$DemRep > 3, "alignment_perceived_amplification"], alignment_df[alignment_df$DemRep < 4, "alignment_perceived_amplification"])

t.test(alignment_df[, "alignment_engage_favorite"])
t.test(alignment_df[alignment_df$DemRep > 3, "alignment_engage_favorite"], alignment_df[alignment_df$DemRep < 4, "alignment_engage_favorite"])
t.test(alignment_df[, "alignment_engage_retweet"])
t.test(alignment_df[alignment_df$DemRep > 3, "alignment_engage_retweet"], alignment_df[alignment_df$DemRep < 4, "alignment_engage_retweet"])
t.test(alignment_df[, "alignment_engage_reply"])
t.test(alignment_df[alignment_df$DemRep > 3, "alignment_engage_reply"], alignment_df[alignment_df$DemRep < 4, "alignment_engage_reply"])

t.test(metrics_df[, "metrics_favorites_pca"])
t.test(metrics_df[metrics_df$DemRep > 3, "metrics_favorites_pca"], metrics_df[metrics_df$DemRep < 4, "metrics_favorites_pca"])
t.test(metrics_df[, "metrics_retweets_pca"])
t.test(metrics_df[metrics_df$DemRep > 3, "metrics_retweets_pca"], metrics_df[metrics_df$DemRep < 4, "metrics_retweets_pca"])
t.test(metrics_df[, "metrics_replies_pca"])
t.test(metrics_df[metrics_df$DemRep > 3, "metrics_replies_pca"], metrics_df[metrics_df$DemRep < 4, "metrics_replies_pca"])

inv_dem_mean <- mean(alignment_df[alignment_df$DemRep < 4, "alignment_inventory"])
inv_rep_mean <- mean(alignment_df[alignment_df$DemRep > 3, "alignment_inventory"])
inv_overall_mean <- mean(alignment_df[, "alignment_inventory"])
amp_dem_mean <- mean(alignment_df[alignment_df$DemRep < 4, "alignment_amplification"])
amp_rep_mean <- mean(alignment_df[alignment_df$DemRep > 3, "alignment_amplification"])
amp_overall_mean <- mean(alignment_df[, "alignment_amplification"])
ampP_dem_mean <- mean(alignment_df[alignment_df$DemRep < 4, "alignment_perceived_amplification"], na.rm = TRUE)
ampP_rep_mean <- mean(alignment_df[alignment_df$DemRep > 3, "alignment_perceived_amplification"], na.rm = TRUE)
ampP_overall_mean <- mean(alignment_df[, "alignment_perceived_amplification"], na.rm = TRUE)

inv_dem_se <- se(alignment_df[alignment_df$DemRep < 4, "alignment_inventory"])
inv_rep_se <- se(alignment_df[alignment_df$DemRep > 3, "alignment_inventory"])
inv_overall_se <- se(alignment_df[, "alignment_inventory"])
amp_dem_se <- se(alignment_df[alignment_df$DemRep < 4, "alignment_amplification"])
amp_rep_se <- se(alignment_df[alignment_df$DemRep > 3, "alignment_amplification"])
amp_overall_se <- se(alignment_df[, "alignment_amplification"])
ampP_dem_se <- se(alignment_df[alignment_df$DemRep < 4, "alignment_perceived_amplification"])
ampP_rep_se <- se(alignment_df[alignment_df$DemRep > 3, "alignment_perceived_amplification"])
ampP_overall_se <- se(alignment_df[, "alignment_perceived_amplification"])

funnel_data <- data.frame(
  Category = rep(c("Inventory", "Amplification", "Perceived"), each = 3),
  Party = rep(c("Democrat", "Republican", "Overall"), 3),
  Mean = c(inv_dem_mean, inv_rep_mean, inv_overall_mean, amp_dem_mean, amp_rep_mean, amp_overall_mean, ampP_dem_mean, ampP_rep_mean, ampP_overall_mean),
  LowerCI = c(inv_dem_mean - inv_dem_se, inv_rep_mean - inv_rep_se, inv_overall_mean - inv_overall_se, amp_dem_mean - amp_dem_se, amp_rep_mean - amp_rep_se, amp_overall_mean - amp_overall_se, ampP_dem_mean - ampP_dem_se, ampP_rep_mean - ampP_rep_se, ampP_overall_mean - ampP_overall_se),
  UpperCI = c(inv_dem_mean + inv_dem_se, inv_rep_mean + inv_rep_se, inv_overall_mean + inv_overall_se, amp_dem_mean + amp_dem_se, amp_rep_mean + amp_rep_se, amp_overall_mean + amp_overall_se, ampP_dem_mean + ampP_dem_se, ampP_rep_mean + ampP_rep_se, ampP_overall_mean + ampP_overall_se)
)
funnel_data$Category <- factor(funnel_data$Category, levels = c("Inventory", "Perceived", "Amplification"))

funnel <- ggplot(funnel_data, aes(x = Category, y = Mean, color = Party, group = Party)) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_errorbar(aes(ymin = LowerCI, ymax = UpperCI), width = 0.2, position = position_dodge(width = 0.5)) +
  labs(title = "Alignment across the social media funnel", y = "Value Alignment", x = "Stage of the funnel") +
  theme_minimal(base_size = 14) +
  scale_color_manual(values = c("Democrat" = "blue", "Republican" = "red", "Overall" = "grey")) +
  ylim(-0.25, 0.4)
ggsave("fig2_left.pdf", plot = funnel, width = 8, height = 6)

favorite_dem_mean <- mean(alignment_df[alignment_df$DemRep < 4, "alignment_engage_favorite"])
favorite_rep_mean <- mean(alignment_df[alignment_df$DemRep > 3, "alignment_engage_favorite"])
favorite_overall_mean <- mean(alignment_df[, "alignment_engage_favorite"])
retweet_dem_mean <- mean(alignment_df[alignment_df$DemRep < 4, "alignment_engage_retweet"])
retweet_rep_mean <- mean(alignment_df[alignment_df$DemRep > 3, "alignment_engage_retweet"])
retweet_overall_mean <- mean(alignment_df[, "alignment_engage_retweet"])
reply_dem_mean <- mean(alignment_df[alignment_df$DemRep < 4, "alignment_engage_reply"])
reply_rep_mean <- mean(alignment_df[alignment_df$DemRep > 3, "alignment_engage_reply"])
reply_overall_mean <- mean(alignment_df[, "alignment_engage_reply"])

favorite_dem_se <- se(alignment_df[alignment_df$DemRep < 4, "alignment_engage_favorite"])
favorite_rep_se <- se(alignment_df[alignment_df$DemRep > 3, "alignment_engage_favorite"])
favorite_overall_se <- se(alignment_df[, "alignment_engage_favorite"])
retweet_dem_se <- se(alignment_df[alignment_df$DemRep < 4, "alignment_engage_retweet"])
retweet_rep_se <- se(alignment_df[alignment_df$DemRep > 3, "alignment_engage_retweet"])
retweet_overall_se <- se(alignment_df[, "alignment_engage_retweet"])
reply_dem_se <- se(alignment_df[alignment_df$DemRep < 4, "alignment_engage_reply"])
reply_rep_se <- se(alignment_df[alignment_df$DemRep > 3, "alignment_engage_reply"])
reply_overall_se <- se(alignment_df[, "alignment_engage_reply"])

engagement_data <- data.frame(
  Category = rep(c("Favorite", "Retweet", "Reply"), each = 3),
  Party = rep(c("Democrat", "Republican", "Overall"), 3),
  Mean = c(favorite_dem_mean, favorite_rep_mean, favorite_overall_mean, retweet_dem_mean, retweet_rep_mean, retweet_overall_mean, reply_dem_mean, reply_rep_mean, reply_overall_mean),
  LowerCI = c(favorite_dem_mean - favorite_dem_se, favorite_rep_mean - favorite_rep_se, favorite_overall_mean - favorite_overall_se, retweet_dem_mean - retweet_dem_se, retweet_rep_mean - retweet_rep_se, retweet_overall_mean - retweet_overall_se, reply_dem_mean - reply_dem_se, reply_rep_mean - reply_rep_se, reply_overall_mean - reply_overall_se),
  UpperCI = c(favorite_dem_mean + favorite_dem_se, favorite_rep_mean + favorite_rep_se, favorite_overall_mean + favorite_overall_se, retweet_dem_mean + retweet_dem_se, retweet_rep_mean + retweet_rep_se, retweet_overall_mean + retweet_overall_se, reply_dem_mean + reply_dem_se, reply_rep_mean + reply_rep_se, reply_overall_mean + reply_overall_se)
)
engagement_data$Category <- factor(engagement_data$Category, levels = c("Favorite", "Retweet", "Reply"))

engagement_plot <- ggplot(engagement_data, aes(x = Category, y = Mean, color = Party, group = Party)) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_errorbar(aes(ymin = LowerCI, ymax = UpperCI), width = 0.2, position = position_dodge(width = 0.5)) +
  labs(title = "Engagement Alignment by Engagement Type", y = "Value Alignment", x = "Engagement Type") +
  theme_minimal(base_size = 14) +
  scale_color_manual(values = c("Democrat" = "blue", "Republican" = "red", "Overall" = "grey")) +
  ylim(-0.2, 0.45)
ggsave("fig_right_individual.pdf", plot = engagement_plot, width = 8, height = 6)

favorite_dem_mean <- mean(metrics_df[metrics_df$DemRep < 4, "metrics_favorites_pca"])
favorite_rep_mean <- mean(metrics_df[metrics_df$DemRep > 3, "metrics_favorites_pca"])
favorite_overall_mean <- mean(metrics_df[, "metrics_favorites_pca"])
retweet_dem_mean <- mean(metrics_df[metrics_df$DemRep < 4, "metrics_retweets_pca"])
retweet_rep_mean <- mean(metrics_df[metrics_df$DemRep > 3, "metrics_retweets_pca"])
retweet_overall_mean <- mean(metrics_df[, "metrics_retweets_pca"])
reply_dem_mean <- mean(metrics_df[metrics_df$DemRep < 4, "metrics_replies_pca"])
reply_rep_mean <- mean(metrics_df[metrics_df$DemRep > 3, "metrics_replies_pca"])
reply_overall_mean <- mean(metrics_df[, "metrics_replies_pca"])

favorite_dem_se <- se(metrics_df[metrics_df$DemRep < 4, "metrics_favorites_pca"])
favorite_rep_se <- se(metrics_df[metrics_df$DemRep > 3, "metrics_favorites_pca"])
favorite_overall_se <- se(metrics_df[, "metrics_favorites_pca"])
retweet_dem_se <- se(metrics_df[metrics_df$DemRep < 4, "metrics_retweets_pca"])
retweet_rep_se <- se(metrics_df[metrics_df$DemRep > 3, "metrics_retweets_pca"])
retweet_overall_se <- se(metrics_df[, "metrics_retweets_pca"])
reply_dem_se <- se(metrics_df[metrics_df$DemRep < 4, "metrics_replies_pca"])
reply_rep_se <- se(metrics_df[metrics_df$DemRep > 3, "metrics_replies_pca"])
reply_overall_se <- se(metrics_df[, "metrics_replies_pca"])

global_engagement_data <- data.frame(
  Category = rep(c("Favorite", "Retweet", "Reply"), each = 3),
  Party = rep(c("Democrat", "Republican", "Overall"), 3),
  Mean = c(favorite_dem_mean, favorite_rep_mean, favorite_overall_mean, retweet_dem_mean, retweet_rep_mean, retweet_overall_mean, reply_dem_mean, reply_rep_mean, reply_overall_mean),
  LowerCI = c(favorite_dem_mean - favorite_dem_se, favorite_rep_mean - favorite_rep_se, favorite_overall_mean - favorite_overall_se, retweet_dem_mean - retweet_dem_se, retweet_rep_mean - retweet_rep_se, retweet_overall_mean - retweet_overall_se, reply_dem_mean - reply_dem_se, reply_rep_mean - reply_rep_se, reply_overall_mean - reply_overall_se),
  UpperCI = c(favorite_dem_mean + favorite_dem_se, favorite_rep_mean + favorite_rep_se, favorite_overall_mean + favorite_overall_se, retweet_dem_mean + retweet_dem_se, retweet_rep_mean + retweet_rep_se, retweet_overall_mean + retweet_overall_se, reply_dem_mean + reply_dem_se, reply_rep_mean + reply_rep_se, reply_overall_mean + reply_overall_se)
)
global_engagement_data$Category <- factor(global_engagement_data$Category, levels = c("Favorite", "Retweet", "Reply"))

global_engagement_plot <- ggplot(global_engagement_data, aes(x = Category, y = Mean, color = Party, group = Party)) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_errorbar(aes(ymin = LowerCI, ymax = UpperCI), width = 0.2, position = position_dodge(width = 0.5)) +
  labs(title = "Engagement Alignment by Engagement Type", y = "Value Alignment", x = "Engagement Type") +
  theme_minimal(base_size = 14) +
  scale_color_manual(values = c("Democrat" = "blue", "Republican" = "red", "Overall" = "grey")) +
  ylim(-0.2, 0.4)
ggsave("fig2_right_global.pdf", plot = global_engagement_plot, width = 8, height = 6)
