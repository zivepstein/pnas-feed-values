local({
  script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  root <- if (length(script_arg) > 0) {
    dirname(normalizePath(sub("^--file=", "", script_arg[1])))
  } else {
    getwd()
  }
  source(file.path(root, "R", "utils.R"))
})

cluster_amp_results <- read.csv(data_path("amplification_data.csv"))[20:23, ]
cluster_amp_results <- cluster_amp_results[c(3, 4, 1, 2), ]
pdf("fig1a.pdf", width = 10, height = 8)
b <- barplot(cluster_amp_results$coefficient_amplification, ylim = c(-0.1, 0.15))
error.bar(b, cluster_amp_results$coefficient_amplification, cluster_amp_results$se_amplification)
abline(h = 0)
dev.off()

amp_results <- read_amp_results()
amp_pca <- amp_results[, 8]

library(fmsb)

radar_data <- rbind(
  max = rep(max(amp_pca), 19),
  min = rep(min(amp_pca), 19),
  Overall = amp_pca
)
colnames(radar_data) <- amp_results[, 1]
val_order <- c(
  "Tolerance", "Self-directed Thoughts", "Self-directed Action", "Stimulation", "Hedonism",
  "Achievement", "Dominance", "Resources", "Face", "Societal Security", "Personal Security",
  "Tradition", "Rule Conformity", "Interpersonal Confirmity", "Humility", "Caring",
  "Dependability", "Universal Concern", "Preservation of Nature"
)
radar_data <- radar_data[, val_order]

colors <- c(rgb(0.1, 0.1, 0.1, 0.1))
pdf("fig1b.pdf", width = 10, height = 8)
radarchart(
  data.frame(radar_data),
  axistype = 1,
  pcol = colors,
  pfcol = colors,
  plwd = 2,
  plty = 1,
  title = "Schwartz Values",
  cglcol = "grey70",
  cglty = 1,
  axislabcol = "grey30",
  caxislabels = round(seq(min(amp_pca), max(amp_pca), length.out = 5), 3),
  cglwd = 0.8,
  vlcex = 0.7
)
dev.off()
