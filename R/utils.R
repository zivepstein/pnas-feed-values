get_script_dir <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg) > 0) {
    return(dirname(normalizePath(sub("^--file=", "", file_arg[1]))))
  }
  NULL
}

repo_root <- function() {
  script_dir <- get_script_dir()
  if (!is.null(script_dir)) {
    return(script_dir)
  }
  getwd()
}

data_path <- function(...) {
  file.path(repo_root(), "data", ...)
}

colSE <- function(x, na.rm = TRUE) {
  sapply(x, function(col) sd(col, na.rm = na.rm) / sqrt(sum(!is.na(col))))
}

se <- function(x) {
  sd(x, na.rm = TRUE) / sqrt(length(x))
}

error.bar <- function(x, y, upper, lower = upper, length = 0.05, ...) {
  if (length(x) != length(y) || length(y) != length(lower) || length(lower) != length(upper)) {
    stop("vectors must be same length")
  }
  arrows(x, y + upper, x, y - lower, angle = 90, code = 3, length = length, ...)
}

spearman_alignment <- function(profile_matrix, reference) {
  reference <- as.numeric(reference)
  apply(profile_matrix, 1, function(row) {
    suppressWarnings(stats::cor(row, reference, method = "spearman"))
  })
}

rowwise_spearman_alignment <- function(x_matrix, y_matrix) {
  vapply(seq_len(nrow(x_matrix)), function(i) {
    suppressWarnings(stats::cor(x_matrix[i, ], y_matrix[i, ], method = "spearman"))
  }, numeric(1))
}

amp_value_reorder <- c(12, 14, 13, 15, 16, 19, 18, 17, 11, 10, 9, 8, 7, 6, 4, 5, 3, 2, 1)

value_norm_reorder <- c(8, 6, 7, 5, 4, 1, 2, 3, 9, 10, 11, 12, 13, 14, 19, 18, 16, 15, 17)

perception_col_reorder <- c(12, 17, 18, 19, 16, 13, 14, 15, 11, 7, 8, 9, 10, 3, 1, 2, 4, 5, 6, 20)

read_amp_results <- function() {
  amp_results <- read.csv(data_path("amplification_data.csv"))[1:19, ]
  amp_results[amp_value_reorder, ]
}
