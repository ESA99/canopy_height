# plot_functions.R
library(dplyr)
library(ggplot2)
library(mgcv) # for GAM smoothing

# --------------------------------------------
# 1️⃣ Scatter + smooth line
# --------------------------------------------
plot_scatter_smooth <- function(df, x_var, y_var, sample_n = 500000,
                                alpha = 0.03, point_size = 0.3,
                                smooth_method = "gam", smooth_formula = y ~ s(x),
                                smooth_color = "red") {
  set.seed(42)
  df_sample <- df %>% slice_sample(n = min(sample_n, nrow(df)))
  
  ggplot(df_sample, aes_string(x = x_var, y = y_var)) +
    geom_point(alpha = alpha, size = point_size) +
    geom_smooth(method = smooth_method, formula = smooth_formula, color = smooth_color) +
    labs(x = x_var, y = y_var) +
    theme_bw()
}

# --------------------------------------------
# 2️⃣ Binned mean ± SD plot
# Can handle one or two y variables with secondary axis
# --------------------------------------------
plot_binned_sd <- function(df, x_var, y_vars, bin_width = 2, colors = c("black", "blue")) {
  # y_vars: list of variables, first is primary, second (optional) is secondary
  primary <- y_vars[[1]]
  secondary <- if(length(y_vars) > 1) y_vars[[2]] else NULL
  
  # Bin data
  df_bin <- df %>%
    mutate(bin = cut(.data[[x_var]], breaks = seq(0, max(.data[[x_var]], na.rm = TRUE) + bin_width, bin_width))) %>%
    group_by(bin) %>%
    summarise(
      height = mean(.data[[x_var]], na.rm = TRUE),
      prim_mean = mean(.data[[primary]], na.rm = TRUE),
      prim_sd = sd(.data[[primary]], na.rm = TRUE),
      sec_mean = if(!is.null(secondary)) mean(.data[[secondary]], na.rm = TRUE) else NA,
      sec_sd = if(!is.null(secondary)) sd(.data[[secondary]], na.rm = TRUE) else NA,
      .groups = "drop"
    )
  
  # Scaling factor for secondary axis
  scale_factor <- if(!is.null(secondary)) max(df_bin$prim_mean + df_bin$prim_sd, na.rm = TRUE) /
    max(df_bin$sec_mean + df_bin$sec_sd, na.rm = TRUE) else 1
  
  p <- ggplot(df_bin, aes(height)) +
    geom_ribbon(aes(ymin = prim_mean - prim_sd, ymax = prim_mean + prim_sd), fill = "grey80", alpha = 0.3) +
    geom_line(aes(y = prim_mean), color = colors[1], linewidth = 1.2, linetype = "solid")
  
  if(!is.null(secondary)) {
    p <- p +
      geom_ribbon(aes(ymin = (sec_mean - sec_sd) * scale_factor, ymax = (sec_mean + sec_sd) * scale_factor),
                  fill = colors[2], alpha = 0.2) +
      geom_line(aes(y = sec_mean * scale_factor), color = colors[2], linewidth = 1.2, linetype = "dashed") +
      scale_y_continuous(
        name = paste(primary),
        sec.axis = sec_axis(~ . / scale_factor, name = paste(secondary))
      )
  } else {
    p <- p + labs(y = primary)
  }
  
  p + labs(x = x_var) + theme_bw()
}

# --------------------------------------------
# 3️⃣ Single binned line plot
# --------------------------------------------
plot_single_bin <- function(df, x_var, y_var, bin_width = 2) {
  df_bin <- df %>%
    mutate(bin = cut(.data[[x_var]], breaks = seq(0, max(.data[[x_var]], na.rm = TRUE) + bin_width, bin_width))) %>%
    group_by(bin) %>%
    summarise(
      height = mean(.data[[x_var]], na.rm = TRUE),
      mean_y = mean(.data[[y_var]], na.rm = TRUE),
      .groups = "drop"
    )
  
  ggplot(df_bin, aes(height, mean_y)) +
    geom_line() +
    geom_point() +
    labs(x = x_var, y = y_var) +
    theme_bw()
}

# --------------------------------------------
# 4️⃣ Histogram + line plot
# --------------------------------------------
plot_histogram_line <- function(df, x_var, y_var, bin_width = 2, col_hist = "grey80", col_line = "red") {
  df_hist <- df %>%
    mutate(bin = cut(.data[[x_var]], breaks = seq(0, max(.data[[x_var]], na.rm = TRUE) + bin_width, bin_width))) %>%
    group_by(bin) %>%
    summarise(
      height = mean(.data[[x_var]], na.rm = TRUE),
      y_mean = mean(.data[[y_var]], na.rm = TRUE),
      n = n(),
      .groups = "drop"
    )
  
  scale_factor <- max(df_hist$y_mean, na.rm = TRUE) / max(df_hist$n)
  
  ggplot(df_hist, aes(x = height)) +
    geom_col(aes(y = n * scale_factor), fill = col_hist, width = bin_width * 0.9) +
    geom_line(aes(y = y_mean), color = col_line, linewidth = 1.2) +
    scale_y_continuous(
      name = y_var,
      sec.axis = sec_axis(~ . / scale_factor, name = "Pixel count")
    ) +
    labs(x = x_var) +
    theme_bw()
}

# --------------------------------------------
# 5️⃣ Log-transformed scatter + smooth
# --------------------------------------------
plot_scatter_log <- function(df, x_var, y_var, sample_n = 500000,
                             alpha = 0.03, point_size = 0.3,
                             smooth_method = "gam", smooth_formula = y ~ s(x),
                             smooth_color = "red") {
  set.seed(42)
  
  # Fix: use nrow() instead of n()
  df_sample <- df %>% slice_sample(n = min(sample_n, nrow(df))) %>%
    mutate(log_y = log(.data[[y_var]] + 1))
  
  ggplot(df_sample, aes_string(x = x_var, y = "log_y")) +
    geom_point(alpha = alpha, size = point_size) +
    geom_smooth(method = smooth_method, formula = smooth_formula, color = smooth_color) +
    labs(x = x_var, y = paste0("log(", y_var, " + 1)")) +
    theme_bw()
}