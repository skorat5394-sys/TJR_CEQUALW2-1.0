save_file_path <-  "C:/W2/post_usage_analysis/CAL_runs/TJRE_TDS_35"
setwd(save_file_path)
getwd()

seg2 <- read.csv("C:/W2/Usage/TJRE_cal_runs/TJRE_TDS_cal/TJRE_TDS_35/tsr_1_seg2.csv")
seg214 <- read.csv("C:/W2/Usage/TJRE_cal_runs/TJRE_TDS_cal/TJRE_TDS_35/tsr_2_seg214.csv")
seg252 <- read.csv("C:/W2/Usage/TJRE_cal_runs/TJRE_TDS_cal/TJRE_TDS_35/tsr_3_seg252.csv")
seg325 <- read.csv("C:/W2/Usage/TJRE_cal_runs/TJRE_TDS_cal/TJRE_TDS_35/tsr_4_seg325.csv")
seg377 <- read.csv("C:/W2/Usage/TJRE_cal_runs/TJRE_TDS_cal/TJRE_TDS_35/tsr_5_seg377.csv")

#observed comparison
Oneonta <- read.csv("C:/W2/Usage/model_inputs/model_cal_val/cal_comp/OS_cal.csv")
BR.top <- read.csv("C:/W2/Usage/model_inputs/model_cal_val/cal_comp/BRSDSU_combined_cal.csv")
BR.bot <- read.csv("C:/W2/Usage/model_inputs/model_cal_val/cal_comp/BR_TJR_cal.csv")
BR.top$SpCond.ms <- BR.top$Sp.Cond/1000

head(BR.top)
head(BR.bot)

EDH <- read.csv("C:/W2/Usage/TJRE_cal_runs/TJRE_TDS_cal/TJRE_TDS_35/EDH_cal.csv", skip = 2)
QIN <- read.csv("C:/W2/Usage/TJRE_cal_runs/TJRE_TDS_cal/TJRE_TDS_35/QIN_cal.csv", skip = 2)

CDH <- read.csv("C:/W2/Usage/TJRE_cal_runs/TJRE_TDS_cal/TJRE_TDS_35/cdh_br1.csv", skip = 2)
CIN <- read.csv("C:/W2/Usage/TJRE_cal_runs/TJRE_TDS_cal/TJRE_TDS_35/CIN_15.csv", skip = 2)

head(seg214)
plot(CDH$JDAY, CDH$TDS, type = 'l', ylim = c(0, 50),
     xlab = 'JDAY', ylab = 'TDS PPT - Salinity')
lines(CIN$JDAY, CIN$TDS, col = 'blue')
legend('topright', legend = c("TDS Tidal", "TDS Riverine"),
       col = c("black",  "blue"),
       lwd = 2)

plot(Oneonta$JDAY, Oneonta$cLevel, type = 'l')
abline(h = 0.65, col = 'red')

# ---- plotting function TDS BR ----
plot_TDS_window <- function(seg252, BR.top, BR.bot, start_day, window = 5) {
  end_day <- start_day + window
  
  plot(seg252$JDAY, seg252$TDS, type = 'l', col = 'black',
       xlim = c(start_day, end_day),
       ylim = c(0,36),
       ylab = 'TDS ppt',
       xlab = 'JDAY',
       main = paste0('BR TDS/SAL (JDAY ', start_day, '-', end_day, ')'))
  lines(BR.top$JDAY, BR.top$Sal, col = 'red')
  lines(BR.bot$JDAY, BR.bot$Sal, col = 'blue')
  legend('bottomright', legend = c("Model Output Segment 252", "BR Top", "BR Bot"),
         col = c("black", "red", "blue"),
         lwd = 2)
}
plot_all_windows <- function(seg252, BR.top, BR.bot, window = 5) {
  max_day <- max(seg252$JDAY, BR.top$JDAY, BR.bot$JDAY, na.rm = TRUE)
  starts <- seq(6, max_day, by = window)
  
  for (s in starts) {
    plot_TDS_window(seg252, BR.top, BR.bot, start_day = s, window = window)
  }
}

#call function
plot_all_windows(seg252, BR.top, BR.bot)

#save as png 
plot_all_windows_png <- function(seg252, BR.top, BR.bot, window = 5,
                                 file_prefix = "BR_TDS_window",
                                 width = 800, height = 500) {
  max_day <- max(seg252$JDAY, BR.top$JDAY, BR.bot$JDAY, na.rm = TRUE)
  starts <- seq(6, max_day, by = window)
  
  for (s in starts) {
    end_day <- s + window
    filename <- sprintf("%s_JDAY%03d-%03d.png", file_prefix, s, end_day)
    
    png(filename, width = width, height = height)
    plot_TDS_window(seg252, BR.top, BR.bot, start_day = s, window = window)
    dev.off()
  }
}
plot_all_windows_png(seg252, BR.top, BR.bot)


#multipanel plot 
plot_TDS_multipanel_png <- function(seg252, BR.top, BR.bot, window = 5,
                                    file = "BR_TDS_multipanel.png",
                                    ncol = 4,
                                    panel_width = 350, panel_height = 300) {
  max_day <- max(seg252$JDAY, BR.top$JDAY, BR.bot$JDAY, na.rm = TRUE)
  starts <- seq(6, max_day, by = window)
  n <- length(starts)
  total_panels <- n + 1  # +1 for legend panel
  nrow <- ceiling(total_panels / ncol)
  
  png(file, width = panel_width * ncol, height = panel_height * nrow)
  par(mfrow = c(nrow, ncol), mar = c(3, 3, 2, 1), oma = c(0, 0, 2, 0))
  
  for (s in starts) {
    end_day <- s + window
    plot(seg252$JDAY, seg252$TDS, type = 'l', col = 'black',
         xlim = c(s, end_day),
         ylim = c(0,36),
         ylab = '', xlab = '',
         main = paste0(s, '-', end_day))
    lines(BR.top$JDAY, BR.top$Sal, col = 'red')
    lines(BR.bot$JDAY, BR.bot$Sal, col = 'blue')
  }
  
  # empty panel just for the legend
  plot.new()
  legend('center', legend = c("Model Output Segment 252", "BR Top", "BR Bot"),
         col = c("black", "red", "blue"),
         lwd = 2, bty = 'n', cex = 1.1)
  
  mtext("BR TDS/SAL — 5-Day Windows", outer = TRUE, cex = 1.3, font = 2)
  dev.off()
}

plot_TDS_multipanel_png(seg252, BR.top, BR.bot)

# ---- plotting function TDS OS ----

plot_OS_window <- function(seg325, Oneonta, start_day, window = 5) {
  end_day <- start_day + window
  
  plot(seg325$JDAY, seg325$TDS, type = 'l', col = 'black',
       xlim = c(start_day, end_day),
       ylim = c(0, 36),
       ylab = 'TDS ppt',
       xlab = 'JDAY',
       main = paste0('OS TDS/SAL (JDAY ', start_day, '-', end_day, ')'))
  lines(Oneonta$JDAY, Oneonta$Sal, col = 'blue')
  legend('bottomright', legend = c("Model Output Segment 325", "Oneonta"),
         col = c("black", "blue"),
         lwd = 2)
}

plot_all_OS_windows <- function(seg325, Oneonta, window = 5) {
  max_day <- max(seg325$JDAY, Oneonta$JDAY, na.rm = TRUE)
  starts <- seq(6, max_day, by = window)
  
  for (s in starts) {
    plot_OS_window(seg325, Oneonta, start_day = s, window = window)
  }
}

#call function

plot_all_OS_windows(seg325, Oneonta)

# save as png 
plot_all_OS_windows_png <- function(seg325, Oneonta, window = 5,
                                    file_prefix = "OS_TDS_window",
                                    width = 800, height = 500) {
  max_day <- max(seg325$JDAY, Oneonta$JDAY, na.rm = TRUE)
  starts <- seq(6, max_day, by = window)
  
  for (s in starts) {
    end_day <- s + window
    filename <- sprintf("%s_JDAY%03d-%03d.png", file_prefix, s, end_day)
    
    png(filename, width = width, height = height)
    plot_OS_window(seg325, Oneonta, start_day = s, window = window)
    dev.off()
  }
}

plot_all_OS_windows_png(seg325, Oneonta)

#multipanel plot 
plot_OS_multipanel_png <- function(seg325, Oneonta, window = 5,
                                   file = "OS_TDS_multipanel.png",
                                   ncol = 4,
                                   panel_width = 350, panel_height = 300) {
  max_day <- max(seg325$JDAY, Oneonta$JDAY, na.rm = TRUE)
  starts <- seq(6, max_day, by = window)
  n <- length(starts)
  total_panels <- n + 1
  nrow <- ceiling(total_panels / ncol)
  
  png(file, width = panel_width * ncol, height = panel_height * nrow)
  par(mfrow = c(nrow, ncol), mar = c(3, 3, 2, 1), oma = c(0, 0, 2, 0))
  
  for (s in starts) {
    end_day <- s + window
    plot(seg325$JDAY, seg325$TDS, type = 'l', col = 'black',
         xlim = c(s, end_day), ylim = c(0,36),
         ylab = '', xlab = '',
         main = paste0(s, '-', end_day))
    lines(Oneonta$JDAY, Oneonta$Sal, col = 'blue')
  }
  
  plot.new()
  legend('center', legend = c("Model Output Segment 325", "Oneonta"),
         col = c("black", "blue"),
         lwd = 2, bty = 'n', cex = 1.1)
  
  mtext("OS TDS/SAL — 5-Day Windows", outer = TRUE, cex = 1.3, font = 2)
  dev.off()
}

plot_OS_multipanel_png(seg325, Oneonta)

# ---- plot BR, OS, EDH, QIN ----

plot(Oneonta$JDAY, Oneonta$cLevel, type = 'l', col = "black",
     xlim = c(6, 55), 
     ylim = c(0, 6))
lines(BR.bot$JDAY, BR.bot$cLevel, col = 'blue')
lines(EDH$JDAY, EDH$ELO, col = 'red')
lines(QIN$JDAY, QIN$QIN, col = 'purple')

# plotting function
plot_level_flow_window <- function(Oneonta, BR.bot, EDH, QIN, start_day, window = 5) {
  end_day <- start_day + window
  
  plot(Oneonta$JDAY, Oneonta$cLevel, type = 'l', col = "black",
       xlim = c(start_day, end_day),
       ylim = c(0, 6),
       ylab = 'Level / Flow',
       xlab = 'JDAY',
       main = paste0('Level & Flow (JDAY ', start_day, '-', end_day, ')'))
  lines(BR.bot$JDAY, BR.bot$cLevel, col = 'blue')
  lines(EDH$JDAY, EDH$ELO, col = 'red')
  lines(QIN$JDAY, QIN$QIN, col = 'purple')
  legend('topright', legend = c("Oneonta cLevel", "BR Bot cLevel", "EDH ELO", "QIN QIN"),
         col = c("black", "blue", "red", "purple"),
         lwd = 2)
}

plot_all_level_flow_windows <- function(Oneonta, BR.bot, EDH, QIN, window = 5) {
  max_day <- max(Oneonta$JDAY, BR.bot$JDAY, EDH$JDAY, QIN$JDAY, na.rm = TRUE)
  starts <- seq(6, max_day, by = window)
  
  for (s in starts) {
    plot_level_flow_window(Oneonta, BR.bot, EDH, QIN, start_day = s, window = window)
  }
}


#call function

# single window
plot_level_flow_window(Oneonta, BR.bot, EDH, QIN, start_day = 6)

# all windows, capped to your original range of 6-55
plot_all_level_flow_windows(Oneonta, BR.bot, EDH, QIN)

#create pngs 
plot_all_level_flow_windows_png <- function(Oneonta, BR.bot, EDH, QIN, window = 5, 
                                            file_prefix = "level_flow_window",
                                            width = 800, height = 500) {
  max_day <- max(Oneonta$JDAY, BR.bot$JDAY, EDH$JDAY, QIN$JDAY, na.rm = TRUE)
  starts <- seq(6, max_day, by = window)
  
  for (s in starts) {
    end_day <- s + window
    filename <- sprintf("%s_JDAY%03d-%03d.png", file_prefix, s, end_day)
    
    png(filename, width = width, height = height)
    plot_level_flow_window(Oneonta, BR.bot, EDH, QIN, start_day = s, window = window)
    dev.off()
  }
}

plot_all_level_flow_windows_png(Oneonta, BR.bot, EDH, QIN)

#multipanel plot
plot_level_flow_multipanel_png <- function(Oneonta, BR.bot, EDH, QIN, window = 5,
                                           file = "level_flow_multipanel.png",
                                           ncol = 4,
                                           panel_width = 350, panel_height = 300) {
  max_day <- max(Oneonta$JDAY, BR.bot$JDAY, EDH$JDAY, QIN$JDAY, na.rm = TRUE)
  starts <- seq(6, max_day, by = window)
  n <- length(starts)
  total_panels <- n + 1
  nrow <- ceiling(total_panels / ncol)
  
  png(file, width = panel_width * ncol, height = panel_height * nrow)
  par(mfrow = c(nrow, ncol), mar = c(3, 3, 2, 1), oma = c(0, 0, 2, 0))
  
  for (s in starts) {
    end_day <- s + window
    plot(Oneonta$JDAY, Oneonta$cLevel, type = 'l', col = "black",
         xlim = c(s, end_day),
         ylim = c(0, 6),
         ylab = '', xlab = '',
         main = paste0(s, '-', end_day))
    lines(BR.bot$JDAY, BR.bot$cLevel, col = 'blue')
    lines(EDH$JDAY, EDH$ELO, col = 'red')
    lines(QIN$JDAY, QIN$QIN, col = 'purple')
  }
  
  plot.new()
  legend('center', legend = c("Oneonta cLevel", "BR Bot cLevel", "EDH ELO", "QIN QIN"),
         col = c("black", "blue", "red", "purple"),
         lwd = 2, bty = 'n', cex = 1.1)
  
  mtext("Level & Flow — 5-Day Windows", outer = TRUE, cex = 1.3, font = 2)
  dev.off()
}

plot_level_flow_multipanel_png(Oneonta, BR.bot, EDH, QIN, ncol = 4)


# ---- compare BR and OS WL modeled and real - multipanel ----
head(seg2)

plot_BR_WL_multipanel_png <- function(seg252, BR.bot, window = 5,
                                      file = "BR_WL_multipanel.png",
                                      ncol = 4,
                                      panel_width = 350, panel_height = 300) {
  max_day <- max(seg252$JDAY, BR.bot$JDAY, na.rm = TRUE)
  starts <- seq(6, max_day, by = window)
  n <- length(starts)
  total_panels <- n + 1
  nrow <- ceiling(total_panels / ncol)
  
  png(file, width = panel_width * ncol, height = panel_height * nrow)
  par(mfrow = c(nrow, ncol), mar = c(3, 3, 2, 1), oma = c(0, 0, 2, 0))
  
  for (s in starts) {
    end_day <- s + window
    plot(seg252$JDAY, seg252$ELWS.m., type = 'l', col = 'black',
         xlim = c(s, end_day),
         ylab = '', xlab = '',
         main = paste0(s, '-', end_day))
    lines(BR.bot$JDAY, BR.bot$cLevel, col = 'blue')
  }
  
  plot.new()
  legend('center', legend = c("Model Output Segment 252", "BR Bot"),
         col = c("black", "blue"),
         lwd = 2, bty = 'n', cex = 1.1)
  
  mtext("BR WL — 5-Day Windows", outer = TRUE, cex = 1.3, font = 2)
  dev.off()
}
plot_OS_WL_multipanel_png <- function(seg325, Oneonta, window = 5,
                                      file = "OS_WL_multipanel.png",
                                      ncol = 4,
                                      panel_width = 350, panel_height = 300) {
  max_day <- max(seg325$JDAY, Oneonta$JDAY, na.rm = TRUE)
  starts <- seq(6, max_day, by = window)
  n <- length(starts)
  total_panels <- n + 1
  nrow <- ceiling(total_panels / ncol)
  
  png(file, width = panel_width * ncol, height = panel_height * nrow)
  par(mfrow = c(nrow, ncol), mar = c(3, 3, 2, 1), oma = c(0, 0, 2, 0))
  
  for (s in starts) {
    end_day <- s + window
    plot(seg325$JDAY, seg325$ELWS.m., type = 'l', col = 'black',
         xlim = c(s, end_day),
         ylab = '', xlab = '',
         main = paste0(s, '-', end_day))
    lines(Oneonta$JDAY, Oneonta$cLevel, col = 'blue')
  }
  
  plot.new()
  legend('center', legend = c("Model Output Segment 325", "Oneonta"),
         col = c("black", "blue"),
         lwd = 2, bty = 'n', cex = 1.1)
  
  mtext("OS WL — 5-Day Windows", outer = TRUE, cex = 1.3, font = 2)
  dev.off()
}


plot_BR_WL_multipanel_png(seg252, BR.bot)
plot_OS_WL_multipanel_png(seg325, Oneonta)


# plot TDS at tidal outlet seg 214
plot(seg214$JDAY, seg214$TDS, type = 'l',
     ylim = c(0, 36),
     xlab = 'JDAY',
     ylab = 'TDS/ Sal.',
     main = 'Tidal Segment TDS')
# ---- WL calibration metrics: Willmott's d, RMSE, NSE, bias-by-phase, peak-timing lag ----

willmott_d <- function(obs, sim) {
  obs_bar <- mean(obs, na.rm = TRUE)
  numerator <- sum((obs - sim)^2, na.rm = TRUE)
  denominator <- sum((abs(sim - obs_bar) + abs(obs - obs_bar))^2, na.rm = TRUE)
  1 - (numerator / denominator)
}

rmse <- function(obs, sim) sqrt(mean((obs - sim)^2, na.rm = TRUE))

nse <- function(obs, sim) {
  1 - sum((obs - sim)^2, na.rm = TRUE) / sum((obs - mean(obs, na.rm = TRUE))^2, na.rm = TRUE)
}

compute_wl_metrics <- function(model_df, obs_df,
                               model_col = "ELWS.m.", obs_col = "cLevel",
                               spinup_end = 31, label = "",
                               dt_minutes = NULL, max_lag = 20,
                               high_low_quantile = 0.2, make_plots = TRUE) {
  model_df <- model_df[model_df$JDAY >= spinup_end, ]
  obs_df   <- obs_df[obs_df$JDAY >= spinup_end, ]
  
  model_interp <- approx(x = model_df$JDAY, y = model_df[[model_col]],
                         xout = obs_df$JDAY, rule = 1)$y
  obs_vals  <- obs_df[[obs_col]]
  jday_vals <- obs_df$JDAY
  
  keep <- complete.cases(obs_vals, model_interp)
  obs_vals <- obs_vals[keep]; model_interp <- model_interp[keep]; jday_vals <- jday_vals[keep]
  
  d <- willmott_d(obs_vals, model_interp)
  rms <- rmse(obs_vals, model_interp)
  nse_val <- nse(obs_vals, model_interp)
  
  ccf_result <- ccf(obs_vals, model_interp, lag.max = max_lag, plot = FALSE)
  lag_at_peak <- ccf_result$lag[which.max(ccf_result$acf)]
  peak_corr <- max(ccf_result$acf)
  
  resid_vals <- obs_vals - model_interp
  overall_bias <- mean(resid_vals)
  
  q_lo <- quantile(obs_vals, high_low_quantile, na.rm = TRUE)
  q_hi <- quantile(obs_vals, 1 - high_low_quantile, na.rm = TRUE)
  phase <- ifelse(obs_vals >= q_hi, "high", ifelse(obs_vals <= q_lo, "low", "mid"))
  
  bias_high <- mean(resid_vals[phase == "high"])
  bias_mid  <- mean(resid_vals[phase == "mid"])
  bias_low  <- mean(resid_vals[phase == "low"])
  rmse_high <- rmse(obs_vals[phase == "high"], model_interp[phase == "high"])
  rmse_low  <- rmse(obs_vals[phase == "low"],  model_interp[phase == "low"])
  
  cat(sprintf("--- %s ---\nWillmott's d: %.3f\nRMSE: %.3f m\nNSE: %.3f\nn = %d\nBias: %+.3f m (high %+.3f / mid %+.3f / low %+.3f)\n\n",
              label, d, rms, nse_val, length(obs_vals), overall_bias, bias_high, bias_mid, bias_low))
  
  invisible(list(d = d, rmse = rms, nse = nse_val, n = length(obs_vals),
                 bias_overall = overall_bias, bias_high = bias_high, bias_mid = bias_mid, bias_low = bias_low,
                 rmse_high = rmse_high, rmse_low = rmse_low,
                 obs = obs_vals, sim = model_interp, jday = jday_vals, phase = phase))
}

find_tidal_peaks <- function(jday, wl, type = c("high", "low"), min_spacing = 0.3) {
  type <- match.arg(type)
  if (type == "low") wl <- -wl
  n <- length(wl); is_peak <- logical(n)
  for (i in 2:(n - 1)) is_peak[i] <- wl[i] > wl[i - 1] && wl[i] >= wl[i + 1]
  peak_idx <- which(is_peak)
  if (length(peak_idx) == 0) return(data.frame(JDAY = numeric(0), value = numeric(0)))
  keep <- logical(length(peak_idx)); last_kept_jday <- -Inf
  for (i in seq_along(peak_idx)) {
    j <- jday[peak_idx[i]]
    if (j - last_kept_jday >= min_spacing) { keep[i] <- TRUE; last_kept_jday <- j }
  }
  idx <- peak_idx[keep]
  out_val <- if (type == "low") -wl[idx] else wl[idx]
  data.frame(JDAY = jday[idx], value = out_val)
}

match_and_diff_peaks <- function(obs_peaks, sim_peaks, max_match_window = 0.15) {
  matched <- data.frame(obs_JDAY = numeric(0), sim_JDAY = numeric(0), lag_minutes = numeric(0))
  for (i in seq_len(nrow(obs_peaks))) {
    dists <- abs(sim_peaks$JDAY - obs_peaks$JDAY[i])
    j <- which.min(dists)
    if (length(j) > 0 && dists[j] <= max_match_window) {
      lag_days <- sim_peaks$JDAY[j] - obs_peaks$JDAY[i]
      matched <- rbind(matched, data.frame(obs_JDAY = obs_peaks$JDAY[i], sim_JDAY = sim_peaks$JDAY[j],
                                           lag_minutes = lag_days * 1440))
    }
  }
  matched
}

check_tidal_timing <- function(model_df, obs_df, model_col = "ELWS.m.", obs_col = "cLevel",
                               spinup_end = 31, label = "") {
  model_df <- model_df[model_df$JDAY >= spinup_end, ]
  obs_df   <- obs_df[obs_df$JDAY >= spinup_end, ]
  obs_high <- find_tidal_peaks(obs_df$JDAY, obs_df[[obs_col]], "high")
  obs_low  <- find_tidal_peaks(obs_df$JDAY, obs_df[[obs_col]], "low")
  sim_high <- find_tidal_peaks(model_df$JDAY, model_df[[model_col]], "high")
  sim_low  <- find_tidal_peaks(model_df$JDAY, model_df[[model_col]], "low")
  high_matches <- match_and_diff_peaks(obs_high, sim_high)
  low_matches  <- match_and_diff_peaks(obs_low, sim_low)
  cat(sprintf("--- %s: tidal peak timing ---\nHigh tide: mean lag = %.2f min\nLow tide:  mean lag = %.2f min\n\n",
              label, mean(high_matches$lag_minutes), mean(low_matches$lag_minutes)))
  invisible(list(high = high_matches, low = low_matches))
}

# ---- run for both stations ----
br_metrics <- compute_wl_metrics(seg252, BR.bot, label = "Boca Rio (seg252 vs BR.bot)")
os_metrics <- compute_wl_metrics(seg325, Oneonta, label = "Oneonta (seg325 vs Oneonta)")
br_timing  <- check_tidal_timing(seg252, BR.bot, label = "Boca Rio")
os_timing  <- check_tidal_timing(seg325, Oneonta, label = "Oneonta")

# ---- summary table for thesis ----
build_wl_summary_table <- function(br_metrics, os_metrics, br_timing, os_timing) {
  extract_row <- function(m, timing, station_name) {
    data.frame(
      Station = station_name,
      `Willmott's d` = round(m$d, 3),
      `RMSE (m)` = round(m$rmse, 3),
      NSE = round(m$nse, 3),
      `n (obs)` = m$n,
      `Overall Bias (m)` = round(m$bias_overall, 3),
      `High Tide Bias (m)` = round(m$bias_high, 3),
      `Mid Tide Bias (m)` = round(m$bias_mid, 3),
      `Low Tide Bias (m)` = round(m$bias_low, 3),
      `High Tide Lag (min)` = round(mean(timing$high$lag_minutes, na.rm = TRUE), 1),
      `Low Tide Lag (min)` = round(mean(timing$low$lag_minutes, na.rm = TRUE), 1),
      check.names = FALSE
    )
  }
  summary_table <- rbind(
    extract_row(br_metrics, br_timing, "Boca Rio"),
    extract_row(os_metrics, os_timing, "Oneonta Slough")
  )
  print(summary_table, row.names = FALSE)
  write.csv(summary_table, "WL_calibration_summary.csv", row.names = FALSE)
  invisible(summary_table)
}

wl_summary <- build_wl_summary_table(br_metrics, os_metrics, br_timing, os_timing)

# ---- compare velocity BR and Tidal Boundary ----
compare_velocity <- function(seg_boundary, seg_br, spinup_end = 31,
                             label_boundary = "Boundary (seg214)",
                             label_br = "BR (seg252)") {
  b <- seg_boundary[seg_boundary$JDAY >= spinup_end, ]
  r <- seg_br[seg_br$JDAY >= spinup_end, ]
  
  stats <- data.frame(
    Segment = c(label_boundary, label_br),
    Mean_abs_U = c(mean(abs(b$U.ms.1.), na.rm = TRUE), mean(abs(r$U.ms.1.), na.rm = TRUE)),
    Max_abs_U  = c(max(abs(b$U.ms.1.), na.rm = TRUE),  max(abs(r$U.ms.1.), na.rm = TRUE)),
    RMS_U      = c(sqrt(mean(b$U.ms.1.^2, na.rm = TRUE)), sqrt(mean(r$U.ms.1.^2, na.rm = TRUE)))
  )
  print(stats)
  
  # quick visual overlay
  plot(b$JDAY, abs(b$U.ms.1.), type = 'l', col = 'black',
       xlab = 'JDAY', ylab = '|Velocity| (m/s)', main = 'Velocity magnitude: boundary vs BR')
  lines(r$JDAY, abs(r$U.ms.1.), col = 'blue')
  legend('topright', legend = c(label_boundary, label_br), col = c('black','blue'), lwd = 2)
  
  invisible(stats)
}

compare_velocity(seg214, seg252)
# ---- compare tidal range BR and Tidal Boundary ----


plot(seg214$JDAY, seg214$ELWS.m., type = 'l', xlim = c(35, 40),
     xlab = 'JDAY', ylab = 'ELWS (m)', main = 'Boundary segment water level (5-day window)')

compare_tidal_range_robust <- function(seg_boundary, seg_br, spinup_end = 31) {
  b <- seg_boundary[seg_boundary$JDAY >= spinup_end, ]
  r <- seg_br[seg_br$JDAY >= spinup_end, ]
  
  range_b <- diff(quantile(b$ELWS.m., c(0.05, 0.95), na.rm = TRUE))
  range_r <- diff(quantile(r$ELWS.m., c(0.05, 0.95), na.rm = TRUE))
  
  cat(sprintf("5th-95th percentile range — Boundary: %.3f m | BR: %.3f m | Damping: %.1f%%\n",
              range_b, range_r, 100 * (1 - range_r / range_b)))
  invisible(list(range_boundary = range_b, range_br = range_r))
}

compare_tidal_range_robust(seg214, seg252)