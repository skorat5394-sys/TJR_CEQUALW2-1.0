library(dplyr)
library(tidyr)
library(ggplot2)


## --- shared data prep -------------------------------------------
## Reads the .opt file and returns a tidy long data frame with columns:
##   Julian_day, Layer (integer, 1 = surface), Depth, Segment, TDS
## NOTE: the raw "Depth" column is the depth-below-surface of each
## reported row, which shifts slightly every timestep as water level
## changes -- it is NOT a stable layer identifier on its own. W2 writes
## layers top-to-bottom within each timestep, so Layer = 1 (nearest
## surface), 2, 3, ... is assigned by row position within each
## Julian_day block instead.
save_file_path <- "C:/W2/post_usage_analysis/CAL_runs/TJRE_TDS_35"
setwd(save_file_path)
getwd()

model_file <- "C:/W2/Usage/TJRE_cal_runs/TJRE_TDS_cal/TJRE_TDS_35/spr1.opt"
br_top_file     <- "C:/W2/Usage/model_inputs/model_cal_val/cal_comp/BRSDSU_combined_cal.csv"
br_bot_file     <- "C:/W2/Usage/model_inputs/model_cal_val/cal_comp/BR_TJR_cal.csv"
br_segment      <- "252"
oneonta_file    <- "C:/W2/Usage/model_inputs/model_cal_val/cal_comp/OS_cal.csv"
os_segment      <- "325"
spinup_end_jday <- 31.0

out_dir      <- "C:/W2/post_usage_analysis/CAL_runs/TJRE_TDS_35"
br_out_dir   <- file.path(out_dir, "BR_comparison")
os_out_dir   <- file.path(out_dir, "OS_comparison")
temp_out_dir <- file.path(out_dir, "temp")
vel_out_dir  <- file.path(out_dir, "velocity")


## --- shared data prep -------------------------------------------
## Reads the .opt file and returns a tidy long data frame with columns:
##   Julian_day, Layer (integer, 1 = surface), Depth, Segment, TDS
## NOTE: the raw "Depth" column is the depth-below-surface of each
## reported row, which shifts slightly every timestep as water level
## changes -- it is NOT a stable layer identifier on its own. W2 writes
## layers top-to-bottom within each timestep, so Layer = 1 (nearest
## surface), 2, 3, ... is assigned by row position within each
## Julian_day block instead.

read_tds_long <- function(file_path, missing_value = -99.00) {
  read_constituent_long(file_path, "^TDS", missing_value) %>%
    rename(TDS = Value)
}

## --- generic constituent reader (shared by TDS, Temperature, Velocity) -
## Same parsing logic as before, generalized to pull any constituent
## block out of the .opt file by matching against the file's own
## Constituent labels. constituent_pattern is a regex, e.g. "^TDS" or
## "^T2|^Temp". Returns a generic "Value" column -- callers (read_tds_long,
## read_temp_long, read_velocity_long) rename it to something specific.
read_constituent_long <- function(file_path, constituent_pattern, missing_value = -99.00) {
  
  raw <- read.csv(file_path, skip = 1, header = FALSE,
                  stringsAsFactors = FALSE, fill = TRUE,
                  colClasses = "character")
  
  header_line <- readLines(file_path, n = 1)
  header <- strsplit(header_line, ",")[[1]]
  seg_labels <- header[grepl("^Seg_", trimws(header))]
  seg_ids <- trimws(gsub("Seg_", "", seg_labels))
  
  col_names <- c("Constituent", "Julian_day", "Depth")
  for (s in seg_ids) {
    col_names <- c(col_names, paste0("Elev_", s), paste0("Value_", s))
  }
  
  raw <- raw[, seq_along(col_names)]
  names(raw) <- col_names
  
  raw <- raw %>%
    mutate(
      Constituent = trimws(Constituent),
      Julian_day  = as.numeric(Julian_day),
      Depth       = as.numeric(Depth)
    ) %>%
    mutate(across(starts_with("Value_"), as.numeric)) %>%
    mutate(across(starts_with("Elev_"), as.numeric))
  
  const_match <- unique(raw$Constituent)[grepl(constituent_pattern, unique(raw$Constituent))]
  if (length(const_match) == 0) {
    stop("No constituent matching '", constituent_pattern, "' found. Available constituents in this file: ",
         paste(unique(raw$Constituent), collapse = ", "))
  }
  
  long_df <- raw %>%
    filter(Constituent == const_match[1]) %>%
    group_by(Julian_day) %>%
    mutate(Layer = row_number()) %>%
    ungroup() %>%
    select(Julian_day, Layer, Depth, starts_with("Value_")) %>%
    pivot_longer(
      cols = starts_with("Value_"),
      names_to = "Segment",
      names_prefix = "Value_",
      values_to = "Value"
    ) %>%
    mutate(
      Value = na_if(Value, missing_value),
      Segment = factor(Segment, levels = seg_ids)
    ) %>%
    filter(!is.na(Value))
  
  message("Matched constituent: ", const_match[1])
  message("Unique Layer values (should be a short run like 1..25): ",
          paste(range(long_df$Layer), collapse = " to "))
  message("Number of distinct layers: ", length(unique(long_df$Layer)))
  
  long_df
}

## --- temperature reader ------------------------------------------------
## Matches whichever constituent label your .opt file uses for
## temperature -- commonly "T2" (W2's internal name) or "Temperature".
## If this errors with "No constituent matching", the error message
## lists every constituent name actually present in your file -- copy
## the right one into constituent_pattern below (or pass your own
## pattern directly to read_constituent_long).
read_temp_long <- function(file_path, missing_value = -99.00,
                           constituent_pattern = "^T2$|^Temp") {
  read_constituent_long(file_path, constituent_pattern, missing_value) %>%
    rename(Temperature = Value)
}

## --- plot: one line per vertical layer, all layers (temperature) -----
## min_jday: optional cutoff -- when set (e.g. 31), only Julian_day >= min_jday
## is plotted/saved, and the title/filename get a "(Day X+)"/"_from{X}" tag so
## the cut version doesn't overwrite the full-range output. NULL (default)
## keeps the original full-range behavior.
plot_temp_by_layer <- function(file_path,
                               out_dir = "temp_plots",
                               missing_value = -99.00,
                               save_plots = TRUE,
                               min_jday = NULL,
                               width = 9, height = 6, dpi = 150) {
  
  temp_long <- read_temp_long(file_path, missing_value) %>%
    mutate(Layer = factor(Layer))
  
  if (!is.null(min_jday)) temp_long <- temp_long %>% filter(Julian_day >= min_jday)
  
  if (save_plots && !dir.exists(out_dir)) dir.create(out_dir)
  
  title_tag <- if (!is.null(min_jday)) paste0(" (Day ", min_jday, "+)") else ""
  file_tag  <- if (!is.null(min_jday)) paste0("_from", min_jday) else ""
  
  plots <- list()
  for (seg in levels(temp_long$Segment)) {
    df_seg <- temp_long %>% filter(Segment == seg)
    if (nrow(df_seg) == 0) next
    
    p <- ggplot(df_seg, aes(x = Julian_day, y = Temperature, color = Layer, group = Layer)) +
      geom_line(linewidth = 0.4) +
      labs(
        title = paste0("Temperature by Vertical Layer — Segment ", seg, title_tag),
        x = "Julian Day",
        y = "Temperature (°C)",
        color = "Layer\n(1 = surface)"
      ) +
      theme_minimal(base_size = 12) +
      theme(legend.key.height = unit(0.3, "cm")) +
      guides(color = guide_legend(ncol = 1))
    
    plots[[seg]] <- p
    
    if (save_plots) {
      ggsave(
        filename = file.path(out_dir, paste0("Temp_Seg_", seg, file_tag, ".png")),
        plot = p, width = width, height = height, dpi = dpi
      )
    }
  }
  
  invisible(plots)
}

## --- plot: second-from-top vs. deepest layer (temperature) -----------
## Same layer-selection logic as plot_tds_top_bottom().
## min_jday: see plot_temp_by_layer() -- same optional cutoff behavior.
plot_temp_top_bottom <- function(file_path,
                                 out_dir = "temp_plots_top_bottom",
                                 missing_value = -99.00,
                                 save_plots = TRUE,
                                 min_jday = NULL,
                                 width = 9, height = 6, dpi = 150) {
  
  temp_long <- read_temp_long(file_path, missing_value)
  
  if (!is.null(min_jday)) temp_long <- temp_long %>% filter(Julian_day >= min_jday)
  
  tb <- temp_long %>%
    group_by(Segment, Julian_day) %>%
    mutate(max_layer = max(Layer)) %>%
    ungroup() %>%
    mutate(target_layer = ifelse(max_layer >= 2, 2, max_layer)) %>%
    filter(Layer == target_layer | Layer == max_layer) %>%
    distinct(Segment, Julian_day, Layer, .keep_all = TRUE) %>%
    mutate(
      Layer_type = ifelse(Layer == max_layer, "Deepest", "Second from top"),
      Layer_type = factor(Layer_type, levels = c("Second from top", "Deepest"))
    )
  
  if (save_plots && !dir.exists(out_dir)) dir.create(out_dir)
  
  title_tag <- if (!is.null(min_jday)) paste0(" (Day ", min_jday, "+)") else ""
  file_tag  <- if (!is.null(min_jday)) paste0("_from", min_jday) else ""
  
  plots <- list()
  for (seg in levels(tb$Segment)) {
    df_seg <- tb %>% filter(Segment == seg)
    if (nrow(df_seg) == 0) next
    
    p <- ggplot(df_seg, aes(x = Julian_day, y = Temperature, color = Layer_type, group = Layer_type)) +
      geom_line(linewidth = 0.5) +
      scale_color_manual(values = c("Second from top" = "#d95f02", "Deepest" = "#1f78b4")) +
      labs(
        title = paste0("Temperature: Second-from-Top vs. Deepest Layer — Segment ", seg, title_tag),
        x = "Julian Day",
        y = "Temperature (°C)",
        color = "Layer"
      ) +
      theme_minimal(base_size = 12)
    
    plots[[seg]] <- p
    
    if (save_plots) {
      ggsave(
        filename = file.path(out_dir, paste0("Temp_TopBottom_Seg_", seg, file_tag, ".png")),
        plot = p, width = width, height = height, dpi = dpi
      )
    }
  }
  
  invisible(plots)
}

## =================================================================
## VELOCITY / FLOW DIRECTION
##
## Mirrors read_temp_long() -- matches whichever constituent label
## your .opt file uses for velocity, typically "HorizontalVelocity(ms-1)".
## Sign convention (per the W2 manual): positive = downstream
## (increasing segment number), negative = upstream (decreasing
## segment number). For this model's branch layout, increasing segment
## number runs toward each branch's DS attachment point -- for Branch 1
## that's the ocean boundary (segment 214), so in Branch 1 positive
## reads as seaward/ebb and negative as landward/flood. Check the same
## logic against your BRANCH G table before labeling Branches 2-4 the
## same way, since DS doesn't always point seaward for a side branch.
## =================================================================

read_velocity_long <- function(file_path, missing_value = -99.00,
                               constituent_pattern = "^HorizontalVelocity") {
  read_constituent_long(file_path, constituent_pattern, missing_value) %>%
    rename(Velocity = Value) %>%
    mutate(Direction = ifelse(Velocity >= 0, "Downstream (+)", "Upstream (-)"))
}

## --- plot: one line per vertical layer, all layers (velocity) --------
## Same min_jday cutoff behavior as plot_temp_by_layer(). A horizontal
## line at 0 marks the direction switch (above = downstream, below =
## upstream) so flow reversals are easy to spot per layer.
plot_velocity_by_layer <- function(file_path,
                                   out_dir = "velocity_plots",
                                   missing_value = -99.00,
                                   save_plots = TRUE,
                                   min_jday = NULL,
                                   width = 9, height = 6, dpi = 150) {
  
  vel_long <- read_velocity_long(file_path, missing_value) %>%
    mutate(Layer = factor(Layer))
  
  if (!is.null(min_jday)) vel_long <- vel_long %>% filter(Julian_day >= min_jday)
  
  if (save_plots && !dir.exists(out_dir)) dir.create(out_dir)
  
  title_tag <- if (!is.null(min_jday)) paste0(" (Day ", min_jday, "+)") else ""
  file_tag  <- if (!is.null(min_jday)) paste0("_from", min_jday) else ""
  
  plots <- list()
  for (seg in levels(vel_long$Segment)) {
    df_seg <- vel_long %>% filter(Segment == seg)
    if (nrow(df_seg) == 0) next
    
    p <- ggplot(df_seg, aes(x = Julian_day, y = Velocity, color = Layer, group = Layer)) +
      geom_hline(yintercept = 0, linewidth = 0.3, color = "grey40") +
      geom_line(linewidth = 0.4) +
      labs(
        title = paste0("Horizontal Velocity by Vertical Layer — Segment ", seg, title_tag),
        x = "Julian Day",
        y = "Velocity (m/s)  [+ downstream / - upstream]",
        color = "Layer\n(1 = surface)"
      ) +
      theme_minimal(base_size = 12) +
      theme(legend.key.height = unit(0.3, "cm")) +
      guides(color = guide_legend(ncol = 1))
    
    plots[[seg]] <- p
    
    if (save_plots) {
      ggsave(
        filename = file.path(out_dir, paste0("Velocity_Seg_", seg, file_tag, ".png")),
        plot = p, width = width, height = height, dpi = dpi
      )
    }
  }
  
  invisible(plots)
}

## --- plot: second-from-top vs. deepest layer (velocity) --------------
## Same layer-selection logic as plot_tds_top_bottom() / plot_temp_top_bottom().
## Useful for spotting shear -- e.g. surface flowing downstream on flood
## while the bottom layer is still draining upstream from the prior ebb.
plot_velocity_top_bottom <- function(file_path,
                                     out_dir = "velocity_plots_top_bottom",
                                     missing_value = -99.00,
                                     save_plots = TRUE,
                                     min_jday = NULL,
                                     width = 9, height = 6, dpi = 150) {
  
  vel_long <- read_velocity_long(file_path, missing_value)
  
  if (!is.null(min_jday)) vel_long <- vel_long %>% filter(Julian_day >= min_jday)
  
  tb <- vel_long %>%
    group_by(Segment, Julian_day) %>%
    mutate(max_layer = max(Layer)) %>%
    ungroup() %>%
    mutate(target_layer = ifelse(max_layer >= 2, 2, max_layer)) %>%
    filter(Layer == target_layer | Layer == max_layer) %>%
    distinct(Segment, Julian_day, Layer, .keep_all = TRUE) %>%
    mutate(
      Layer_type = ifelse(Layer == max_layer, "Deepest", "Second from top"),
      Layer_type = factor(Layer_type, levels = c("Second from top", "Deepest"))
    )
  
  if (save_plots && !dir.exists(out_dir)) dir.create(out_dir)
  
  title_tag <- if (!is.null(min_jday)) paste0(" (Day ", min_jday, "+)") else ""
  file_tag  <- if (!is.null(min_jday)) paste0("_from", min_jday) else ""
  
  plots <- list()
  for (seg in levels(tb$Segment)) {
    df_seg <- tb %>% filter(Segment == seg)
    if (nrow(df_seg) == 0) next
    
    p <- ggplot(df_seg, aes(x = Julian_day, y = Velocity, color = Layer_type, group = Layer_type)) +
      geom_hline(yintercept = 0, linewidth = 0.3, color = "grey40") +
      geom_line(linewidth = 0.5) +
      scale_color_manual(values = c("Second from top" = "#e6550d", "Deepest" = "#3182bd")) +
      labs(
        title = paste0("Velocity: Second-from-Top vs. Deepest Layer — Segment ", seg, title_tag),
        x = "Julian Day",
        y = "Velocity (m/s)  [+ downstream / - upstream]",
        color = "Layer"
      ) +
      theme_minimal(base_size = 12)
    
    plots[[seg]] <- p
    
    if (save_plots) {
      ggsave(
        filename = file.path(out_dir, paste0("Velocity_TopBottom_Seg_", seg, file_tag, ".png")),
        plot = p, width = width, height = height, dpi = dpi
      )
    }
  }
  
  invisible(plots)
}

## --- Whole-domain flow-direction heatmap ------------------------------
## Depth-averages velocity within each segment at each Julian_day, then
## plots Segment (y, numeric order) vs Julian_day (x), colored on a
## diverging scale (blue = upstream/landward, red = downstream/seaward).
## This is the "which way is water flowing through the whole model"
## view -- one figure, whole domain, whole time series. It only covers
## whatever segments are in your SPR SEG list, so widen that list if
## you want full-domain coverage rather than just BR/OS.
plot_velocity_domain_heatmap <- function(file_path,
                                         out_dir = "velocity_plots",
                                         missing_value = -99.00,
                                         min_jday = NULL,
                                         save_plot = TRUE,
                                         filename = "velocity_domain_heatmap.png",
                                         width = 12, height = 8, dpi = 150) {
  
  vel_long <- read_velocity_long(file_path, missing_value)
  
  if (!is.null(min_jday)) vel_long <- vel_long %>% filter(Julian_day >= min_jday)
  
  domain_avg <- vel_long %>%
    mutate(Segment_num = as.numeric(as.character(Segment))) %>%
    group_by(Segment_num, Julian_day) %>%
    summarise(Velocity = mean(Velocity, na.rm = TRUE), .groups = "drop")
  
  max_abs <- max(abs(domain_avg$Velocity), na.rm = TRUE)
  
  if (save_plot && !dir.exists(out_dir)) dir.create(out_dir)
  
  p <- ggplot(domain_avg, aes(x = Julian_day, y = Segment_num, fill = Velocity)) +
    geom_tile() +
    scale_fill_gradient2(
      low = "#2166ac", mid = "white", high = "#b2182b", midpoint = 0,
      limits = c(-max_abs, max_abs),
      name = "Velocity (m/s)\n+ downstream\n- upstream"
    ) +
    labs(
      title = "Depth-Averaged Flow Direction Across the Model Domain",
      x = "Julian Day",
      y = "Segment"
    ) +
    theme_minimal(base_size = 12)
  
  if (save_plot) {
    ggsave(filename = file.path(out_dir, filename), plot = p, width = width, height = height, dpi = dpi)
  }
  
  p
}

## --- plot: all model vertical layers of temperature at one segment --
## Same idea as plot_br_all_layers(), but for temperature.
plot_br_temp_all_layers <- function(model_file,
                                    segment = "252",
                                    out_dir = "BR_comparison",
                                    missing_value = -99.00,
                                    spinup_end_jday = 31.0,
                                    save_plot = TRUE,
                                    width = 10, height = 6, dpi = 150) {
  
  if (save_plot && !dir.exists(out_dir)) dir.create(out_dir)
  
  temp_long <- read_temp_long(model_file, missing_value)
  
  segment <- as.character(segment)
  if (!(segment %in% levels(temp_long$Segment))) {
    stop("Segment '", segment, "' not found. Available segments: ",
         paste(levels(temp_long$Segment), collapse = ", "))
  }
  
  df_seg <- temp_long %>%
    filter(Segment == segment, Julian_day > spinup_end_jday) %>%
    mutate(Layer = factor(Layer))
  
  p <- ggplot(df_seg, aes(x = Julian_day, y = Temperature, color = Layer, group = Layer)) +
    geom_line(linewidth = 0.4) +
    labs(
      title = paste0("Model Temperature by Vertical Layer — Segment ", segment),
      x = "Julian Day",
      y = "Temperature (°C)",
      color = "Layer\n(1 = surface)"
    ) +
    theme_minimal(base_size = 12) +
    theme(legend.key.height = unit(0.3, "cm")) +
    guides(color = guide_legend(ncol = 1))
  
  if (save_plot) {
    ggsave(filename = file.path(out_dir, paste0("BR_temp_all_layers_seg", segment, ".png")),
           plot = p, width = width, height = height, dpi = dpi)
  }
  
  p
}

## --- plot: one line per vertical layer, all layers ---------------
## min_jday: see plot_temp_by_layer() -- same optional cutoff behavior.
plot_tds_by_layer <- function(file_path,
                              out_dir = "tds_plots",
                              missing_value = -99.00,
                              save_plots = TRUE,
                              min_jday = NULL,
                              width = 9, height = 6, dpi = 150) {
  
  tds_long <- read_tds_long(file_path, missing_value) %>%
    mutate(Layer = factor(Layer))
  
  if (!is.null(min_jday)) tds_long <- tds_long %>% filter(Julian_day >= min_jday)
  
  if (save_plots && !dir.exists(out_dir)) dir.create(out_dir)
  
  title_tag <- if (!is.null(min_jday)) paste0(" (Day ", min_jday, "+)") else ""
  file_tag  <- if (!is.null(min_jday)) paste0("_from", min_jday) else ""
  
  plots <- list()
  for (seg in levels(tds_long$Segment)) {
    df_seg <- tds_long %>% filter(Segment == seg)
    if (nrow(df_seg) == 0) next
    
    p <- ggplot(df_seg, aes(x = Julian_day, y = TDS, color = Layer, group = Layer)) +
      geom_line(linewidth = 0.4) +
      labs(
        title = paste0("TDS by Vertical Layer — Segment ", seg, title_tag),
        x = "Julian Day",
        y = "TDS (g/m^3)",
        color = "Layer\n(1 = surface)"
      ) +
      theme_minimal(base_size = 12) +
      theme(legend.key.height = unit(0.3, "cm")) +
      guides(color = guide_legend(ncol = 1))
    
    plots[[seg]] <- p
    
    if (save_plots) {
      ggsave(
        filename = file.path(out_dir, paste0("TDS_Seg_", seg, file_tag, ".png")),
        plot = p, width = width, height = height, dpi = dpi
      )
    }
  }
  
  invisible(plots)
}

## --- plot: just the second-from-top layer and the deepest layer --
## For each segment and timestep, "deepest" is that segment's own
## deepest layer with a valid (non -99) reading at that timestep --
## not the global max layer across all segments, since shallower
## segments don't reach as deep and would otherwise show gaps.
## "Second from top" is Layer 2 (falls back to Layer 1 on timesteps
## where a segment only has a single valid layer reported).
## min_jday: see plot_temp_by_layer() -- same optional cutoff behavior.
plot_tds_top_bottom <- function(file_path,
                                out_dir = "tds_plots_top_bottom",
                                missing_value = -99.00,
                                save_plots = TRUE,
                                min_jday = NULL,
                                width = 9, height = 6, dpi = 150) {
  
  tds_long <- read_tds_long(file_path, missing_value)
  
  if (!is.null(min_jday)) tds_long <- tds_long %>% filter(Julian_day >= min_jday)
  
  tb <- tds_long %>%
    group_by(Segment, Julian_day) %>%
    mutate(max_layer = max(Layer)) %>%
    ungroup() %>%
    mutate(target_layer = ifelse(max_layer >= 2, 2, max_layer)) %>%
    filter(Layer == target_layer | Layer == max_layer) %>%
    distinct(Segment, Julian_day, Layer, .keep_all = TRUE) %>%
    mutate(
      Layer_type = ifelse(Layer == max_layer, "Deepest", "Second from top"),
      Layer_type = factor(Layer_type, levels = c("Second from top", "Deepest"))
    )
  
  if (save_plots && !dir.exists(out_dir)) dir.create(out_dir)
  
  title_tag <- if (!is.null(min_jday)) paste0(" (Day ", min_jday, "+)") else ""
  file_tag  <- if (!is.null(min_jday)) paste0("_from", min_jday) else ""
  
  plots <- list()
  for (seg in levels(tb$Segment)) {
    df_seg <- tb %>% filter(Segment == seg)
    if (nrow(df_seg) == 0) next
    
    p <- ggplot(df_seg, aes(x = Julian_day, y = TDS, color = Layer_type, group = Layer_type)) +
      geom_line(linewidth = 0.5) +
      scale_color_manual(values = c("Second from top" = "#1b9e77", "Deepest" = "#7570b3")) +
      labs(
        title = paste0("TDS: Second-from-Top vs. Deepest Layer — Segment ", seg, title_tag),
        x = "Julian Day",
        y = "TDS (g/m^3)",
        color = "Layer"
      ) +
      theme_minimal(base_size = 12)
    
    plots[[seg]] <- p
    
    if (save_plots) {
      ggsave(
        filename = file.path(out_dir, paste0("TDS_TopBottom_Seg_", seg, file_tag, ".png")),
        plot = p, width = width, height = height, dpi = dpi
      )
    }
  }
  
  invisible(plots)
}

## =================================================================
## BOCA RIO (BR) MODEL-VS-FIELD COMPARISON
##
## Adds: a practical-salinity calculation for the BR top sensor
## (which logs specific conductance + temperature rather than
## salinity directly), a model top/bottom extractor for the BR
## segment, a 4-line comparison plot (BR top / BR bot / model top /
## model bot), and goodness-of-fit statistics (NSE, RMSE, PBIAS, R2,
## and Willmott's index of agreement d) comparing model to field
## salinity.
##
## UNIT NOTE: this model is run in CE-QUAL-W2 estuary/salt-water mode
## with an ocean-boundary TDS of 35, which means the "TDS" constituent
## is being used directly as salinity (ppt/PSU) for the density
## calculations -- NOT literal total dissolved solids in mg/L (real
## seawater TDS in mg/L would be ~35,000, not 35). So model TDS output
## is already on the same ppt/PSU scale as field salinity and needs no
## unit conversion; convert_tds_to_salinity() below is a pass-through
## kept only so the rest of the script has one place to touch if that
## ever changes (e.g. a future run switched back to literal TDS mg/L).
## =================================================================

## --- Practical salinity (PSS-78 / UNESCO 1983, P = 0) -------------
## Converts specific conductance (mS/cm) + temperature (deg C) to
## practical salinity (PSU/ppt). This is the standard algorithm used
## internally by most environmental sondes (YSI, HOBO/Onset, EXO).
## Valid over 2 <= S <= 42 PSU, -2 <= T <= 35 degC; values outside
## that range are still computed but should be treated with caution.
calc_salinity_pss78 <- function(cond_mScm, temp_C) {
  
  C_STD <- 42.914  # mS/cm, conductivity of standard seawater (S=35, T=15 IPTS-68, P=0)
  
  a0 <- 0.0080; a1 <- -0.1692; a2 <- 25.3851; a3 <- 14.0941; a4 <- -7.0261; a5 <- 2.7081
  b0 <- 0.0005; b1 <- -0.0056; b2 <- -0.0066; b3 <- -0.0375; b4 <- 0.0636; b5 <- -0.0144
  c0 <- 0.6766097; c1 <- 2.00564e-2; c2 <- 1.104259e-4; c3 <- -6.9698e-7; c4 <- 1.0031e-9
  k  <- 0.0162
  
  R  <- cond_mScm / C_STD
  rt <- c0 + c1 * temp_C + c2 * temp_C^2 + c3 * temp_C^3 + c4 * temp_C^4
  Rt <- R / rt
  
  sqrtRt <- sqrt(Rt)
  
  S <- a0 + a1 * sqrtRt + a2 * Rt + a3 * Rt^1.5 + a4 * Rt^2 + a5 * Rt^2.5 +
    ((temp_C - 15) / (1 + k * (temp_C - 15))) *
    (b0 + b1 * sqrtRt + b2 * Rt + b3 * Rt^1.5 + b4 * Rt^2 + b5 * Rt^2.5)
  
  S[cond_mScm <= 0 | is.na(cond_mScm) | is.na(temp_C)] <- NA
  S
}

## Converts model TDS (already ppt/PSU-equivalent in this estuary-mode
## run, ocean boundary = 35) to salinity. Currently a pass-through --
## change this in one place if a future run goes back to literal
## TDS in mg/L (in which case you'd want tds_mgL / 1000).
convert_tds_to_salinity <- function(tds_mgL) {
  tds_mgL
}

## --- Simple factor-based salinity conversion --------------------------
## Salinity (ppt) = SpCond (mS/cm) * factor. This is the simplified,
## commonly-used approximation for the higher end of the salinity range
## (near-seawater conductivities) -- much simpler than the full PSS-78
## algorithm above, and specifically requested for BR top given the
## data-quality problems PSS-78 produced there (values >35 PSU).
calc_salinity_simple_factor <- function(spcond_mScm, factor = 0.7) {
  spcond_mScm * factor
}

## --- Field readers -------------------------------------------------
## BR.top file columns (matches BRSDSU_combined_cal.csv): JDAY,
## Temperature, Sp.Cond (raw, microS/cm -- converted to mS/cm here).
## BR.bot file columns (matches BR_TJR_cal.csv / NERRS SWMP export):
## JDAY, SpCond (already mS/cm, NERRS/CDMO standard reporting unit --
## used as-is, no conversion), Sal (salinity already computed by the
## source, used as-is), Temp (field temperature, deg C -- NERRS/CDMO
## naming convention differs from BR top's "Temperature" column).
## Adjust the column-name arguments below if your CSV headers differ.
##
## SALINITY METHOD for BR top: SpCond.ms <- Sp.Cond / 1000, then
## Salinity <- SpCond.ms * 0.7 (simple factor method, see
## calc_salinity_simple_factor() above) -- NOT the PSS-78 algorithm,
## per the data-quality issue found in BRSDSU_combined_cal.csv. The
## PSS-78 result is still computed and kept as Salinity_pss78 for
## reference/comparison, but Salinity (used everywhere downstream) is
## now the simple-factor version.
read_br_top <- function(file_path,
                        jday_col = "JDAY",
                        spcond_col = "Sp.Cond",
                        spcond_unit = c("uS", "mS"),
                        temp_col = "Temperature",
                        salinity_factor = 0.7) {
  spcond_unit <- match.arg(spcond_unit)
  df <- read.csv(file_path, stringsAsFactors = FALSE)
  
  spcond_raw <- as.numeric(df[[spcond_col]])
  spcond_mScm <- if (spcond_unit == "uS") spcond_raw / 1000 else spcond_raw
  
  out <- data.frame(
    Julian_day = as.numeric(df[[jday_col]]),
    SpCond_mScm = spcond_mScm,
    Temperature_C = as.numeric(df[[temp_col]])
  )
  out$Salinity_pss78 <- calc_salinity_pss78(out$SpCond_mScm, out$Temperature_C)
  out$Salinity <- calc_salinity_simple_factor(out$SpCond_mScm, salinity_factor)
  out
}

## temp_col default is "Temp" per the NERRS/CDMO export convention this
## file follows (distinct from BR top's "Temperature" column naming).
## Set temp_col = NA if this file has no temperature column -- Temperature_C
## will be filled with NA and any temperature comparison using this field
## source will simply have no data to compare against.
read_br_bot <- function(file_path,
                        jday_col = "JDAY",
                        sal_col = "Sal",
                        spcond_col = "SpCond",
                        temp_col = "Temp") {
  df <- read.csv(file_path, stringsAsFactors = FALSE)
  
  out <- data.frame(
    Julian_day = as.numeric(df[[jday_col]]),
    SpCond_mScm = as.numeric(df[[spcond_col]]),
    Salinity = as.numeric(df[[sal_col]])
  )
  out$Temperature_C <- if (!is.na(temp_col) && temp_col %in% names(df)) as.numeric(df[[temp_col]]) else NA_real_
  out
}

## --- Field reader: Oneonta Slough --------------------------------------
## Same column expectations as read_br_bot() (NERRS/CDMO-style export:
## JDAY, SpCond in mS/cm, Sal already computed, Temp in deg C). Oneonta
## is treated as a single-sensor station (no top/bottom split), the same
## way BR bottom is a single series -- it's compared against BOTH model
## top and model bottom at segment 325, same pattern as compare_br().
read_oneonta <- function(file_path,
                         jday_col = "JDAY",
                         sal_col = "Sal",
                         spcond_col = "SpCond",
                         temp_col = "Temp") {
  df <- read.csv(file_path, stringsAsFactors = FALSE)
  
  out <- data.frame(
    Julian_day = as.numeric(df[[jday_col]]),
    SpCond_mScm = as.numeric(df[[spcond_col]]),
    Salinity = as.numeric(df[[sal_col]])
  )
  out$Temperature_C <- if (!is.na(temp_col) && temp_col %in% names(df)) as.numeric(df[[temp_col]]) else NA_real_
  out
}

## --- Model top/bottom extractor at a single segment ----------------
## Reuses the same "second from top" / "deepest" logic as
## plot_tds_top_bottom(), but returns a wide data frame for one
## segment with a Salinity_model column, ready to merge with field data.
get_model_top_bottom <- function(file_path, segment, missing_value = -99.00) {
  
  tds_long <- read_tds_long(file_path, missing_value)
  
  segment <- as.character(segment)
  if (!(segment %in% levels(tds_long$Segment))) {
    stop("Segment '", segment, "' not found. Available segments: ",
         paste(levels(tds_long$Segment), collapse = ", "))
  }
  
  tb <- tds_long %>%
    filter(Segment == segment) %>%
    group_by(Julian_day) %>%
    mutate(max_layer = max(Layer)) %>%
    ungroup() %>%
    mutate(target_layer = ifelse(max_layer >= 2, 2, max_layer)) %>%
    filter(Layer == target_layer | Layer == max_layer) %>%
    distinct(Julian_day, Layer, .keep_all = TRUE) %>%
    mutate(Layer_type = ifelse(Layer == max_layer, "Model_bottom", "Model_top")) %>%
    select(Julian_day, Layer_type, TDS) %>%
    mutate(Salinity_model = convert_tds_to_salinity(TDS))
  
  list(
    top    = tb %>% filter(Layer_type == "Model_top")    %>% select(Julian_day, TDS, Salinity_model),
    bottom = tb %>% filter(Layer_type == "Model_bottom") %>% select(Julian_day, TDS, Salinity_model)
  )
}

## --- Model top/bottom extractor at a single segment (TEMPERATURE) ------
## Same "second from top" / "deepest" logic as get_model_top_bottom(),
## applied to the Temperature constituent block instead of TDS. No unit
## conversion needed -- model temperature is already in deg C, same as
## the field sensors.
get_model_top_bottom_temp <- function(file_path, segment, missing_value = -99.00) {
  
  temp_long <- read_temp_long(file_path, missing_value)
  
  segment <- as.character(segment)
  if (!(segment %in% levels(temp_long$Segment))) {
    stop("Segment '", segment, "' not found. Available segments: ",
         paste(levels(temp_long$Segment), collapse = ", "))
  }
  
  tb <- temp_long %>%
    filter(Segment == segment) %>%
    group_by(Julian_day) %>%
    mutate(max_layer = max(Layer)) %>%
    ungroup() %>%
    mutate(target_layer = ifelse(max_layer >= 2, 2, max_layer)) %>%
    filter(Layer == target_layer | Layer == max_layer) %>%
    distinct(Julian_day, Layer, .keep_all = TRUE) %>%
    mutate(Layer_type = ifelse(Layer == max_layer, "Model_bottom", "Model_top")) %>%
    select(Julian_day, Layer_type, Temperature)
  
  list(
    top    = tb %>% filter(Layer_type == "Model_top")    %>% select(Julian_day, Temperature),
    bottom = tb %>% filter(Layer_type == "Model_bottom") %>% select(Julian_day, Temperature)
  )
}

## --- Willmott's Index of Agreement (d) --------------------------------
## d = 1 - [ sum((sim - obs)^2) / sum((|sim - mean(obs)| + |obs - mean(obs)|)^2) ]
## Bounded 0 (no agreement) to 1 (perfect agreement) -- unlike NSE, which
## is unbounded below and can go arbitrarily negative for a badly biased
## or low-amplitude model, so d is often reported alongside NSE as a
## second, bounded measure of fit.
willmott_d <- function(obs, sim) {
  keep <- complete.cases(obs, sim)
  obs <- obs[keep]; sim <- sim[keep]
  
  if (length(obs) == 0) return(NA_real_)
  
  obs_mean <- mean(obs)
  numerator   <- sum((sim - obs)^2)
  denominator <- sum((abs(sim - obs_mean) + abs(obs - obs_mean))^2)
  
  if (denominator == 0) return(NA_real_)
  1 - numerator / denominator
}

## --- Goodness-of-fit statistics -------------------------------------
## obs and sim must be the same length and already time-matched.
## Now includes Willmott_d alongside the existing metrics -- this
## automatically adds Willmott's d everywhere gof_stats() is already
## called (BR top/bottom TDS stats, Oneonta TDS stats, and the new
## temperature comparison stats below), with no separate call needed.
gof_stats <- function(obs, sim) {
  keep <- complete.cases(obs, sim)
  obs <- obs[keep]; sim <- sim[keep]
  
  n   <- length(obs)
  rmse  <- sqrt(mean((sim - obs)^2))
  mae   <- mean(abs(sim - obs))
  bias  <- mean(sim - obs)
  pbias <- 100 * sum(sim - obs) / sum(obs)
  nse   <- 1 - sum((obs - sim)^2) / sum((obs - mean(obs))^2)
  r2    <- if (n > 1) cor(obs, sim)^2 else NA
  d     <- willmott_d(obs, sim)
  
  data.frame(n = n, RMSE = rmse, MAE = mae, Bias = bias,
             PBIAS_pct = pbias, NSE = nse, R2 = r2, Willmott_d = d)
}

## --- Match model output to field sample times -----------------------
## Field sondes typically sample at a finer/different interval than
## the model's Julian_day output steps, so the model series is
## linearly interpolated onto the field Julian_day timestamps
## (values outside the model's time range become NA and are dropped
## by gof_stats / plotting as appropriate).
interp_model_to_field <- function(field_jday, model_jday, model_val) {
  approx(x = model_jday, y = model_val, xout = field_jday, rule = 1)$y
}

## --- Combined BR comparison: plot + stats ----------------------------
## model_file : path to the FULL-DEPTH profile .opt file (e.g. spr1.opt)
##   -- NOT a tsr_*.csv segment output. The tsr files carry only one
##   TDS value per JDAY (a single reported layer), so there's no
##   "bottom" series to extract from them. Use the same kind of file
##   your original plot_tds_by_layer()/plot_tds_top_bottom() calls used.
## br_bot_file : field CSV for BR bottom (see read_br_bot).
## segment : model segment number for Boca Rio in THIS run's .opt file,
##   e.g. "252" -- double check it hasn't shifted between run folders.
##
## NOTE: BR top is intentionally excluded here (both from the plot and
## from the stats) pending the SpCond/salinity data-quality issue in
## BRSDSU_combined_cal.csv (values >35 PSU appearing). Once that's
## resolved, BR top can be added back in the same way BR bottom is
## handled below.
compare_br <- function(model_file,
                       br_bot_file,
                       segment = "252",
                       spinup_end_jday = 31.0,
                       out_dir = "BR_comparison",
                       missing_value = -99.00,
                       width = 10, height = 6, dpi = 150,
                       save_plot = TRUE) {
  
  if (save_plot && !dir.exists(out_dir)) dir.create(out_dir)
  
  model_tb <- get_model_top_bottom(model_file, segment, missing_value)
  br_bot   <- read_br_bot(br_bot_file)   %>% filter(Julian_day > spinup_end_jday)
  model_top    <- model_tb$top    %>% filter(Julian_day > spinup_end_jday)
  model_bottom <- model_tb$bottom %>% filter(Julian_day > spinup_end_jday)
  
  ## ---- plot: BR bottom + model top + model bottom (BR top excluded) ----
  plot_df <- bind_rows(
    br_bot       %>% transmute(Julian_day, Salinity = Salinity,       Series = "BR bottom (field)"),
    model_top    %>% transmute(Julian_day, Salinity = Salinity_model, Series = "Model top"),
    model_bottom %>% transmute(Julian_day, Salinity = Salinity_model, Series = "Model bottom")
  ) %>%
    mutate(Series = factor(Series, levels = c("BR bottom (field)", "Model top", "Model bottom")))
  
  p <- ggplot(plot_df, aes(x = Julian_day, y = Salinity, color = Series)) +
    geom_line(linewidth = 0.5) +
    scale_color_manual(values = c(
      "BR bottom (field)"  = "blue",
      "Model top"          = "orange",
      "Model bottom"       = "green"
    )) +
    labs(
      title = paste0("Boca Rio: Model vs. Field (Bottom) Salinity — Segment ", segment),
      x = "Julian Day",
      y = "Salinity (PSU / ppt)",
      color = NULL
    ) +
    theme_minimal(base_size = 12)
  
  if (save_plot) {
    ggsave(filename = file.path(out_dir, paste0("BR_model_vs_field_seg", segment, ".png")),
           plot = p, width = width, height = height, dpi = dpi)
  }
  
  ## ---- stats: BR bottom vs model bottom only (includes Willmott_d) ----
  br_bot <- br_bot %>%
    mutate(Salinity_model = interp_model_to_field(Julian_day, model_bottom$Julian_day, model_bottom$Salinity_model))
  
  stats_bottom <- gof_stats(br_bot$Salinity, br_bot$Salinity_model)
  stats_out <- cbind(Layer = "Bottom", stats_bottom)
  
  if (save_plot) {
    write.csv(stats_out, file.path(out_dir, paste0("BR_gof_stats_seg", segment, ".csv")), row.names = FALSE)
  }
  
  list(plot = p, stats = stats_out, br_bot_matched = br_bot)
}

## --- Full BR comparison: model top/bottom vs. BOTH field top & bottom -
## Same idea as compare_br(), but restores BR top into the plot and
## stats now that it's using the simple SpCond*0.7 salinity method
## (read_br_top()) instead of the PSS-78 method that was producing
## bad values (>35 PSU) for this sensor. Stats now include Willmott's d
## for both top and bottom via the updated gof_stats().
compare_br_full <- function(model_file,
                            br_top_file,
                            br_bot_file,
                            segment = "252",
                            spinup_end_jday = 31.0,
                            out_dir = "BR_comparison",
                            missing_value = -99.00,
                            width = 10, height = 6, dpi = 150,
                            save_plot = TRUE) {
  
  if (save_plot && !dir.exists(out_dir)) dir.create(out_dir)
  
  model_tb <- get_model_top_bottom(model_file, segment, missing_value)
  br_top   <- read_br_top(br_top_file) %>% filter(Julian_day > spinup_end_jday)
  br_bot   <- read_br_bot(br_bot_file) %>% filter(Julian_day > spinup_end_jday)
  model_top    <- model_tb$top    %>% filter(Julian_day > spinup_end_jday)
  model_bottom <- model_tb$bottom %>% filter(Julian_day > spinup_end_jday)
  
  ## ---- plot: BR top + BR bottom + model top + model bottom ----
  plot_df <- bind_rows(
    br_top       %>% transmute(Julian_day, Salinity = Salinity,       Series = "BR top (field)"),
    br_bot       %>% transmute(Julian_day, Salinity = Salinity,       Series = "BR bottom (field)"),
    model_top    %>% transmute(Julian_day, Salinity = Salinity_model, Series = "Model top"),
    model_bottom %>% transmute(Julian_day, Salinity = Salinity_model, Series = "Model bottom")
  ) %>%
    mutate(Series = factor(Series, levels = c("BR top (field)", "BR bottom (field)",
                                              "Model top", "Model bottom")))
  
  p <- ggplot(plot_df, aes(x = Julian_day, y = Salinity, color = Series)) +
    geom_line(linewidth = 0.5) +
    scale_color_manual(values = c(
      "BR top (field)"     = "red",
      "BR bottom (field)"  = "blue",
      "Model top"          = "orange",
      "Model bottom"       = "green"
    )) +
    labs(
      title = paste0("Boca Rio: Model vs. Field Salinity (Top & Bottom) — Segment ", segment),
      x = "Julian Day",
      y = "Salinity (PSU / ppt)",
      color = NULL
    ) +
    theme_minimal(base_size = 12)
  
  if (save_plot) {
    ggsave(filename = file.path(out_dir, paste0("BR_model_vs_field_full_seg", segment, ".png")),
           plot = p, width = width, height = height, dpi = dpi)
  }
  
  ## ---- stats: top vs model top, bottom vs model bottom (Willmott_d included) ----
  br_top <- br_top %>%
    mutate(Salinity_model = interp_model_to_field(Julian_day, model_top$Julian_day, model_top$Salinity_model))
  br_bot <- br_bot %>%
    mutate(Salinity_model = interp_model_to_field(Julian_day, model_bottom$Julian_day, model_bottom$Salinity_model))
  
  stats_top    <- gof_stats(br_top$Salinity, br_top$Salinity_model)
  stats_bottom <- gof_stats(br_bot$Salinity, br_bot$Salinity_model)
  
  stats_out <- bind_rows(
    cbind(Layer = "Top",    stats_top),
    cbind(Layer = "Bottom", stats_bottom)
  )
  
  if (save_plot) {
    write.csv(stats_out, file.path(out_dir, paste0("BR_gof_stats_full_seg", segment, ".csv")), row.names = FALSE)
  }
  
  list(plot = p, stats = stats_out, br_top_matched = br_top, br_bot_matched = br_bot)
}

## --- Oneonta Slough comparison: model top/bottom vs. Oneonta field ----
## Same pattern as compare_br() (single field series compared against
## BOTH model top and model bottom), applied to segment 325 / Oneonta.
## Stats now include Willmott's d via the updated gof_stats().
compare_os <- function(model_file,
                       oneonta_file,
                       segment = "325",
                       spinup_end_jday = 31.0,
                       out_dir = "OS_comparison",
                       missing_value = -99.00,
                       width = 10, height = 6, dpi = 150,
                       save_plot = TRUE) {
  
  if (save_plot && !dir.exists(out_dir)) dir.create(out_dir)
  
  model_tb <- get_model_top_bottom(model_file, segment, missing_value)
  os_obs   <- read_oneonta(oneonta_file) %>% filter(Julian_day > spinup_end_jday)
  model_top    <- model_tb$top    %>% filter(Julian_day > spinup_end_jday)
  model_bottom <- model_tb$bottom %>% filter(Julian_day > spinup_end_jday)
  
  ## ---- plot: Oneonta observed + model top + model bottom ----
  plot_df <- bind_rows(
    os_obs       %>% transmute(Julian_day, Salinity = Salinity,       Series = "Oneonta (field)"),
    model_top    %>% transmute(Julian_day, Salinity = Salinity_model, Series = "Model top"),
    model_bottom %>% transmute(Julian_day, Salinity = Salinity_model, Series = "Model bottom")
  ) %>%
    mutate(Series = factor(Series, levels = c("Oneonta (field)", "Model top", "Model bottom")))
  
  p <- ggplot(plot_df, aes(x = Julian_day, y = Salinity, color = Series)) +
    geom_line(linewidth = 0.5) +
    scale_color_manual(values = c(
      "Oneonta (field)" = "blue",
      "Model top"       = "orange",
      "Model bottom"    = "green"
    )) +
    labs(
      title = paste0("Oneonta Slough: Model vs. Field Salinity — Segment ", segment),
      x = "Julian Day",
      y = "Salinity (PSU / ppt)",
      color = NULL
    ) +
    theme_minimal(base_size = 12)
  
  if (save_plot) {
    ggsave(filename = file.path(out_dir, paste0("OS_model_vs_field_seg", segment, ".png")),
           plot = p, width = width, height = height, dpi = dpi)
  }
  
  ## ---- stats: Oneonta observed vs model bottom only (Willmott_d included) ----
  ## (matches how compare_br() computes stats against the single
  ## field series -- change to model_top below if bottom isn't the
  ## right layer for how your Oneonta sensor is deployed)
  os_obs <- os_obs %>%
    mutate(Salinity_model = interp_model_to_field(Julian_day, model_bottom$Julian_day, model_bottom$Salinity_model))
  
  stats_bottom <- gof_stats(os_obs$Salinity, os_obs$Salinity_model)
  stats_out <- cbind(Layer = "Bottom", stats_bottom)
  
  if (save_plot) {
    write.csv(stats_out, file.path(out_dir, paste0("OS_gof_stats_seg", segment, ".csv")), row.names = FALSE)
  }
  
  list(plot = p, stats = stats_out, os_matched = os_obs)
}

## =================================================================
## OBSERVED VS. MODELED TEMPERATURE COMPARISON
##
## Mirrors the TDS/salinity comparison functions above (compare_br_full,
## compare_os), but for temperature. No unit conversion is needed --
## field sensors and model output are both in deg C. Stats include
## Willmott's d automatically via the shared gof_stats() function.
## =================================================================

## --- Full BR temperature comparison: model top/bottom vs. field top/bottom
## BR top field temperature comes from read_br_top()'s Temperature_C
## column (BRSDSU_combined_cal.csv, "Temperature" column). BR bottom
## field temperature comes from read_br_bot()'s Temperature_C column
## (BR_TJR_cal.csv, "Temp" column) -- if that file has no temperature
## column, Temperature_C will be all NA and the bottom stats/plot series
## will simply be empty; check read_br_bot(temp_col = ...) if your file
## uses a different column name.
compare_br_temp_full <- function(model_file,
                                 br_top_file,
                                 br_bot_file,
                                 segment = "252",
                                 spinup_end_jday = 31.0,
                                 out_dir = "BR_comparison",
                                 missing_value = -99.00,
                                 width = 10, height = 6, dpi = 150,
                                 save_plot = TRUE) {
  
  if (save_plot && !dir.exists(out_dir)) dir.create(out_dir)
  
  model_tb <- get_model_top_bottom_temp(model_file, segment, missing_value)
  br_top   <- read_br_top(br_top_file) %>% filter(Julian_day > spinup_end_jday)
  br_bot   <- read_br_bot(br_bot_file) %>% filter(Julian_day > spinup_end_jday)
  model_top    <- model_tb$top    %>% filter(Julian_day > spinup_end_jday)
  model_bottom <- model_tb$bottom %>% filter(Julian_day > spinup_end_jday)
  
  ## ---- plot: BR top + BR bottom + model top + model bottom ----
  plot_df <- bind_rows(
    br_top       %>% transmute(Julian_day, Temperature = Temperature_C, Series = "BR top (field)"),
    br_bot       %>% transmute(Julian_day, Temperature = Temperature_C, Series = "BR bottom (field)"),
    model_top    %>% transmute(Julian_day, Temperature = Temperature,   Series = "Model top"),
    model_bottom %>% transmute(Julian_day, Temperature = Temperature,   Series = "Model bottom")
  ) %>%
    mutate(Series = factor(Series, levels = c("BR top (field)", "BR bottom (field)",
                                              "Model top", "Model bottom")))
  
  p <- ggplot(plot_df, aes(x = Julian_day, y = Temperature, color = Series)) +
    geom_line(linewidth = 0.5) +
    scale_color_manual(values = c(
      "BR top (field)"     = "red",
      "BR bottom (field)"  = "blue",
      "Model top"          = "orange",
      "Model bottom"       = "green"
    )) +
    labs(
      title = paste0("Boca Rio: Model vs. Field Temperature (Top & Bottom) — Segment ", segment),
      x = "Julian Day",
      y = "Temperature (°C)",
      color = NULL
    ) +
    theme_minimal(base_size = 12)
  
  if (save_plot) {
    ggsave(filename = file.path(out_dir, paste0("BR_temp_model_vs_field_full_seg", segment, ".png")),
           plot = p, width = width, height = height, dpi = dpi)
  }
  
  ## ---- stats: top vs model top, bottom vs model bottom (Willmott_d included) ----
  br_top <- br_top %>%
    mutate(Temperature_model = interp_model_to_field(Julian_day, model_top$Julian_day, model_top$Temperature))
  br_bot <- br_bot %>%
    mutate(Temperature_model = interp_model_to_field(Julian_day, model_bottom$Julian_day, model_bottom$Temperature))
  
  stats_top    <- gof_stats(br_top$Temperature_C, br_top$Temperature_model)
  stats_bottom <- gof_stats(br_bot$Temperature_C, br_bot$Temperature_model)
  
  stats_out <- bind_rows(
    cbind(Layer = "Top",    stats_top),
    cbind(Layer = "Bottom", stats_bottom)
  )
  
  if (save_plot) {
    write.csv(stats_out, file.path(out_dir, paste0("BR_temp_gof_stats_full_seg", segment, ".csv")), row.names = FALSE)
  }
  
  list(plot = p, stats = stats_out, br_top_matched = br_top, br_bot_matched = br_bot)
}

## --- Oneonta Slough temperature comparison: model top/bottom vs. field -
## Same pattern as compare_os() -- single field series compared against
## both model top and model bottom, stats computed against model bottom
## (change to model_top below if that's the right layer for your sensor
## deployment depth).
compare_os_temp <- function(model_file,
                            oneonta_file,
                            segment = "325",
                            spinup_end_jday = 31.0,
                            out_dir = "OS_comparison",
                            missing_value = -99.00,
                            width = 10, height = 6, dpi = 150,
                            save_plot = TRUE) {
  
  if (save_plot && !dir.exists(out_dir)) dir.create(out_dir)
  
  model_tb <- get_model_top_bottom_temp(model_file, segment, missing_value)
  os_obs   <- read_oneonta(oneonta_file) %>% filter(Julian_day > spinup_end_jday)
  model_top    <- model_tb$top    %>% filter(Julian_day > spinup_end_jday)
  model_bottom <- model_tb$bottom %>% filter(Julian_day > spinup_end_jday)
  
  ## ---- plot: Oneonta observed + model top + model bottom ----
  plot_df <- bind_rows(
    os_obs       %>% transmute(Julian_day, Temperature = Temperature_C, Series = "Oneonta (field)"),
    model_top    %>% transmute(Julian_day, Temperature = Temperature,   Series = "Model top"),
    model_bottom %>% transmute(Julian_day, Temperature = Temperature,   Series = "Model bottom")
  ) %>%
    mutate(Series = factor(Series, levels = c("Oneonta (field)", "Model top", "Model bottom")))
  
  p <- ggplot(plot_df, aes(x = Julian_day, y = Temperature, color = Series)) +
    geom_line(linewidth = 0.5) +
    scale_color_manual(values = c(
      "Oneonta (field)" = "blue",
      "Model top"       = "orange",
      "Model bottom"    = "green"
    )) +
    labs(
      title = paste0("Oneonta Slough: Model vs. Field Temperature — Segment ", segment),
      x = "Julian Day",
      y = "Temperature (°C)",
      color = NULL
    ) +
    theme_minimal(base_size = 12)
  
  if (save_plot) {
    ggsave(filename = file.path(out_dir, paste0("OS_temp_model_vs_field_seg", segment, ".png")),
           plot = p, width = width, height = height, dpi = dpi)
  }
  
  ## ---- stats: Oneonta observed vs model bottom only (Willmott_d included) ----
  os_obs <- os_obs %>%
    mutate(Temperature_model = interp_model_to_field(Julian_day, model_bottom$Julian_day, model_bottom$Temperature))
  
  stats_bottom <- gof_stats(os_obs$Temperature_C, os_obs$Temperature_model)
  stats_out <- cbind(Layer = "Bottom", stats_bottom)
  
  if (save_plot) {
    write.csv(stats_out, file.path(out_dir, paste0("OS_temp_gof_stats_seg", segment, ".csv")), row.names = FALSE)
  }
  
  list(plot = p, stats = stats_out, os_matched = os_obs)
}

## =================================================================
## DUAL-AXIS FIELD VS. MODEL COMPARISON (BR)
##
## Field top+bottom on the LEFT axis, model top+bottom on the RIGHT axis.
## Built with base R graphics (par(new=TRUE) + axis(4)) rather than
## ggplot2's scale_y_continuous(sec.axis = sec_axis(...)), because
## ggplot2 requires the secondary axis to be a fixed linear transform of
## the primary axis -- not appropriate here, since field and model are
## independent series that may have very different amplitude/offset and
## aren't related by a single known linear formula. Base R's twin-axis
## approach draws each series against its own, fully independent scale.
## =================================================================

## --- Generic dual-axis plot builder ---------------------------------
## out_file = NULL draws to the current device (e.g. for interactive use);
## otherwise saves a PNG of the given size/resolution.
plot_dual_axis_comparison <- function(field_top_jday, field_top_val,
                                      field_bot_jday, field_bot_val,
                                      model_top_jday, model_top_val,
                                      model_bot_jday, model_bot_val,
                                      title, ylab_left, ylab_right,
                                      out_file = NULL,
                                      width = 10, height = 6, dpi = 150) {
  
  x_range <- range(c(field_top_jday, field_bot_jday, model_top_jday, model_bot_jday), na.rm = TRUE)
  
  draw <- function() {
    layout(matrix(c(1, 2), nrow = 2), heights = c(5, 1))
    on.exit(layout(1))
    
    ## ---- panel 1: the twin-axis plot itself (no legend drawn here) ----
    op <- par(mar = c(5, 4, 4, 4.5))
    
    ## ---- left axis: field top + field bottom ----
    plot(field_top_jday, field_top_val, type = "l", col = "red",
         xlim = x_range, ylim = range(c(field_top_val, field_bot_val), na.rm = TRUE),
         xlab = "Julian Day", ylab = ylab_left, main = title)
    lines(field_bot_jday, field_bot_val, col = "blue")
    
    ## ---- right axis: model top + model bottom, independent scale ----
    par(new = TRUE)
    plot(model_top_jday, model_top_val, type = "l", col = "orange",
         xlim = x_range, ylim = range(c(model_top_val, model_bot_val), na.rm = TRUE),
         axes = FALSE, xlab = "", ylab = "")
    lines(model_bot_jday, model_bot_val, col = "darkgreen")
    axis(side = 4)
    mtext(ylab_right, side = 4, line = 3)
    par(op)
    
    ## ---- panel 2: legend only, in its own strip below the plot ----
    ## (fully outside the data panel above, rather than drawn on top of it)
    par(mar = c(0, 0, 0, 0))
    plot.new()
    legend("center", bty = "n", horiz = TRUE, cex = 0.9,
           legend = c("Field top", "Field bottom", "Model top", "Model bottom"),
           col = c("red", "blue", "orange", "darkgreen"), lty = 1)
  }
  
  if (!is.null(out_file)) {
    png(filename = out_file, width = width, height = height, units = "in", res = dpi)
    draw()
    dev.off()
  } else {
    draw()
  }
  
  invisible(NULL)
}

## --- BR salinity, dual axis: field (left) vs model (right), segment 252 -
compare_br_dualaxis_salinity <- function(model_file,
                                         br_top_file,
                                         br_bot_file,
                                         segment = "252",
                                         spinup_end_jday = 31.0,
                                         out_dir = "BR_comparison",
                                         missing_value = -99.00,
                                         width = 10, height = 6, dpi = 150,
                                         save_plot = TRUE) {
  
  if (save_plot && !dir.exists(out_dir)) dir.create(out_dir)
  
  model_tb <- get_model_top_bottom(model_file, segment, missing_value)
  br_top   <- read_br_top(br_top_file) %>% filter(Julian_day > spinup_end_jday)
  br_bot   <- read_br_bot(br_bot_file) %>% filter(Julian_day > spinup_end_jday)
  model_top    <- model_tb$top    %>% filter(Julian_day > spinup_end_jday)
  model_bottom <- model_tb$bottom %>% filter(Julian_day > spinup_end_jday)
  
  out_file <- if (save_plot) file.path(out_dir, paste0("BR_salinity_dualaxis_seg", segment, ".png")) else NULL
  
  plot_dual_axis_comparison(
    field_top_jday = br_top$Julian_day, field_top_val = br_top$Salinity,
    field_bot_jday = br_bot$Julian_day, field_bot_val = br_bot$Salinity,
    model_top_jday = model_top$Julian_day, model_top_val = model_top$Salinity_model,
    model_bot_jday = model_bottom$Julian_day, model_bot_val = model_bottom$Salinity_model,
    title = paste0("Boca Rio: Field vs. Model Salinity (Dual Axis) — Segment ", segment),
    ylab_left = "Field Salinity (PSU / ppt)",
    ylab_right = "Model Salinity (PSU / ppt)",
    out_file = out_file, width = width, height = height, dpi = dpi
  )
}

## --- BR temperature, dual axis: field (left) vs model (right), segment 252
compare_br_dualaxis_temp <- function(model_file,
                                     br_top_file,
                                     br_bot_file,
                                     segment = "252",
                                     spinup_end_jday = 31.0,
                                     out_dir = "BR_comparison",
                                     missing_value = -99.00,
                                     width = 10, height = 6, dpi = 150,
                                     save_plot = TRUE) {
  
  if (save_plot && !dir.exists(out_dir)) dir.create(out_dir)
  
  model_tb <- get_model_top_bottom_temp(model_file, segment, missing_value)
  br_top   <- read_br_top(br_top_file) %>% filter(Julian_day > spinup_end_jday)
  br_bot   <- read_br_bot(br_bot_file) %>% filter(Julian_day > spinup_end_jday)
  model_top    <- model_tb$top    %>% filter(Julian_day > spinup_end_jday)
  model_bottom <- model_tb$bottom %>% filter(Julian_day > spinup_end_jday)
  
  out_file <- if (save_plot) file.path(out_dir, paste0("BR_temp_dualaxis_seg", segment, ".png")) else NULL
  
  plot_dual_axis_comparison(
    field_top_jday = br_top$Julian_day, field_top_val = br_top$Temperature_C,
    field_bot_jday = br_bot$Julian_day, field_bot_val = br_bot$Temperature_C,
    model_top_jday = model_top$Julian_day, model_top_val = model_top$Temperature,
    model_bot_jday = model_bottom$Julian_day, model_bot_val = model_bottom$Temperature,
    title = paste0("Boca Rio: Field vs. Model Temperature (Dual Axis) — Segment ", segment),
    ylab_left = "Field Temperature (°C)",
    ylab_right = "Model Temperature (°C)",
    out_file = out_file, width = width, height = height, dpi = dpi
  )
}

## --- All-layers plot for a single segment (e.g. BR = segment 252) -----
## Same idea as plot_tds_by_layer(), but scoped to one segment and
## returns/saves a single plot instead of looping + saving per segment.
plot_br_all_layers <- function(model_file,
                               segment = "252",
                               out_dir = "BR_comparison",
                               missing_value = -99.00,
                               spinup_end_jday = 31.0,
                               save_plot = TRUE,
                               width = 10, height = 6, dpi = 150) {
  
  if (save_plot && !dir.exists(out_dir)) dir.create(out_dir)
  
  tds_long <- read_tds_long(model_file, missing_value)
  
  segment <- as.character(segment)
  if (!(segment %in% levels(tds_long$Segment))) {
    stop("Segment '", segment, "' not found. Available segments: ",
         paste(levels(tds_long$Segment), collapse = ", "))
  }
  
  df_seg <- tds_long %>%
    filter(Segment == segment, Julian_day > spinup_end_jday) %>%
    mutate(Layer = factor(Layer))
  
  p <- ggplot(df_seg, aes(x = Julian_day, y = TDS, color = Layer, group = Layer)) +
    geom_line(linewidth = 0.4) +
    labs(
      title = paste0("Model TDS/Salinity by Vertical Layer — Segment ", segment),
      x = "Julian Day",
      y = "TDS / Salinity (ppt)",
      color = "Layer\n(1 = surface)"
    ) +
    theme_minimal(base_size = 12) +
    theme(legend.key.height = unit(0.3, "cm")) +
    guides(color = guide_legend(ncol = 1))
  
  if (save_plot) {
    ggsave(filename = file.path(out_dir, paste0("BR_all_layers_seg", segment, ".png")),
           plot = p, width = width, height = height, dpi = dpi)
  }
  
  p
}

## --- Shading helper: contiguous TRUE runs of a condition, as x-ranges -
## Used to build background shading rectangles for periods matching a
## threshold condition. Returns a data frame of xmin/xmax pairs (one row
## per contiguous run), or a zero-row data frame if condition is never
## TRUE (no shading drawn).
make_shade_intervals <- function(jday, condition) {
  condition[is.na(condition)] <- FALSE
  ord <- order(jday)
  jday <- jday[ord]; condition <- condition[ord]
  
  r <- rle(condition)
  ends <- cumsum(r$lengths)
  starts <- ends - r$lengths + 1
  keep <- r$values
  
  data.frame(xmin = jday[starts[keep]], xmax = jday[ends[keep]])
}

## --- Plot: BR top & bottom Specific Conductivity (mS/cm), with -------
## shading wherever EITHER series exceeds spcond_threshold (default 10
## mS/cm). Shading is drawn as two overlapping rect layers (one per
## series' own timestamps), so any interval where either sensor reads
## above the threshold gets shaded, even though top/bottom aren't on
## the same sampling grid.
plot_br_spcond <- function(br_top_file,
                           br_bot_file,
                           spcond_threshold = 10,
                           spinup_end_jday = 31.0,
                           shade_color = "khaki",
                           shade_alpha = 0.3,
                           out_dir = "BR_comparison",
                           save_plot = TRUE,
                           width = 10, height = 6, dpi = 150) {
  
  if (save_plot && !dir.exists(out_dir)) dir.create(out_dir)
  
  br_top <- read_br_top(br_top_file) %>% filter(Julian_day > spinup_end_jday)
  br_bot <- read_br_bot(br_bot_file) %>% filter(Julian_day > spinup_end_jday)
  
  line_df <- bind_rows(
    br_top %>% transmute(Julian_day, SpCond_mScm, Series = "BR top"),
    br_bot %>% transmute(Julian_day, SpCond_mScm, Series = "BR bottom")
  ) %>%
    mutate(Series = factor(Series, levels = c("BR top", "BR bottom")))
  
  shade_top <- make_shade_intervals(br_top$Julian_day, br_top$SpCond_mScm > spcond_threshold)
  shade_bot <- make_shade_intervals(br_bot$Julian_day, br_bot$SpCond_mScm > spcond_threshold)
  shade_df  <- bind_rows(shade_top, shade_bot)
  
  p <- ggplot()
  if (nrow(shade_df) > 0) {
    p <- p + geom_rect(data = shade_df, aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
                       fill = shade_color, alpha = shade_alpha, inherit.aes = FALSE)
  }
  p <- p +
    geom_line(data = line_df, aes(x = Julian_day, y = SpCond_mScm, color = Series), linewidth = 0.5) +
    scale_color_manual(values = c("BR top" = "red", "BR bottom" = "blue")) +
    labs(
      title = paste0("BR Specific Conductivity — shaded where either series > ", spcond_threshold, " mS/cm"),
      x = "Julian Day",
      y = "Specific Conductivity (mS/cm)",
      color = NULL
    ) +
    theme_minimal(base_size = 12)
  
  if (save_plot) {
    ggsave(filename = file.path(out_dir, paste0("BR_SpCond_threshold", spcond_threshold, ".png")),
           plot = p, width = width, height = height, dpi = dpi)
  }
  
  p
}

## --- Plot: BR top & bottom Specific Conductivity, shaded wherever ----
## the top-minus-bottom difference exceeds diff_threshold (default 15
## mS/cm) -- i.e. periods of strong vertical stratification at BR.
## Bottom is linearly interpolated onto top's timestamps to compute the
## difference, since the two sensors aren't on the same sampling grid.
plot_br_spcond_diff <- function(br_top_file,
                                br_bot_file,
                                diff_threshold = 15,
                                spinup_end_jday = 31.0,
                                shade_color = "tomato",
                                shade_alpha = 0.3,
                                out_dir = "BR_comparison",
                                save_plot = TRUE,
                                width = 10, height = 6, dpi = 150) {
  
  if (save_plot && !dir.exists(out_dir)) dir.create(out_dir)
  
  br_top <- read_br_top(br_top_file) %>% filter(Julian_day > spinup_end_jday)
  br_bot <- read_br_bot(br_bot_file) %>% filter(Julian_day > spinup_end_jday)
  
  bot_interp <- interp_model_to_field(br_top$Julian_day, br_bot$Julian_day, br_bot$SpCond_mScm)
  spcond_diff <- br_top$SpCond_mScm - bot_interp
  
  shade_df <- make_shade_intervals(br_top$Julian_day, abs(spcond_diff) > diff_threshold)
  
  line_df <- bind_rows(
    br_top %>% transmute(Julian_day, SpCond_mScm, Series = "BR top"),
    br_bot %>% transmute(Julian_day, SpCond_mScm, Series = "BR bottom")
  ) %>%
    mutate(Series = factor(Series, levels = c("BR top", "BR bottom")))
  
  p <- ggplot()
  if (nrow(shade_df) > 0) {
    p <- p + geom_rect(data = shade_df, aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
                       fill = shade_color, alpha = shade_alpha, inherit.aes = FALSE)
  }
  p <- p +
    geom_line(data = line_df, aes(x = Julian_day, y = SpCond_mScm, color = Series), linewidth = 0.5) +
    scale_color_manual(values = c("BR top" = "red", "BR bottom" = "blue")) +
    labs(
      title = paste0("BR Specific Conductivity — shaded where |top - bottom| > ", diff_threshold, " mS/cm"),
      x = "Julian Day",
      y = "Specific Conductivity (mS/cm)",
      color = NULL
    ) +
    theme_minimal(base_size = 12)
  
  if (save_plot) {
    ggsave(filename = file.path(out_dir, paste0("BR_SpCond_diff", diff_threshold, ".png")),
           plot = p, width = width, height = height, dpi = dpi)
  }
  
  p
}

## --- Run when sourced directly ------------------------------------
## All calls below use the variables set in the FILE PATHS / CONFIG
## section at the top of the file -- edit the paths up there, not here.
if (sys.nframe() == 0) {
  plot_tds_by_layer(model_file, out_dir = out_dir)
  plot_tds_top_bottom(model_file, out_dir = out_dir)
  
  #Temperature -- same two views, mirroring the TDS calls above:
  plot_temp_by_layer(model_file, out_dir = temp_out_dir)
  plot_temp_top_bottom(model_file, out_dir = temp_out_dir)
  
  ## Velocity / flow direction -- by layer, top/bottom, and whole-domain:
  plot_velocity_by_layer(model_file, out_dir = vel_out_dir)
  plot_velocity_top_bottom(model_file, out_dir = vel_out_dir)
  plot_velocity_domain_heatmap(model_file, out_dir = vel_out_dir, min_jday = spinup_end_jday)
  
  ## --- Same six plots above, cut to JDAY 31-end (spin-up period removed) ---
  ## Saved to a separate "_from31" subfolder so they don't overwrite the
  ## full-range versions above.
  plot_tds_by_layer(model_file, out_dir = file.path(out_dir, "from31"), min_jday = 31)
  plot_tds_top_bottom(model_file, out_dir = file.path(out_dir, "from31"), min_jday = 31)
  plot_temp_by_layer(model_file, out_dir = file.path(temp_out_dir, "from31"), min_jday = 31)
  plot_temp_top_bottom(model_file, out_dir = file.path(temp_out_dir, "from31"), min_jday = 31)
  plot_velocity_by_layer(model_file, out_dir = file.path(vel_out_dir, "from31"), min_jday = 31)
  plot_velocity_top_bottom(model_file, out_dir = file.path(vel_out_dir, "from31"), min_jday = 31)
  
  ## Model-vs-field Boca Rio comparison (bottom vs model bottom only):
  br_result <- compare_br(
    model_file  = model_file,
    br_bot_file = br_bot_file,
    segment     = br_segment,
    spinup_end_jday = spinup_end_jday,
    out_dir     = br_out_dir)
  
  print(br_result$plot)
  print(br_result$stats)
  
  ## Model-vs-field Boca Rio comparison, full (top + bottom vs both
  ## field series -- BR top now uses the SpCond*0.7 salinity method,
  ## stats include Willmott's d):
  br_full_result <- compare_br_full(
    model_file  = model_file,
    br_top_file = br_top_file,
    br_bot_file = br_bot_file,
    segment     = br_segment,
    spinup_end_jday = spinup_end_jday,
    out_dir     = br_out_dir)
  
  print(br_full_result$plot)
  print(br_full_result$stats)
  
  ## Model-vs-field Oneonta Slough comparison (segment 325, model
  ## top & bottom vs single Oneonta field series, stats include
  ## Willmott's d):
  os_result <- compare_os(
    model_file   = model_file,
    oneonta_file = oneonta_file,
    segment      = os_segment,
    spinup_end_jday = spinup_end_jday,
    out_dir      = os_out_dir)
  
  print(os_result$plot)
  print(os_result$stats)
  
  ## Model-vs-field Boca Rio TEMPERATURE comparison (top + bottom vs
  ## both field series):
  br_temp_result <- compare_br_temp_full(
    model_file  = model_file,
    br_top_file = br_top_file,
    br_bot_file = br_bot_file,
    segment     = br_segment,
    spinup_end_jday = spinup_end_jday,
    out_dir     = br_out_dir)
  
  print(br_temp_result$plot)
  print(br_temp_result$stats)
  
  ## Model-vs-field Oneonta Slough TEMPERATURE comparison (segment 325):
  os_temp_result <- compare_os_temp(
    model_file   = model_file,
    oneonta_file = oneonta_file,
    segment      = os_segment,
    spinup_end_jday = spinup_end_jday,
    out_dir      = os_out_dir)
  
  print(os_temp_result$plot)
  print(os_temp_result$stats)
  
  ## Dual-axis BR comparisons: field top+bottom on the left axis, model
  ## top+bottom on the right axis (independent scales) -- salinity and
  ## temperature versions:
  compare_br_dualaxis_salinity(
    model_file  = model_file,
    br_top_file = br_top_file,
    br_bot_file = br_bot_file,
    segment     = br_segment,
    spinup_end_jday = spinup_end_jday,
    out_dir     = br_out_dir)
  
  compare_br_dualaxis_temp(
    model_file  = model_file,
    br_top_file = br_top_file,
    br_bot_file = br_bot_file,
    segment     = br_segment,
    spinup_end_jday = spinup_end_jday,
    out_dir     = br_out_dir)
  
  ## All model vertical layers at BR (TDS/salinity):
  plot_br_all_layers(
    model_file = model_file,
    segment    = br_segment,
    spinup_end_jday = spinup_end_jday,
    out_dir    = br_out_dir)
  
  ## All model vertical layers at BR (temperature):
  plot_br_temp_all_layers(
    model_file = model_file,
    segment    = br_segment,
    spinup_end_jday = spinup_end_jday,
    out_dir    = br_out_dir)
  
  ## BR top/bottom SpCond, shaded where either > 10 mS/cm:
  plot_br_spcond(
    br_top_file = br_top_file,
    br_bot_file = br_bot_file,
    spinup_end_jday = spinup_end_jday,
    out_dir     = br_out_dir )
  
  ## BR top/bottom SpCond, shaded where |top - bottom| > 15 mS/cm (stratification events):
  plot_br_spcond_diff(
    br_top_file = br_top_file,
    br_bot_file = br_bot_file,
    spinup_end_jday = spinup_end_jday,
    out_dir = br_out_dir)
}