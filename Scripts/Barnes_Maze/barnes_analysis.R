# BARNES MAZE PLOTTING =========================================================

# R Heggen, 5th March 2026
# Requires prior analysis in SimBA (Simon Nilsson)

# For 'total' metrics, video must have been trimmed to end when EB fully entered. 
# For other metrics, ROIs must be named sequentially from 1 (EB) to 18.
# For all data, set a minimum likelihood threshold of 0.6 in SimBA.

# See 'test_description_and_files.csv' for more explanation.
rm(list = ls())

# Packages ----
library(here) # Facilitates relative paths
library(tidyverse) # For dataframe logic and plotting
library(ggpattern) # Allows patterns on graph

# INPUTS =======================================================================
# Reference table (key) files ----
key_filepath         <- here('Data', 'Reference_Tables', 'ratID_key.csv')
trainday_key_filepath <- here('Data', 'Reference_Tables', 'barnes_day_key.csv')

# SimBA output files ----
## Detailed ROI data (for strategy and primary latency, errors, or path length)
nose_filepath  <- here('Data', 'Simba_Output', 'Barnes_Maze', 'Detailed_ROI_data_nose.csv')
l_ear_filepath <- here('Data', 'Simba_Output', 'Barnes_Maze', 'Detailed_ROI_data_ear_left.csv')
r_ear_filepath <- here('Data', 'Simba_Output', 'Barnes_Maze', 'Detailed_ROI_data_ear_right.csv')

## Movement logs (for path lengths)
pathlength_filepath <- here('Data', 'Simba_Output', 'Barnes_Maze', 'Movement_log.csv')
pathlength_timebin_filepath <- here('Data', 'Simba_Output', 'Barnes_Maze', 'Time_bins_1.0s_movement_results.csv')

## Summary data (for total latency)
aggregate_ROI_filepath     <- here('Data', 'Simba_Output', 'Barnes_Maze', 'ROI_descriptive_statistics.csv')


# Calculation options ----
# For detailed ROI data, use only a single body part, or combine multiple.
## Options: TRUE  = average for primary, max for score
##          FALSE = only account for nose tracking. Warning: may be inaccurate.
combine_bodyparts <- TRUE

# Loop through all metrics and export both Prism CSV, summary CSV and graph.
export_all <- TRUE


# INSTRUCTIONS =================================================================
# First, run entirety of code.

# Exporting to a Prism-readable format -----------------------------------------
# To export a csv rearranged appropriately for GraphPad Prism, type in console:

# export_prism(metric_input)

    # Where metric_input is the test you are investigating.
    ## Options: "latency_total", "latency_primary", "pathlength_total", 
    ## "pathlength_primary", "strategy", "errors_total", "errors_primary" 

  # Other optional parameters:
    ## average_days (default TRUE)
    ## Takes the average of all training sessions in one day and presents as one
    ## cell. Formula avoids pseudoreplication errors.

    ## save (default TRUE)
    ## Saves a csv file with columns for each rat (arranged into treatment-sex
    ## groups) and rows for each training session or day to outdir.
    
    ## outdir
    ## Directory to store .csv files.

# Graphing ---------------------------------------------------------------------
# To graph or save summary statistics, type the following in the console:

# make_graph(metric_input)
  
    # Where metric_input is the metric used in the final clustered column chart.
    ## Options: "latency_total", "latency_primary", "pathlength_total", 
    ## "pathlength_primary", "strategy", "errors_primary", "errors_total"
      
  # Other optional parameters:
    ## average_days (default TRUE)
    ## Takes the average of all training sessions in one day and presents as one
    ## column. Formula avoids pseudoreplication errors.
    
    ## group_sexes (default TRUE)
    ## Makes separate columns for males and females.
    
    ## save_graph (default FALSE)
    ## Exports editable vector .pdf image file of graph to graph_dir.
    
    ## graph_dir (default "barnes_graph")
    ## Directory to store graph images.
    
    ## save_summary_csv (default FALSE)
    ## Saves the csv file with means for each day x treatment/sex-treatment
    ##  group, including stdev, n, standard error, to summary_csv_dir.
    
    ## summary_csv_dir
    ## Directory to store summary csvs.

# SCRIPT =======================================================================
# addIDs function ----
# Extracts identifiers for rat and training session from video name, and checks
# against key documents to add extra ID columns

addIDs <- function(df){
  # RatID: string after final underscore
  df$RatID <- str_extract(df$VIDEO, "[^_]+$") 
  # Train: number preceding 'Training' unless 'Test'
  df$Train <- ifelse(grepl("Test", df$VIDEO),
                     "Test",
                     str_extract(df$VIDEO, "\\d+(?=Training)"))
  # Sex/Treatment from key
  df$Sex       <- key$Sex[match(df$RatID, key$RatID)]
  df$Treatment <- key$Treatment[match(df$RatID, key$RatID)]
  # Day from Train-Day key
  df$Day <- trainday_key$Day[match(df$Train, trainday_key$Train)]
  df
}

# READ DATA --------------------------------------------------------------------
key         <- read.csv(key_filepath)
trainday_key <- read.csv(trainday_key_filepath)

nose  <- addIDs(read.csv(nose_filepath))
l_ear <- addIDs(read.csv(l_ear_filepath))
r_ear <- addIDs(read.csv(r_ear_filepath))

aggregate_ROI      <- addIDs(read.csv(aggregate_ROI_filepath))
pathlength_total   <- addIDs(read.csv(pathlength_filepath))
pathlength_timebin <- addIDs(read.csv(pathlength_timebin_filepath))


# Prepare a list of relevant bodyparts for ROI-based metrics
if (combine_bodyparts) {
  bodyparts <- list(nose  = nose,
                    l_ear = l_ear,
                    r_ear = r_ear)
} else {
  bodyparts <- list(nose = nose)
}

# SCORE PER RAT PER SESSION ----------------------------------------------------
# TOTAL LATENCY ----
# Assumes videos have been trimmed to end exactly when rodent fully enters EB.
# 'Video length' must be included in SimBA output.
# Subsets SHAPE == 1 so there is only 1 row per video.
latency_total <- subset(aggregate_ROI, 
                       MEASUREMENT == 'VIDEO LENGTH (S)' 
                       & SHAPE == 1)


# PRIMARY LATENCY ----
# Within a formula for ease when working with 3 body parts.
calculate_latency_primary <- function(detail) {
  detail %>%
    filter(SHAPE.NAME == 1) %>%
    group_by(VIDEO) %>%
    slice_min(order_by = START.TIME, n = 1) %>%
    ungroup()
}

# Compute primary latencies for each bodypart
primarylatencies <- map(bodyparts, calculate_latency_primary)

# Combine, then take median START.TIME across bodyparts by video
# Does not affect values if only 1 body part entered.
latency_primary <- primarylatencies %>%
  bind_rows(.id = "bodypart") %>%
  group_by(VIDEO) %>%
  summarise(
    latency_primary = median(START.TIME, na.rm = TRUE),
    n_bodyparts    = sum(!is.na(START.TIME)),
    .groups = "drop"
  ) %>%
  addIDs()


# TOTAL PATH LENGTH ----
# Assumes videos trimmed to the moment rodent enters EB.
# Takes total distance travelled from start to finish, based on aggregate file.
pathlength_total <- subset(pathlength_total, MEASUREMENT == 'Distance (cm)')
pathlength_total$VALUE <- pathlength_total$VALUE / 100 # Convert to metres


# PRIMARY PATH LENGTH ----
# Takes (to the nearest second) the total distance travelled before primary 
# latency timepoint for each video. Uses movement per timebin file,
# assuming 1 s timebins.

# Round latency_primary to the nearest whole number
latency_primary$latency_primary_rounded <-
  as.integer(round(latency_primary$latency_primary))

# Sum VALUE for each VIDEO up to (and including) the rounded latency time bin.
pathlength_primary <- pathlength_timebin %>%
  inner_join(
    select(latency_primary, VIDEO, latency_primary_rounded), by = "VIDEO") %>%
  filter(`TIME.BIN..` <= latency_primary_rounded) %>%
  group_by(VIDEO, latency_primary_rounded) %>%
  summarise(distance_cm = sum(.data$VALUE, na.rm = TRUE),
            .groups = "drop") %>%
  addIDs

# Convert to metres
pathlength_primary$distance <- pathlength_primary$distance_cm / 100


# STRATEGY, AND TOTAL/PRIMARY ERRORS ----
source(here('Scripts', 'Barnes_Maze', 'search_tests.R'))
searchtests_results <- map(bodyparts, search_tests)

# Combine, then take median errors and max scores across bodyparts by video
# Does not affect values if only 1 body part entered.
searchtest <- searchtests_results %>%
  bind_rows(.id = "bodypart") %>%
  group_by(VIDEO) %>%
  summarise(
    errors_total = median(total_errors, na.rm = TRUE),
    errors_primary = median(primary_errors, na.rm = TRUE),
    reason = reason[which.max(strategy)],    
    strategy = max(strategy, na.rm = TRUE),
    n_bodyparts    = sum(!is.na(VIDEO)),
    .groups = "drop"
  )

searchtest <- addIDs(searchtest)

# COMBINE ALL SCORES INTO ONE DATAFRAME ----------------------------------------
master_train <- searchtest %>%
  select(VIDEO, errors_total, errors_primary, strategy, reason) %>%
  
  
  left_join(latency_total %>% select(VIDEO, latency_total = VALUE),
            by = "VIDEO") %>%
  left_join(latency_primary %>% select(VIDEO, latency_primary),
            by = "VIDEO") %>%
  
  
  left_join(pathlength_total %>% select(VIDEO, pathlength_total = VALUE),
            by = "VIDEO") %>%
  left_join(pathlength_primary %>% select(VIDEO, pathlength_primary = distance),
            by = "VIDEO") %>%
  
  
  addIDs



# Primary values if rodent never locates EB ----
# Set primary latency to the maximum duration for the phase
# Set primary path length to total path length

master_train <- master_train %>%
  mutate(
    latency_primary    = if_else(is.na(latency_primary), 
                                 latency_total, 
                                 latency_primary), 
    pathlength_primary = if_else(is.na(pathlength_primary),
                                 pathlength_total,
                                 pathlength_primary
    )
  )

# SCORE PER RAT PER DAY --------------------------------------------------------
# Averages each metric across training sessions that occurred on the same day,
# retaining columns for RatID, Sex and Treatment.

master_day <- master_train %>%
  group_by(RatID, Sex, Treatment, Day) %>%
  summarise(
    across(c(errors_total, errors_primary, strategy, latency_total,
             latency_primary, pathlength_total, pathlength_primary), mean),
    
    reason = list(unique(reason)),
    
    n_averaged = n(),
    
    .groups = "drop"
  )


# MAKE SINGLE MASTER AND REDEFINABLE METRIC ------------------------------------
select_metric <- function(metric_input, average_days) {
  # Avoids any coding conflicts between choosing 'Day' or 'Train' columns by
  # copying the appropriate column to 'timepoint', and metric column to 'value'.
  
  master <- if (average_days) {master_day} else {master_train}
  master$timepoint <- if (average_days) {master$Day} else {master$Train}
  
  master$value <- master[[which(names(master) %in% metric_input)]]
  master$metric <- metric_input
  
  master
}

# OUTPUT FOR GRAPHPAD PRISM ----------------------------------------------------
# Converts a single metric into a CSV file that can be copied into GraphPad
# Prism. Each animal receives its own column, and timepoints are rows.

export_prism <- function(metric_input,
                         average_days = TRUE,
                         save = TRUE,
                         outdir = here('Output', 'Barnes_Maze', 'Prism_CSV')) {
  
  master <- select_metric(metric_input, average_days)
  
  # Take just the value column you selected.
  metric_table_long <- master %>%
    select(RatID, Sex, Treatment, timepoint, value, metric) %>%
    mutate(name = paste(Treatment, Sex, RatID, sep = "_"))
  
  # Convert to wide format, and sort columns by Treat_Sex_RatID.
  metric_table_wide <- metric_table_long %>%
    select(name, timepoint, value) %>%
    pivot_wider(names_from = name, values_from = value) %>%
    {
      first <- "timepoint"
      rest  <- sort(setdiff(names(.), first))
      select(., all_of(c(first, rest)))
    }
  
  # Optionally, write to CSV.
  if (save) {
    dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
    write_csv(metric_table_wide, 
              file.path(outdir, paste0(metric_input, ".csv")))
    message(paste("Prism CSV saved -", metric_input))
  }
  
  metric_table_wide
}

# GROUP SUMMARY VALUES ---------------------------------------------------------
# Finds the average per treatment or sex-treatment x day.
# Dependent on group_sexes.

summarise_groups <- function(metric_input,
                             group_sexes,
                             average_days) {
  
  master <- select_metric(metric_input, average_days)
  
  group_vars <- c("timepoint", "Treatment", if (group_sexes) "Sex")
  
  master %>%
    group_by(across(all_of(group_vars))) %>%
    summarise(
      mean_value = mean(value, na.rm = TRUE),
      n          = sum(!is.na(value)),
      std_dev    = sd(value, na.rm = TRUE),
      .groups    = "drop"
    ) %>%
    mutate(se = std_dev / sqrt(n))
}

# GRAPHING PARAMETERS ----------------------------------------------------------
# Finds labels and breaks for axes. Returns as a list.

set_graphing_parameters <- function(metric_input, average_days) {
  
  x_lab <- if (average_days)
    "Training Day"
  else
    "Training Session"
  
  if (metric_input == "latency_total") {
    list(
      title = "Total Latency",
      y_lab = "Time (s)",
      y_limits = c(0, 110),
      y_breaks = seq(0, 100, by = 20),
      x_lab = x_lab
    )
  } else if (metric_input == "latency_primary") {
    list(
      title = "Primary Latency",
      y_lab = "Time (s)",
      y_limits = c(0, 80),
      y_breaks = seq(0, 80, by = 20),
      x_lab = x_lab
    )
  } else if (metric_input == "pathlength_total") {
    list(
      title = "Total Path Length",
      y_lab = "Distance (m)",
      y_limits = c(0, 12),
      y_breaks = seq(0, 12, by = 2), 
      x_lab = x_lab
    )
  } else if (metric_input == "pathlength_primary") {
    list(
      title = "Primary Path Length",
      y_lab = "Distance (m)",
      y_limits = NULL,
      y_breaks = waiver(),
      x_lab = x_lab
    )
  } else if (metric_input == "strategy") {
    list(
      title = "Search Strategy",
      y_lab = "Score",
      y_limits = c(0, 11),
      y_breaks = 0:11,
      x_lab = x_lab
    )
  } else if (metric_input == "errors_total") {
    list(
      title = "Total Errors",
      y_lab = "Count",
      y_limits = NULL,
      y_breaks = waiver(),
      x_lab = x_lab
    )
  } else if (metric_input == "errors_primary") {
    list(
      title = "Primary Errors",
      y_lab = "Count",
      y_limits = NULL,
      y_breaks = waiver(),
      x_lab = x_lab
    )
  } else {
    print("Ensure your metric input is one of the available options.")
  }
}

# GRAPH ------------------------------------------------------------------------
make_graph <- function(metric_input,
                       average_days = TRUE,
                       group_sexes  = TRUE,
                       save_graph   = FALSE,
                       graph_dir    = here('Output', 'Barnes_Maze', 'Graphs'),
                       save_summary_csv     = FALSE,
                       summary_csv_dir      = here('Output', 'Barnes_Maze', 'Summary_CSV')) {
  
  # Consistent dodging for bars/error bars
  dodge <- position_dodge(width = 0.8)
  
  # Summarised data for plot ----
  plot_df <- summarise_groups(metric_input, group_sexes, average_days)
  
  # Get all graphing parameters from helper function.
  parameters <- set_graphing_parameters(metric_input, average_days)
  y_lab      <- parameters$y_lab
  plot_title <- parameters$title
  y_limits   <- parameters$y_limits
  y_breaks   <- parameters$y_breaks
  x_lab      <- parameters$x_lab
  
  # Grouping and factor order ----
  if (group_sexes) {
    plot_df <- plot_df %>%
      mutate(Sex = factor(Sex, levels = c("M", "F")),
             Treatment = factor(Treatment, levels = c("Control", "ASO")))
    colour_group <- with(plot_df, interaction(Sex, Treatment, sep = ".", drop = TRUE))
    colour_group <- factor(colour_group,
                           levels = c("M.Control", "M.ASO", "F.Control", "F.ASO"))
  } else {
    plot_df <- plot_df %>%
      mutate(Treatment = factor(Treatment, levels = c("Control", "ASO")))
    colour_group <- plot_df$Treatment
  }
  plot_df$colour_group <- colour_group
  
  # Colours
  my_colors <- c(
    "F.Control" = "#FF8000",
    "M.Control" = "#00FF00",
    "F.ASO"     = "#FF0000",
    "M.ASO"     = "#0000FF",
    "Control"   = "#808080",
    "ASO"       = "#800080" 
  )

    
  # Base plot ----
  p <- ggplot(
    plot_df,
    aes(
      x     = .data$timepoint,
      y     = .data$mean_value,
      fill  = colour_group,
      group = colour_group
    )
  )
  
  # Bars ----
  # Edit my_patterns to set a sex-specific pattern
  if (group_sexes) {
    my_patterns <- c("F" = "stripe", "M" = "none")
    p <- p +
      geom_col_pattern(
        position        = dodge,
        width           = 0.6,
        alpha           = 0.5,
        linewidth       = 1,
        aes(pattern     = .data$Sex, color = colour_group),
        pattern_fill    = "white",
        pattern_colour  = NA,
        pattern_angle   = 45,
        pattern_spacing = 0.03,
        pattern_density = 0.4
      )
  } else {
    p <- p +
      geom_col_pattern(
        position       = dodge,
        width          = 0.6,
        alpha          = 0.5,
        linewidth      = 1,
        aes(color      = colour_group),
        pattern        = "none"
      )
  }
  
  # Error bars
  p <- p +
    geom_errorbar(
      aes(
        ymin = .data$mean_value - .data$se,
        ymax = .data$mean_value + .data$se
      ),
      width     = 0.3,
      linewidth = 0.75,
      position  = dodge
    ) +
    geom_hline(yintercept = 0, color = "black", linewidth = 0.5) +
    theme_minimal(base_family = "sans") +
    scale_y_continuous(
      limits = y_limits,
      breaks = y_breaks,
      expand = expansion(mult = c(0, 0.02))
    ) +
    theme(
      axis.line         = element_line(color = "black"),
      panel.grid.minor  = element_blank(),
      panel.grid.major  = element_blank(),
      axis.ticks        = element_line(color = "black"),
      axis.ticks.length = unit(6, "points"),
      axis.title        = element_text(color = "black", size = 12, face = "bold"),
      axis.text         = element_text(color = "black", size = 12),
      plot.title        = element_text(color = "black", size = 14, face = "bold", hjust = 0.5),
      legend.position   = "none"
    ) +
    labs(
      x     = x_lab,
      y     = y_lab,
      title = plot_title
    ) +
    scale_fill_manual(values = my_colors) +
    scale_color_manual(values = my_colors)
  
  if (group_sexes) {
    p <- p + scale_pattern_manual(
      values = c("M" = "none", 
                 "F" = "none")
      )
  }
  
  # Print and optionally save
  print(p)
  
  if (save_graph) {
    dir.create(graph_dir, showWarnings = FALSE, recursive = TRUE)
    ggsave(
      filename = file.path(
        graph_dir,
        paste0(metric_input, ifelse(group_sexes, "", "-Treat"), ".pdf")
      ),
      plot   = p,
      device = "pdf",
      width  = 14,
      height = 10,
      units  = "cm"
    )
    message(paste("Graph saved -", metric_input))
  }
  
  if (save_summary_csv) {
    dir.create(summary_csv_dir, showWarnings = FALSE, recursive = TRUE)
    out_name <- paste0(metric_input, ifelse(average_days, "_day", "_train"), ".csv")
    write_csv(plot_df, file = file.path(summary_csv_dir, out_name))
    message(paste("Summary data CSV saved -", metric_input))
  }
}

# LOOP TO ANALYSE ALL DATA =====================================================
if (export_all) {
  metrics <- c(
    "latency_total",
    "latency_primary",
    "pathlength_total",
    "pathlength_primary",
    "strategy",
    "errors_primary",
    "errors_total"
  )
  
  for (metric_input in metrics) {
    export_prism(metric_input)
    
    make_graph(metric_input,
               save_graph = TRUE,
               save_summary_csv = TRUE)
  }
}
