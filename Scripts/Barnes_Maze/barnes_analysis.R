
# =========================
# Barnes Maze Analysis
# =========================

# Requires prior analysis in SimBA (Simon Nilsson)
# Holes in the Barnes maze are SimBA ROIs labelled 1 – 18, where 1 is the escape box.

workingdirectory <- 'Scripts/Barnes_Maze'
setwd(workingdirectory)
rm(list = ls())


# Inputs ----

keyfilepath         <- 'Data/Reference_Tables/ratID_key.csv'
traindaykeyfilepath <- 'Data/Reference_Tables/barnes_day_key.csv'

summaryfilepath     <- 'Data/Simba_Output/Barnes_Maze/ROI_descriptive_statistics.csv'
detailfilepath      <- 'Data/Simba_Output/Barnes_Maze/Detailed_ROI_data.csv'
pathlengthfilepath  <- 'Data/Simba_Output/Barnes_Maze/Movement_log.csv'

show_days  <- TRUE # Where multiple training sessions happened in one day, average them.
groupsexes <- TRUE # Show Sex x Treatment groups, rather than just Treatment groups
errorbars  <- "se" # "ci" or "se"

savegraph  <- FALSE # Saves ggplot
savecsv    <- FALSE # Saves csv of summary statatistics for each group (e.g. mean, se)

saveprismcsv <- FALSE # Rearranges data for import into GraphPad Prism.

# Toggle the metric used in the final clustered column chart:
# Options: "pathlength", "latency_primary", "latency_total", "strategy"
metric_input <- "strategy"

# Named color and pattern palettes so they map robustly to factor levels
my_colors <- c(
  "F.Control" = "#808080",
  "M.Control" = "#808080",
  "F.ASO"     = "#800080",
  "M.ASO"     = "#800080",
  "Control"   = "#808080",
  "ASO"       = "#800080"
)
my_patterns <- c(
  "F" = "stripe", 
  "M" = "none"
)


# Packages ----

library(tidyverse)
library(rlang)
library(ggpattern)


# Read data ----

key         <- read.csv(keyfilepath)
traindaykey <- read.csv(traindaykeyfilepath)

summary     <- read.csv(summaryfilepath)
detail      <- read.csv(detailfilepath)
pathlength  <- read.csv(pathlengthfilepath)


# addIDs function ----

# Extracts identifiers from video filename, assuming it has the following structure:
# '04Training_rat31' or 'Test_rat31'
# '04' is the training number OR
# 'Test' indicates the final session of the Barnes Maze, where no escape box is present
#      The traindaykeyfilepath can convert between training numbers and day numbers
# 'rat31' is the personal identifier of the rat
#      The keyfilepath notes Sex and Treatment for all RatIDs


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
  df$Day <- traindaykey$Day[match(df$Train, traindaykey$Train)]
  df
}


# Prepare dataframes and add IDs ----

summary    <- addIDs(summary)
detail     <- addIDs(detail)
pathlength <- addIDs(pathlength)

timeunit <- ifelse(show_days, "Day", "Train")

# Find the correct columns of the SimBA output to include in dataframes for each metric
totallatency <- subset(summary, MEASUREMENT == 'VIDEO LENGTH (S)' & SHAPE == 1 & Day != 'Test')

primarylatency <- detail %>%
  filter(SHAPE.NAME == 1) %>%
  group_by(VIDEO) %>%
  slice_min(order_by = START.TIME, n = 1) %>%
  ungroup()

pathlength <- subset(pathlength, MEASURE == 'Distance (cm)')
pathlength$VALUE <- pathlength$VALUE / 100 # Convert to metres


# Averaging per-rat if show_days == TRUE to avoid pseudoreplication ----

totallatency_days <- totallatency %>%
  filter(MEASUREMENT == "VIDEO LENGTH (S)") %>%
  mutate(
    VALUE = as.numeric(VALUE),
    Day = as.character(Day),
    RatID = as.character(RatID)
  ) %>%
  group_by(RatID, Day, Sex, Treatment) %>%
  summarise(
    VALUE = mean(VALUE, na.rm = TRUE),
    n_sessions_averaged = n(),
    .groups = "drop"
  )

primarylatency_days <- primarylatency %>%
  mutate(
    START.TIME = as.numeric(START.TIME),
    Day = as.character(Day),
    RatID = as.character(RatID)
  ) %>%
  group_by(RatID, Day, Sex, Treatment) %>%
  summarise(
    START.TIME = mean(START.TIME, na.rm = TRUE),
    n_sessions_averaged = n(),
    .groups = "drop"
  )

pathlength_days <- pathlength %>%
  mutate(
    VALUE = as.numeric(VALUE),
    Day = as.character(Day),
    RatID = as.character(RatID)
  ) %>%
  group_by(RatID, Day, Sex, Treatment) %>%
  summarise(
    VALUE = mean(VALUE, na.rm = TRUE),
    n_sessions_averaged = n(),
    .groups = "drop"
  )


# Calculating strategy scores from external script ----

source("search_tests.R")
results <- search_tests(detail)
results <- addIDs(results)

results_days <- results %>%
  mutate(
    Day = as.character(Day),
    RatID = as.character(RatID),
    score = as.numeric(score)
  ) %>%
  group_by(RatID, Day, Sex, Treatment) %>%
  summarise(
    score = mean(score, na.rm = TRUE),
    n_sessions_averaged = n(),
    .groups = "drop"
  )

df_in_results <- if(isTRUE(show_days)) results_days else results

# Calculate summary statistics for graphing, with toggle for grouping sexes
strategygrouped <- df_in_results %>%
  group_by(Treatment, !!sym(timeunit), !!!(if (groupsexes) rlang::syms("Sex") else NULL)) %>%
  summarise(
    mean_score = mean(score, na.rm = TRUE),
    count      = n(),
    std_dev    = sd(score, na.rm = TRUE),
    .groups    = "drop"
  ) %>%
  mutate(se = std_dev / sqrt(count),
         ci = qt(0.975, df = count - 1) * se)


# Unified data prep for final chart ----

x_lab <- if (timeunit == "Day") {
  "Training Day"
} else if (timeunit == "Train") {
  "Training Session"
}

prepare_metric_grouped <- function(metric_input, timeunit, strategy_df = NULL) {
  if (metric_input == "pathlength") {
    df_in_pathlength <- if (isTRUE(show_days)) pathlength_days else pathlength
    df <- df_in_pathlength %>%
      group_by(
        Treatment,
        !!sym(timeunit),
        !!!(if (groupsexes) rlang::syms("Sex") else NULL)
      ) %>%
      summarise(
        mean_value = mean(VALUE, na.rm = TRUE),
        count      = n(),
        std_dev    = sd(VALUE, na.rm = TRUE),
        .groups    = "drop"
      ) %>%
      mutate(
        se = std_dev / sqrt(count),
        ci = qt(0.975, df = count - 1) * se
      )
    y_lab    <- "Average Distance (m)"
    title    <- "Path Length"
    y_limits <- c(0, 20)
    y_breaks <- waiver()
    
  } else if (metric_input == "latency_total") {
    df_in_latency_total <- if (isTRUE(show_days)) totallatency_days else totallatency
    df <- df_in_latency_total %>%
      group_by(
        Treatment,
        !!sym(timeunit),
        !!!(if (groupsexes) rlang::syms("Sex") else NULL)
      ) %>%
      summarise(
        mean_value = mean(VALUE, na.rm = TRUE),
        count      = n(),
        std_dev    = sd(VALUE, na.rm = TRUE),
        .groups    = "drop"
      ) %>%
      mutate(
        se = std_dev / sqrt(count),
        ci = qt(0.975, df = count - 1) * se
      )
    y_lab    <- "Average Time (s)"
    title    <- "Total Latency"
    y_limits <- c(0,110)
    y_breaks <- seq(0, 100, by = 20)
    
  } else if (metric_input == "latency_primary") {
    df_in_latency_primary <- if (isTRUE(show_days)) primarylatency_days else primarylatency
    df <- df_in_latency_primary %>%
      group_by(
        Treatment,
        !!sym(timeunit),
        !!!(if (groupsexes) rlang::syms("Sex") else NULL)
      ) %>%
      summarise(
        mean_value = mean(START.TIME, na.rm = TRUE),
        count      = n(),
        std_dev    = sd(START.TIME, na.rm = TRUE),
        .groups    = "drop"
      ) %>%
      mutate(
        se = std_dev / sqrt(count),
        ci = qt(0.975, df = count - 1) * se
      )
    y_lab    <- "Average Time (s)"
    title    <- "Primary Latency"
    y_limits <- NULL
    y_breaks <- waiver()
    
  } else if (metric_input == "strategy") {
    if (is.null(strategy_df)) stop("For metric_input = 'strategy', pass strategygrouped as strategy_df.")
    df <- strategy_df %>% mutate(mean_value = mean_score)
    y_lab    <- "Average Score"
    title    <- "Search Strategy"
    y_limits <- c(0, 11)
    y_breaks <- 0:11
    
  } else {
    stop("metric_input must be one of: 'pathlength', 'latency_total', 'latency_primary', 'strategy'")
  }
  list(df = df, y_lab = y_lab, title = title, y_limits = y_limits, y_breaks = y_breaks)
}


# Final clustered column chart ----

plot_data  <- prepare_metric_grouped(metric_input, timeunit, strategy_df = strategygrouped)

plot_df     <- plot_data$df
y_lab       <- plot_data$y_lab
plot_title  <- plot_data$title
y_limits    <- plot_data$y_limits
y_breaks    <- plot_data$y_breaks

# Consistent dodging for bars/error bars
dodge <- position_dodge(width = 0.8)

# Fill/group factor with explicit order for sexes+treatment
if (groupsexes) {
  color_group <- interaction(plot_df$Sex, plot_df$Treatment, sep = ".", drop = TRUE)
  color_group <- factor(color_group, levels = c("M.Control", "M.ASO", "F.Control", "F.ASO"))
} else {
  color_group <- factor(plot_df$Treatment, levels = c("Control", "ASO"))
}
plot_df$color_group <- color_group

# Rename "Test" to "Test Day"
if ("Day" %in% names(plot_df)) {
  plot_df$Day <- ifelse(plot_df$Day == "Test", "Test Day", plot_df$Day)
}

# Base ggplot
p <- ggplot(
  plot_df,
  aes(
    x     = .data[[timeunit]],
    y     = mean_value,
    fill  = color_group,
    group = color_group
  )
)

# Conditional pattern mapping to avoid missing 'Sex' when groupsexes == FALSE ----
if (groupsexes) {
  p <- p +
    geom_col_pattern(
      position        = dodge,
      width           = 0.7,
      aes(pattern = Sex, color = color_group),
      pattern_fill    = "white",
      pattern_colour  = NA,
      pattern_angle   = 45,
      pattern_spacing = 0.03,
      pattern_density = 0.4
    )
} else {
  p <- p +
    geom_col_pattern(
      position        = dodge,
      width           = 0.7,
      aes(color = color_group),
      pattern         = "none",
      pattern_fill    = "white",
      pattern_colour  = NA
    )
}


p <- p +
  geom_errorbar(
    aes(
      ymin = mean_value - .data[[errorbars]],
      ymax = mean_value + .data[[errorbars]]
    ),
    width = 0.3,
    linewidth = 0.75,
    position = dodge
  ) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.5) +
  theme_minimal(base_family = "serif") +
  scale_y_continuous(
    limits = y_limits,
    breaks = y_breaks,
    expand = expansion(mult = c(0, 0.02))
  ) +
  theme(
    axis.line          = element_line(color = "black"),
    panel.grid.minor   = element_blank(),
    panel.grid.major   = element_blank(),
    axis.ticks         = element_line(color = "black"),
    axis.ticks.length  = unit(6, "points"),
    axis.title         = element_text(color = "black", size = 12, face = "bold"),
    axis.text          = element_text(color = "black", size = 12),
    plot.title         = element_text(color = "black", size = 14, face = "bold", hjust = 0.5),
    legend.position    = "none"
  ) +
  labs(
    x     = x_lab,
    y     = y_lab,
    title = plot_title
  ) +
  scale_fill_manual(
    values = my_colors,
    labels = if (groupsexes)
      c("Male Control", "Male ASO", "Female Control", "Female ASO")
    else
      c("Control", "ASO"),
    name   = if (groupsexes) "Sex and Treatment" else "Treatment"
  ) +
  scale_color_manual(
    values = my_colors
  )

# Only add pattern scale when mapping to Sex
if (groupsexes) {
  p <- p + scale_pattern_manual(values = my_patterns)
  # else: no pattern scale needed when pattern is constant "none"
}

print(p)

# Save graph or summary csv files ----
    
if (savegraph == TRUE) {
  ggsave(
    filename = paste0("Graphs/StyledForSaba/",
                      metric_input,
                      ifelse(groupsexes, "", "-Treat"),
                      ".pdf"),
    plot = last_plot(),
    device = "pdf",
    width = 14,
    height = 10,
    units = "cm"
  )
  print("Graph saved!")
}

if (savecsv == TRUE) {
  write_csv(plot_df,
            file = paste0("ExportCSVs/", metric_input,
                          ifelse(show_days, "_day", "_train"),
                          ".csv"))
}


# Export Prism csv ----

# Select appropriate all-datapoint dataframe based on metric_input 
# "pathlength", "latency_primary", "latency_total", "strategy"
df <- if (metric_input == "strategy") {df_in_results} else
  if (metric_input == "latency_primary") {primarylatency_days} else
    if (metric_input == "latency_total") {totallatency_days} else
      if (metric_input == "pathlength") {pathlength_days}

# Find the name of the value column
valuecolumn <- colnames(df)[colnames(df) %in% c("VALUE", "score", "START.TIME")]

# 1) Order days: numerics first (1, 2, 3, ...), then any non-numeric labels (e.g., "Test", "Probe")
day_levels <- df %>%
  mutate(Day_num = suppressWarnings(as.numeric(Day)),
         is_special = ifelse(is.na(Day_num), 1L, 0L)) %>%
  arrange(is_special, Day_num, Day) %>%
  distinct(Day) %>%
  pull(Day)

# 2) Normalize Treatment order: Control first, then ASO
df2 <- df %>%
  mutate(
    Day = factor(Day, levels = day_levels),
    Treatment = factor(Treatment, levels = c("Control", "ASO")),
    col_label = paste(Sex, RatID, sep = "_")  # e.g., "M_rat01"
  )

# 3) Prepare deterministic column orders within each Treatment block (by Sex, then RatID)
col_order_control <- df2 %>%
  filter(Treatment == "Control") %>%
  distinct(Sex, RatID) %>%
  arrange(Sex, RatID) %>%
  transmute(name = paste("Control", paste(Sex, RatID, sep = "_"), sep = "_")) %>%
  pull(name)

col_order_treatment <- df2 %>%
  filter(Treatment == "ASO") %>%
  distinct(Sex, RatID) %>%
  arrange(Sex, RatID) %>%
  transmute(name = paste("ASO", paste(Sex, RatID, sep = "_"), sep = "_")) %>%
  pull(name)

# 4) Pivot to wide. If there were any duplicate Day×Rat rows, average them.
wide_prism <- df2 %>%
  select(Day, Treatment, col_label, !!sym(valuecolumn)) %>%
  pivot_wider(
    names_from  = c(Treatment, col_label),
    values_from = !!sym(valuecolumn),
    values_fn   = mean # safely handle any accidental duplicates
  ) %>%
  arrange(Day) %>%
  # enforce the Control block first, then Treatment, each sorted by Sex then RatID
  select(Day, all_of(col_order_control), all_of(col_order_treatment))

# 5) (Optional) write to CSV for Prism
if (saveprismcsv) {
  write_csv(wide_prism, paste0("PrismCSV/",metric_input,".csv"))}
