# NOR-NPR Analysis ----

# Notes: 
# "NPR" (Novel Placement Recognition Test) was used as a synonym of "OLT" 
# (Object Location Task) throughout this script

# Requires prior analysis in SimBa (Simon Nilsson).
# Choose 'Analyze Machine Predictions: Aggregates'

# Assumes your video filenames are in the format:
# (RNA)_(test)_P(phase)_(ratID)
# e.g. Dory_NOR_P2_rat07

# For publication, CSVs from this script were exported to GraphPad Prism 10, for 
# further statistical analysis, outlier removal and final graphing.

workingdirectory <- 'Scripts/NOR_OLT'
setwd(workingdirectory)
rm(list = ls())

# Inputs ----
# Filepaths
keyfilepath <- 'Data/Reference_Tables/ratID_key.csv'
simba_machinepredictions_aggregates <- 'Data/Simba_Output/NOR_OLT/data_summary.csv'
videostoexclude <- 'videos_to_exclude.csv'

# Data selection
test <- "NOR"
RNA <- "Dory"
phase <- 2 # Phase 1 is the training phase, Phase 2 is the test phase.

#  Column from SimBA output to read. Do not change for DI and exploration.
measurement <- "Total event duration (s)"

# Minimum exploration time (s) for inclusion in analysis. Default 10 s.
min_exploration_cutoff <- 10 

# Graph settings
output <- "DI" # "DI" or "exploration"
discriminatesexes <- FALSE # Separate males and females for graphing
perform_t_tests <- TRUE # Note: 2-way ANOVA required if sexes separated.

# Export
savegraph <- FALSE
savecsv <- FALSE

# Packages ----
library(tidyverse)
library(ggpubr)
library(ggnewscale)

# Read files ----
key <- read.csv(keyfilepath)

data <- read.csv(simba_machinepredictions_aggregates)

# Add extra IDs to data ----
# Assumes your filenames are in the format:
# (RNA)_(test)_P(phase)_(RatID)
# e.g. Dory_NOR_P2_rat07

# Test, RNA, Phase and RatID: extract from video filenames
data <- data %>%
  mutate(
    RNA  = str_extract(VIDEO, "^[^_]+"), # Before first underscore
    test = str_extract(VIDEO, "(?<=_)[^_]+"), # After first underscore
    Phase = as.integer(
      str_extract(VIDEO, "(?<=_P)\\d+")), # Digit after first '_P',
    RatID = str_extract(VIDEO, "[^_]+$") # After final underscore
  )

# Filtering data ----
videostoexclude <- read.csv(videostoexclude)
data <- data[! data$VIDEO %in% videostoexclude$VIDEO, ]

filtdata <- data[data$test == test &
                   data$RNA == RNA &
                   data$MEASUREMENT == measurement & data$Phase == phase, ]


if (phase == 2) {
  filtdata$NovelSide <- key$NovelSide[match(filtdata$RatID, key$RatID)]
  
  # Determine whether each row is novel or not
  filtdata$Novel <- (filtdata$NovelSide == "L" &
                       grepl("Left", filtdata$CLASSIFIER)) |
    (filtdata$NovelSide == "R" &
       grepl("Right", filtdata$CLASSIFIER))
}

# If in phase 1, map Left to TRUE and Right to FALSE for the discrimination index.
if (phase == 1) {
  filtdata$Novel <- ifelse(filtdata$CLASSIFIER == "Left", TRUE, FALSE)
}

filtdata$Sex <- as.factor(key$Sex[match(filtdata$RatID, key$RatID)])
filtdata$Treatment <- as.factor(key$Treatment[match(filtdata$RatID, key$RatID)])

# Keeping only necessary columns
df <- subset(filtdata, select = c(RatID, VALUE, Novel, Sex, Treatment))

df <- arrange(df, RatID)

# Place novel and familiar in separate columns of the same row.
# Then calculate DI and exploration.
DI_df <- filtdata %>%
  pivot_wider(
    id_cols = c(RatID, Sex, Treatment),
    names_from = Novel,
    values_from = VALUE
  ) %>%
  rename(familiar = `FALSE`, novel = `TRUE`) %>%
  mutate(DI = (novel - familiar) / (novel + familiar)) %>%
  mutate(exploration = novel + familiar)

# Filter out any instances where exploration is below the cutoff
DI_df <- DI_df[DI_df$exploration > min_exploration_cutoff,]
DI_df <- arrange(DI_df, RatID)


# Prepare dataframe for graph ----
  graph_df <- DI_df
  graph_df$st <- factor(paste(graph_df$Sex, graph_df$Treatment),
                   levels = c("M Control", "M ASO", "F Control", "F ASO"))
  
  graph_df$Sex <- factor(graph_df$Sex,
                    levels = c("M", "F"))
  graph_df$Treatment <- factor(graph_df$Treatment,
                    levels = c("Control", "ASO"))
  
# Exploration/DI toggles for graph appearance ----
  y_label  <- if (output == "DI") "Discrimination Index" else "Total exploration (s)"
  
  # Custom gridlines (horizontal reference lines)
  gridlines <- if (output == "DI") c(-1, -0.5, 0, 0.5, 1) else seq(0, 150, by = 50)
  
  # Axis display breaks
  y_breaks  <- if (output == "DI") seq(-1, 1.2, by = 0.5) else seq(0, 150, by = 50)
  
  # Visible plotting window with extra headroom for significance bars
  # DI: your current values; exploration: 0..150 scale with +50 units headroom to 200
  ylim_cart <- if (output == "DI") c(-1, 1.75) else c(0, 200)
  
  # Custom left axis segment extents (matches the "visible axis" range, not the extra headroom)
  axis_segment_df <- if (output == "DI") {
    data.frame(x = 0, xend = 0, y = -1,   yend = 1.05)
  } else {
    data.frame(x = 0, xend = 0, y = 0,    yend = 150)
  }
  
# Revised graph ----
  # ---- Base plot ----
  p2 <- ggplot(graph_df, aes(x = get(ifelse(discriminatesexes == TRUE, "st", "Treatment")),
                        y = .data[[output]])) +
    
    # Custom horizontal gridlines FIRST
    geom_hline(
      yintercept = gridlines,
      color = "grey80",
      linewidth = 0.5
    ) +
    
    # Boxplot with Treatment fill
    geom_boxplot(
      aes(fill = Treatment, color = Treatment),
      linewidth = 1,
      fatten = 1,
      color = "black",
      outlier.shape = NA
    ) +
    scale_fill_manual(values = c("Control" = "#48d568", "ASO" = "#ff76b5")) +
    
    # Add new fill scale for points
    ggnewscale::new_scale_fill() +
    geom_jitter(
      aes(fill = Sex),
      shape = 21,
      color = "black",
      stroke = 1,
      width = 0.15,
      size = 3
    ) +
    scale_fill_manual(values = c("M" = "#5eabed", "F" = "#f8da2b")) +
    
    
    # Grap limits
    coord_cartesian(
      ylim = ylim_cart,
      xlim = if (discriminatesexes) c(0, 5) else c(0, 3),
      expand = c(bottom = FALSE)
    ) +
    scale_y_continuous(breaks = y_breaks) +
    
    # Text and line styling
    theme_minimal() +
    theme(
      axis.line.x = element_line(color = "black", linewidth = 1),
      axis.ticks.length  = unit(0.25, "cm"),
      axis.ticks = element_line(color = "black", linewidth = 1),
      axis.title = element_text(size = 12, color = "black", face = "bold"),
      axis.title.y = element_text(hjust = 1 / 2.75),
      axis.text = element_text(size = 12, color = "black"),
      plot.title = element_text(hjust = 0.5, size = 14, color = "black", face = "bold"),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      legend.position = "none"
    ) +
    
    # Custom axis line (left y-axis)
    geom_segment(
      data = axis_segment_df,
      aes(x = x, xend = xend, y = y, yend = yend),
      inherit.aes = FALSE,
      linewidth = 2,
      color = "black"
    )
  
  # ---- Comparisons & significance ----
  if (perform_t_tests) {
  
  if (discriminatesexes) {
    comparisons1 <- list(c("M Control", "M ASO"))
    comparisons2 <- list(
      c("F Control", "F ASO"),
      c("M ASO", "F ASO"),
      c("M Control", "F Control")
    )
  } else {
    comparisons1 <- list(c("Control", "ASO"))
  }
  
  bracket.size   <- 1
  tip.length     <- 0.03
  step.increase  <- 0.1
  method         <- "t.test"
  label          <- "p.format"
  hide.ns        <- FALSE
  
  p2 <- p2 + stat_compare_means(
    comparisons   = comparisons1,
    method        = method,
    label         = label,
    bracket.size  = bracket.size,
    tip.length    = tip.length,
    step.increase = step.increase,
    hide.ns       = hide.ns
  )
  
  if (discriminatesexes) {
    p2 <- p2 + stat_compare_means(
      comparisons   = comparisons2,
      method        = method,
      label         = label,
      bracket.size  = bracket.size,
      tip.length    = tip.length,
      step.increase = step.increase,
      hide.ns       = hide.ns
    )
  }
  }
  
  # ---- Axes & title labels ----
  p2 <- p2 +
    xlab("Experiment") +
    ylab(y_label) +
    ggtitle(paste(RNA, test, "Phase", phase)) +
    if (discriminatesexes) {
      scale_x_discrete(labels = c("Male\nControl","Male\nASO","Female\nControl","Female\nASO"))
    } else {
      scale_x_discrete(labels = c("Control","ASO"))
    }
  
  p2
  
  # ---- Saving graph and table ----
  if (savegraph == TRUE) {
    ggsave(
      filename = paste0("Graphs/",
                           if(output == "exploration"){"Exploration/"} else "",
                           if(discriminatesexes == FALSE){"NoSexDiscrimination/"} else "",
                           paste0(RNA, "_", test, "_", phase),
                                  ".pdf"),
      plot = last_plot(),
      device = "pdf",
      width = 10,
      height = 14,
      units = "cm"
    )
    print("Graph saved!")
  }
  
  
# Saving table
if (savecsv == TRUE) {
  write.csv(graph_df,
            file = paste0("ExportCSVs/",
                          RNA, "_", test, "_", phase, ".csv"))
}

