# OpenField Percent Inner ----

# Requires prior analysis in SimBA (Simon Nilsson).
# ROI 'Inner' defined as inner four squares (1/4 area)
# Choose 'Analyze ROI Data: Aggregates'
# Check 'Outside ROI Zones Data' 

# Assumes your video filenames are in the format:
# (RNA)_..._(ratID)
# e.g. Dory_OpenField_rat29

# For publication, CSVs from this script were exported to GraphPad Prism 10, for 
# further statistical analysis, outlier removal and final graphing.

workingdirectory <- 'Scripts/Open_Field'
setwd(workingdirectory)
rm(list = ls())

# Inputs ----
# Data selection
RNA <- "Dory"
test <- "percentInner" # Do not change for this script.

# Filepaths
keyfilepath <- 'Data/Reference_Tables/ratID_key.csv'
simba_ROI_aggregates <- 'Data/Simba_Output/Open_Field/ROI_descriptive_statistics.csv'
videostoexclude <- 'Data/Reference_Tables/videos_to_exclude.csv'

# Graph settings
discriminatesexes <- FALSE
perform_t_tests <- TRUE # Note: 2-way ANOVA required if sexes separated.

# Export
savegraph <- FALSE
savecsv <- FALSE


# Packages ----
library(tidyverse)
library(ggpubr) # For adding p-values to plots (stat_compare_means)
library(ggnewscale) # For colouring points and boxes separately


# Read files ----
key <- read.csv(keyfilepath)

data <- read.csv(simba_ROI_aggregates)

# Add extra IDs to data ----
# RatID: extract string after final underscore
data$RatID <- str_extract(data$VIDEO, "[^_]+$")

# RNA: extract string before first underscore
data$RNA <- str_extract(data$VIDEO, "^[^_]+")

# Filtering data ----
# Filer out videos named in videos_to_exclude.csv
videostoexclude <- read.csv(videostoexclude)
data <- data[! data$VIDEO %in% videostoexclude$VIDEO, ]

# Make a new column to show whether it was inside or outside the ROI
data$Region <- ifelse(data$SHAPE == "OUTSIDE REGIONS OF INTEREST", "Outer", "Inner")

# Add data from reference table 'key'
data$Sex <- factor(key$Sex[match(data$RatID, key$RatID)],
                   levels=c("M", "F"))
data$Treatment <- factor(key$Treatment[match(data$RatID, key$RatID)],
                         levels = c("Control", "ASO"))

# Keeping only necessary columns
df <- subset(data, select = c(RatID, VALUE, Region, Sex, Treatment)) 

df <- arrange(df,RatID)

# Place Inner and Outer in two columns of the same row 
of <- data %>%
  pivot_wider(
    id_cols = c(RatID, Sex, Treatment),
    names_from = Region,
    values_from = VALUE
  ) %>%
  
  # Calculate percent of time spent in the inner region
  mutate(percentInner = Inner/Outer * 100) %>%
  mutate(exploration = Inner + Outer)

# Arrange, and make 'st' (Sex-Treatment) column for x-axis
of <- arrange(of,RatID)
of$st <- factor(paste(of$Sex, of$Treatment),
                levels = c("M Control", "M ASO", "F Control", "F ASO"))

# Graph ----
p <- ggplot(data=of, aes(x = get(ifelse(discriminatesexes == TRUE, "st", "Treatment")),
                         y = get(test))) +
  
  # Custom horizontal gridlines FIRST
  geom_hline(
    yintercept = c(5, 10, 15, 20),
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

  # Axis limits
  coord_cartesian(
    ylim = c(0, 27),
    xlim = if (discriminatesexes) c(0, 5) else c(0,3),
    expand = c(bottom = FALSE)
  ) +
  scale_y_continuous(breaks = seq(0, 20, by = 5)) +
  
  theme_minimal() +
  
  # Line and axis styling
  theme(
    axis.line.x = element_line(color = "black", linewidth = 1),
    axis.ticks.length  = unit(0.25, "cm"),
    axis.ticks = element_line(color = "black", linewidth = 1),
    axis.title = element_text(
      size = 12,
      color = "black",
      face = "bold"
    ),
    axis.title.y = element_text(hjust = 6 / 26),
    axis.text = element_text(size = 12, color = "black"),
    plot.title = element_text(
      hjust = 0.5,
      size = 14,
      color = "black",
      face = "bold"
    ),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    legend.position = "none"
    
  ) +
  
  # Custom axis lines
  geom_segment(
    data = data.frame(x = 0, xend = 0, y = 0, yend = 20.5),
    aes(x = x, xend = xend, y = y, yend = yend),
    inherit.aes = FALSE,
    size = 2,
    color = "black"
  )

if (perform_t_tests){
# Add comparisons, separated into 2 so that Male/Male and Female/Female can be plotted at same height
if(discriminatesexes){
  comparisons1 <- list(
    c("M Control", "M ASO")
  )
  
  comparisons2 <- list(
    c("F Control", "F ASO"),
    c("M ASO", "F ASO"),
    c("M Control", "F Control")
  )
  
} else {
  
  comparisons1 <- list(
    c("Control", "ASO")
  )
}

bracket.size = 1
tip.length = 0.03
step.increase = 0.1
method = "t.test"
label = "p.format"
hide.ns = FALSE


p <- p + stat_compare_means(comparisons = comparisons1,
                              method = method,
                              label = label,
                              bracket.size = bracket.size,
                              tip.length = tip.length,
                              step.increase = step.increase,
                              hide.ns = hide.ns)

if (discriminatesexes){
  p <- p + stat_compare_means(comparisons = comparisons2,
                              method = method,
                              label = label,
                              bracket.size = bracket.size,
                              tip.length = tip.length,
                              step.increase = step.increase,
                              hide.ns = hide.ns)
}
}

# Labels
p <- p +
  xlab("Experiment") +
  ylab("Duration within inner quadrant (%)") +
  ggtitle(paste(RNA, "Open Field")) +
  if(discriminatesexes){
    scale_x_discrete(labels=c("Male\nControl","Male\nASO","Female\nControl","Female\nASO"))
  } else {
    scale_x_discrete(labels=c("Control","ASO"))
  }

p

# Export graph ----
if (savegraph == TRUE) {
  ggsave(
    filename = file.path("Graphs", paste0(RNA, "_", test, "_", if(discriminatesexes == FALSE){"NoSex"}, ".pdf")),
    plot = last_plot(),
    device = "pdf",
    width = 10,
    height = 14,
    units = "cm"
  )
}

# Export csv ----
if (savecsv == TRUE) {
  write.csv(of,
            file = paste0("ExportCSVs/",
                          RNA, "_PercentInner.csv"))
}
