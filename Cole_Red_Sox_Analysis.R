# ==========================================
# Gerrit Cole vs. Boston Red Sox
# 2026 MLB Pitching Matchup Strategy
# ==========================================

# Load packages
library(tidyverse)
library(ggplot2)
library(dplyr)
library(readr)

# Create project folders if needed
dir.create("data", showWarnings = FALSE)
dir.create("visualizations", showWarnings = FALSE)
dir.create("reports", showWarnings = FALSE)

# Project information
pitcher_name <- "Gerrit Cole"
opponent_name <- "Boston Red Sox"
season <- 2026

cat("Project:", pitcher_name, "vs.", opponent_name, "\n")
cat("Season:", season, "\n")
cat("Project setup complete!\n")

install.packages("baseballr")

# ==========================================
# Download Gerrit Cole's 2026 Statcast Data
# ==========================================

library(baseballr)

cole <- statcast_search_pitchers(
  start_date = "2026-03-01",
  end_date = "2026-10-01",
  pitcherid = 543037
)

# Check the number of rows and columns
dim(cole)

# Preview the data
head(cole)

# Save to project data folder
write_csv(cole, "data/Gerrit_Cole_2026_Statcast.csv")

# ==========================================
# Clean and Validate Gerrit Cole Data
# ==========================================

# Keep 2026 regular-season pitches
cole_clean <- cole %>%
  filter(
    game_year == 2026,
    game_type == "R"
  )

# Check number of pitches
nrow(cole_clean)

# Check pitch types
cole_clean %>%
  count(pitch_type, sort = TRUE)

# Check missing values in important columns
cole_clean %>%
  summarise(
    missing_velocity = sum(is.na(release_speed)),
    missing_spin = sum(is.na(release_spin_rate)),
    missing_movement = sum(
      is.na(pfx_x) | is.na(pfx_z)
    ),
    missing_location = sum(
      is.na(plate_x) | is.na(plate_z)
    )
  )

# Check how many games are represented
n_distinct(cole_clean$game_pk)

# ==========================================
# Visualization 1: Pitch Arsenal
# ==========================================

# Calculate pitch usage
pitch_usage <- cole_clean %>%
  filter(!is.na(pitch_type)) %>%
  count(pitch_type, name = "pitches") %>%
  mutate(
    usage_pct = round(pitches / sum(pitches) * 100, 1)
  ) %>%
  arrange(desc(usage_pct))

# View results
print(pitch_usage)

# Create pitch usage chart
pitch_usage_chart <- ggplot(
  pitch_usage,
  aes(x = reorder(pitch_type, usage_pct),
      y = usage_pct)
) +
  geom_col(fill = "#003087", width = 0.7) +
  geom_text(
    aes(label = paste0(usage_pct, "%")),
    hjust = -0.15,
    size = 4
  ) +
  coord_flip() +
  labs(
    title = "Gerrit Cole 2026 Pitch Arsenal",
    subtitle = "Regular Season | Pitch Usage",
    x = "Pitch Type",
    y = "Usage (%)",
    caption = "Source: Baseball Savant / Statcast"
  ) +
  scale_y_continuous(
    limits = c(0, max(pitch_usage$usage_pct) + 10)
  ) +
  theme_minimal(base_size = 13)

# Display chart
print(pitch_usage_chart)

# Save chart
ggsave(
  "visualizations/Cole_2026_Pitch_Usage.png",
  plot = pitch_usage_chart,
  width = 10,
  height = 6,
  dpi = 300
)

# ==========================================
# Improve Visualization 1
# ==========================================

pitch_usage <- pitch_usage %>%
  mutate(
    pitch_name = case_when(
      pitch_type == "FF" ~ "Four-Seam Fastball",
      pitch_type == "SL" ~ "Slider",
      pitch_type == "KC" ~ "Knuckle Curve",
      pitch_type == "CH" ~ "Changeup",
      pitch_type == "SI" ~ "Sinker",
      pitch_type == "FC" ~ "Cutter",
      TRUE ~ pitch_type
    )
  )

pitch_usage_chart <- ggplot(
  pitch_usage,
  aes(
    x = reorder(pitch_name, usage_pct),
    y = usage_pct
  )
) +
  geom_col(fill = "#003087", width = 0.7) +
  geom_text(
    aes(label = paste0(usage_pct, "%")),
    hjust = -0.15,
    size = 4
  ) +
  coord_flip() +
  labs(
    title = "Gerrit Cole 2026 Pitch Arsenal",
    subtitle = "Regular Season | Pitch Usage",
    x = NULL,
    y = "Pitch Usage (%)",
    caption = "Source: Baseball Savant / Statcast"
  ) +
  scale_y_continuous(
    limits = c(0, max(pitch_usage$usage_pct) + 10)
  ) +
  theme_minimal(base_size = 13)

print(pitch_usage_chart)

ggsave(
  "visualizations/Cole_2026_Pitch_Usage.png",
  plot = pitch_usage_chart,
  width = 10,
  height = 6,
  dpi = 300
)

# ==========================================
# Visualization 2: Average Pitch Velocity
# ==========================================

# Calculate average velocity by pitch type
pitch_velocity <- cole_clean %>%
  filter(
    !is.na(pitch_type),
    !is.na(release_speed)
  ) %>%
  group_by(pitch_type) %>%
  summarise(
    pitches = n(),
    avg_velocity = round(mean(release_speed), 1),
    .groups = "drop"
  ) %>%
  mutate(
    pitch_name = case_when(
      pitch_type == "FF" ~ "Four-Seam Fastball",
      pitch_type == "SL" ~ "Slider",
      pitch_type == "KC" ~ "Knuckle Curve",
      pitch_type == "CH" ~ "Changeup",
      pitch_type == "SI" ~ "Sinker",
      pitch_type == "FC" ~ "Cutter",
      TRUE ~ pitch_type
    )
  ) %>%
  arrange(desc(avg_velocity))

# View results
print(pitch_velocity)

# Create velocity chart
velocity_chart <- ggplot(
  pitch_velocity,
  aes(
    x = reorder(pitch_name, avg_velocity),
    y = avg_velocity
  )
) +
  geom_col(fill = "#003087", width = 0.7) +
  geom_text(
    aes(label = paste0(avg_velocity, " mph")),
    hjust = -0.1,
    size = 4
  ) +
  coord_flip() +
  scale_y_continuous(
    limits = c(0, max(pitch_velocity$avg_velocity) + 15)
  ) +
  labs(
    title = "Gerrit Cole 2026 Average Pitch Velocity",
    subtitle = "Regular Season | Average Velocity by Pitch Type",
    x = NULL,
    y = "Average Velocity (mph)",
    caption = "Source: Baseball Savant / Statcast"
  ) +
  theme_minimal(base_size = 13)

# Display chart
print(velocity_chart)

# Save chart
ggsave(
  "visualizations/Cole_2026_Pitch_Velocity.png",
  plot = velocity_chart,
  width = 10,
  height = 6,
  dpi = 300
)

# ==========================================
# Visualization 3: Pitch Movement
# ==========================================

# Prepare pitch movement data
pitch_movement <- cole_clean %>%
  filter(
    !is.na(pitch_type),
    !is.na(pfx_x),
    !is.na(pfx_z)
  ) %>%
  mutate(
    horizontal_break = -pfx_x * 12,
    vertical_break = pfx_z * 12,
    pitch_name = case_when(
      pitch_type == "FF" ~ "Four-Seam Fastball",
      pitch_type == "SL" ~ "Slider",
      pitch_type == "KC" ~ "Knuckle Curve",
      pitch_type == "CH" ~ "Changeup",
      pitch_type == "SI" ~ "Sinker",
      pitch_type == "FC" ~ "Cutter",
      TRUE ~ pitch_type
    )
  )

# Calculate average movement for each pitch
movement_summary <- pitch_movement %>%
  group_by(pitch_name) %>%
  summarise(
    pitches = n(),
    avg_horizontal = round(mean(horizontal_break), 1),
    avg_vertical = round(mean(vertical_break), 1),
    .groups = "drop"
  )

print(movement_summary)

# Create pitch movement chart
movement_chart <- ggplot(
  pitch_movement,
  aes(
    x = horizontal_break,
    y = vertical_break,
    color = pitch_name
  )
) +
  geom_point(alpha = 0.25, size = 1.5) +
  geom_point(
    data = movement_summary,
    aes(
      x = avg_horizontal,
      y = avg_vertical,
      color = pitch_name
    ),
    size = 5,
    shape = 18
  ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(
    title = "Gerrit Cole 2026 Pitch Movement",
    subtitle = "Regular Season | Horizontal vs. Vertical Movement",
    x = "Horizontal Break (inches)",
    y = "Vertical Break (inches)",
    color = "Pitch Type",
    caption = "Source: Baseball Savant / Statcast"
  ) +
  theme_minimal(base_size = 13)

# Display chart
print(movement_chart)

# Save chart
ggsave(
  "visualizations/Cole_2026_Pitch_Movement.png",
  plot = movement_chart,
  width = 10,
  height = 7,
  dpi = 300
)

# ==========================================
# Visualization 4: Whiff Rate by Pitch Type
# ==========================================

# Identify swings and misses
whiff_data <- cole_clean %>%
  filter(!is.na(pitch_type)) %>%
  mutate(
    is_whiff = description %in% c(
      "swinging_strike",
      "swinging_strike_blocked",
      "foul_tip"
    ),
    is_swing = description %in% c(
      "swinging_strike",
      "swinging_strike_blocked",
      "foul_tip",
      "foul",
      "foul_bunt",
      "hit_into_play",
      "missed_bunt"
    ),
    pitch_name = case_when(
      pitch_type == "FF" ~ "Four-Seam Fastball",
      pitch_type == "SL" ~ "Slider",
      pitch_type == "KC" ~ "Knuckle Curve",
      pitch_type == "CH" ~ "Changeup",
      pitch_type == "SI" ~ "Sinker",
      pitch_type == "FC" ~ "Cutter",
      TRUE ~ pitch_type
    )
  )

# Calculate whiff rate
whiff_summary <- whiff_data %>%
  group_by(pitch_name) %>%
  summarise(
    pitches = n(),
    swings = sum(is_swing),
    whiffs = sum(is_whiff),
    whiff_rate = if_else(
      swings > 0,
      round(whiffs / swings * 100, 1),
      NA_real_
    ),
    .groups = "drop"
  ) %>%
  arrange(desc(whiff_rate))

print(whiff_summary)

# Create whiff rate chart
whiff_chart <- whiff_summary %>%
  filter(!is.na(whiff_rate)) %>%
  ggplot(
    aes(
      x = reorder(pitch_name, whiff_rate),
      y = whiff_rate
    )
  ) +
  geom_col(fill = "#003087", width = 0.7) +
  geom_text(
    aes(label = paste0(whiff_rate, "%")),
    hjust = -0.15,
    size = 4
  ) +
  coord_flip() +
  scale_y_continuous(
    limits = c(
      0,
      max(whiff_summary$whiff_rate, na.rm = TRUE) + 10
    )
  ) +
  labs(
    title = "Gerrit Cole 2026 Whiff Rate by Pitch Type",
    subtitle = "Regular Season | Swing-and-Miss Effectiveness",
    x = NULL,
    y = "Whiff Rate (%)",
    caption = "Source: Baseball Savant / Statcast"
  ) +
  theme_minimal(base_size = 13)

# Display chart
print(whiff_chart)

# Save chart
ggsave(
  "visualizations/Cole_2026_Whiff_Rate.png",
  plot = whiff_chart,
  width = 10,
  height = 6,
  dpi = 300
)

# ==========================================
# Visualization 5: Whiff Rate by Handedness
# ==========================================

# Calculate whiff rates against LHB and RHB
handedness_summary <- whiff_data %>%
  filter(stand %in% c("L", "R")) %>%
  group_by(pitch_name, stand) %>%
  summarise(
    pitches = n(),
    swings = sum(is_swing),
    whiffs = sum(is_whiff),
    whiff_rate = if_else(
      swings > 0,
      round(whiffs / swings * 100, 1),
      NA_real_
    ),
    .groups = "drop"
  ) %>%
  mutate(
    batter_side = if_else(
      stand == "L",
      "Left-Handed Batters",
      "Right-Handed Batters"
    )
  )

print(handedness_summary)

# Create handedness comparison chart
handedness_chart <- handedness_summary %>%
  filter(!is.na(whiff_rate)) %>%
  ggplot(
    aes(
      x = pitch_name,
      y = whiff_rate,
      fill = batter_side
    )
  ) +
  geom_col(
    position = position_dodge(width = 0.8),
    width = 0.7
  ) +
  coord_flip() +
  labs(
    title = "Gerrit Cole 2026 Whiff Rate by Batter Handedness",
    subtitle = "Regular Season | Left-Handed vs. Right-Handed Batters",
    x = NULL,
    y = "Whiff Rate (%)",
    fill = "Batter Handedness",
    caption = "Source: Baseball Savant / Statcast"
  ) +
  theme_minimal(base_size = 13)

# Display chart
print(handedness_chart)

# Save chart
ggsave(
  "visualizations/Cole_2026_Whiff_Rate_Handedness.png",
  plot = handedness_chart,
  width = 11,
  height = 7,
  dpi = 300
)

# ==========================================
# Check Handedness Sample Sizes
# ==========================================

handedness_summary %>%
  select(
    pitch_name,
    batter_side,
    pitches,
    swings,
    whiffs,
    whiff_rate
  ) %>%
  arrange(pitch_name, batter_side) %>%
  print(n = Inf)

# ==========================================
# Visualization 6: Pitch Location Heatmaps
# ==========================================

# Prepare pitch location data
location_data <- cole_clean %>%
  filter(
    !is.na(plate_x),
    !is.na(plate_z),
    pitch_type %in% c("FF", "SL", "KC", "CH")
  ) %>%
  mutate(
    pitch_name = case_when(
      pitch_type == "FF" ~ "Four-Seam Fastball",
      pitch_type == "SL" ~ "Slider",
      pitch_type == "KC" ~ "Knuckle Curve",
      pitch_type == "CH" ~ "Changeup"
    )
  )

# Create pitch location heatmaps
location_chart <- ggplot(
  location_data,
  aes(x = plate_x, y = plate_z)
) +
  stat_density_2d(
    aes(fill = after_stat(level)),
    geom = "polygon",
    contour = TRUE,
    alpha = 0.8
  ) +
  scale_fill_gradient(
    low = "lightblue",
    high = "#003087"
  ) +
  annotate(
    "rect",
    xmin = -0.83,
    xmax = 0.83,
    ymin = 1.5,
    ymax = 3.5,
    fill = NA,
    color = "black",
    linewidth = 0.8
  ) +
  facet_wrap(~ pitch_name, ncol = 2) +
  coord_fixed(
    xlim = c(-2, 2),
    ylim = c(0, 5)
  ) +
  labs(
    title = "Gerrit Cole 2026 Pitch Location Heatmaps",
    subtitle = "Regular Season | Four Primary Pitch Types",
    x = "Horizontal Pitch Location (ft)",
    y = "Vertical Pitch Location (ft)",
    fill = "Density",
    caption = "Source: Baseball Savant / Statcast"
  ) +
  theme_minimal(base_size = 12)

# Display chart
print(location_chart)

# Save chart
ggsave(
  "visualizations/Cole_2026_Pitch_Locations.png",
  plot = location_chart,
  width = 11,
  height = 8,
  dpi = 300
)

# ==========================================
# Visualization 7: Whiff Rate by Location
# ==========================================

# Divide pitch locations into a 3x3 grid
location_whiff <- whiff_data %>%
  filter(
    pitch_type %in% c("FF", "SL", "KC", "CH"),
    !is.na(plate_x),
    !is.na(plate_z),
    is_swing
  ) %>%
  mutate(
    horizontal_zone = case_when(
      plate_x < -0.83 ~ "Outside Left",
      plate_x < -0.28 ~ "Left",
      plate_x <= 0.28 ~ "Middle",
      plate_x <= 0.83 ~ "Right",
      TRUE ~ "Outside Right"
    ),
    vertical_zone = case_when(
      plate_z < 1.5 ~ "Below Zone",
      plate_z < 2.17 ~ "Low",
      plate_z <= 2.83 ~ "Middle",
      plate_z <= 3.5 ~ "High",
      TRUE ~ "Above Zone"
    )
  )

# Calculate whiff rate for each location
location_whiff_summary <- location_whiff %>%
  group_by(
    pitch_name,
    horizontal_zone,
    vertical_zone
  ) %>%
  summarise(
    swings = n(),
    whiffs = sum(is_whiff),
    whiff_rate = round(whiffs / swings * 100, 1),
    .groups = "drop"
  ) %>%
  mutate(
    # Small samples will not receive a plotted rate
    plotted_rate = if_else(
      swings >= 10,
      whiff_rate,
      NA_real_
    ),
    horizontal_zone = factor(
      horizontal_zone,
      levels = c(
        "Outside Left", "Left", "Middle",
        "Right", "Outside Right"
      )
    ),
    vertical_zone = factor(
      vertical_zone,
      levels = c(
        "Below Zone", "Low", "Middle",
        "High", "Above Zone"
      )
    )
  )

# Create location effectiveness heatmap
location_whiff_chart <- ggplot(
  location_whiff_summary,
  aes(
    x = horizontal_zone,
    y = vertical_zone,
    fill = plotted_rate
  )
) +
  geom_tile(color = "white", linewidth = 0.7) +
  geom_text(
    aes(
      label = if_else(
        is.na(plotted_rate),
        "",
        paste0(round(plotted_rate), "%")
      )
    ),
    size = 3
  ) +
  facet_wrap(~ pitch_name, ncol = 2) +
  scale_fill_gradient(
    low = "lightblue",
    high = "#003087",
    na.value = "grey90"
  ) +
  labs(
    title = "Gerrit Cole 2026 Whiff Rate by Pitch Location",
    subtitle = "Regular Season | Minimum 10 Swings per Location",
    x = "Horizontal Location (Catcher's View)",
    y = "Vertical Location",
    fill = "Whiff Rate (%)",
    caption = "Source: Baseball Savant / Statcast"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    )
  )

# Display chart
print(location_whiff_chart)

# Save chart
ggsave(
  "visualizations/Cole_2026_Whiff_Rate_Location.png",
  plot = location_whiff_chart,
  width = 13,
  height = 9,
  dpi = 300
)

# ==========================================
# Visualization 8: Pitch Selection by Count
# ==========================================

count_data <- cole_clean %>%
  filter(
    !is.na(pitch_type),
    !is.na(balls),
    !is.na(strikes),
    balls >= 0,
    balls <= 3,
    strikes >= 0,
    strikes <= 2
  ) %>%
  mutate(
    count_group = case_when(
      strikes > balls ~ "Pitcher Ahead",
      balls > strikes ~ "Hitter Ahead",
      TRUE ~ "Even Count"
    ),
    pitch_name = case_when(
      pitch_type == "FF" ~ "Four-Seam Fastball",
      pitch_type == "SL" ~ "Slider",
      pitch_type == "KC" ~ "Knuckle Curve",
      pitch_type == "CH" ~ "Changeup",
      pitch_type == "SI" ~ "Sinker",
      pitch_type == "FC" ~ "Cutter",
      TRUE ~ pitch_type
    )
  )

# Calculate pitch usage by count
count_summary <- count_data %>%
  group_by(count_group, pitch_name) %>%
  summarise(
    pitches = n(),
    .groups = "drop"
  ) %>%
  group_by(count_group) %>%
  mutate(
    usage_pct = round(
      pitches / sum(pitches) * 100, 1
    )
  ) %>%
  ungroup()

print(count_summary, n = Inf)

# Create pitch selection chart
count_chart <- ggplot(
  count_summary,
  aes(
    x = count_group,
    y = usage_pct,
    fill = pitch_name
  )
) +
  geom_col(width = 0.7) +
  scale_x_discrete(
    limits = c(
      "Pitcher Ahead",
      "Even Count",
      "Hitter Ahead"
    )
  ) +
  labs(
    title = "Gerrit Cole 2026 Pitch Selection by Count",
    subtitle = "Regular Season | Pitch Usage by Count Situation",
    x = "Count Situation",
    y = "Pitch Usage (%)",
    fill = "Pitch Type",
    caption = "Source: Baseball Savant / Statcast"
  ) +
  theme_minimal(base_size = 13)

# Display chart
print(count_chart)

# Save chart
ggsave(
  "visualizations/Cole_2026_Pitch_Selection_Count.png",
  plot = count_chart,
  width = 11,
  height = 7,
  dpi = 300
)

# ==========================================
# Visualization 9: Pitch Selection by Exact Count
# ==========================================

# Create individual count labels
exact_count_data <- count_data %>%
  mutate(
    count = paste0(balls, "-", strikes)
  )

# Calculate pitch usage for each count
exact_count_summary <- exact_count_data %>%
  group_by(count, pitch_name) %>%
  summarise(
    pitches = n(),
    .groups = "drop"
  ) %>%
  group_by(count) %>%
  mutate(
    usage_pct = round(
      pitches / sum(pitches) * 100, 1
    )
  ) %>%
  ungroup() %>%
  mutate(
    count = factor(
      count,
      levels = c(
        "0-0", "0-1", "0-2",
        "1-0", "1-1", "1-2",
        "2-0", "2-1", "2-2",
        "3-0", "3-1", "3-2"
      )
    )
  )

# View exact count breakdown
print(exact_count_summary, n = Inf)

# Create visualization
exact_count_chart <- ggplot(
  exact_count_summary,
  aes(
    x = count,
    y = usage_pct,
    fill = pitch_name
  )
) +
  geom_col(width = 0.8) +
  labs(
    title = "Gerrit Cole 2026 Pitch Selection by Exact Count",
    subtitle = "Regular Season | Pitch Usage Across All 12 Counts",
    x = "Ball-Strike Count",
    y = "Pitch Usage (%)",
    fill = "Pitch Type",
    caption = "Source: Baseball Savant / Statcast"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    )
  )

# Display chart
print(exact_count_chart)

# Save visualization
ggsave(
  "visualizations/Cole_2026_Pitch_Selection_Exact_Count.png",
  plot = exact_count_chart,
  width = 12,
  height = 7,
  dpi = 300
)

# ==========================================
# Boston Red Sox 2026 Hitter Data
# ==========================================

# Download Statcast pitch-level data
mlb_2026 <- statcast_search(
  start_date = "2026-03-01",
  end_date = "2026-10-01",
  player_type = "batter"
)

# Keep pitches thrown to Boston hitters
boston_data <- mlb_2026 %>%
  filter(
    game_year == 2026,
    game_type == "R",
    home_team == "BOS" | away_team == "BOS"
  ) %>%
  filter(
    (home_team == "BOS" & inning_topbot == "Bot") |
      (away_team == "BOS" & inning_topbot == "Top")
  )

# Check the data
dim(boston_data)

# Count Boston hitters
n_distinct(boston_data$batter)

# Save Boston data
write_csv(
  boston_data,
  "data/Boston_2026_Statcast.csv"
)

# ==========================================
# Step 16: Complete Boston 2026 Statcast Data
# ==========================================

library(baseballr)
library(dplyr)
library(purrr)
library(lubridate)
library(readr)

# Create weekly date ranges
start_dates <- seq(
  as.Date("2026-03-01"),
  as.Date("2026-09-27"),
  by = "7 days"
)

# Download each week separately
boston_weeks <- map(
  start_dates,
  function(start_date) {
    
    end_date <- min(
      start_date + 6,
      as.Date("2026-10-01")
    )
    
    cat(
      "Downloading:",
      as.character(start_date),
      "to",
      as.character(end_date),
      "\n"
    )
    
    weekly_data <- tryCatch(
      statcast_search(
        start_date = as.character(start_date),
        end_date = as.character(end_date),
        player_type = "batter"
      ),
      error = function(e) {
        warning(
          "Download failed for ",
          start_date,
          ": ",
          conditionMessage(e)
        )
        NULL
      }
    )
    
    if (is.null(weekly_data)) {
      return(NULL)
    }
    
    weekly_data %>%
      filter(
        game_year == 2026,
        game_type == "R",
        (home_team == "BOS" & inning_topbot == "Bot") |
          (away_team == "BOS" & inning_topbot == "Top")
      )
  }
)

# Combine all downloaded weeks
boston_full <- bind_rows(boston_weeks) %>%
  distinct(game_pk, at_bat_number, pitch_number, .keep_all = TRUE)

# Check dataset size
dim(boston_full)

# Count unique hitters
n_distinct(boston_full$batter)

# Save completed dataset
write_csv(
  boston_full,
  "data/Boston_2026_Full_Statcast.csv"
)

# ==========================================
# Step 17: Validate Boston Statcast Data
# ==========================================

# Check number of downloaded weeks
length(boston_weeks)

# Check pitch counts by month
boston_full %>%
  mutate(month = format(as.Date(game_date), "%Y-%m")) %>%
  count(month) %>%
  print(n = Inf)

# Check number of Boston games
n_distinct(boston_full$game_pk)

# Check dates covered
range(as.Date(boston_full$game_date), na.rm = TRUE)

# Check for duplicate pitches
boston_full %>%
  count(game_pk, at_bat_number, pitch_number) %>%
  filter(n > 1)

# Check number of pitches per hitter
boston_full %>%
  count(batter, sort = TRUE) %>%
  print(n = Inf)

# ==========================================
# Final Boston Data Verification
# ==========================================

cat("Downloaded weeks:", length(boston_weeks), "\n")

cat("Total pitches:", nrow(boston_full), "\n")

cat("Boston games:",
    n_distinct(boston_full$game_pk), "\n")

cat("Unique hitters:",
    n_distinct(boston_full$batter), "\n")

cat("First date:",
    as.character(min(boston_full$game_date)), "\n")

cat("Last date:",
    as.character(max(boston_full$game_date)), "\n")

cat("Duplicate pitches:",
    boston_full %>%
      count(game_pk, at_bat_number, pitch_number) %>%
      filter(n > 1) %>%
      nrow(),
    "\n")

# Check the number of pitches downloaded each week
tibble(
  week_start = start_dates,
  pitches = sapply(boston_weeks, nrow)
) %>%
  print(n = Inf)

# ==========================================
# Visualization 10: Boston Hitters vs Pitch Types
# ==========================================

# Prepare Boston hitter data
boston_whiff <- boston_full %>%
  filter(
    pitch_type %in% c("FF", "SL", "KC", "CH"),
    !is.na(batter)
  ) %>%
  mutate(
    pitch_name = case_when(
      pitch_type == "FF" ~ "Four-Seam Fastball",
      pitch_type == "SL" ~ "Slider",
      pitch_type == "KC" ~ "Knuckle Curve",
      pitch_type == "CH" ~ "Changeup"
    ),
    is_swing = description %in% c(
      "swinging_strike",
      "swinging_strike_blocked",
      "foul",
      "foul_tip",
      "hit_into_play",
      "foul_bunt",
      "missed_bunt"
    ),
    is_whiff = description %in% c(
      "swinging_strike",
      "swinging_strike_blocked",
      "missed_bunt"
    )
  )

# Calculate whiff rate by hitter and pitch type
boston_whiff_summary <- boston_whiff %>%
  group_by(batter, pitch_name) %>%
  summarise(
    pitches = n(),
    swings = sum(is_swing, na.rm = TRUE),
    whiffs = sum(is_whiff, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    whiff_rate = if_else(
      swings > 0,
      round(whiffs / swings * 100, 1),
      NA_real_
    )
  )

# Select hitters with at least 150 total swings
qualified_hitters <- boston_whiff %>%
  group_by(batter) %>%
  summarise(
    total_swings = sum(is_swing, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(total_swings >= 150)

# Keep qualified hitters and pitch-type samples
boston_whiff_plot <- boston_whiff_summary %>%
  semi_join(qualified_hitters, by = "batter") %>%
  filter(swings >= 20)

# View results
print(
  boston_whiff_plot %>%
    arrange(desc(whiff_rate)),
  n = Inf
)

# Create heatmap
boston_whiff_chart <- ggplot(
  boston_whiff_plot,
  aes(
    x = pitch_name,
    y = factor(batter),
    fill = whiff_rate
  )
) +
  geom_tile(color = "white") +
  geom_text(
    aes(label = paste0(whiff_rate, "%")),
    size = 3
  ) +
  scale_fill_gradient(
    low = "lightblue",
    high = "#003087"
  ) +
  labs(
    title = "Boston Red Sox 2026 Whiff Rates by Pitch Type",
    subtitle = "Minimum 150 Total Swings and 20 Swings per Pitch Type",
    x = "Pitch Type",
    y = "Batter MLB ID",
    fill = "Whiff Rate (%)",
    caption = "Source: Baseball Savant / Statcast"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(
      angle = 30,
      hjust = 1
    )
  )

# Display chart
print(boston_whiff_chart)

# Save chart
ggsave(
  "visualizations/Boston_2026_Whiff_Rate_Pitch_Type.png",
  plot = boston_whiff_chart,
  width = 12,
  height = 9,
  dpi = 300
)

# ==========================================
# Step 20: Add Boston Hitter Names
# ==========================================

library(baseballr)
library(dplyr)
library(ggplot2)

# Get unique MLB player IDs
boston_player_ids <- unique(boston_whiff_plot$batter)

# Look up MLB player information
boston_player_names <- purrr::map_dfr(
  boston_player_ids,
  function(id) {
    tryCatch(
      {
        player <- mlb_people(person_ids = id)
        
        tibble(
          batter = as.numeric(id),
          player_name = player$fullName[1]
        )
      },
      error = function(e) {
        tibble(
          batter = as.numeric(id),
          player_name = as.character(id)
        )
      }
    )
  }
)

# Add player names to heatmap data
boston_whiff_plot <- boston_whiff_plot %>%
  left_join(
    boston_player_names,
    by = "batter"
  )

# Check names
boston_whiff_plot %>%
  select(player_name, pitch_name, swings, whiff_rate) %>%
  arrange(player_name, pitch_name) %>%
  print(n = Inf)

names(boston_whiff_plot)
names(boston_player_names)

View(boston_player_names)
boston_whiff_plot <- boston_whiff_plot %>%
  select(-any_of("player_name")) %>%
  left_join(
    boston_player_names,
    by = "batter"
  )

boston_whiff_plot %>%
  select(player_name, pitch_name, swings, whiff_rate) %>%
  print(n = Inf)

names(boston_player_names)
print(boston_player_names, n = Inf)

colnames(boston_player_names)
str(boston_player_names)

library(baseballr)
library(dplyr)

# Download MLB player information
player_lookup <- baseballr::chadwick_player_lu()

# Match Boston hitters to their names
boston_player_names <- boston_whiff_plot %>%
  distinct(batter) %>%
  left_join(
    player_lookup %>%
      transmute(
        batter = as.numeric(key_mlbam),
        player_name = paste(name_first, name_last)
      ),
    by = "batter"
  )

# Check player names
print(boston_player_names, n = Inf)

boston_whiff_plot <- boston_whiff_plot %>%
  select(-any_of("player_name")) %>%
  left_join(boston_player_names, by = "batter")

# Verify names
boston_whiff_plot %>%
  select(player_name, pitch_name, swings, whiff_rate) %>%
  print(n = Inf)

# ==========================================
# Updated Boston Red Sox Whiff Rate Heatmap
# ==========================================

boston_whiff_chart <- ggplot(
  boston_whiff_plot,
  aes(
    x = pitch_name,
    y = reorder(player_name, batter),
    fill = whiff_rate
  )
) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(
    aes(label = paste0(round(whiff_rate, 1), "%")),
    size = 3.5
  ) +
  scale_fill_gradient(
    low = "lightblue",
    high = "darkblue",
    name = "Whiff Rate (%)"
  ) +
  labs(
    title = "Boston Red Sox 2026 Whiff Rates by Pitch Type",
    subtitle = "Minimum 150 Total Swings and 20 Swings per Pitch Type",
    x = "Pitch Type",
    y = "Red Sox Hitter",
    caption = "Source: Baseball Savant / Statcast"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1)
  )

# Display updated chart
print(boston_whiff_chart)

# Save updated chart
ggsave(
  "visualizations/Boston_2026_Whiff_Rate_Pitch_Type.png",
  plot = boston_whiff_chart,
  width = 12,
  height = 9,
  dpi = 300
)

# ==========================================
# Visualization 10: Cole vs. Boston Matchups
# ==========================================

# Calculate Cole's whiff rate by pitch type
cole_matchup <- cole_clean %>%
  filter(!is.na(pitch_type)) %>%
  mutate(
    is_swing = description %in% c(
      "swinging_strike",
      "swinging_strike_blocked",
      "foul",
      "foul_tip",
      "hit_into_play",
      "hit_into_play_no_out",
      "hit_into_play_score",
      "foul_bunt",
      "missed_bunt"
    ),
    is_whiff = description %in% c(
      "swinging_strike",
      "swinging_strike_blocked",
      "missed_bunt"
    )
  ) %>%
  group_by(pitch_type) %>%
  summarise(
    cole_swings = sum(is_swing),
    cole_whiffs = sum(is_whiff),
    cole_whiff_rate = 100 * cole_whiffs / cole_swings,
    .groups = "drop"
  )

# Check Cole's results
print(cole_matchup)

# ==========================================
# Visualization 10: Cole vs. Boston Matchups
# ==========================================

library(dplyr)
library(ggplot2)

# Match pitch type codes to names
cole_matchup <- cole_matchup %>%
  mutate(
    pitch_name = case_when(
      pitch_type == "FF" ~ "Four-Seam Fastball",
      pitch_type == "SL" ~ "Slider",
      pitch_type == "CH" ~ "Changeup",
      pitch_type == "KC" ~ "Knuckle Curve",
      pitch_type == "SI" ~ "Sinker",
      pitch_type == "FC" ~ "Cutter",
      TRUE ~ pitch_type
    )
  )

# Combine Cole's whiff rates with Boston hitters
matchup_data <- boston_whiff_plot %>%
  inner_join(
    cole_matchup,
    by = "pitch_name"
  ) %>%
  filter(
    swings >= 20,
    cole_swings >= 30
  ) %>%
  mutate(
    matchup_score = (
      whiff_rate + cole_whiff_rate
    ) / 2
  )

# Check results
print(
  matchup_data %>%
    select(
      player_name,
      pitch_name,
      swings,
      whiff_rate,
      cole_whiff_rate,
      matchup_score
    ),
  n = Inf
)

# ==========================================
# Visualization 10: Cole vs. Red Sox Heatmap
# ==========================================

library(ggplot2)
library(dplyr)

# Create matchup heatmap
matchup_chart <- ggplot(
  matchup_data,
  aes(
    x = pitch_name,
    y = reorder(player_name, matchup_score),
    fill = matchup_score
  )
) +
  geom_tile(color = "white", linewidth = 0.8) +
  geom_text(
    aes(label = paste0(round(matchup_score, 1), "%")),
    size = 3.5
  ) +
  scale_fill_gradient(
    low = "lightblue",
    high = "darkblue",
    name = "Matchup Score (%)"
  ) +
  labs(
    title = "Gerrit Cole vs. Boston Red Sox: 2026 Pitch Matchups",
    subtitle = "Average of Cole's Pitch Whiff Rate and Boston Hitters' Whiff Rate",
    x = "Cole Pitch Type",
    y = "Boston Red Sox Hitter",
    caption = "Source: Baseball Savant / Statcast | Exploratory score, not a predicted whiff rate"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1),
    plot.title = element_text(face = "bold"),
    panel.grid = element_blank()
  )

# Display chart
print(matchup_chart)

# Save chart
ggsave(
  "visualizations/Cole_2026_Red_Sox_Matchup_Heatmap.png",
  plot = matchup_chart,
  width = 13,
  height = 9,
  dpi = 300
)

# ==========================================
# Top 5 Potential Pitch Matchups
# ==========================================

top_matchups <- matchup_data %>%
  arrange(desc(matchup_score)) %>%
  select(
    player_name,
    pitch_name,
    swings,
    whiff_rate,
    cole_swings,
    cole_whiff_rate,
    matchup_score
  ) %>%
  slice_head(n = 5)

print(top_matchups, n = Inf)

# ==========================================
# Save Cole vs. Boston Matchup Results
# ==========================================

# Save all matchup results
write.csv(
  matchup_data,
  "data/Cole_2026_Boston_Matchups.csv",
  row.names = FALSE
)

# Save top five matchups
write.csv(
  top_matchups,
  "data/Cole_2026_Top_5_Matchups.csv",
  row.names = FALSE
)

# Confirm files were saved
file.exists("data/Cole_2026_Boston_Matchups.csv")
file.exists("data/Cole_2026_Top_5_Matchups.csv")