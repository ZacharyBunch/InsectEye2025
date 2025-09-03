### Libraries ####
library(tidyverse)
library(readxl)
library(readr)
library(dplyr)
library(purrr)
library(stringr)
library(lubridate)
library(ggplot2)
library(tidyr)


### Load Harner A CSVs ####

folder_path_H_A <- "C:/Users/Zachary/OneDrive - The Pennsylvania State University/InsectEye Summer 2025 Pilot Data/Harner A"
files <- list.files(
  path = folder_path_H_A,
  pattern = "\\.csv$",
  full.names = TRUE,
  recursive = TRUE
)

print(files)  # Check that files are now detected

#Combine into dataframe

Harner_A_combined_data <- files %>%
  map_dfr(read_csv, .id = "source_file") %>%
  mutate(
    filename     = files[as.integer(source_file)],
    source_file  = basename(filename),
    date         = str_extract(source_file, "\\d{4}-\\d{2}-\\d{2}")
  )

Harner_A_combined_data <- Harner_A_combined_data %>%
  mutate(
    # Extract time portion from motionEvent
    time_str = str_extract(motionEvent, "(?<=T)\\d{6}"),
    
    # Extract hours, minutes, seconds
    hour = str_sub(time_str, 1, 2) %>% as.integer(),
    minute = str_sub(time_str, 3, 4) %>% as.integer(),
    second = str_sub(time_str, 5, 6) %>% as.integer(),
    
    # Combine with date to make a proper datetime
    datetime = ymd_hms(paste(date, hour, minute, second, sep = " "))
  )


Harner_A_Clean <- Harner_A_combined_data %>%
  mutate(site = "Harner A")

### Load Harner C CSVs ####

folder_path_H_C <- "C:/Users/Zachary/OneDrive - The Pennsylvania State University/InsectEye Summer 2025 Pilot Data/Harner C"

files_H_C <- list.files(
  path = folder_path_H_C,
  pattern = "\\.csv$",
  full.names = TRUE,
  recursive = TRUE
)

print(files_H_C)  # Check that files are now detected

# Combine into dataframe
Harner_C_combined_data <- files_H_C %>%
  map_dfr(read_csv, .id = "source_file") %>%
  mutate(
    filename     = files_H_C[as.integer(source_file)],
    source_file  = basename(filename),
    date         = str_extract(source_file, "\\d{4}-\\d{2}-\\d{2}")
  )

# Extract time components and create datetime
Harner_C_combined_data <- Harner_C_combined_data %>%
  mutate(
    time_str = str_extract(motionEvent, "(?<=T)\\d{6}"),
    hour = str_sub(time_str, 1, 2) %>% as.integer(),
    minute = str_sub(time_str, 3, 4) %>% as.integer(),
    second = str_sub(time_str, 5, 6) %>% as.integer(),
    datetime = ymd_hms(paste(date, hour, minute, second, sep = " "))
  )

# Add site column
Harner_C_Clean <- Harner_C_combined_data %>%
  mutate(site = "Harner C")


### Load Rock Springs A CSVs ####

folder_path_RS_A <- "C:/Users/Zachary/OneDrive - The Pennsylvania State University/InsectEye Summer 2025 Pilot Data/Rock Springs A"

files_RS_A <- list.files(
  path = folder_path_RS_A,
  pattern = "\\.csv$",
  full.names = TRUE,
  recursive = TRUE
)

print(files_RS_A)  # Confirm files detected

# Combine into dataframe
RockSprings_A_combined_data <- files_RS_A %>%
  map_dfr(read_csv, .id = "source_file") %>%
  mutate(
    filename     = files_RS_A[as.integer(source_file)],
    source_file  = basename(filename),
    date         = str_extract(source_file, "\\d{4}-\\d{2}-\\d{2}")
  )

# Extract time components and create datetime
RockSprings_A_combined_data <- RockSprings_A_combined_data %>%
  mutate(
    time_str = str_extract(motionEvent, "(?<=T)\\d{6}"),
    hour = str_sub(time_str, 1, 2) %>% as.integer(),
    minute = str_sub(time_str, 3, 4) %>% as.integer(),
    second = str_sub(time_str, 5, 6) %>% as.integer(),
    datetime = ymd_hms(paste(date, hour, minute, second, sep = " "))
  )

# Add site column
RockSprings_A_Clean <- RockSprings_A_combined_data %>%
  mutate(site = "Rock Springs A")

### Load Rock Springs B CSVs ####

folder_path_RS_B <- "C:/Users/Zachary/OneDrive - The Pennsylvania State University/InsectEye Summer 2025 Pilot Data/Rock Springs B"

files_RS_B <- list.files(
  path = folder_path_RS_B,
  pattern = "\\.csv$",
  full.names = TRUE,
  recursive = TRUE
)

print(files_RS_B)  # Confirm files detected

# Combine into dataframe
RockSprings_B_combined_data <- files_RS_B %>%
  map_dfr(read_csv, .id = "source_file") %>%
  mutate(
    filename     = files_RS_B[as.integer(source_file)],
    source_file  = basename(filename),
    date         = str_extract(source_file, "\\d{4}-\\d{2}-\\d{2}")
  )

# Extract time components and create datetime
RockSprings_B_combined_data <- RockSprings_B_combined_data %>%
  mutate(
    time_str = str_extract(motionEvent, "(?<=T)\\d{6}"),
    hour = str_sub(time_str, 1, 2) %>% as.integer(),
    minute = str_sub(time_str, 3, 4) %>% as.integer(),
    second = str_sub(time_str, 5, 6) %>% as.integer(),
    datetime = ymd_hms(paste(date, hour, minute, second, sep = " "))
  )

# Add site column
RockSprings_B_Clean <- RockSprings_B_combined_data %>%
  mutate(site = "Rock Springs B")


### Combine all sites & Data dropping ####
combined_all_sites <- bind_rows(
  Harner_A_Clean,
  Harner_C_Clean,
  RockSprings_A_Clean,
  RockSprings_B_Clean
)

### Combine all sites ####
combined_all_sites <- bind_rows(
  Harner_A_Clean,
  Harner_C_Clean,
  RockSprings_A_Clean,
  RockSprings_B_Clean
)

# Remove any row with >1 insect recorded for any order
combined_all_sites <- combined_all_sites %>%
  filter(
    Diptera     <= 1,
    Hymenoptera <= 1,
    Lepidoptera <= 1,
    Coleoptera  <= 1
  )

# Exclude all Lepidoptera observations on 2025-07-14 for Harner A
combined_all_sites <- combined_all_sites %>%
  filter(!(site == "Harner A" & date == "2025-07-14" & Lepidoptera > 0))

### Total Abundance Real-Time Graph ####
combined_all_sites <- combined_all_sites %>%
  mutate(total_abundance = Diptera + Hymenoptera + Lepidoptera + Coleoptera)

ggplot(combined_all_sites, aes(x = datetime, y = total_abundance, color = site)) +
  geom_line(stat = "summary", fun = sum, size = 1) +
  labs(
    title = "Total Insect Abundance Over Time",
    x = "Time",
    y = "Total Abundance",
    color = "Site"
  ) +
  theme_minimal() +
  scale_x_datetime(date_labels = "%b %d\n%H:%M", date_breaks = "12 hours") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top"
  )


### Total Abundance by Day ####
plot_data <- combined_all_sites %>%
  group_by(site, date) %>%
  summarise(total_abundance = sum(total_abundance), .groups = "drop")

ggplot(plot_data, aes(x = ymd(date), y = total_abundance, color = site, shape = site)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +  # increase size if needed for visibility
  labs(
    title = "Daily Total Insect Abundance",
    x = "Date",
    y = "Total Abundance",
    color = "Site",
    shape = "Site"
  ) +
  theme_minimal()


### Graph by Order Real-Time Graph ####
long_data <- combined_all_sites %>%
  pivot_longer(
    cols = c(Diptera, Hymenoptera, Lepidoptera, Coleoptera),
    names_to = "order",
    values_to = "abundance"
  )

ggplot(long_data, aes(x = datetime, y = abundance, color = site)) +
  geom_line(stat = "summary", fun = sum, size = 0.8) +
  facet_wrap(~ order, scales = "free_y") +
  labs(
    title = "Real-Time Abundance by Insect Order",
    x = "Time",
    y = "Abundance",
    color = "Site"
  ) +
  scale_x_datetime(date_labels = "%b %d\n%H:%M", date_breaks = "1 day") +
  theme_minimal() +
  theme(
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top"
  )


### Graph by Order by Day ####
long_data <- combined_all_sites %>%
  pivot_longer(
    cols = c(Diptera, Hymenoptera, Lepidoptera, Coleoptera),
    names_to = "order",
    values_to = "abundance"
  )

plot_orders <- long_data %>%
  group_by(site, date, order) %>%
  summarise(total_abundance = sum(abundance), .groups = "drop")

ggplot(plot_orders, aes(x = ymd(date), y = total_abundance, color = site, shape = site)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  facet_wrap(~ order, scales = "free_y") +
  labs(
    title = "Daily Abundance by Insect Order",
    x = "Date",
    y = "Total Abundance",
    color = "Site",
    shape = "Site"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


#### Corn Only By Day ####


# Summarize total abundance by date and site
daily_corn_total <- combined_all_sites %>%
  filter(site %in% c("Harner C", "Rock Springs A")) %>%
  mutate(
    date = as.Date(datetime),
    total_abundance = Diptera + Hymenoptera + Lepidoptera + Coleoptera
  ) %>%
  group_by(site, date) %>%
  summarise(total_abundance = sum(total_abundance, na.rm = TRUE), .groups = "drop")

# Plot total abundance by day
ggplot(daily_corn_total, aes(x = date, y = total_abundance, color = site)) +
  geom_line(size = 1) +
  geom_point(aes(shape = site), size = 2) +  # Add points with shape
  labs(
    title = "Daily Total Insect Abundance (Corn-Adjacent Sites)",
    x = "Date",
    y = "Total Abundance",
    color = "Site",
    shape = "Site"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top"
  )


# Summarize daily abundance by order
daily_corn_orders <- combined_all_sites %>%
  filter(site %in% c("Harner C", "Rock Springs A")) %>%
  mutate(date = as.Date(datetime)) %>%
  pivot_longer(
    cols = c(Diptera, Hymenoptera, Lepidoptera, Coleoptera),
    names_to = "order",
    values_to = "abundance"
  ) %>%
  group_by(site, date, order) %>%
  summarise(abundance = sum(abundance, na.rm = TRUE), .groups = "drop")

# Plot daily abundance by order
ggplot(daily_corn_orders, aes(x = date, y = abundance, color = site)) +
  geom_line(size = 0.9) +
  geom_point(aes(shape = site), size = 1.8) +  # Add points with shape
  facet_wrap(~ order, scales = "free_y") +
  labs(
    title = "Daily Insect Abundance by Order (Corn-Adjacent Sites)",
    x = "Date",
    y = "Abundance",
    color = "Site",
    shape = "Site"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top"
  )



### Adding weather data ####
library(lubridate)

# Read weather data
weather_data <- read_csv("weather_data.csv", skip = 2)

# clean up column names
colnames(weather_data) <- c("datetime", "air_temp", "humidity", "rain", "solar")
weather_data <- weather_data[, !sapply(weather_data, function(x) is.logical(x) && all(is.na(x)))]



library(dplyr)
library(lubridate)

# Round timestamps to 1hr without overwriting
combined_all_sites_rounded <- combined_all_sites %>%
  mutate(datetime_rounded = round_date(datetime, unit = "1 hours"))

weather_data_rounded <- weather_data %>%
  mutate(datetime_rounded = round_date(datetime, unit = "1 minutes"))

# Join (site remains intact in combined_all_sites)
combined_with_weather <- combined_all_sites_rounded %>%
  left_join(weather_data_rounded, by = "datetime_rounded")

#### Weather Heat Graphs ####

#Heat map total abundance#

library(dplyr)

heat_data <- combined_with_weather %>%
  mutate(hour = lubridate::hour(datetime_rounded)) %>%
  group_by(site, hour) %>%
  summarise(abundance = mean(total_abundance, na.rm = TRUE), .groups = "drop")

ggplot(heat_data, aes(x = hour, y = site, fill = abundance)) +
  geom_tile() +
  scale_fill_viridis_c() +
  labs(title = "Mean Abundance by Hour of Day", x = "Hour", y = "Site")



#### Abundance with weather data ####
# Multi-panel time series plot
# Insect abundance (hourly, all sites combined) + Weather
# Aligned exactly by date

library(tidyverse)
library(lubridate)
library(patchwork)
library(scales)

# Parameters:
# BIN        -> the unit of time to bin data (here "hour")
# X_BREAKS   -> spacing of tick marks on the x-axis (here "2 days")
# DATE_LABEL -> formatting for the x-axis labels
# TZ_USED    -> force everything into one consistent timezone
BIN        <- "hour"
X_BREAKS   <- "2 days"
DATE_LABEL <- "%b %d"
TZ_USED    <- "America/New_York"

# Helper function: creates a daily sequence of dates to use as vertical guides
daily_seq <- function(xmin, xmax) {
  seq(floor_date(xmin, "day"), ceiling_date(xmax, "day"), by = "1 day")
}


# Process insect abundance data


abund_hour <- combined_with_weather %>%
  mutate(
    # Use the rounded datetime already created, force into a consistent timezone
    datetime = with_tz(datetime_rounded, TZ_USED),
    # Round each timestamp to the nearest BIN (hour)
    hour     = floor_date(datetime, BIN),
    # Create a total abundance measure by summing across the insect orders
    total_abundance = Diptera + Hymenoptera + Lepidoptera + Coleoptera
  ) %>%
  filter(!is.na(hour)) %>%
  group_by(hour) %>%
  # Collapse down to one row per hour across all sites
  summarise(total_abundance = sum(total_abundance, na.rm = TRUE), .groups = "drop")


# Process weather data

weather_hour <- weather_data_rounded %>%
  transmute(
    # Again use the rounded datetime and force into consistent timezone
    datetime = with_tz(datetime_rounded, TZ_USED),
    # Bin into hours
    hour     = floor_date(datetime, BIN),
    air_temp, humidity, rain, solar
  ) %>%
  filter(!is.na(hour)) %>%
  group_by(hour) %>%
  summarise(
    # Average values for continuous variables
    air_temp = mean(air_temp, na.rm = TRUE),
    humidity = mean(humidity, na.rm = TRUE),
    solar    = mean(solar, na.rm = TRUE),
    # Sum rainfall within each hour (if rain is a rate instead of accumulation, switch to mean)
    rain     = sum(rain, na.rm = TRUE),
    .groups  = "drop"
  )


# Define global x-axis limits
# Ensures both panels use the same time range


x_min <- min(min(abund_hour$hour, na.rm = TRUE), min(weather_hour$hour, na.rm = TRUE))
x_max <- max(max(abund_hour$hour, na.rm = TRUE), max(weather_hour$hour, na.rm = TRUE))
x_limits <- c(x_min, x_max)

# Shared daily lines (vertical lines at midnight)
daily_lines <- tibble(day = daily_seq(x_min, x_max))

# Abundance plot


p_abund <- ggplot(abund_hour, aes(x = hour, y = total_abundance)) +
  # Add vertical guides at midnight
  geom_vline(data = daily_lines, aes(xintercept = day), color = "grey88", linewidth = 0.3) +
  # Show abundance as hourly bars
  geom_col(width = 3600, alpha = 0.9) +   # width = 3600 seconds (1 hour)
  labs(
    title = "Insect abundance (hourly, all sites combined)",
    x = NULL,  # no x-axis label (to avoid duplication)
    y = "Total abundance"
  ) +
  # Force same x-axis range across plots, add 2-day ticks
  scale_x_datetime(
    limits = x_limits,
    breaks = date_breaks(X_BREAKS),
    labels = date_format(DATE_LABEL),
    expand = expansion(mult = 0)   # no padding so both panels line up exactly
  ) +
  # Keep bars anchored at zero
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),   # remove extra grid clutter
    panel.grid.major.x = element_blank()  # hide vertical grid lines (we use vlines instead)
  )

# Weather plot

# Convert to long format for faceting (one column = variable name, one = value)
weather_long <- weather_hour %>%
  pivot_longer(
    cols = c(air_temp, humidity, rain, solar),
    names_to = "variable",
    values_to = "value"
  )

p_weather <- ggplot(weather_long, aes(x = hour, y = value)) +
  # Same daily vertical guides
  geom_vline(data = daily_lines, aes(xintercept = day), color = "grey92", linewidth = 0.25) +
  # Continuous variables as lines
  geom_line(
    data = subset(weather_long, variable %in% c("air_temp", "humidity", "solar")),
    linewidth = 0.5, na.rm = TRUE
  ) +
  # Rainfall as hourly bars
  geom_col(
    data = subset(weather_long, variable == "rain"),
    width = 3600, alpha = 0.85
  ) +
  # Facet into separate panels for each weather variable
  facet_wrap(
    ~ variable, ncol = 1, scales = "free_y",
    labeller = as_labeller(c(
      air_temp = "Air temperature (F)",
      humidity = "Humidity (%)",
      rain     = "Rain (in)",
      solar    = "Solar (W/m^2)"
    ))
  ) +
  labs(
    title = NULL,     # no extra title (to save space)
    x = "Time",       # single x label for the bottom panel
    y = NULL
  ) +
  scale_x_datetime(
    limits = x_limits,
    breaks = date_breaks(X_BREAKS),
    labels = date_format(DATE_LABEL),
    expand = expansion(mult = 0)   # match abundance plot exactly
  ) +
  theme_minimal(base_size = 12) +
  theme(
    strip.background = element_blank(),      # cleaner facet strips
    strip.text = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.spacing.y = unit(6, "pt")          # reduce vertical space between weather facets
  )

# Combine panels

final_plot <- (p_abund / p_weather) +
  plot_layout(heights = c(1, 1.35)) &   # give weather more space
  theme(plot.margin = margin(t = 5, r = 10, b = 5, l = 5))

# Print final figure
final_plot

#### Diel Activity ####
# Diel activity curves for four focal orders
# Y = number of positive occurrences (Count > 0) per hour

library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)

orders <- c("Diptera","Hymenoptera","Lepidoptera","Coleoptera")

df_hour <- combined_with_weather %>%
  mutate(hour = lubridate::hour(datetime_rounded)) %>%
  pivot_longer(cols = all_of(orders), names_to = "Order", values_to = "Count") %>%
  group_by(Order, hour) %>%
  summarise(
    n_pos = sum(Count > 0, na.rm = TRUE),   # number of positive detections
    .groups = "drop"
  ) %>%
  complete(Order, hour = 0:23, fill = list(n_pos = 0)) %>%
  arrange(Order, hour)

# Four-line plot exact data
p_four_orders <- ggplot(df_hour, aes(x = hour, y = n_pos, color = Order, linetype = Order)) +
  geom_line(size = 1) +
  scale_x_continuous(breaks = seq(0, 24, 3), limits = c(0, 24)) +
  labs(
    title = "Detections by hour (four focal orders)",
    x = "Time (h)",
    y = "Number of positive detections"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "right")

p_four_orders

# Smoothed multi-order plot
p_four_orders_smooth <- ggplot(df_hour, aes(x = hour, y = n_pos, color = Order, linetype = Order)) +
#  geom_point(alpha = 0.4, size = 1.5) +   # show raw binned values
  geom_smooth(se = FALSE, method = "loess", span = 0.5, size = 1) +
  scale_x_continuous(breaks = seq(0, 24, 3), limits = c(0, 24)) +
  labs(
    title = "Smoothed diel activity (four focal orders)",
    x = "Time (h)",
    y = "Number of positive detections"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "right")

p_four_orders_smooth

#### Analysis ####
library(MASS)
library(MASS)
library(DHARMa)
library(lme4)
library(glmmTMB)
library(mgcv)


# GAM

combined_with_weather$site <- droplevels(factor(combined_with_weather$site))

# Won't run with rain
gam_pois <- gam(total_abundance ~ 
                  s(air_temp) + 
                  s(humidity) + 
                  s(solar) +
                  s(site, bs = "re"), #site as random effect
                family = poisson,
                data = combined_with_weather)


gam_nb <- gam(
  total_abundance ~ 
    s(air_temp) +
    s(humidity) +
    s(solar) +
    s(site, bs = "re"),
  family = nb(),     # negative binomial with log link
  method  = "REML",
  data    = combined_with_weather,
  na.action = na.omit
)

# Summary of the GAM
summary(gam_pois)



#AIC
AIC(gam_nb, gam_pois)


#### GAMs For Taxa ####
# Packages
library(tidyverse)
library(mgcv)

df <- combined_with_weather


taxa_cols <- c("Diptera", "Hymenoptera", "Lepidoptera", "Coleoptera")
taxa_cols <- intersect(names(df), taxa_cols)  # keep only those that exist
long_df <- df %>%
  pivot_longer(
    cols = all_of(taxa_cols),
    names_to = "taxa",
    values_to = "count"
  ) %>%
  mutate(
    taxa = factor(taxa),
    site = factor(site),
    count = as.integer(count)
  ) %>%
  filter(!is.na(count), count > 0)

library(tidyr)
library(parallel)

orders <- c("Diptera","Hymenoptera","Lepidoptera","Coleoptera")

long_hour <- combined_with_weather %>%
  pivot_longer(all_of(orders), names_to = "taxa", values_to = "count") %>%
  mutate(
    taxa = factor(taxa),
    site = factor(site)
  ) %>%
  group_by(site, taxa, datetime_rounded) %>%
  summarise(
    count    = sum(count, na.rm = TRUE),     # now can be 0, 1, 2, ...
    air_temp = mean(air_temp, na.rm = TRUE),
    humidity = mean(humidity, na.rm = TRUE),
    solar    = mean(solar,    na.rm = TRUE),
    rain     = sum(rain,      na.rm = TRUE), # if rain is accumulative per record
    .groups  = "drop"
  ) %>%
  filter(is.finite(air_temp), is.finite(humidity), is.finite(solar)) %>%
  filter(!is.na(count), count > 0)

# sanity check that we have variation again
table(long_hour$count)


# long_df <- df %>%
#   pivot_longer(
#     cols = all_of(taxa_cols),
#     names_to = "taxa",
#     values_to = "count"
#   ) %>%
#   mutate(
#     taxa = factor(taxa),
#     site = factor(site),
#     count = as.integer(count)
#   ) %>%
#   filter(!is.na(count), count > 0)

# 2) GAM with factor-smooth interactions
m_gam_pois <- gam(
  count ~ taxa +
    s(air_temp, by = taxa) +
    s(solar,    by = taxa) +
    s(humidity, by = taxa) +
    s(site, bs = "re"),
  data   = long_hour,
  family = poisson(link = "log"),
  method = "REML",
  select = TRUE
)

summary(m_gam_pois)
gam.check(m_gam_pois)

