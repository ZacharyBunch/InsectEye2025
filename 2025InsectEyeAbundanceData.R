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



library(dplyr)
library(lubridate)



# define daytime window
daytime_hours <- 1:24

# average and total abundance across all sites, excluding zero counts
avg_total_by_hour_daytime_all_zero <- combined_all_sites %>%
  mutate(date = as.Date(datetime)) %>%
  filter(hour %in% daytime_hours, total_abundance > 0) %>%   # exclude zeros
  group_by(hour) %>%
  summarise(
    mean_total_abundance = mean(total_abundance, na.rm = TRUE),
    total_abundance_hour = sum(total_abundance, na.rm = TRUE),
    .groups = "drop"
  )

avg_total_by_hour_daytime_all_zero

# step 2: overall average of the per-hour means
grand_mean <- avg_total_by_hour_daytime_all_zero %>%
  summarise(avg_of_hourly_means = mean(mean_total_abundance, na.rm = TRUE))


grand_mean

library(writexl)

write_xlsx(avg_total_by_hour_daytime_all_zero,
           path = "avg_total_by_hour_daytime_all_zero.xlsx")

library(dplyr)
library(lubridate)
library(writexl)

daytime_hours <- 1:24

# average and total abundance per site and hour, excluding zeros
avg_total_by_hour_daytime_site_zero <- combined_all_sites %>%
  mutate(date = as.Date(datetime)) %>%
  filter(hour %in% daytime_hours, total_abundance > 0) %>%
  group_by(site, hour) %>%
  summarise(
    mean_total_abundance = mean(total_abundance, na.rm = TRUE),
    total_abundance_hour = sum(total_abundance, na.rm = TRUE),
    .groups = "drop"
  )

# overall average of the per-hour means, per site
grand_mean_site <- avg_total_by_hour_daytime_site_zero %>%
  group_by(site) %>%
  summarise(avg_of_hourly_means = mean(mean_total_abundance, na.rm = TRUE),
            .groups = "drop")

# save both tables into one Excel file with two sheets
write_xlsx(
  list(
    "hourly_by_site" = avg_total_by_hour_daytime_site_zero,
    "grand_mean_by_site" = grand_mean_site
  ),
  path = "avg_total_by_hour_daytime_site_zero.xlsx"
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
library(lubridate)

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

#july data frame
long_hour_july <- combined_with_weather %>%
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
  filter(!is.na(count), count > 0) %>% 
  filter(lubridate::month(datetime_rounded) == 7)

#August data frame
long_hour_august <- combined_with_weather %>%
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
  filter(!is.na(count), count > 0) %>% 
  filter(lubridate::month(datetime_rounded) == 8)
  

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


#### GLM OVerall ####
# 
# library(MASS)
# library(dplyr)
# 
# # quick Poisson model
# library(MASS)
# 
# m1 <- glm.nb(
#   count ~ taxa:scale(air_temp) + 
#     taxa:scale(humidity) + 
#     taxa:scale(solar) +
#   taxa:scale(rain),
#   data = long_hour
# )



# test overdispersion
overdispersion <- sum(residuals(m1, type = "pearson")^2) / m1$df.residual
overdispersion

#Regression model

library(ggeffects)
# 
# pred_hum <- ggpredict(m1, terms = c("humidity [all]", "taxa"))
# plot(pred_hum)
# 
# pred_rain <- ggpredict(m1, terms = c("rain [all]", "taxa"))
# 
# plot(pred_rain)
# 
# 
# pred_air <- ggpredict(m1, terms = c("air_temp [all]", "taxa"))
# 
# plot(pred_air)
# 


# 
# #### GLM July ####
m_gam_pois <- gam(
  count ~ taxa +
    s(air_temp, by = taxa) +
    s(solar,    by = taxa) +
    s(humidity, by = taxa) +
    #   s(rain, by = taxa),
    s(site, bs = "re"),
  data   = long_hour,
  family = poisson(link = "log"),
  method = "REML",
  select = TRUE
)

summary(m_gam_pois)
gam.check(m_gam_pois)

pred_air <- ggpredict(m_gam_pois, terms = c("air_temp [all]", "taxa"))

plot(pred_air)

pred_humidity <- ggpredict(m_gam_pois, terms = c("humidity [all]", "taxa"))

plot(pred_humidity)

pred_solar <- ggpredict(m_gam_pois, terms = c("solar [all]", "taxa"))
plot(pred_solar)


m_gam_pois_july <- gam(
  count ~ taxa +
    s(air_temp, by = taxa) +
    s(solar,    by = taxa) +
    s(humidity, by = taxa) +
    #   s(rain, by = taxa),
    s(site, bs = "re"),
  data   = long_hour_july,
  family = poisson(link = "log"),
  method = "REML",
  select = TRUE
)


summary(m_gam_pois_july)

pred_air_july <- ggpredict(m_gam_pois_july, terms = c("air_temp [all]", "taxa"))

plot(pred_air_july)

pred_humidity_july <- ggpredict(m_gam_pois, terms = c("humidity [all]", "taxa"))

plot(pred_humidity_july)

library(mgcv)
library(ggeffects)

#### GLM August ####
m_gam_pois_august <- gam(
  count ~ taxa +
    s(air_temp, by = taxa) +
    s(solar,    by = taxa) +
    s(humidity, by = taxa) +
    s(site, bs = "re"),
  data   = long_hour_august,
  family = poisson(link = "log"),
  method = "REML",
  select = TRUE
)

summary(m_gam_pois_august)

# Predictions
pred_air_august <- ggpredict(m_gam_pois_august, terms = c("air_temp [all]", "taxa"))
plot(pred_air_august)

pred_solar_august <- ggpredict(m_gam_pois_august, terms = c("solar [all]", "taxa"))
plot(pred_solar_august)

pred_humidity_august <- ggpredict(m_gam_pois_august, terms = c("humidity [all]", "taxa"))
plot(pred_humidity_august)



#### Count Data ####

library(dplyr)

# Total counts per taxon
total_counts <- long_hour %>%
  group_by(taxa) %>%
  summarise(
    total_count = sum(count, na.rm = TRUE),
  ) %>%
  arrange(desc(total_count))


total_counts


library(dplyr)
library(lubridate)

# Summarize counts per taxon for July and August
counts_july_aug <- long_hour %>%
  mutate(month = month(datetime_rounded, label = TRUE)) %>%  # use the correct column name
  filter(month %in% c("Jul", "Aug")) %>%
  group_by(taxa, month) %>%
  summarise(
    total_count = sum(count, na.rm = TRUE),
  ) %>%
  arrange(desc(total_count))

counts_july_aug


#### Uptime ####

library(dplyr)
library(lubridate)
library(tidyr)
library(stringr)

# INPUTS
# Data frame must have:
#   - machine ID column named `site`  (rename below if needed)
#   - timestamp column named `datetime_rounded` (POSIXct or character parsable to POSIXct)
#   - any other columns are fine
df <- long_hour

# Ensure datetime_rounded is POSIXct
if (!inherits(df$datetime_rounded, "POSIXt")) {
  df <- df %>%
    mutate(datetime_rounded = ymd_hms(as.character(datetime_rounded), quiet = TRUE))
}

# Extract date and year
df_dates <- df %>%
  mutate(
    date = as.Date(datetime_rounded),
    year = year(date),
    month_num = month(date),
    day_num = day(date)
  ) %>%
  filter(month_num %in% c(7, 8, 9)) %>%
  # Keep September only through the 15th
  filter(!(month_num == 9 & day_num > 15))

# Build a complete calendar grid: every site x every date in Jul 1 to Sep 15 for each year present
years_in_scope <- df_dates %>% distinct(year) %>% pull(year)

calendar_grid <- lapply(
  years_in_scope,
  function(y) tibble(date = seq.Date(as.Date(paste0(y, "-07-01")),
                                     as.Date(paste0(y, "-09-15")),
                                     by = "day"),
                     year = y)
) %>%
  bind_rows()

sites <- df %>% distinct(site)

full_grid <- sites %>%
  crossing(calendar_grid)

# Mark presence per site per date
presence <- df_dates %>%
  group_by(site, date, year) %>%
  summarise(present = n() > 0, .groups = "drop")

# Join to the full calendar so missing days become FALSE
uptime_daily <- full_grid %>%
  left_join(presence, by = c("site", "date", "year")) %>%
  mutate(present = if_else(is.na(present), FALSE, present))


# Overall uptime by site across all years combined
uptime_overall_by_site <- uptime_daily %>%
  group_by(site) %>%
  summarise(
    up_days = sum(present),
    total_days = n(),
    uptime_pct = 100 * up_days / total_days,
    .groups = "drop"
  ) %>%
  arrange(desc(uptime_pct))

uptime_overall_by_site


#### JUly, aug, sept uptime ####
library(dplyr)
library(lubridate)
library(tidyr)

# Ensure datetime_rounded is POSIXct
if (!inherits(long_hour$datetime_rounded, "POSIXt")) {
  long_hour <- long_hour %>%
    mutate(datetime_rounded = ymd_hms(as.character(datetime_rounded), quiet = TRUE))
}

# Extract date and month
df_dates <- long_hour %>%
  mutate(
    date = as.Date(datetime_rounded),
    year = year(date),
    month_num = month(date),
    day_num = day(date)
  )

# Filter for July, August, September (1-15)
df_dates <- df_dates %>%
  filter(month_num %in% c(7, 8, 9)) %>%
  filter(!(month_num == 9 & day_num > 15))

# Identify all years and sites
years_in_scope <- unique(df_dates$year)
sites <- df_dates %>% distinct(site)

# Helper function for uptime calculation by month
calc_uptime_month <- function(m) {
  cal_grid <- lapply(
    years_in_scope,
    function(y) tibble(date = seq.Date(
      as.Date(paste0(y, "-", sprintf("%02d", m), "-01")),
      if (m == 9) as.Date(paste0(y, "-", sprintf("%02d", m), "-15")) else as.Date(paste0(y, "-", sprintf("%02d", m), "-", days_in_month(as.Date(paste0(y, "-", m, "-01"))))),
      by = "day"
    ),
    year = y)
  ) %>% bind_rows()
  
  full_grid <- sites %>%
    crossing(cal_grid)
  
  presence <- df_dates %>%
    filter(month_num == m) %>%
    group_by(site, date, year) %>%
    summarise(present = n() > 0, .groups = "drop")
  
  uptime_daily <- full_grid %>%
    left_join(presence, by = c("site", "date", "year")) %>%
    mutate(present = if_else(is.na(present), FALSE, present))
  
  uptime_month <- uptime_daily %>%
    group_by(site, year) %>%
    summarise(
      up_days = sum(present),
      total_days = n(),
      uptime_pct = 100 * up_days / total_days,
      .groups = "drop"
    ) %>%
    mutate(month = month.name[m])
  
  uptime_month
}

# Run for July, August, September
uptime_july <- calc_uptime_month(7)
uptime_august <- calc_uptime_month(8)
uptime_september <- calc_uptime_month(9)

# Combine results
uptime_all <- bind_rows(uptime_july, uptime_august, uptime_september)

uptime_all

#### New weather graph ####


Long_abundance_climate <- combined_with_weather %>% 
  group_by()

library(dplyr)

library(dplyr)

summed_by_time_site_order <- combined_with_weather %>%
  group_by(datetime_rounded, date, site) %>%
  summarise(
    Diptera = sum(Diptera, na.rm = TRUE),
    Hymenoptera = sum(Hymenoptera, na.rm = TRUE),
    Lepidoptera = sum(Lepidoptera, na.rm = TRUE),
    Coleoptera = sum(Coleoptera, na.rm = TRUE),
    total_abundance = sum(total_abundance, na.rm = TRUE),
    air_temp = mean(air_temp, na.rm = TRUE),
    humidity = mean(humidity, na.rm = TRUE),
    solar = mean(solar, na.rm = TRUE),
    rain = sum(rain, na.rm = TRUE)
  ) %>%
  ungroup()

head(summed_by_time_site_order)

library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)

# 1) Aggregate to date x hour x site and summarize weather
by_date_hour <- combined_with_weather %>%
  mutate(
    date = as.Date(date),
    hour = hour(datetime_rounded)
  ) %>%
  group_by(site, date, hour) %>%
  summarise(
    total_abundance = sum(total_abundance, na.rm = TRUE),
    air_temp = mean(air_temp, na.rm = TRUE),
    humidity = mean(humidity, na.rm = TRUE),
    solar = mean(solar, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(hour = factor(hour, levels = 0:23))
view(by_date)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)

# 1) Aggregate across all sites to date x hour
agg_all <- combined_with_weather %>%
  mutate(
    date = as.Date(date),
    hour = hour(datetime_rounded)
  ) %>%
  group_by(date, hour) %>%
  summarise(
    total_abundance = sum(total_abundance, na.rm = TRUE),
    air_temp  = mean(air_temp,  na.rm = TRUE),
    humidity  = mean(humidity,  na.rm = TRUE),
    solar     = mean(solar,     na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(hour = factor(hour, levels = 0:23))

# 2) Scale weather lines to overlay on the abundance axis
max_abund <- max(agg_all$total_abundance, na.rm = TRUE)

weather_long_all <- agg_all %>%
  dplyr::select(date, hour, air_temp, humidity, solar) %>%
  tidyr::pivot_longer(cols = c(air_temp, humidity, solar),
                      names_to = "variable", values_to = "value") %>%
  group_by(variable) %>%
  mutate(
    v_min = min(value, na.rm = TRUE),
    v_max = max(value, na.rm = TRUE),
    v_scaled01 = ifelse(v_max > v_min, (value - v_min) / (v_max - v_min), 0)
  ) %>%
  ungroup() %>%
  mutate(y_overlay = v_scaled01 * max_abund * 0.95)

# 3) Plot with smoothed weather overlays
ggplot() +
  geom_col(
    data = agg_all,
    aes(x = date, y = total_abundance,fill = hour),
    position = position_dodge(width = 0.4),
    width = 0.4) +
  geom_smooth(
    data = weather_long_all,
    aes(x = date, y = y_overlay, color = variable),
    se = FALSE,
    method = "loess",
    span = 0.3,
    linewidth = 0.9
  )
  labs(
    x = "Date",
    y = "Total abundance (all sites summed)",
    title = "Daily abundance by hour with smoothed weather overlays (all sites combined)",
    subtitle = "Weather lines are scaled within variable to the global max abundance"
  ) +
  scale_x_date(date_breaks = "7 days", date_labels = "%b %d") +
  guides(fill = guide_legend(title = "Hour"), color = guide_legend(title = "Weather")) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0.5)
  )



#### New Weather Graph 2 ####

#### Zachary Weather Graph ####
  # ONE WEEK: hourly insect bars + raw weather lines (no binning, no smoothing)
  # Weather is scaled per day to the insect axis. All sites combined.
  # Adds a vertical line at local noon for each day and removes day shading.
  
  library(tidyverse)
  library(lubridate)
  library(scales)
  
  day_start <- 7
  day_end   <- 19
  
  # Pick any date inside the target week
  week_date  <- as.Date("2025-08-12")   # change as needed
  week_start <- floor_date(week_date, "week", week_start = 1)
  week_end   <- week_start + days(6)
  
  # ============================================
  # Hourly insect bars + weather lines (week)
  # Weather minutes interpolated on a single-week grid,
  # then scaled PER DAY to the insect axis.
  # --------------------------------------------
  # Assumes you already have:
  #   combined_all_sites : data.frame with a POSIXct 'datetime' and insect columns
  #   weather_data       : data.frame with a POSIXct 'datetime' and columns
  #                        air_temp, humidity, solar (numeric/coercible)
  # ============================================
  
  suppressPackageStartupMessages({
    library(tidyverse)
    library(lubridate)
    library(scales)
    library(zoo)
    library(purrr)
  })
  
  # ----------------- params -------------------
  day_start <- 0                         # start hour (inclusive)
  day_end   <- 24                        # end hour (exclusive)
  week_date  <- as.Date("2025-07-14")    # any date inside the target ISO week
  week_start <- floor_date(week_date, "week", week_start = 1) # Monday
  week_end   <- week_start + days(6)
  
  # ----------------- insect counts -----------
  possible_insect_cols <- c("total_abundance","CountOfInsect","insect_count","Count")
  have_insect <- intersect(possible_insect_cols, names(combined_all_sites))
  
  if (length(have_insect) == 1) {
    combined_counts <- combined_all_sites %>%
      mutate(insect_raw = .data[[have_insect]])
  } else {
    exclude_cols <- c("source_file","motionEvent","filename","site","date","time_str",
                      "hour","minute","second","datetime")
    num_cols  <- names(combined_all_sites)[vapply(combined_all_sites, is.numeric, logical(1))]
    taxa_cols <- setdiff(num_cols, intersect(num_cols, exclude_cols))
    stopifnot(length(taxa_cols) > 0)
    combined_counts <- combined_all_sites %>%
      mutate(insect_raw = rowSums(across(all_of(taxa_cols)), na.rm = TRUE))
  }
  
  counts_hr_raw <- combined_counts %>%
    mutate(hour_bin = floor_date(datetime, "hour")) %>%
    group_by(hour_bin) %>%
    summarise(insect_count = sum(insect_raw, na.rm = TRUE), .groups = "drop")
  
  grid_hours <- tibble(
    hour_bin = seq(as_datetime(week_start) + hours(day_start),
                   as_datetime(week_end)   + hours(day_end),
                   by = "1 hour")
  ) %>%
    filter(hour(hour_bin) >= day_start, hour(hour_bin) < day_end)
  
  counts_hr <- grid_hours %>%
    left_join(counts_hr_raw, by = "hour_bin") %>%
    mutate(insect_count = replace_na(insect_count, 0))
  
  # ----------------- weather (continuous week grid + interpolation) -----------
  wx_raw <- weather_data %>%
    filter(datetime >= as_datetime(week_start),
           datetime <  as_datetime(week_end) + days(1)) %>%
    transmute(
      datetime = as_datetime(datetime),
      temp     = readr::parse_number(as.character(air_temp)),
      humidity = readr::parse_number(as.character(humidity)),
      solar    = readr::parse_number(as.character(solar))
    ) %>%
    arrange(datetime)
  
  tz_str <- tz(wx_raw$datetime[1]); if (is.na(tz_str) || tz_str == "") tz_str <- "UTC"
  
  # One continuous minute grid from 00:00 Mon -> 00:00 next Mon (includes 24:00 of Sun)
  grid_all <- tibble(
    datetime = seq(as.POSIXct(week_start, tz = tz_str),
                   as.POSIXct(week_end + days(1), tz = tz_str),
                   by = "1 min")
  )
  
  # Collapse raw to unique minutes (if multiple per minute, average)
  wx_min <- wx_raw %>%
    mutate(datetime = floor_date(datetime, "minute")) %>%
    group_by(datetime) %>%
    summarise(
      temp     = mean(temp, na.rm = TRUE),
      humidity = mean(humidity, na.rm = TRUE),
      solar    = mean(solar, na.rm = TRUE),
      .groups  = "drop"
    )
  
  # Interpolation helper (rule=2 extrapolates edges; tweak max_gap_minutes if you want breaks)
  interp <- function(x, tnum, max_gap_minutes = Inf) {
    mg <- if (is.finite(max_gap_minutes)) as.integer(max_gap_minutes) else .Machine$integer.max
    zoo::na.approx(x, x = tnum, na.rm = FALSE, rule = 2, maxgap = mg)
  }
  
  wx_filled <- grid_all %>%
    left_join(wx_min, by = "datetime") %>%
    arrange(datetime) %>%
    mutate(
      tnum     = as.numeric(datetime),
      temp     = interp(temp,     tnum, max_gap_minutes = Inf),  # set to e.g. 120 to cap bridging
      humidity = interp(humidity, tnum, max_gap_minutes = Inf),
      solar    = interp(solar,    tnum, max_gap_minutes = Inf)
    ) %>%
    mutate(day = as_date(datetime)) %>%
    select(-tnum)
  
  # ----------------- scale weather per day to insect axis ----------------------
  ymax <- max(counts_hr$insect_count, na.rm = TRUE); if (!is.finite(ymax) || ymax == 0) ymax <- 1
  # set different “heights” for each series
  amp <- c(temp = 0.35, humidity = 0.25, solar = 0.20)
  
  per_day_scale <- function(x, target) {
    lo <- suppressWarnings(min(x, na.rm = TRUE))
    hi <- suppressWarnings(max(x, na.rm = TRUE))
    if (!is.finite(lo) || !is.finite(hi) || hi <= lo) return(rep(target * 0.5, length(x)))
    target * (x - lo) / (hi - lo)
  }
  
  wx_scaled <- wx_filled %>%
    group_by(day) %>%
    mutate(
      temp_s  = per_day_scale(temp,     ymax * amp["temp"]),
      hum_s   = per_day_scale(humidity, ymax * amp["humidity"]),
      solar_s = per_day_scale(solar,    ymax * amp["solar"])
    ) %>%
    ungroup()
  
  wx_long <- wx_scaled %>%
    select(datetime, day, temp_s, hum_s, solar_s) %>%
    pivot_longer(-c(datetime, day), names_to = "series", values_to = "y") %>%
    mutate(series = recode(series,
                           temp_s  = "Temperature",
                           hum_s   = "Humidity",
                           solar_s = "Solar")) %>%
    arrange(series, datetime) %>%
    filter(is.finite(y))
  
  # ----------------- day labels and noon lines -------------------------------
  days_seq <- seq.Date(week_start, week_end, by = "day")
  days_tbl <- tibble(day = days_seq) %>%
    mutate(label_x = as_datetime(day) + hours(12),
           label   = format(day, "%m-%d"))
  noon_df  <- days_tbl %>% transmute(noon = as_datetime(day) + hours(12))
  
  # ----------------- colors ---------------------------------------------------
  bar_col    <- rgb(211,211,212, maxColorValue = 255)
  temp_col   <- rgb(221,110, 96, maxColorValue = 255)   # red
  hum_col    <- rgb(153,120,190, maxColorValue = 255)   # purple
  solar_col  <- rgb(231,206,143, maxColorValue = 255)   # yellow
  pal <- c("Temperature" = temp_col, "Humidity" = hum_col, "Solar" = solar_col)
  
  # ----------------- plot -----------------------------------------------------
  # ---------- Updated plot: darker abundance bars ----------
  ggplot() +
    geom_vline(data = noon_df,
               aes(xintercept = as.numeric(noon)),
               color = "grey40", linewidth = 0.4) +     # slightly darker noon lines
    geom_col(
      data = counts_hr,
      aes(x = hour_bin, y = insect_count),
      fill = rgb(130,130,135, maxColorValue = 255),     # darker grey fill
      alpha = 0.45,                                     # less transparency (was 0.35)
      width = 3600, colour = NA
    ) +
    geom_text(
      data = days_tbl,
      aes(x = label_x, y = -0.06 * ymax, label = label),
      size = 3, vjust = 1
    ) +
    coord_cartesian(ylim = c(-0.08 * ymax, 1.05 * ymax), clip = "off") +
    scale_x_datetime(expand = expansion(mult = c(0, 0))) +
    scale_color_manual(values = pal, name = "Weather") +
    labs(
      x = NULL, y = "Insect Count",
      title = paste("Hourly insect counts with weather | Week of", format(week_start, "%Y-%m-%d")),
      subtitle = "Full day 00:00 to 24:00. Insect hours filled to zero. Weather lines interpolated on a single-week grid and scaled per day.",
      caption  = "Weather scaled per day to insect axis"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      panel.grid.minor = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      plot.margin = margin(10, 20, 35, 10),
      legend.position = "top",
      legend.title = element_text(size = 10),
      legend.text  = element_text(size = 9)
    ) +
    # group ONLY by series to avoid breaks at midnight
    geom_path(
      data = wx_long,
      aes(x = datetime, y = y, color = series, group = series),
      linewidth = 0.9,
      lineend = "round",
      linejoin = "round"
    )
  
#### Bunch Weather Graph with extended days ####

  suppressPackageStartupMessages({
    library(tidyverse); library(lubridate); library(zoo); library(scales); library(purrr)
  })
  
  # ----------------- window: 30 days from July 8 -----------------
  day_start  <- 0
  day_end    <- 24
  span_start <- as.Date("2025-07-08")
  span_days  <- 58
  span_end   <- span_start + days(span_days - 1)
  
  # ----------------- insect counts -----------------
  possible_insect_cols <- c("total_abundance","CountOfInsect","insect_count","Count")
  have_insect <- intersect(possible_insect_cols, names(combined_all_sites))
  
  if (length(have_insect) == 1) {
    combined_counts <- combined_all_sites %>% mutate(insect_raw = .data[[have_insect]])
  } else {
    exclude_cols <- c("source_file","motionEvent","filename","site","date","time_str",
                      "hour","minute","second","datetime")
    num_cols  <- names(combined_all_sites)[vapply(combined_all_sites, is.numeric, logical(1))]
    taxa_cols <- setdiff(num_cols, intersect(num_cols, exclude_cols))
    stopifnot(length(taxa_cols) > 0)
    combined_counts <- combined_all_sites %>%
      mutate(insect_raw = rowSums(across(all_of(taxa_cols)), na.rm = TRUE))
  }
  
  counts_hr_raw <- combined_counts %>%
    mutate(hour_bin = floor_date(datetime, "hour")) %>%
    group_by(hour_bin) %>%
    summarise(insect_count = sum(insect_raw, na.rm = TRUE), .groups = "drop")
  
  grid_hours <- tibble(
    hour_bin = seq(as_datetime(span_start) + hours(day_start),
                   as_datetime(span_end)   + days(1) + hours(0), # include last day to 24:00
                   by = "1 hour")
  ) %>% filter(hour(hour_bin) >= day_start, hour(hour_bin) < day_end)
  
  counts_hr <- grid_hours %>%
    left_join(counts_hr_raw, by = "hour_bin") %>%
    mutate(insect_count = replace_na(insect_count, 0))
  
  # ----------------- weather (continuous span grid + interpolation) -----------------
  wx_raw <- weather_data %>%
    filter(datetime >= as_datetime(span_start),
           datetime <  as_datetime(span_end) + days(1)) %>%
    transmute(
      datetime = as_datetime(datetime),
      temp     = readr::parse_number(as.character(air_temp)),
      humidity = readr::parse_number(as.character(humidity)),
      solar    = readr::parse_number(as.character(solar))
    ) %>%
    arrange(datetime)
  
  tz_str <- tz(wx_raw$datetime[1]); if (is.na(tz_str) || tz_str == "") tz_str <- "UTC"
  
  # minute grid: start 00:00 on span_start to 00:00 after span_end (i.e., includes 24:00 of last day)
  grid_all <- tibble(
    datetime = seq(as.POSIXct(span_start, tz = tz_str),
                   as.POSIXct(span_end + days(1), tz = tz_str),
                   by = "1 min")
  )
  
  wx_min <- wx_raw %>%
    mutate(datetime = floor_date(datetime, "minute")) %>%
    group_by(datetime) %>%
    summarise(
      temp     = mean(temp, na.rm = TRUE),
      humidity = mean(humidity, na.rm = TRUE),
      solar    = mean(solar, na.rm = TRUE),
      .groups  = "drop"
    )
  
  interp <- function(x, tnum, max_gap_minutes = Inf) {
    mg <- if (is.finite(max_gap_minutes)) as.integer(max_gap_minutes) else .Machine$integer.max
    zoo::na.approx(x, x = tnum, na.rm = FALSE, rule = 2, maxgap = mg)
  }
  
  wx_filled <- grid_all %>%
    left_join(wx_min, by = "datetime") %>%
    arrange(datetime) %>%
    mutate(
      tnum     = as.numeric(datetime),
      temp     = interp(temp,     tnum, max_gap_minutes = Inf),
      humidity = interp(humidity, tnum, max_gap_minutes = Inf),
      solar    = interp(solar,    tnum, max_gap_minutes = Inf)
    ) %>%
    mutate(day = as_date(datetime)) %>%
    select(-tnum)
  
  # ----------------- scale weather per day to insect axis -----------------
  ymax <- max(counts_hr$insect_count, na.rm = TRUE); if (!is.finite(ymax) || ymax == 0) ymax <- 1
  
  # per-series dampening (adjust to taste)
  amp <- c(temp = 0.20, humidity = 0.15, solar = 0.15)
  
  per_day_scale <- function(x, target) {
    lo <- suppressWarnings(min(x, na.rm = TRUE))
    hi <- suppressWarnings(max(x, na.rm = TRUE))
    if (!is.finite(lo) || !is.finite(hi) || hi <= lo) return(rep(target * 0.5, length(x)))
    target * (x - lo) / (hi - lo)
  }
  
  wx_scaled <- wx_filled %>%
    group_by(day) %>%
    mutate(
      temp_s  = per_day_scale(temp,     ymax * amp["temp"]),
      hum_s   = per_day_scale(humidity, ymax * amp["humidity"]),
      solar_s = per_day_scale(solar,    ymax * amp["solar"])
    ) %>%
    ungroup()
  
  wx_long <- wx_scaled %>%
    select(datetime, day, temp_s, hum_s, solar_s) %>%
    pivot_longer(-c(datetime, day), names_to = "series", values_to = "y") %>%
    mutate(series = recode(series,
                           temp_s  = "Temperature",
                           hum_s   = "Humidity",
                           solar_s = "Solar")) %>%
    arrange(series, datetime) %>%
    filter(is.finite(y))
  
  # ----------------- day labels & noon lines -----------------
  days_seq <- seq.Date(span_start, span_end, by = "day")
  days_tbl <- tibble(day = days_seq) %>%
    mutate(label_x = as_datetime(day) + hours(12),
           label   = format(day, "%m-%d"))
  noon_df  <- days_tbl %>% transmute(noon = as_datetime(day) + hours(12))
  
  # Optional: thin labels to every 2nd or 3rd day to reduce clutter
  label_every <- 2
  days_tbl_lab <- days_tbl %>% filter(row_number() %% label_every == 1)
  
  # ----------------- colors -----------------
  bar_col    <- rgb(130,130,135, maxColorValue = 255)  # darker bars
  temp_col   <- rgb(221,110, 96, maxColorValue = 255)  # red
  hum_col    <- rgb(153,120,190, maxColorValue = 255)  # purple
  solar_col  <- rgb(231,206,143, maxColorValue = 255)  # yellow
  pal <- c("Temperature" = temp_col, "Humidity" = hum_col, "Solar" = solar_col)
  
  # ----------------- plot -----------------
  p <- ggplot() +
    geom_vline(data = noon_df, aes(xintercept = as.numeric(noon)),
               color = "grey40", linewidth = 0.4) +
    geom_col(data = counts_hr, aes(x = hour_bin, y = insect_count),
             fill = bar_col, alpha = 0.45, width = 3600, colour = NA) +
    geom_text(data = days_tbl_lab,
              aes(x = label_x, y = -0.06 * ymax, label = label),
              size = 3, vjust = 1) +
    coord_cartesian(ylim = c(-0.08 * ymax, 1.05 * ymax), clip = "off") +
    scale_x_datetime(expand = expansion(mult = c(0, 0))) +
    scale_color_manual(values = pal, name = "Weather") +
    labs(
      x = NULL, y = "Insect Count",
      title = paste0("Hourly insect counts with weather | ",
                     format(span_start, "%Y-%m-%d"), " to ", format(span_end, "%Y-%m-%d")),
      subtitle = "30-day window; insect hours filled to zero. Weather lines interpolated on a single span and scaled per day.",
      caption  = "Weather scaled per day to insect axis"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      panel.grid.minor = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      plot.margin = margin(10, 20, 35, 10),
      legend.position = "left",                # << moved from "top" to left
      legend.justification = "center",
      legend.box.margin = margin(0, 12, 0, 0), # a little space between legend and plot
      legend.title = element_text(size = 10),
      legend.text  = element_text(size = 9),
      legend.key.height = unit(0.8, "lines"),
      legend.key.width  = unit(1.2, "lines")
    ) +
    geom_path(data = wx_long,
              aes(x = datetime, y = y, color = series, group = series),
              linewidth = 0.9, lineend = "round", linejoin = "round")
  
  ggsave("insects_weather_30d.pdf", p,
         width = 55, height = 6, units = "in",
         device = cairo_pdf, dpi = 300, bg = "white", limitsize = FALSE)
  

#### Bunch Weather and Order Graph ####
  # --- Libraries ---
  library(tidyverse)
  library(lubridate)
  library(patchwork)
  library(scales)
  library(rlang)
  library(zoo)
  library(purrr)
  
  # --- Parameters ---
  week_start <- ymd_hms("2025-07-24 00:00:00")
  week_end   <- week_start + days(7)
  bar_width_sec <- 0.7 * 3600
  bar_half_sec  <- bar_width_sec / 2
  
  taxa_cols_wanted <- c("Diptera","Hymenoptera","Lepidoptera","Coleoptera")
  taxa_pal <- c(Diptera="#3B82F6", Hymenoptera="#EF4444",
                Lepidoptera="#22C55E", Coleoptera="#E879F9")
  
  # --- Helpers ---
  dt_coalesce <- function(df) {
    cands <- intersect(names(df), c("datetime","datetime_rounded","datetime.x","datetime.y"))
    if (length(cands) == 0) stop("No datetime column found.")
    mutate(df, datetime_any = coalesce(!!!syms(cands)))
  }
  
  hours_grid <- tibble(datetime = seq(week_start, week_end - hours(1), by = "1 hour"))
  
  # ===================== Insects (all sites) =====================
  taxa_cols <- intersect(taxa_cols_wanted, names(combined_all_sites))
  
  counts_all <- combined_all_sites %>%
    dt_coalesce() %>%
    filter(!is.na(datetime_any)) %>%
    mutate(datetime = floor_date(datetime_any, "hour")) %>%
    group_by(datetime) %>%
    summarise(across(all_of(taxa_cols), ~ sum(.x, na.rm = TRUE)), .groups = "drop") %>%
    right_join(hours_grid, by = "datetime") %>%
    arrange(datetime) %>%
    mutate(across(all_of(taxa_cols), ~ replace_na(.x, 0L))) %>%
    filter(datetime >= week_start, datetime < week_end)
  
  hour_totals <- counts_all %>%
    transmute(datetime, total = rowSums(across(all_of(taxa_cols)), na.rm = TRUE))
  
  counts_long <- counts_all %>%
    pivot_longer(all_of(taxa_cols), names_to = "taxon", values_to = "count") %>%
    mutate(taxon = factor(taxon, levels = taxa_cols_wanted[taxa_cols_wanted %in% taxa_cols]))
  
  ymax_counts <- max(hour_totals$total, na.rm = TRUE)
  if (!is.finite(ymax_counts) || ymax_counts < 1) ymax_counts <- 1
  
  # ===================== Grey rectangles for gaps (never touch bars or edges) =====================
  r <- rle(hour_totals$total == 0)
  ends   <- cumsum(r$lengths)
  starts <- ends - r$lengths + 1
  runs <- tibble(start_idx = starts, end_idx = ends, is_zero = r$values)
  
  n_rows <- nrow(hour_totals)
  gap_sec <- 600          # buffer from bars
  edge_pad_sec <- 600     # buffer from plot edges
  
  runs_checked <- runs %>%
    mutate(
      has_left  = start_idx > 1,
      has_right = end_idx   < n_rows,
      left_idx  = if_else(has_left,  start_idx - 1L, 1L),
      right_idx = if_else(has_right, end_idx + 1L, n_rows),
      left_val  = if_else(has_left,  hour_totals$total[left_idx],  NA_real_),
      right_val = if_else(has_right, hour_totals$total[right_idx], NA_real_),
      left_nonzero  = has_left  & replace_na(left_val  > 0, FALSE),
      right_nonzero = has_right & replace_na(right_val > 0, FALSE)
    )
  
  internal <- runs_checked %>%
    filter(is_zero & left_nonzero & right_nonzero) %>%
    transmute(
      prev_center = hour_totals$datetime[start_idx - 1L],
      next_center = hour_totals$datetime[end_idx + 1L],
      xmin = prev_center + seconds(bar_half_sec + gap_sec),
      xmax = next_center - seconds(bar_half_sec + gap_sec)
    )
  
  nz_idx <- which(hour_totals$total > 0)
  first_nz <- if (length(nz_idx)) nz_idx[1] else NA_integer_
  leading <- if (!is.na(first_nz) && first_nz > 1) {
    tibble(
      xmin = week_start + seconds(edge_pad_sec),
      xmax = hour_totals$datetime[first_nz] - seconds(bar_half_sec + gap_sec)
    )
  } else tibble(xmin = as.POSIXct(character()), xmax = as.POSIXct(character()))
  
  last_nz <- if (length(nz_idx)) tail(nz_idx, 1) else NA_integer_
  trailing <- if (!is.na(last_nz) && last_nz < n_rows) {
    tibble(
      xmin = hour_totals$datetime[last_nz] + seconds(bar_half_sec + gap_sec),
      xmax = week_end - seconds(edge_pad_sec)
    )
  } else tibble(xmin = as.POSIXct(character()), xmax = as.POSIXct(character()))
  
  gaps_df <- bind_rows(internal, leading, trailing) %>%
    filter(xmax > xmin) %>%
    mutate(ymin = 0, ymax = 0.10 * ymax_counts)
  
  # ===================== Weather (raw points only, no interpolation, no grid join) =====================
  # Aggregate to hourly, filter to week, keep only observed timestamps so lines connect across actual points.
  wx_all <- combined_with_weather %>%
    dt_coalesce() %>%
    filter(!is.na(datetime_any)) %>%
    mutate(datetime = floor_date(datetime_any, "hour")) %>%
    filter(datetime >= week_start, datetime < week_end) %>%
    group_by(datetime) %>%
    summarise(
      air_temp = mean(air_temp, na.rm = TRUE),
      solar    = mean(solar,    na.rm = TRUE),
      humidity = mean(humidity, na.rm = TRUE),
      .groups  = "drop"
    ) %>%
    arrange(datetime)
  
  # scale to temperature range using observed maxima within the window
  max_temp <- max(wx_all$air_temp, na.rm = TRUE); if (!is.finite(max_temp)) max_temp <- 1
  smax <- max(wx_all$solar, na.rm = TRUE); solar_scale <- if (is.finite(smax) && smax > 0) max_temp / smax else 1
  hmax <- max(wx_all$humidity, na.rm = TRUE); hum_scale <- if (is.finite(hmax) && hmax > 0) max_temp / hmax else 1
  
  wx_plot <- wx_all %>%
    mutate(
      solar_plot = solar * solar_scale,
      hum_plot   = humidity * hum_scale
    )
  
  # ===================== Plots =====================
  p_top <- ggplot() +
    geom_rect(
      data = gaps_df,
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      fill = "grey70", alpha = 0.45, inherit.aes = FALSE
    ) +
    geom_col(
      data = counts_long,
      aes(x = datetime, y = count, fill = taxon),
      width = bar_width_sec, color = NA
    ) +
    scale_fill_manual(values = taxa_pal, breaks = names(taxa_pal), name = NULL) +
    coord_cartesian(ylim = c(0, ymax_counts * 1.05), xlim = c(week_start, week_end), clip = "off") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.03))) +
    scale_x_datetime(NULL, breaks = pretty_breaks(n = 14), date_labels = "%b %d\n%H:%M", expand = c(0, 0)) +
    labs(
      title = paste0("All sites combined  |  Week starting ", format(week_start, "%Y-%m-%d")),
      y = "Insect Number"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "left",
      panel.grid.minor = element_blank(),
      plot.title = element_text(face = "bold", size = 13)
    )
  
  p_bottom <- ggplot(wx_plot, aes(x = datetime)) +
    geom_line(aes(y = air_temp,  color = "Temperature (°C)"), linewidth = 0.9, na.rm = TRUE) +
    geom_point(aes(y = air_temp,  color = "Temperature (°C)"), size = 0.8, na.rm = TRUE) +
    geom_line(aes(y = solar_plot, color = "Solar (scaled)"),   linewidth = 0.85, na.rm = TRUE) +
    geom_point(aes(y = solar_plot, color = "Solar (scaled)"),  size = 0.7, na.rm = TRUE) +
    geom_line(aes(y = hum_plot,   color = "Humidity (scaled)"),linewidth = 0.85, na.rm = TRUE) +
    geom_point(aes(y = hum_plot,  color = "Humidity (scaled)"),size = 0.7, na.rm = TRUE) +
    scale_color_manual(
      values = c("Temperature (°C)" = "#DC2626",
                 "Solar (scaled)"   = "#EAB308",
                 "Humidity (scaled)" = "#22D3EE"),
      name = NULL
    ) +
    scale_y_continuous(name = "Weather Metrics") +
    coord_cartesian(xlim = c(week_start, week_end)) +
    scale_x_datetime(NULL, breaks = pretty_breaks(n = 14), date_labels = "%b %d\n%H:%M", expand = c(0, 0)) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "left",
      panel.grid.minor = element_blank()
    )
  
  final_plot <- p_top / p_bottom + plot_layout(heights = c(2, 1))
  
  ggsave(
    filename = paste0("insect_weather_week_", format(week_start, "%Y-%m-%d"), "_ALL.png"),
    plot = final_plot,
    width = 36, height = 10, units = "in", dpi = 300,
    bg = "white", limitsize = FALSE
  )
  