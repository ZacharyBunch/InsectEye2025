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


### Combine all sites ####
combined_all_sites <- bind_rows(
  Harner_A_Clean,
  Harner_C_Clean,
  RockSprings_A_Clean,
  RockSprings_B_Clean
)
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

