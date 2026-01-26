~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#title: WISQAR Insights
#author: Jackie Cho
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
# close all, clear all
rm(list=ls())
graphics.off()

# load tidyverse
library(tidyr)
library(tidyverse)
library(arrow)
library(dplyr)
library(ggplot2)
library(ggbeeswarm)

# load in file
ds1 <-vroom::vroom(
  "https://raw.githubusercontent.com/PopHIVE/Ingest/main/data/wisqars/standard/data.csv.gz",
  delim = ","
  )
# check if load in correctly
head(ds1)

## Firearm Injury ~~~~~~~~~~

# subset data
firearm = subset(ds1, select=c(geography, time, age, wisqars_rate_firearm_accident, wisqars_rate_firearm_intentional, wisqars_deaths_firearm_accident, wisqars_deaths_firearm_intentional))

# pull url for FIPS codes
url="https://www2.census.gov/geo/docs/reference/state.txt"
fips=read.delim(url,sep="|")

# change to double digits for 1-9
fips <- fips |>
  mutate(
    STATEFP = stringr::str_pad(STATE, width = 2, pad = "0")
  )

# create new column state name in firearm data set and match to fips df
firearm$state_name = fips$STATE_NAME[match(firearm$geography,fips$STATEFP)]

# add 00 = National
firearm$state_name[firearm$geography == "00"] <- "National"

# load library to convert time to year as numeric
library(lubridate)
# convert time to year as numeric
firearm$year <- year(ymd(firearm$time))

# subset for only National values
firearm_national = firearm[which(firearm$state_name == "National"),]

# subset for each age group
firearm_national_0_14 = firearm_national[which(firearm_national$age == "0-14 Years"),]
firearm_national_15_24 = firearm_national[which(firearm_national$age == "15-24 Years"),]
firearm_national_25_44 = firearm_national[which(firearm_national$age == "25-44 Years"),]
firearm_national_45_64 = firearm_national[which(firearm_national$age == "45-64 Years"),]
firearm_national_25_44 = firearm_national[which(firearm_national$age == "25-44 Years"),]
firearm_national_65 = firearm_national[which(firearm_national$age == "65+ Years"),]
firearm_national_Total = firearm_national[which(firearm_national$age == "Total"),]

## SCATTERPLOT FOR FIREARM ACCIDENTAL DEATH RATE ##
## Stratified by Age Group ##

# Determine the overall year range across all age groups
year_min <- min(firearm$year, na.rm = TRUE)
year_max <- max(firearm$year, na.rm = TRUE)

# Helper function to make a single scatterplot with regression line
plot_firearm_age <- function(data, title_text) {
  plot(
    data$year,
    data$wisqars_rate_firearm_accident,
    main = title_text,
    xlab = "Year",
    ylab = "Death Rate (per 100,000)",
    pch = 19,                     # solid dots
    col = "darkred",
    xlim = c(year_min, year_max)  # consistent x-axis
  )
  
  # Add regression line
  model <- lm(wisqars_rate_firearm_accident ~ year, data = data)
  abline(model, col = "blue", lwd = 2)
  
  # Add R² value label
  r2 <- round(summary(model)$r.squared, 3)
  text(
    x = year_min + 1,
    y = max(data$wisqars_rate_firearm_accident, na.rm = TRUE),
    labels = paste("R² =", r2),
    pos = 4,
    col = "blue"
  )
}

# 2x3 layout, bigger outer top margin
par(mfrow = c(2, 3),
    mar  = c(4, 4, 3, 1),   # inner margins
    oma  = c(0, 0, 6, 0))   # outer margins: bottom, left, top, right

# One plot per age group
plot_firearm_age(firearm_national_0_14, "Age 0–14 Years")
plot_firearm_age(firearm_national_15_24, "Age 15–24 Years")
plot_firearm_age(firearm_national_25_44, "Age 25–44 Years")
plot_firearm_age(firearm_national_45_64, "Age 45–64 Years")
plot_firearm_age(firearm_national_65,   "Age 65+ Years")
plot_firearm_age(firearm_national_Total, "All Ages")

# Main Title
mtext(
  "Accidental Firearm Death Rates by Age Group",
  outer = TRUE,
  cex   = 1.4,   # a bit smaller so it fits
  font  = 2,
  line  = 2      # move it slightly down from the very top
)

par(mfrow = c(1, 1))


## INTENTIONAL FIREARM RATES ##
## Stratified by Age Group ##

# Helper function to make a single scatterplot with regression line
plot_firearm_intent_age <- function(data, title_text) {
  plot(
    data$year,
    data$wisqars_rate_firearm_intentional,
    main = title_text,
    xlab = "Year",
    ylab = "Death Rate (per 100,000)",
    pch = 19,                     # solid dots
    col = "darkred",
    xlim = c(year_min, year_max)  # consistent x-axis
  )
  
  # Add regression line
  model <- lm(wisqars_rate_firearm_intentional ~ year, data = data)
  abline(model, col = "blue", lwd = 2)
  
  # Add R² value label
  r2 <- round(summary(model)$r.squared, 3)
  text(
    x = year_min + 1,
    y = max(data$wisqars_rate_firearm_intentional, na.rm = TRUE),
    labels = paste("R² =", r2),
    pos = 4,
    col = "blue"
  )
}

# One plot per age group — each will open in a new window

# 2x3 layout, bigger outer top margin
par(mfrow = c(2, 3),
    mar  = c(4, 4, 3, 1),   # inner margins
    oma  = c(0, 0, 6, 0))   # outer margins: bottom, left, top, right

plot_firearm_intent_age(firearm_national_0_14, "Age 0–14 Years")
plot_firearm_intent_age(firearm_national_15_24, "Age 15–24 Years")
plot_firearm_intent_age(firearm_national_25_44, "Age 25–44 Years")
plot_firearm_intent_age(firearm_national_45_64, "Age 45–64 Years")
plot_firearm_intent_age(firearm_national_65,    "Age 65+ Years")
plot_firearm_intent_age(firearm_national_Total,    "All Ages")

# Main Title
mtext(
  "Intentional Firearm Death Rates by Age Group",
  outer = TRUE,
  cex   = 1.4,   # a bit smaller so it fits
  font  = 2,
  line  = 2      # move it slightly down from the very top
)

par(mfrow = c(1, 1))

## TOP 5 STATES WITH HIGHEST ACCIDENTAL RATE ##

# subset firearm ft to only include Total ages
firearm_bystate_total = firearm[which(firearm$age == "Total"),]

# Aggregate both accidental and intentional firearm rates by year
firearm_bystate_avg <- aggregate(
  cbind(wisqars_rate_firearm_accident, wisqars_rate_firearm_intentional) ~ state_name,
  data = firearm_bystate_total,
  FUN = mean,
  na.rm = TRUE
)

# Identify Top 5 states in avg rate of intentional firearm deaths past 25 years
top5_intentional <- firearm_bystate_avg %>%
  group_by(state_name) %>%
  summarise(mean_intentional = mean(wisqars_rate_firearm_intentional, na.rm = TRUE)) %>%
  arrange(desc(mean_intentional)) %>%
  slice(1:5)

# Identify Top 5 states in avg rate of accidental firearm deaths past 25 years
top5_accidental <- firearm_bystate_avg %>%
  group_by(state_name) %>%
  summarise(mean_accident = mean(wisqars_rate_firearm_accident, na.rm = TRUE)) %>%
  arrange(desc(mean_accident)) %>%
  slice(1:5)

## INTENTIONAL LINE GRAPH ##

# Filter the Total-age dataset for top 5 intentional states
top5_intent_states <- top5_intentional$state_name

firearm_intent_top5 <- firearm_bystate_total %>%
  filter(state_name %in% top5_intent_states)

# Define consistent axis limits
year_min <- min(firearm_intent_top5$year, na.rm = TRUE)
year_max <- max(firearm_intent_top5$year, na.rm = TRUE)
rate_max <- max(firearm_intent_top5$wisqars_rate_firearm_intentional, na.rm = TRUE)

# Pick 5 distinct colors
colors <- c("red", "blue", "darkgreen", "purple", "orange")

# Plot the first state to create the base frame
first_state <- top5_intent_states[1]
data_first <- subset(firearm_intent_top5, state_name == first_state)

plot(
  data_first$year,
  data_first$wisqars_rate_firearm_intentional,
  type = "l",                # line plot
  lwd = 2,
  col = colors[1],
  xlim = c(year_min, year_max),
  ylim = c(0, rate_max),
  xlab = "Year",
  ylab = "Intentional Firearm Death Rate (per 100,000)",
  main = "Top 5 States: Intentional Firearm Death Rates (All Ages)\nOver Time"
)

# Add the remaining 4 states
for (i in 2:5) {
  state_data <- subset(firearm_intent_top5, state_name == top5_intent_states[i])
  lines(state_data$year, state_data$wisqars_rate_firearm_intentional, col = colors[i], lwd = 2)
}

# Add legend
legend("bottomright", legend = top5_intent_states, col = colors, lwd = 2, bty = "n")

## ACCIDENTAL LINE GRAPH ##
# Filter for top 5 accidental states (from your earlier table)
top5_acc_states <- top5_accidental$state_name

# Keep only those 5 states’ data from the "Total" age group
firearm_acc_top5 <- firearm_bystate_total %>%
  filter(state_name %in% top5_acc_states)

# Consistent axis limits
year_min <- min(firearm_acc_top5$year, na.rm = TRUE)
year_max <- max(firearm_acc_top5$year, na.rm = TRUE)
rate_max <- max(firearm_acc_top5$wisqars_rate_firearm_accident, na.rm = TRUE)

# 5 distinct colors for clarity
colors <- c("red", "blue", "darkgreen", "purple", "orange")

# Base line plot using first state to set up axes
first_state <- top5_acc_states[1]
data_first <- subset(firearm_acc_top5, state_name == first_state)
data_first <- data_first[order(data_first$year), ]  # ensure sorted by year

plot(
  data_first$year,
  data_first$wisqars_rate_firearm_accident,
  type = "l",       # line graph
  lwd = 2,
  col = colors[1],
  xlim = c(year_min, year_max),
  ylim = c(0, rate_max),
  xlab = "Year",
  ylab = "Accidental Firearm Death Rate (per 100,000)",
  main = "Top 5 States: Accidental Firearm Death Rates (All Ages)\nOver Time"
)

# Add the other 4 states to the same plot
for (i in 2:5) {
  state_data <- subset(firearm_acc_top5, state_name == top5_acc_states[i])
  state_data <- state_data[order(state_data$year), ]  # sort by year
  lines(state_data$year, state_data$wisqars_rate_firearm_accident, col = colors[i], lwd = 2)
}

# Add legend
legend("topright", legend = top5_acc_states, col = colors, lwd = 2, bty = "n")

## Top 5 States past 5 years ##

firearm_national_0_14_2020 <- firearm_national_0_14[firearm_national_0_14$year >= 2020, ]
firearm_national_15_24_2020 <- firearm_national_15_24[firearm_national_15_24$year >= 2020, ]
firearm_national_25_44_2020 <- firearm_national_25_44[firearm_national_25_44$year >= 2020, ]
firearm_national_45_64_2020 <- firearm_national_45_64[firearm_national_45_64$year >= 2020, ]
firearm_national_65_2020    <- firearm_national_65[firearm_national_65$year >= 2020, ]
firearm_national_Total_2020 <- firearm_national_Total[firearm_national_Total$year >= 2020, ]

# Make Function
plot_firearm_int_age_recent <- function(data, title_text) {
  plot(
    data$year,
    data$wisqars_rate_firearm_intentional,
    main = paste0(title_text, "\n(2020–Present)"),
    xlab = "Year",
    ylab = "Intentional Firearm Death Rate (per 100,000)",
    pch = 19,
    col = "darkred",
    xlim = c(2020, year_max)
  )
  
  model <- lm(wisqars_rate_firearm_intentional ~ year, data = data)
  abline(model, col = "blue", lwd = 2)
  
  r2 <- round(summary(model)$r.squared, 3)
  text(
    x = year_min + 0.2,
    y = max(data$wisqars_rate_firearm_intentional, na.rm = TRUE),
    labels = paste("R² =", r2),
    pos = 4,
    col = "blue"
  )
}

# Apply to all DF
plot_firearm_int_age_recent(firearm_national_0_14_2020,
                            "Intentional Firearm Death Rate – Age 0–14 Years")
plot_firearm_int_age_recent(firearm_national_15_24_2020,
                            "Intentional Firearm Death Rate – Age 15–24 Years")
plot_firearm_int_age_recent(firearm_national_25_44_2020,
                            "Intentional Firearm Death Rate – Age 25–44 Years")
plot_firearm_int_age_recent(firearm_national_45_64_2020,
                            "Intentional Firearm Death Rate – Age 45–64 Years")
plot_firearm_int_age_recent(firearm_national_65_2020,
                            "Intentional Firearm Death Rate – Age 65+ Years")
plot_firearm_int_age_recent(firearm_national_Total_2020,
                            "Intentional Firearm Death Rate – All Ages")

## MATCH WITH GTRENDS

# load data
ds2 <- read_parquet(
  "https://raw.githubusercontent.com/PopHIVE/Ingest/4d6fd7364be0ea50b5a2c641c9ca16e554783971/data/bundle_injury_overdose/dist/firearms_geography_source.parquet"
)

# change to double digits for 1-9
fips <- fips |>
  mutate(
    STATEFP = stringr::str_pad(STATE, width = 2, pad = "0")
  )

# create new column state name in firearm data set and match to fips df
ds2$State = fips$STATE_NAME[match(ds2$geography,fips$STATEFP)]

# add 00 = National
ds2$State[ds2$geography == "00"] <- "National"

# convert time to year as numeric
ds2$year <- year(ymd(ds2$time))

# Filter for gtrends
gtrends_shotgun <- ds2 %>%
  filter(source %in% "gtrends_shotgun")
gtrends_9mm <- ds2 %>%
  filter(source %in% "gtrends_9mm")

# Load lib
library(dplyr)

# Aggregate per year. State specific only.
gtrends_shotgun_yearly <- gtrends_shotgun %>%
  group_by(State, year) %>%
  summarise(
    shotgun_avg = mean(value, na.rm = TRUE),
    .groups = "drop"
  )

gtrends_9mm_yearly <- gtrends_9mm %>%
  group_by(State, year) %>%
  summarise(
    mm9_avg = mean(value, na.rm = TRUE),
    .groups = "drop"
  )

national_shotgun_yearly <- gtrends_shotgun %>%
  filter(State == "National") %>%
  group_by(year) %>%
  summarise(
    shotgun_avg = mean(value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(State = "National")

national_9mm_yearly <- gtrends_9mm %>%
  filter(State == "National") %>%
  group_by(year) %>%
  summarise(
    mm9_avg = mean(value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(State = "National")

# Merge with Firearm data
firearm_national_Total$State <- "National"

national_gtrends <- firearm_national_Total %>%
  left_join(national_shotgun_yearly, by = c("State", "year")) %>%
  left_join(national_9mm_yearly,    by = c("State", "year"))

# 2014 and onwards because that's what gtrends offers
national_gtrends <- national_gtrends %>%
  filter(year >= 2014)

# Plot shotgun avg vs. accidental firearm rate
plot(
  national_gtrends$shotgun_avg,
  national_gtrends$wisqars_rate_firearm_accident,
  xlab = "Number of Shotgun Google Searches",
  ylab = "Accidental Firearm Death Rate",
  main = "Shotgun Searches vs Accidental Firearm Death Rate",
  pch = 19, col = "darkred"
)

abline(lm(wisqars_rate_firearm_accident ~ shotgun_avg, national_gtrends), col = "blue", lwd = 2)

# Plot shotgun avg vs. intentional firearm rate
plot(
  national_gtrends$shotgun_avg,
  national_gtrends$wisqars_rate_firearm_intentional,
  xlab = "Number of Shotgun Google Searches",
  ylab = "Intentional Firearm Death Rate",
  main = "Shotgun Searches vs Intentional Firearm Death Rate",
  pch = 19, col = "darkred"
)

abline(lm(wisqars_rate_firearm_intentional ~ shotgun_avg, national_gtrends), col = "blue", lwd = 2)

# Plot 9mm avg vs. intentional firearm rate
plot(
  national_gtrends$mm9_avg,
  national_gtrends$wisqars_rate_firearm_intentional,
  xlab = "Number of 9mm Google Searches",
  ylab = "Intentional Firearm Death Rate",
  main = "9mm Searches vs Intentional Firearm Death Rate",
  pch = 19, col = "darkred"
)

abline(lm(wisqars_rate_firearm_intentional ~ mm9_avg, national_gtrends), col = "blue", lwd = 2)

# Plot 9mm avg vs. acc firearm rate
plot(
  national_gtrends$mm9_avg,
  national_gtrends$wisqars_rate_firearm_accident,
  xlab = "Number of 9mm Google Searches",
  ylab = "Accidental Firearm Death Rate",
  main = "9mm Searches vs Accidental Firearm Death Rate",
  pch = 19, col = "darkred"
)

abline(lm(wisqars_rate_firearm_accident ~ mm9_avg, national_gtrends), col = "blue", lwd = 2)

# Linear Regression
lm_acc <- lm(wisqars_rate_firearm_accident ~ shotgun_avg + mm9_avg, data = national_gtrends)
summary(lm_acc)

lm_int <- lm(wisqars_rate_firearm_intentional ~ shotgun_avg + mm9_avg, data = national_gtrends)
summary(lm_int)

# ggplot

library(ggplot2)

firearm_total_states <- firearm %>%
  filter(age == "Total", !is.na(state_name))

ggplot(
  firearm_total_states,
  aes(x = year, y = wisqars_rate_firearm_intentional)
) +
  geom_point(alpha = 0.6, size = 1) +
  geom_smooth(method = "lm", se = FALSE, color = "blue", linewidth = 0.7) +
  facet_wrap(~ state_name) +
  labs(
    title    = "Intentional Firearm Death Rates Over Time",
    subtitle = "All Ages (Total), by State",
    x        = "Year",
    y        = "Intentional Firearm Death Rate (per 100,000)"
  ) +
  theme_bw(base_size = 10) +
  theme(
    strip.text = element_text(size = 8),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggplot(
  firearm_total_states,
  aes(x = year, y = wisqars_rate_firearm_accident)
) +
  geom_point(alpha = 0.6, size = 1) +
  geom_smooth(method = "lm", se = FALSE, color = "blue", linewidth = 0.7) +
  facet_wrap(~ state_name) +
  labs(
    title    = "Accidental Firearm Death Rates Over Time",
    subtitle = "All Ages (Total), by State",
    x        = "Year",
    y        = "Accidental Firearm Death Rate (per 100,000)"
  ) +
  theme_bw(base_size = 10) +
  theme(
    strip.text = element_text(size = 8),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
