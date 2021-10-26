# This is a code to produce the graphs we would need for the presentation
# The time series were generated and saved to .png file

# Load packages
library(ggplot2)
library(dplyr)
library(gghighlight)

# Load dataframe
filename <- "all_countries_percentage.csv"
df <- read.csv(filename, header = TRUE)

# Create country_vector but only countries
country_vector <- c("Chile",
                    "Estonia",
                    "Latvia",
                    "Slovenia")

# Import the dataset and format the date
df$date <- as.Date(df$date, format = "%Y-%m-%d")
df$month <- format(df$date, "%m")

# We want to aggregate the dataset into months only instead of day.
# The reason is that we will get a smoother curve and easier to analyze,
# While not losing the meaning of the graph.
# The code below is to create a new frame that aggregates by country and month
df_aggregate <- aggregate(df, by = list(df$month, df$country), FUN = mean)
df_aggregate <- select(df_aggregate, -c("date", "total_cases", "total_deaths","total_vaccinations", "month", "country"))
colnames(df_aggregate)[1] <- "month"
colnames(df_aggregate)[2] <- "country"

# Generate frame only consist of data from Jan to Mar
df_early <- filter(df_aggregate,
                   month == df_aggregate$month[1] |
                   month == df_aggregate$month[2] |
                   month == df_aggregate$month[3])

# Generate frame only consist of data from Mar to Jul
df_mid <- filter(df_aggregate,
                 month == df_aggregate$month[3] |
                 month == df_aggregate$month[4] |
                 month == df_aggregate$month[5] |
                 month == df_aggregate$month[6] |
                 month == df_aggregate$month[7])

# Generate frame only consist of data from Jul to Sep
df_late <- filter(df_aggregate,
                  month == df_aggregate$month[7] |
                  month == df_aggregate$month[8] |
                  month == df_aggregate$month[9])

# Plot time series of new cases
ggplot(data = df_aggregate, aes(x = month, y = new_cases, group = country, colour = country)) +
  geom_line() +
  ggtitle("Time series for new cases") +
  labs(y = "New Cases (%)", x = "Month") + 
  theme_minimal()

# Save plot
ggsave("plots/plot_cases_all.png", dpi = 320, width = 1500, height = 800, units = "px")

# Plot time series of new cases for Jan, Feb, Mar
ggplot(data = df_aggregate, aes(x = month, y = new_cases, group = country)) +
  geom_line() +
  theme_minimal() +
  geom_line(data = df_early, aes(x = month, y = new_cases, group = country, colour = country)) +
  geom_vline(xintercept = df_aggregate$month[3], linetype = "dotted") +
  ggtitle("Time series for new cases per Jan, Feb, Mar") +
  labs(y = "New Cases (%)", x = "Month") + 
  theme_minimal()

# Save plot
ggsave("plots/plot_cases_early.png", dpi = 320, width = 1500, height = 800, units = "px")

# Plot time series of new cases for Mid
ggplot(data = df_aggregate, aes(x = month, y = new_cases, group = country)) +
  geom_line() +
  theme_minimal() +
  geom_line(data = df_mid, aes(x = month, y = new_cases, group = country, colour = country)) +
  geom_vline(xintercept = df_aggregate$month[3], linetype = "dotted") +
  geom_vline(xintercept = df_aggregate$month[7], linetype = "dotted") +
  ggtitle("Time series for new cases per Apr, May, Jun") +
  labs(y = "New Cases (%)", x = "Month") + 
  theme_minimal()

# Save plot
ggsave("plots/plot_cases_mid.png", dpi = 320, width = 1500, height = 800, units = "px")

# Plot time series of new cases for Jul Aug Sep
ggplot(data = df_aggregate, aes(x = month, y = new_cases, group = country)) +
  geom_line() +
  theme_minimal() +
  geom_line(data = df_late, aes(x = month, y = new_cases, group = country, colour = country)) +
  geom_vline(xintercept = df_aggregate$month[7], linetype = "dotted") +
  ggtitle("Time series for new cases per Jul, Aug, Sep") +
  labs(y = "New Cases (%)", x = "Month") + 
  theme_minimal()

# Save plot
ggsave("plots/plot_cases_late.png", dpi = 320, width = 1500, height = 800, units = "px")

# Plot time series of new deaths
ggplot(data = df_aggregate, aes(x = month, y = new_deaths, group = country, colour = country)) +
  geom_line() +
  ggtitle("Time series for new deaths") +
  labs(y = "New Deaths (%)", x = "Month") + 
  theme_minimal()

# Save plot
ggsave("plots/plot_deaths_all.png", dpi = 320, width = 1500, height = 800, units = "px")

# Plot time series of new deaths for Jan, Feb, Mar
ggplot(data = df_aggregate, aes(x = month, y = new_deaths, group = country)) +
  geom_line() +
  theme_minimal() +
  geom_line(data = df_early, aes(x = month, y = new_deaths, group = country, colour = country)) +
  geom_vline(xintercept = df_aggregate$month[3], linetype = "dotted") +
  ggtitle("Time series for new deaths per Jan, Feb, Mar") +
  labs(y = "New Deaths (%)", x = "Month") + 
  theme_minimal()

# Save plot
ggsave("plots/plot_deaths_early.png", dpi = 320, width = 1500, height = 800, units = "px")

# Plot time series of new deaths for Mid
ggplot(data = df_aggregate, aes(x = month, y = new_deaths, group = country)) +
  geom_line() +
  theme_minimal() +
  geom_line(data = df_mid, aes(x = month, y = new_deaths, group = country, colour = country)) +
  geom_vline(xintercept = df_aggregate$month[3], linetype = "dotted") +
  geom_vline(xintercept = df_aggregate$month[7], linetype = "dotted") +
  ggtitle("Time series for new deaths per Apr, May, Jun") +
  labs(y = "New Deaths (%)", x = "Month") + 
  theme_minimal()

# Save plot
ggsave("plots/plot_deaths_mid.png", dpi = 320, width = 1500, height = 800, units = "px")

# Plot time series of new deaths for Jul Aug Sep
ggplot(data = df_aggregate, aes(x = month, y = new_deaths, group = country)) +
  geom_line() +
  theme_minimal() +
  geom_line(data = df_late, aes(x = month, y = new_deaths, group = country, colour = country)) +
  geom_vline(xintercept = df_aggregate$month[7], linetype = "dotted") +
  ggtitle("Time series for new deaths per Jul, Aug, Sep") +
  labs(y = "New Deaths (%)", x = "Month") + 
  theme_minimal()

# Save plot
ggsave("plots/plot_deaths_late.png", dpi = 320, width = 1500, height = 800, units = "px")

# Plot time series of total vaccination
ggplot(data = df, aes(x = date, y = total_vaccinations, group = country, colour = country)) +
  geom_line() +
  ggtitle("Time series for total vaccination") +
  labs(y = "Total Vaccination (%)", x = "Month") + 
  theme_minimal()

# Save plot
ggsave("plots/plot_total_vaccination_all.png", dpi = 320, width = 1500, height = 800, units = "px")

# Plot time series of total vaccination in Chile
ggplot(data = df, aes(x = date, y = total_vaccinations, group = country, colour = country)) +
  geom_line() +
  gghighlight(country == "Chile", use_direct_label = FALSE, keep_scales = TRUE) +
  ggtitle("Time series for total vaccination in Chile") +
  labs(y = "Total Vaccination (%)", x = "Month") + 
  theme_minimal()

# Save plot
ggsave("plots/plot_total_vaccination_chile.png", dpi = 320, width = 1500, height = 800, units = "px")