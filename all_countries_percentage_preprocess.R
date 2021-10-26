# This R code for preprocessing purposes.
# We take all relevant data and put them into a frame.
# We then scale them based on their population to revert all to their percentages.

# Set filenames
filenames <- c("covid_data_per_country/covid_data_asia.csv",
               "covid_data_per_country/covid_data_chile.csv",
               "covid_data_per_country/covid_data_estonia.csv",
               "covid_data_per_country/covid_data_europe.csv",
               "covid_data_per_country/covid_data_latvia.csv",
               "covid_data_per_country/covid_data_north_america.csv",
               "covid_data_per_country/covid_data_slovenia.csv",
               "covid_data_per_country/covid_data_south_america.csv")

# Set country names
country_vector <- c("Asia",
                    "Chile",
                    "Estonia",
                    "Europe",
                    "Latvia",
                    "North America",
                    "Slovenia",
                    "South America")

# Establish populations

asia_pop <- 4560667108
# Source: "World Population prospects - Population division"".
# population.un.org. United Nations Department of Economic and Social Affairs,
# Population Division. Retrieved 9 November 2019.

chile_pop <- 38125245
# Source: https://ourworldindata.org/world-population-growth

estonia_pop <- 2322914
# Source: https://ourworldindata.org/world-population-growth

europe_pop <- 746419440
# Source: ""World Population prospects - Population division"". population.un.org.
# United Nations Department of Economic and Social Affairs, Population Division.
# Retrieved 9 November 2019.

latvia_pop <- 2838073
# Source:  https://ourworldindata.org/world-population-growth

north_america_pop <- 592296223
# Source:  ""World Population prospects - Population division"".
# population.un.org. United Nations Department of Economic and Social Affairs,
# Population Division. Retrieved November 9, 2019.

slovenia_pop <- 3736505
# Source: https://ourworldindata.org/world-population-growth

south_america_pop <- 423581078
# Source:  ""World Population prospects - Population division"".
# population.un.org. United Nations Department of Economic and Social Affairs,
# Population Division. Retrieved November 9, 2019.

# Put the populations together in a vector
population_vector <- c(asia_pop,
                       chile_pop,
                       estonia_pop,
                       europe_pop,
                       latvia_pop,
                       north_america_pop,
                       slovenia_pop,
                       south_america_pop)

# Create an empty vector for the dataframes
df_vector <- vector(mode = "list", length = length(filenames))

# Loop add all frames into the df_vector and preprocess
for(i in 1:length(filenames)){
  # Import df
  df_vector[[i]] <- read.csv(filenames[i], header = TRUE)
  # Format date
  df_vector[[i]]$date <- as.Date(df_vector[[i]]$date, format = "%Y-%m-%d")
  # Add lethality new_deahts / total_cases
  df_vector[[i]]$lethality <- df_vector[[i]]$new_deaths / df_vector[[i]]$total_cases
  
  # Process all values to proportion to their population
  df_vector[[i]]$total_cases <- (df_vector[[i]]$total_cases / population_vector[i]) * 100
  df_vector[[i]]$new_cases <- (df_vector[[i]]$new_cases / population_vector[i]) * 100
  df_vector[[i]]$total_deaths <- (df_vector[[i]]$total_deaths / population_vector[i]) * 100
  df_vector[[i]]$new_deaths <- (df_vector[[i]]$new_deaths / population_vector[i]) * 100
  df_vector[[i]]$total_vaccinations <- (df_vector[[i]]$total_vaccinations / population_vector[i]) * 100
  
  df_vector[[i]]$country <- country_vector[i]
}

# Put everything together to one df
df <- df_vector[[1]]

for(i in 2:length(filenames)){
  df <- merge(df, df_vector[[i]], all = TRUE)
}

# Export the combined df
write.csv(df, "all_percentage.csv", row.names = FALSE)

# Create country_vector but only countries
country_vector <- c("Chile",
                    "Estonia",
                    "Latvia",
                    "Slovenia")

# Import tidyverse library
library(tidyverse)

# Filter the combined df to just the countries
df_country <- filter(df,
                     country %in% country_vector)

# Export countries only df
write.csv(df_country, "all_countries_percentage.csv", row.names = FALSE)