# Set your working directory here
path = "C:/Users/calda/OneDrive/Documents/University - UC Master/Semester 2 2021/11524 ARVR for Data Analysis and Communication PG/Assignment 2"
setwd(path)

# CSV file path
file_north_america = "covid_data_per_country/covid_data_north_america.csv"

# Import dataframe
north_america_df <- read.csv(file_north_america, header = TRUE)

# Convert date column to R date datatype
north_america_df$date <- as.Date(north_america_df$date, format = "%Y-%m-%d")

# Add lethality column of new_deaths/total_cases
north_america_df$lethality <- north_america_df$new_deaths / north_america_df$total_cases

# Plot time series lethality
plot(north_america_df$date,
     north_america_df$lethality,
     type = "l",
     main = "Lethality in North America",
     xlab = "Time",
     ylab = "Lethality")