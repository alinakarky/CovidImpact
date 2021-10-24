# Set your working directory here
path = "C:/Users/calda/OneDrive/Documents/University - UC Master/Semester 2 2021/11524 ARVR for Data Analysis and Communication PG/Assignment 2"
setwd(path)

# CSV file path
file_north_america = "covid_data_per_country/covid_data_north_america.csv"

# Import dataframe
north_america_df <- read.csv(file_north_america, header = TRUE)

# Convert date column to R date datatype
north_america_df$date <- as.Date(north_america_df$date, format = "%Y-%m-%d")

# Plot time series of new cases
plot(north_america_df$date,
     north_america_df$new_cases,
     type = "l",
     main = "New Cases per Day in North America",
     xlab = "Time",
     ylab = "New Cases")

# Plot time series of new deaths
plot(north_america_df$date,
     north_america_df$new_deaths,
     type = "l",
     main = "New Death per Day in North America",
     xlab = "Time",
     ylab = "New Death")

# Plot time series of total vaccination
plot(north_america_df$date,
     north_america_df$total_vaccinations,
     type = "l",
     main = "Total Vaccination per Date in North America",
     xlab = "Time",
     ylab = "Total Vaccination")

# Here we can break down our dataset.
# Our total vaccination has a linear growth over time, however there has been a
# slight exponential increase from January to April. After that we have a relatively
# linear increase until September.
# We could observe that during the exponential increase in total vaccination,
# new cases also have an exponential decrease, and the new death also decreasing until that time.
# New cases exploded in July, we should look at the news what happened in July that explains
# the sudden increase.
# As the new case exploded, the new deaths also increased approaching September.