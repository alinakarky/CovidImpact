# Set your working directory here
path = "C:/Users/calda/OneDrive/Documents/University - UC Master/Semester 2 2021/11524 ARVR for Data Analysis and Communication PG/Assignment 2"
setwd(path)

# CSV file path
file_january = "covid_data_per_month/covid_data_january.csv"

# Import dataframe
january_df <- read.csv(file_january, header = TRUE)

# Convert date column to R date datatype
january_df$date <- as.Date(january_df$date, format = "%Y-%m-%d")

# Plot time series of new cases
plot(january_df$date,
     january_df$new_cases,
     main = "New Cases per Day in January",
     xlab = "Time",
     ylab = "New Cases")

# Plot total_vaccine vs new_cases
plot(january_df$total_vaccinations,
     january_df$new_cases,
     main = "total_vaccine vs new_cases",
     xlab = "Total Vaccine",
     ylab = "New Cases")