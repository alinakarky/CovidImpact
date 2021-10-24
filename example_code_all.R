# Set your working directory here
path = "C:/Users/calda/OneDrive/Documents/University - UC Master/Semester 2 2021/11524 ARVR for Data Analysis and Communication PG/Assignment 2"
setwd(path)

filename <- "all_countries_percentage.csv"

df <- read.csv(filename, header = TRUE)

library(ggplot2)

# All new cases plotted on new cases %
ggplot(data = df, aes(x = date, y = new_cases, group = country, color = country)) +
  geom_line()

# All new deaths plotted on new cases %
ggplot(data = df, aes(x = date, y = new_deaths, group = country, color = country)) +
  geom_line()

# All total vaccination plotted on new cases %
ggplot(data = df, aes(x = date, y = total_vaccinations, group = country, color = country)) +
  geom_line()