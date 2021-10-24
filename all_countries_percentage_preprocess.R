# Set your working directory here
path = "C:/Users/calda/OneDrive/Documents/University - UC Master/Semester 2 2021/11524 ARVR for Data Analysis and Communication PG/Assignment 2"
setwd(path)

filenames <- c("covid_data_per_country/covid_data_asia.csv",
               "covid_data_per_country/covid_data_chile.csv",
               "covid_data_per_country/covid_data_estonia.csv",
               "covid_data_per_country/covid_data_europe.csv",
               "covid_data_per_country/covid_data_latvia.csv",
               "covid_data_per_country/covid_data_north_america.csv",
               "covid_data_per_country/covid_data_slovenia.csv",
               "covid_data_per_country/covid_data_south_america.csv")

country_vector <- c("Asia",
                    "Chile",
                    "Estonia",
                    "Europe",
                    "Latvia",
                    "North America",
                    "Slovenia",
                    "South America")

asia_pop <- 4560667108
# Source: "World Population prospects - Population division"".
# population.un.org. United Nations Department of Economic and Social Affairs,
# Population Division. Retrieved 9 November 2019.

chile_pop <- 17574003
# Source: "RESULTADOS CENSO 2017" (PDF). RESULTADOS DEFINITIVOS CENSO 2017.
# National Statistics Institute. 1 January 2018.
# Retrieved 18 January 2017.

estonia_pop <- 1330068
# Source: "Population at beginning of year". Statistics Estonia. 2021.

europe_pop <- 746419440
# Source: ""World Population prospects - Population division"". population.un.org.
# United Nations Department of Economic and Social Affairs, Population Division.
# Retrieved 9 November 2019.

latvia_pop <- 1907675
# Source:  "Population number, its changes and density | Central Statistical Bureau of Latvia".
# www.csb.gov.lv.

north_america_pop <- 592296223
# Source:  ""World Population prospects - Population division"".
# population.un.org. United Nations Department of Economic and Social Affairs,
# Population Division. Retrieved November 9, 2019.

slovenia_pop <- 2107126
# Source: "Prebivalstvo" (in Slovenian).
# Statistical Office of Slovenia. Retrieved 1 April 2021.

south_america_pop <- 423581078
# Source:  ""World Population prospects - Population division"".
# population.un.org. United Nations Department of Economic and Social Affairs,
# Population Division. Retrieved November 9, 2019.

population_vector <- c(asia_pop,
                       chile_pop,
                       estonia_pop,
                       europe_pop,
                       latvia_pop,
                       north_america_pop,
                       slovenia_pop,
                       south_america_pop)

df_vector <- vector(mode = "list", length = length(filenames))

for(i in 1:length(filenames)){
  df_vector[[i]] <- read.csv(filenames[i], header = TRUE)
  df_vector[[i]]$date <- as.Date(df_vector[[i]]$date, format = "%Y-%m-%d")
  df_vector[[i]]$lethality <- df_vector[[i]]$new_deaths / df_vector[[i]]$total_cases
  
  df_vector[[i]]$total_cases <- (df_vector[[i]]$total_cases / population_vector[i]) * 100
  df_vector[[i]]$new_cases <- (df_vector[[i]]$new_cases / population_vector[i]) * 100
  df_vector[[i]]$total_deaths <- (df_vector[[i]]$total_deaths / population_vector[i]) * 100
  df_vector[[i]]$new_deaths <- (df_vector[[i]]$new_deaths / population_vector[i]) * 100
  df_vector[[i]]$total_vaccinations <- (df_vector[[i]]$total_vaccinations / population_vector[i]) * 100
  
  df_vector[[i]]$country <- country_vector[i]
}

df <- df_vector[[1]]

for(i in 2:length(filenames)){
  df <- merge(df, df_vector[[i]], all = TRUE)
}

write.csv(df, "all_countries_percentage.csv", row.names = FALSE)