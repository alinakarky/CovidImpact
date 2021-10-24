#load shiny
library(shiny)

# Set your working directory here
path = "C:/Users/calda/OneDrive/Documents/University - UC Master/Semester 2 2021/11524 ARVR for Data Analysis and Communication PG/Assignment 2"
setwd(path)

filenames <- c("covid_data_per_country/covid_data_asia.csv",
               "covid_data_per_country/covid_data_chile.csv",
               "covid_data_per_country/covid_data_estonia.csv",
               "covid_data_per_country/covid_data_europe.csv",
               "covid_data_per_country/covid_data_european_union.csv",
               "covid_data_per_country/covid_data_latvia.csv",
               "covid_data_per_country/covid_data_north_america.csv",
               "covid_data_per_country/covid_data_slovenia.csv",
               "covid_data_per_country/covid_data_south_america.csv")

month_boundary <- c("2021-01-01", "2021-01-31",
                    "2021-02-01", "2021-02-28",
                    "2021-03-01", "2021-03-31",
                    "2021-04-01", "2021-04-30",
                    "2021-05-01", "2021-05-31",
                    "2021-06-01", "2021-06-30",
                    "2021-07-01", "2021-07-31",
                    "2021-08-01", "2021-08-31",
                    "2021-09-01", "2021-09-30")

month_boundary <- matrix(month_boundary,
                         nrow = 9,
                         ncol = 2,
                         byrow = TRUE)

country_vector <- c("Asia",
                    "Chile",
                    "Estonia",
                    "Europe",
                    "European Union",
                    "Latvia",
                    "North America",
                    "Slovenia",
                    "South America")

month_vector <- c("All",
                  "January",
                  "February",
                  "March",
                  "April",
                  "June",
                  "July",
                  "August",
                  "September")

df_vector <- vector(mode = "list", length = length(filenames))

for(i in 1:length(filenames)){
  df_vector[[i]] <- read.csv(filenames[i], header = TRUE)
  df_vector[[i]]$date <- as.Date(df_vector[[i]]$date, format = "%Y-%m-%d")
  df_vector[[i]]$lethality <- df_vector[[i]]$new_deaths / df_vector[[i]]$total_cases
}

#create ui
ui <- fluidPage(
  titlePanel("Countries by Month App"),
  sidebarPanel(
    selectInput(inputId = "countryName",
                label = "Select country:",
                choices = country_vector,
                selected = country_vector[1],
                width = '100%'),
    selectInput(inputId = "monthName",
                label = "Select month:",
                choices = month_vector,
                selected = month_vector[1],
                width = '100%'),
    textOutput("informationOutput")
    
  ),
  mainPanel(
    plotOutput(outputId = "newCasesHistogram"),
    plotOutput(outputId = "newDeathsHistogram"),
    plotOutput(outputId = "totalVaccinationHistogram"),
    plotOutput(outputId = "lethalityHistogram")
  )
)

#create server
server <- function(input, output) {
  output$informationOutput <- renderText({
    selected_index <- match(input$countryName, country_vector)
    df <- df_vector[[selected_index]]
    if(!(input$monthName == "All")){
      selected_index <- match(input$monthName, month_vector)
      selected_index <- selected_index - 1
      df <- subset(df, date >= month_boundary[[selected_index, 1]] & date <= month_boundary[[selected_index,2]])
    }
    
    vaccinationDifference <- df$total_vaccination[length(df$total_vaccination)] - df$total_vaccination[1]
    paste("Total vaccination:", vaccinationDifference)
  })
  
  output$newCasesHistogram <- renderPlot({
    selected_index <- match(input$countryName, country_vector)
    df <- df_vector[[selected_index]]
    if(!(input$monthName == "All")){
      selected_index <- match(input$monthName, month_vector)
      selected_index <- selected_index - 1
      df <- subset(df, date >= month_boundary[[selected_index, 1]] & date <= month_boundary[[selected_index,2]])
    }
    
    #plot the boxplot
    plot(df$date,
         df$new_cases,
         type = "l",
         main = paste("New Cases per Day in", input$countryName),
         xlab = "Time",
         ylab = "New Cases")
  })
  
  output$newDeathsHistogram <- renderPlot({
    selected_index <- match(input$countryName, country_vector)
    df <- df_vector[[selected_index]]
    if(!(input$monthName == "All")){
      selected_index <- match(input$monthName, month_vector)
      selected_index <- selected_index - 1
      df <- subset(df, date >= month_boundary[[selected_index, 1]] & date <= month_boundary[[selected_index,2]])
    }
    #plot the boxplot
    plot(df$date,
         df$new_deaths,
         type = "l",
         main = paste("New Deaths per Day in", input$countryName),
         xlab = "Time",
         ylab = "New Deaths")
  })
  
  output$totalVaccinationHistogram <- renderPlot({
    selected_index <- match(input$countryName, country_vector)
    df <- df_vector[[selected_index]]
    if(!(input$monthName == "All")){
      selected_index <- match(input$monthName, month_vector)
      selected_index <- selected_index - 1
      df <- subset(df, date >= month_boundary[[selected_index, 1]] & date <= month_boundary[[selected_index,2]])
    }
    #plot the boxplot
    plot(df$date,
         df$total_vaccinations,
         type = "l",
         main = paste("Total Vaccination per Day in", input$countryName),
         xlab = "Time",
         ylab = "Total Vaccination")
  })
  
  output$lethalityHistogram <- renderPlot({
    selected_index <- match(input$countryName, country_vector)
    df <- df_vector[[selected_index]]
    if(!(input$monthName == "All")){
      selected_index <- match(input$monthName, month_vector)
      selected_index <- selected_index - 1
      df <- subset(df, date >= month_boundary[[selected_index, 1]] & date <= month_boundary[[selected_index,2]])
    }
    #plot the boxplot
    plot(df$date,
         df$lethality,
         type = "l",
         main = paste("Lethality per Day in", input$countryName),
         xlab = "Time",
         ylab = "Lethality")
  })
  
}

#run shinyApp
shinyApp(ui = ui, server = server)