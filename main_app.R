# This is the app that allows us to see the graphs.
# The app lets you to choose a country and a month.
# Then the app will draw a time series on the new cases,
# new deaths, and total vaccination that focuses on that setting.

# Load shiny
library(shiny)

# Load packages
library(ggplot2)
library(dplyr)
library(gghighlight)

# Set the filename
filename <- "all_countries_percentage.csv"

# Vector to dictates the months
month_boundary <- c("2021-01-01", "2021-01-31",
                    "2021-02-01", "2021-02-28",
                    "2021-03-01", "2021-03-31",
                    "2021-04-01", "2021-04-30",
                    "2021-05-01", "2021-05-31",
                    "2021-06-01", "2021-06-30",
                    "2021-07-01", "2021-07-31",
                    "2021-08-01", "2021-08-31",
                    "2021-09-01", "2021-09-30")

# Turn into 9,2 matrix
month_boundary <- matrix(month_boundary,
                         nrow = 9,
                         ncol = 2,
                         byrow = TRUE)

# Vector of countries
country_vector <- c("All",
                    "Chile",
                    "Estonia",
                    "Latvia",
                    "Slovenia")

# Vector of months
month_vector <- c("All",
                  "January",
                  "February",
                  "March",
                  "April",
                  "June",
                  "July",
                  "August",
                  "September")

# Import the dataset and format the date
df <- read.csv(filename, header = TRUE)
df$date <- as.Date(df$date, format = "%Y-%m-%d")

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
    plotOutput(outputId = "newCasesPlot"),
    plotOutput(outputId = "newDeathsPlot"),
    plotOutput(outputId = "totalVaccinationPlot")
  )
)

#create server
server <- function(input, output) {
  
  output$newCasesPlot <- renderPlot({
    df_plot <- df
    
    if(!(input$monthName == "All")){
      selected_index <- match(input$monthName, month_vector)
      selected_index <- selected_index - 1
      df_plot <- subset(df, date >= month_boundary[[selected_index, 1]] & date <= month_boundary[[selected_index,2]])
    }
    
    if(!(input$countryName == "All")){
      ggplot(data = df_plot, aes(x = date, y = new_cases, group = country, color = country)) +
        geom_line() +
        gghighlight(country == input$countryName, use_direct_label = FALSE, keep_scales = TRUE) +
        ggtitle("Time series for new cases") +
        labs(y = "New Cases (%)", x = "Date") +
        theme_minimal()
    } else {
      ggplot() +
        geom_line(data = df_plot, aes(x = date, y = new_cases, group = country, colour = country)) + 
        ggtitle("Time series for new cases") +
        labs(y = "New Cases (%)", x = "Date")
    }
  })
  
  output$newDeathsPlot <- renderPlot({
    df_plot <- df
    
    if(!(input$monthName == "All")){
      selected_index <- match(input$monthName, month_vector)
      selected_index <- selected_index - 1
      df_plot <- subset(df, date >= month_boundary[[selected_index, 1]] & date <= month_boundary[[selected_index,2]])
    }
    
    if(!(input$countryName == "All")){
      ggplot(data = df_plot, aes(x = date, y = new_deaths, group = country, color = country)) +
        geom_line() +
        gghighlight(country == input$countryName, use_direct_label = FALSE, keep_scales = TRUE) +
        ggtitle("Time series for new deaths") +
        labs(y = "New Deaths (%)", x = "Date") +
        theme_minimal()
    } else {
      ggplot() +
        geom_line(data = df_plot, aes(x = date, y = new_deaths, group = country, colour = country)) + 
        ggtitle("Time series for new deaths") +
        labs(y = "New Deaths (%)", x = "Date")
    }
  })
  
  output$totalVaccinationPlot <- renderPlot({
    df_plot <- df
    
    if(!(input$monthName == "All")){
      selected_index <- match(input$monthName, month_vector)
      selected_index <- selected_index - 1
      df_plot <- subset(df, date >= month_boundary[[selected_index, 1]] & date <= month_boundary[[selected_index,2]])
    }
    
    if(!(input$countryName == "All")){
      ggplot(data = df_plot, aes(x = date, y = total_vaccinations, group = country, color = country)) +
        geom_line() +
        gghighlight(country == input$countryName, use_direct_label = FALSE, keep_scales = TRUE) +
        ggtitle("Time series for total vaccination") +
        labs(y = "Total Vaccination (%)", x = "Date") +
        theme_minimal()
    } else {
      ggplot() +
        geom_line(data = df_plot, aes(x = date, y = total_vaccinations, group = country, colour = country)) + 
        ggtitle("Time series for total vaccination") +
        labs(y = "Total Vaccination (%)", x = "Date")
    }
  })
}

#run shinyApp
shinyApp(ui = ui, server = server)