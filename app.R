#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(scales)  # for percentage scales


filenames <- c("covid_data_per_month/covid_data_january.csv",
               "covid_data_per_month/covid_data_february.csv",
               "covid_data_per_month/covid_data_march.csv",
               "covid_data_per_month/covid_data_april.csv",
               "covid_data_per_month/covid_data_may.csv",
               "covid_data_per_month/covid_data_june.csv",
               "covid_data_per_month/covid_data_july.csv",
               "covid_data_per_month/covid_data_august.csv",
               "covid_data_per_month/covid_data_september.csv"
               )

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

month_vector <- c("January",
                  "February",
                  "March",
                  "April",
                  "June",
                  "July",
                  "August",
                  "September")

# data is loaded from csv file into dataframe
#april_data <- read.csv("covid_data/covid_data_april.csv")
#april_data$date <- as.Date(april_data$date, format = "%Y-%m-%d")

#
df_vector <- vector(mode = "list", length = length(filenames))

for(i in 1:length(filenames)){
  df_vector[[i]] <- read.csv(filenames[i], header = TRUE)
  df_vector[[i]]$date <- as.Date(df_vector[[i]]$date, format = "%Y-%m-%d")
  df_vector[[i]]$lethality <- df_vector[[i]]$new_deaths / df_vector[[i]]$total_cases
}
#dataframe is created by grouping data based on country and then total covid cases
# and total vaccinations were calculated using sum() along with the percentage value of each data


#since a lot of values are much lesser, only percentage value over 5% are selected

# Define UI for application that draws a barplot
ui <- fluidPage(

    # Application title
    sidebarPanel(
      selectInput(inputId = "monthName",
                  label = "Select month:",
                  choices = month_vector,
                  selected = month_vector[1],
                  width = '100%'),
      textOutput("informationOutput")
      
    ),
                  mainPanel(
                               titlePanel("COVID Cases and Vaccinations along various Countries"),
                               plotOutput("distPlot"), 
                               plotOutput("vacPlot"),
                               titlePanel("New Cases Vs Date Timeseries for different Countries"),
                               plotOutput("timePlot")
                  )  
    )

#  Server logic required to draw a barplot
server <- function(input, output) {
  

    output$distPlot <- renderPlot({
      selected_index <- match(input$monthName, month_vector)
      #selected_index <- selected_index - 1
      df <- df_vector[[selected_index]]
      df <- subset(df, date >= month_boundary[[selected_index, 1]] & date <= month_boundary[[selected_index,2]])
      totalcaseswithcount <- as.data.frame(df %>% group_by(location)
                                           %>% summarise(TotalCase=sum(total_cases), TotalVaccination=sum(total_vaccinations),Count=n())
                                           %>% mutate(per=round(TotalCase/sum(TotalCase)*100, 2),vacPer=round(TotalVaccination/sum(TotalVaccination)*100, 2))
                                           %>% arrange(desc(per)))
      totalcaseswithcount <- subset(totalcaseswithcount, per>5, vacPer>5)
      
       ggplot(totalcaseswithcount, aes(x = location, y=per, fill=as.factor(location)))+
        geom_bar(stat = "identity",width = 0.5) +
        #coord_flip()+
        geom_text(aes(label=per), vjust=-0.3, size=3.5)+
        xlab("Country")+
        ylab("Total Covid Cases (%)")+
        ggtitle("Country wise covid cases")+
        theme(axis.title.x = element_text(size = 11, hjust=0.5),
              axis.title.y = element_text(size = 11, hjust=0.5),
              plot.title = element_text(size = 12, face = "bold" ,hjust=0.5))
      
           })
    
    output$vacPlot <- renderPlot({
      selected_index <- match(input$monthName, month_vector)
      #selected_index <- selected_index - 1
      df <- df_vector[[selected_index]]
      df <- subset(df, date >= month_boundary[[selected_index, 1]] & date <= month_boundary[[selected_index,2]])
      totalcaseswithcount <- as.data.frame(df %>% group_by(location)
                                           %>% summarise(TotalCase=sum(total_cases), TotalVaccination=sum(total_vaccinations),Count=n())
                                           %>% mutate(per=round(TotalCase/sum(TotalCase)*100, 2),vacPer=round(TotalVaccination/sum(TotalVaccination)*100, 2))
                                           %>% arrange(desc(per)))
      totalcaseswithcount <- subset(totalcaseswithcount, per>5, vacPer>5)
      
      ggplot(totalcaseswithcount, aes(x = location, y=vacPer, fill=as.factor(location)))+
        geom_bar(stat = "identity", width=0.5) +
        #coord_flip()+
        geom_text(aes(label=vacPer), vjust=-0.3, size=3.5)+
        xlab("Country")+
        ylab("Total Covid Vaccinations (%)")+
        ggtitle("Country wise covid vaccinations")+
        theme(axis.title.x = element_text(size = 11, hjust=0.5),
              axis.title.y = element_text(size = 11, hjust=0.5),
              plot.title = element_text(size = 12, face = "bold" ,hjust=0.5))
    })
    
    output$timePlot <- renderPlot({
      selected_index <- match(input$monthName, month_vector)
      #selected_index <- selected_index - 1
      df <- df_vector[[selected_index]]
      ggplot(data = df, aes(x = date, y = new_cases, color=location))+geom_line()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
