# This is the app that will plot total vaccinations and total cases bar plot for 4 countries 
# The app lets you to choose a month.

library(shiny)
library(ggplot2)
library(tidyverse)
library(dplyr)


# all the month based files are loaded into dataset "filenames"
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

# Vector to dictate the months
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
# vector of months
month_vector <- c("January",
                  "February",
                  "March",
                  "April",
                  "June",
                  "July",
                  "August",
                  "September")


#based on length of filenames dataset a list mode vector is created
df_vector <- vector(mode = "list", length = length(filenames))

#for all the files, a loop is performed on df_vector so that we can get each month's dataset at once
for(i in 1:length(filenames)){
  df_vector[[i]] <- read.csv(filenames[i], header = TRUE)
  # date filed in the dataframe is formatted as year-month-date
}


# Define UI for application that draws a barplot
ui <- fluidPage(

    sidebarPanel(
      # to generate select input for names of month based on month vector we created
      selectInput(inputId = "monthName",
                  label = "Select month:",
                  choices = month_vector,
                  selected = month_vector[1],
                  width = '100%')
    ),
                  mainPanel(
                               titlePanel("COVID Cases and Vaccinations along various Countries"),
                               
                                         fluidRow(
                                           splitLayout(cellWidths = c("44%", "45%"), plotOutput("covidcasesPlot"), plotOutput("vacPlot"))
                                         )
                               
                  )  
    )

#  Server logic required to draw a barplot
server <- function(input, output) {
  
  # Plot for Covid Cases
    output$covidcasesPlot <- renderPlot({
      # to get selected values from select input 
      
      # index of selected month
      selected_index <- match(input$monthName, month_vector)
      
      #based on index, particular data from the vector is chosen
      df <- df_vector[[selected_index]]
      
      # date filter applied based on index
      df <- subset(df, date >= month_boundary[[selected_index, 1]] & date <= month_boundary[[selected_index,2]])
      
      # data frame is created group all daya by location and applying filter for 4 selected countried
      # per and vacPer fields created to represent data as percentage as data comes in log value 
      totalcaseswithcount <- as.data.frame(df %>% group_by(location)
                                           %>%filter(location == c("Chile", "Slovenia","Estonia","Latvia" ))
                                           %>% summarise(TotalCase=sum(total_cases), TotalVaccination=sum(total_vaccinations),Count=n())
                                           %>% mutate(per=round(TotalCase/sum(TotalCase)*100, 2),vacPer=round(TotalVaccination/sum(TotalVaccination)*100, 2)))

      # bar plot is created with lavels and theme
      
       ggplot(totalcaseswithcount, aes(x = location, y=per, fill=location))+
        geom_bar(stat = "identity",width = 0.5) +
        geom_text(aes(label=per), vjust=-0.3, size=3.5)+
        xlab("Country")+
        ylab("Total Covid Cases (%)")+
        ggtitle("Country wise covid cases")+
        theme(axis.title.x = element_text(size = 11, hjust=0.5),
              axis.title.y = element_text(size = 11, hjust=0.5),
              plot.title = element_text(size = 12, face = "bold" ,hjust=0.5),
              aspect.ratio = 1/1)
      
           })
    
    # Plot for vaccinations
    output$vacPlot <- renderPlot({
      
      # indx of selected month
      selected_index <- match(input$monthName, month_vector)
      # data based on index 
      df <- df_vector[[selected_index]]
      
      # data frame grouped by country and applied filter for 4 countries
      totalcaseswithcount <- as.data.frame(df %>% group_by(location)
                                           %>%filter(location == c("Chile", "Slovenia","Estonia","Latvia" ))
                                           %>% summarise(TotalCase=sum(total_cases), TotalVaccination=sum(total_vaccinations),Count=n())
                                           %>% mutate(per=round(TotalCase/sum(TotalCase)*100, 2),vacPer=round(TotalVaccination/sum(TotalVaccination)*100, 2)))

      # barplot for vaccinations
      ggplot(totalcaseswithcount, aes(x = location, y=vacPer, fill=location))+
        geom_bar(stat = "identity", width=0.5) +
        geom_text(aes(label=vacPer), vjust=-0.3, size=3.5)+
        xlab("Country")+
        ylab("Total Covid Vaccinations (%)")+
        ggtitle("Country wise covid vaccinations")+
        theme(axis.title.x = element_text(size = 11, hjust=0.5),
              axis.title.y = element_text(size = 11, hjust=0.5),
              plot.title = element_text(size = 12, face = "bold" ,hjust=0.5),
              aspect.ratio = 1/1)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
