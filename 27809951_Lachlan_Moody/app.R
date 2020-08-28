library(shiny)
library(tidyverse)
library(ggplot2)


Locations <- read_csv("data/Pedestrian_Counting_System_-_Sensor_Locations (Exercise 2).csv")


Counts <- read_csv("data/Pedestrian_Counting_System_2019 (Exercise 2).csv")


Counts$Day <- factor(Counts$Day, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))


Counts <- Counts %>%
    mutate(Sensor_Name = if_else(Sensor_Name == "Collins St (North)", "Collins Street (North)", Sensor_Name)) %>%
    mutate(Sensor_Name = if_else(Sensor_Name == "Flinders la - Swanston St (West) Temp", "Flinders La - Swanston St (West) Temporary", Sensor_Name)) %>%
    mutate(Sensor_Name = if_else(Sensor_Name == "Flinders La - Swanston St (West) Temp", "Flinders La - Swanston St (West) Temporary", Sensor_Name)) %>%
    mutate(Sensor_Name = if_else(Sensor_Name == "Lincoln-Swanston(West)", "Lincoln-Swanston (West)", Sensor_Name)) %>%
    mutate(Sensor_Name = if_else(Sensor_Name == "Pelham St (S)", "Pelham St (South)", Sensor_Name))


Locations <-  Locations %>%
    mutate(sensor_name = if_else(sensor_name == "Melbourne Central-Elizabeth St (East)Melbourne Central-Elizabeth St (East)", "Melbourne Central-Elizabeth St (East)", sensor_name)) %>%
    mutate(sensor_name = if_else(sensor_name == "Building 80 RMIT", "Swanston St - RMIT Building 80", sensor_name)) %>%
    mutate(sensor_name = if_else(sensor_name == "RMIT Building 14", "Swanston St - RMIT Building 14", sensor_name))


Total_Counts <- Counts %>%
    group_by(Sensor_Name) %>%
    summarise(average = mean(Hourly_Counts))


Combined <- Total_Counts %>% 
    left_join(Locations, by = c("Sensor_Name" = "sensor_name"))



Daily_Counts <- Counts %>%
    group_by(Sensor_Name, Time, Day) %>%
    summarise(average = mean(Hourly_Counts))


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
