library(shiny)
library(tidyverse)
library(ggplot2)
library(leaflet)

Locations <- read_csv("data/Pedestrian_Counting_System_-_Sensor_Locations (Exercise 2).csv")


Counts <- read_csv("data/Pedestrian_Counting_System_2019 (Exercise 2).csv")


Counts$Day <- factor(Counts$Day, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))


Counts$Sensor_Name <- recode(Counts$Sensor_Name,
                             "Collins St (North)" = "Collins Street (North)",
                             "Flinders la - Swanston St (West) Temp" = "Flinders La - Swanston St (West) Temporary",
                             "Flinders La - Swanston St (West) Temp" = "Flinders La - Swanston St (West) Temporary",
                             "Lincoln-Swanston(West)" = "Lincoln-Swanston (West)",
                             "Pelham St (S)" = "Pelham St (South)"
)

Counts$Sensor_Name <- recode(Counts$Sensor_Name,
                             "Flinders La - Swanston St (West) Temporary" = "Flinders La-Swanston St (West)"
)

Locations$sensor_name <- recode(Locations$sensor_name,
                                "Melbourne Central-Elizabeth St (East)Melbourne Central-Elizabeth St (East)" = "Melbourne Central-Elizabeth St (East)",
                                "Building 80 RMIT" = "Swanston St - RMIT Building 80",
                                "RMIT Building 14" = "Swanston St - RMIT Building 14"
)



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
    titlePanel(
        h1("FIT5147 Programming Exercise 2: R", align = "center")
        ),

    # Sidebar with a slider input for number of bins
    sidebarLayout(position = "right",
        sidebarPanel(
            selectInput("select",
                        "Select sensor",
                        choices = unique(Combined$Sensor_Name),
                        selected = "Alfred Place")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           h2("Mapping Melbourne pedestrian counts"),
           leafletOutput("mymap", height = 500),
           h2("Average hourly counts for 2019 per day"),
           plotOutput("myplot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$mymap <- renderLeaflet({
        Combined %>%
            leaflet() %>%
            addProviderTiles("CartoDB.DarkMatter") %>%
            addCircles(~longitude, ~latitude,
                       radius = ~average/20,
                       label = paste("Sensor:", Combined$Sensor_Name),
                       popup = paste("Sensor:", Combined$Sensor_Name, "<br>",
                                     "Average Hourly Count:", round(Combined$average,digits = 0)),
                       col = "pink") %>%
            setView(mean(Combined$longitude), mean(Combined$latitude) + 0.002, zoom = 14)
    })

    output$myplot <- renderPlot({
        Plot_Daily <- Daily_Counts %>%
            filter(Sensor_Name == input$select) %>%
            ggplot(aes(x = Time,
                       y = average,
                       group = Sensor_Name)) +
            geom_line(colour = "pink",
                      size = 1) +
            labs(x = "Hour of day", y = "Average Count") +
            scale_x_continuous(breaks = 0:24) +
            theme(axis.text.x = element_text(size = 8)) +
            facet_wrap(~Day, ncol = 2) +
            theme_dark()
        print(Plot_Daily)
    })
}

# Run the application
shinyApp(ui = ui, server = server)
