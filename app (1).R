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
library(dplyr)
library(tidyr)

# Load the data
crash_data <- read.csv("C:/Users/chowd/OneDrive/Desktop/adv_R/ADV_R- project/crash_data.csv")

crash_data
# UI
ui <- fluidPage(
  
  # Title
  titlePanel("Crash Data Visualization"),
  
  # Sidebar
  sidebarLayout(
    sidebarPanel(
      
      # Select a plot to display
      selectInput("plot_type", label = "Select a plot:", 
                  choices = c("Top 10 Counties by Number of Crashes",
                              "Speed Limit vs Number of Vehicles Involved in Crashes",
                              "Number of Crashes by Crash Type and Drug Use",
                              "Heatmap of Crashes by Month and County",
                              "Box Plot of Speed Limit by Crash Type",
                              "Number of Crashes by Crash Type and Drinking Involvement"),
                  selected = "Top 10 Counties by Number of Crashes")
    ),
    
    # Main panel
    mainPanel(
      
      # Output plot
      plotOutput("plot")
    )
  )
)

# Server
server <- function(input, output) {
  
  # Top 10 Counties by Number of Crashes
  output$plot <- renderPlot({
    if (input$plot_type == "Top 10 Counties by Number of Crashes") {
      crash_count_by_county <- crash_data %>%
        group_by(County) %>%
        summarise(count = n()) %>%
        arrange(desc(count)) %>%
        head(10)
      
      ggplot(crash_count_by_county, aes(x = County, y = count, fill = County)) +
        geom_bar(stat = "identity") +
        labs(title = "Top 10 Counties by Number of Crashes", x = "County", y = "Number of Crashes") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
  })
  
  # Speed Limit vs Number of Vehicles Involved in Crashes
  output$plot <- renderPlot({
    if (input$plot_type == "Speed Limit vs Number of Vehicles Involved in Crashes") {
      ggplot(crash_data, aes(x = Speed_Limit_at_Crash_Site, y = Total_Motor_Vehicles)) +
        geom_point(alpha = 0.5) +
        labs(title = "Speed Limit vs Number of Vehicles Involved in Crashes", x = "Speed Limit at Crash Site", y = "Number of Vehicles Involved")
    }
  })
  
  # Number of Crashes by Crash Type and Drug Use
  output$plot <- renderPlot({
    if (input$plot_type == "Number of Crashes by Crash Type and Drug Use") {
      crash_count_by_type_and_drug_use <- crash_data %>%
        group_by(Crash_Type, Crash_Drug_Use) %>%
        summarise(count = n()) %>%
        arrange(desc(count))
      
      ggplot(crash_count_by_type_and_drug_use, aes(x = Crash_Type, y = count, fill = Crash_Drug_Use)) +
        geom_bar(stat = "identity") +
        labs(title = "Number of Crashes by Crash Type and Drug Use", x = "Crash Type", y = "Number of Crashes") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
