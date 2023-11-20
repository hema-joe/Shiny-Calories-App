

getwd()
setwd("C:/Users/conta/OneDrive/Documents")

# Install required packages

install.packages("shiny") #for Shiny Apps R operations
install.packages("shinyjs") #to perform JavaScript operations
install.packages("shinytheme") #for Theme selection

# Load libraries
library(shinythemes)
library(shiny)
library(shinyjs)

# Define UI
ui <- fluidPage(theme = shinytheme("united"),
  titlePanel("Calories Calculator with Chart"),
  sidebarLayout(
    sidebarPanel(
      numericInput("food1", "Food Option 1 Calories", value = 0),
      numericInput("food2", "Food Option 2 Calories", value = 0),
      numericInput("food3", "Food Option 3 Calories", value = 0),
      actionButton("calculate", "Calculate"),
      br(),
      h4("Calories Information:"),
      textOutput("totalCalories"),
      textOutput("healthStatus"),
      textOutput("exerciseAdvice")
    ),
    mainPanel(
      plotOutput("caloriesChart")
    )
  )
)

# Define server
server <- function(input, output) {
  observe({
    shinyjs::enable("calculate")
  })
  
  observeEvent(c(input$food1, input$food2, input$food3), {
    # Disable "Calculate" button if there is a negative number input
    if (any(c(input$food1, input$food2, input$food3) >= 0)) {
      shinyjs::enable("calculate")
    } else {
      shinyjs::disablet("calculate")
    }
  })
  
  
  observeEvent(input$calculate, {
    # Calculate total calories
    total_calories <- input$food1 + input$food2 + input$food3
    output$totalCalories <- renderText(paste("Total Calories: ", total_calories))
    
    # Check Calories status
    health_status <- total_calories - 330
    output$healthStatus <- renderText({
      if (health_status == 0) {
        "Okay"
      } else if (health_status > 0) {
        "Unhealthy"
      } else {
        "Healthy; Recommended!"
      }
    })
    
    # Calculate exercise advice
    if (health_status > 0) {
      minutes_to_burn <- health_status / 10
      output$exerciseAdvice <- renderText(paste("Jog", round(minutes_to_burn),
                      "minutes to burn the extra calories as per MET formular"))
    } else {
      output$exerciseAdvice <- renderText("")
    }
    
    # Plot the calories chart with color gradient
    output$caloriesChart <- renderPlot({
      bar_data <- data.frame(
        Food = c("Option 1", "Option 2", "Option 3"),
        Calories = c(input$food1, input$food2, input$food3)
      )
      
      # Set color based on total calories
      color_gradient <- ifelse(total_calories > 330,
                               colorRampPalette("red")(3), "green")
                      #Color transition from green (healthy) to red (unhealthy)
      
      barplot(bar_data$Calories, names.arg = bar_data$Food, col = color_gradient,
              main = "Calories Chart", xlab = "Food Options", ylab = "Calories",
              ylim = c(0, max(bar_data$Calories) + 100))
    })
  })
}

# Run the application
shinyApp(ui, server)



install.packages('rsconnect')
library(rsconnect)
rsconnect::deployApp("C:/Users/conta/OneDrive/Documents")
