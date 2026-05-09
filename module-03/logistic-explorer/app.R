library(shiny)
library(tidyverse)
# Slider inputs for beta0 and beta1
ui <- fluidPage(
  sliderInput("beta0", "β₀ (beta0):", min = -5, max = 5, value = 0, step = 0.1),
  sliderInput("beta1", "β₁ (beta1):", min = -5, max = 5, value = 1, step = 0.1),
  plotOutput("logisticPlot", width = "400px", height = "200px")
)

# Server function to calculate logistic function and plot
server <- function(input, output) {
  
  output$logisticPlot <- renderPlot({
    # Define the logistic function
    logistic_func = function(x, beta0, beta1) {
      return (1/(1 + exp(-(beta0 + beta1 * x))))
    }
    
    # Generate data for plotting
    x_vals = seq(-10, 10, by = 0.1)
    y_vals = logistic_func(x_vals, input$beta0, input$beta1)
    
    # Plot the logistic function
    ggplot() +
      # Logistic curve
      geom_line(data = data.frame(x = x_vals, y = y_vals), aes(x = x, y = y))
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
