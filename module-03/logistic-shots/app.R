library(shiny)
library(tidyverse)

url="https://raw.githubusercontent.com/jmayberr/math133-stat-learning-fall2026/refs/heads/main/Datasets/womenShots.csv"
shots=read.csv(url)
# UI: Sliders for beta0 and beta1
ui <- fluidPage(
  sliderInput("beta0", "β₀ (Intercept):", min = -5, max = 5, value = 0, step = 0.1),
  sliderInput("beta1", "β₁ (Slope):", min = -5, max = 5, value = 1, step = 0.1),
  plotOutput("logisticPlot", width = "400px", height = "200px")
)

# Server: Compute logistic function and binned proportions
server <- function(input, output) {
  
  output$logisticPlot <- renderPlot({
    # Define the logistic function
    logistic_func = function(x, beta0, beta1) {
      (1/(1 + exp(-(beta0 + beta1 * x))))
    }
    
    # Generate logistic curve data
    x_vals = seq(0, 8, by = 0.1)
    y_vals = logistic_func(x_vals, input$beta0, input$beta1)
    
    # Bin the shots based on vertical slices
    shots_binned = shots %>%
      group_by(y) %>%
      summarize(prop_goal = mean(Goal == "Goal")) 
    
    # Create the plot
    ggplot() +
      # Logistic curve
      geom_line(data = data.frame(x = x_vals, y = y_vals), aes(x = x, y = y), 
                color = "red2", size = 1) +
      # Bar plot of binned proportions
      geom_bar(data = shots_binned, aes(x = y, y = prop_goal), 
               stat = "identity", fill = "steelblue", color="white", 
               alpha = 0.6, width = 1) +
      labs(title = paste("Logistic Regression: β₀ =", input$beta0, ", β₁ =", input$beta1),
           x = "Vertical Distance", y = "Proportion of Goals")
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)