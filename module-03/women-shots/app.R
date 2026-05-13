library(shiny)
library(ggplot2)
library(viridis)
library(dplyr)

url="https://raw.githubusercontent.com/jmayberr/math133-stat-learning-fall2026/refs/heads/main/Datasets/womenShots.csv"
womenShots=read.csv(url) |>
  dplyr::filter(Hand!="Unknown",Type!="Unknown",Tactic!="Unknown") %>%
  mutate(Hand=fct_relevel(Hand,"Right","Left"),
         Type=fct_relevel(Type,"Normal"),
         Defense=fct_relevel(Defense,"Uncontested"),
         Tactic_Type=fct_collapse(Tactic,"Even"=c("Center","Direct","Double Post",
                                                  "Drive","End Quarter","Individual",
                                                  "Perimeter","Post up","Rebound",
                                                  "Shot Clock"),
                                  "Power Play"=c("5 on 4","6 on 4","6 on 5","Quick"),
                                  "Counter"=c("First Wave","Second Wave","Transition")),
         Tactic_Type=fct_relevel(Tactic_Type,"Even")) %>%
  select(-c("X.1","X","start.time","end.time","category","Nth.instance","X..descriptors"))
goal_glm1=glm(Goal=="Goal"~abs(xj)+yj,family="binomial",data=womenShots)
goal_glm2=update(goal_glm1,.~.+Hand+Type+Defense+Tactic_Type)

# Define UI
ui <- fluidPage(
  titlePanel("Predicted Goal Probabilities"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("hand", "Select Hand:", choices = c("Right", "Left"), selected = "Right"),
      selectInput("type", "Select Type:", choices = unique(womenShots$Type), selected = "Normal"),
      selectInput("defense", "Select Defense:", choices = unique(womenShots$Defense), selected = "Uncontested"),
      selectInput("tactic", "Select Tactic Type:", choices = unique(womenShots$Tactic_Type), selected = "Even")
    ),
    
    mainPanel(
      plotOutput("heatmap")
    )
  )
)

# Define Server
server <- function(input, output) {
  
  output$heatmap <- renderPlot({
    
    # Generate grid based on user selections
    x_range <- seq(min(womenShots$xj), max(womenShots$xj), length.out = 100)
    y_range <- seq(0, max(womenShots$yj), length.out = 100)
    
    grid <- expand.grid(
      xj = x_range, yj = y_range,
      Hand = input$hand,
      Type = input$type,
      Defense = input$defense,
      Tactic_Type = input$tactic
    )
    
    # Predict probabilities
    grid$pred_prob <- predict(goal_glm2, newdata = grid, type = "response")
    
    # Create heatmap
    ggplot(grid, aes(x = xj, y = yj, fill = pred_prob)) +
      geom_tile() +
      scale_fill_viridis_c(option = "plasma") +  
      labs(x = "xj", y = "yj", fill = "Probability") +
      theme_minimal() +
      theme(panel.grid = element_blank())
  })
}

# Run the application 
shinyApp(ui = ui, server = server)