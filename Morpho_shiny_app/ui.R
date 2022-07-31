library(shiny)

# Define UI ----
ui <- fluidPage(
  
  # Sidebar Layout ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Stages
      checkboxGroupInput("var_stages", 
                         h5("Select Stages"), 
                         choices = list("Instar 1" = 1, 
                                        "Instar 2" = 2, 
                                        "Instar 3" = 3,
                                        "Instar 4" = 4,
                                        "Instar 5" = 5,
                                        "Adult" = 6),
                         selected = 3),
      
      # Input: x axis
      selectInput("var_x", 
                  label = "Choose the x axis",
                  choices = list("Hue" = 13, 
                                 "Area" = 6,
                                 "Weight" = 4, 
                                 "Thickness" = 3),
                  selected = 4),
      
      # Input: y axis
      selectInput("var_y", 
                  label = "Choose the y axis",
                  choices = list("Hue" = 13, 
                                 "Area" = 6,
                                 "Weight" = 4, 
                                 "Thickness" = 3),
                  selected = 6),
      
      # Text: biary options
      helpText("Use the below to have 2 colours rather than a gradient."),
      
      # Input: binary options
      checkboxInput("var_binary", "Binary Colours", value = FALSE),
      
      # Input: binary cut off
      sliderInput("var_slider", h5("Green - Red Division Point"),
                  min = 0, max = 100, value = 50)
      
    ),
    
    # The main display panel
    mainPanel(
      
      # The main scatter plot
      plotOutput(outputId = "scat_plot")
      
    )
    
  )
  
)