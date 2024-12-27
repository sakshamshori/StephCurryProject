#library(shiny)
library(ggplot2)
library(dplyr)

# dataset
stephstats = read.csv("./stephstats.csv")
steph = na.omit(stephstats)

# PPM calcultion
steph$PPM = steph$PTS / steph$MIN

# user interface
ui <- fluidPage(
  #title
  titlePanel("Steph Curry Career Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      #dropdown for 1 of 4 variables
      selectInput("var1", "Select a Variable (Univariate Graph):", 
                  choices = c("PTS", "AST", "REB", "PPM"), 
                  selected = "PTS"),
      #second dropdown for multivariate analysis
      selectInput("var2", "Select a Second Variable (Optional for Multivariate Graph, select None for univariate analysis.):", 
                  choices = c("None", "PTS", "AST", "REB", "PPM"), 
                  selected = "None"),
      #slider to specify how many points to consider in graphs
      sliderInput("filterPTS", "Filter Games by Points Scored:", 
                  min = min(steph$PTS), max = max(steph$PTS), 
                  value = c(min(steph$PTS), max(steph$PTS))),
      #displays mean for univariate graphs
      checkboxInput("showMean", "Display Mean on Plot (will only show for univariate)", value = FALSE),
      #color selection
      radioButtons("plotColor", "Choose a Plot Color:", 
                   choices = c("Red" = "red", "Green" = "green", "Lavender" = "lavender"),
                   selected = "blue"),
      actionButton("updatePlot", "Click Here to Visualize the First Plot")
    ),
    
    mainPanel(
      tabsetPanel(
        #tab 1
        tabPanel("Plot", 
                 plotOutput("mainPlot"),
                  #tags$hr(), 
                 textOutput("plotDescription")),
        #tab 2 
        tabPanel("Project Outline and Description", 
                 #steph curry description
                 HTML("<p><strong>Stephen Curry</strong> is widely regarded as one of the greatest basketball players in NBA history. Known for his exceptional shooting ability, 
                      he revolutionized the game with his three-point shooting and has set numerous records throughout his career. This analysis explores key aspects of Curry's career, 
                      including his points, assists, rebounds, and three-point shooting efficiency. By visualizing these statistics, we can gain insights into what makes Curry a dominant force on the court.</p>"),
                 tags$br(),
                 HTML("<p>In this project, there are 4 main components and factors that are considered in Steph Curry' career.<strong>Points (PTS), Rebounds (RBS), Assists (AST), and Points Per Minute (PPM).<strong> Each plot will provide analysis on the relationship between two statistics, or provide a singular statistic with the mean of that statistic.
                      All in all, this analysis will provide insight into what leads to Steph's high scoring games, and whether other factors such as rebounds and assists affect Curry's scoring abilities. <p>"),
                 img(src = "https://upload.wikimedia.org/wikipedia/commons/thumb/1/1a/Stephen_Curry_2.jpg/1071px-Stephen_Curry_2.jpg", 
                     height = "500px"),
                 p("Photo Source: Wikimedia Commons"))
      )
    )
  )
)

# Server logic
server = function(input, output) {
  # Reactive dataset filtered by Points Scored
  filteredData = reactive({
    filter(steph, PTS >= input$filterPTS[1] & PTS <= input$filterPTS[2])
  })
  
  # Render the Plot
  output$mainPlot = renderPlot({
    req(input$updatePlot)
    data = filteredData()
    
    validate(
      need(nrow(data) > 0, "No data available for the selected filter.")
    )
    
    if (input$var2 == "None") {
      # Univariate plot
      p = ggplot(data, aes_string(x = input$var1)) +
        geom_histogram(binwidth = 10, fill = input$plotColor, color = "black") +
        labs(title = paste("Distribution of", input$var1),
             x = input$var1, y = "Frequency") +
        theme_minimal()
      
      if (input$showMean) {
        p = p + geom_vline(aes_string(xintercept = mean(data[[input$var1]], na.rm = TRUE)),
                            color = "black", linetype = "dashed")
      }
      
      p
    } else {
      # Multivariate plot
      ggplot(data, aes_string(x = input$var1, y = input$var2)) +
        geom_point(color = input$plotColor, size = 3) +
        labs(title = paste(input$var1, "vs.", input$var2),
             x = input$var1, y = input$var2) +
        theme_minimal()
    }
  })
  
  # Descriptive Text
  output$plotDescription = renderText({
    req(input$updatePlot)  # Ensure this runs only when "Update Plot" is clicked
    data = filteredData()
    
    validate(
      need(nrow(data) > 0, "No data available for the selected filter.")
    )
    
    if (input$var2 == "None") {
      # Univariate plot descrip
      mean_val = mean(data[[input$var1]], na.rm = TRUE)
      paste("The selected variable is", input$var1, 
            ". The mean value for the filtered data is", round(mean_val, 2), ".")
    } else {
      # Multivariate plot descrip
      correlation = cor(data[[input$var1]], data[[input$var2]], use = "complete.obs")
      paste("The selected variables are", input$var1, "and", input$var2, 
            ". The correlation between these variables is", round(correlation, 2), ".")
    }
  })
}

shinyApp(ui = ui, server = server)
