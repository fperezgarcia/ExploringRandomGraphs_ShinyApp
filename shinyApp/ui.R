library(shiny)

# Define UI for dataset viewer application
shinyUI(pageWithSidebar(
    
    # Application title
    headerPanel("Exploring Erdos-Renyi Poisson Random Graphs"),
    
    # Sidebar with controls to select a dataset and specify the number
    # of observations to view
    sidebarPanel(
        sliderInput("nodes", "Number of nodes ", min = 10, max = 200, value = 50, step = 10),
        numericInput("prob", "Link probability", 0.025, min=0.001, max=1, step=0.001),
        selectInput("color", "Color nodes by:",choices = c("degree", "pagerank", "betweenness")),
        submitButton("Update Graph")
    ),
    
    # Show a summary of the dataset and an HTML table with the requested
    # number of observations
    mainPanel(
        
        tabsetPanel(
            tabPanel("Plot", htmlOutput("hint"), plotOutput("graphPlot")),
            tabPanel("Degree Distribution", plotOutput("histPlot")),
            tabPanel("Help", htmlOutput("helpText"))
        )

    )
))