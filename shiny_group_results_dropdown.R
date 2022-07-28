### Shiny Functionality - Group data with navigation
#     Breaks data in to equally sized groups. Uses selectInput (drop downs) to
#     specify size of groups and for changing which group is displayed
#
#     By Patrick Rogers, California Research Bureau
#       July 2022
#
#     Uses the following Packages
#       {shiny}

library(shiny)

# Sample data values. These would be determined by the actual data in production
max_results <- 200
data_raw <- round(runif(n=max_results, min = 0, max=1000))

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Grouped Results Example"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      h4("Settings"),
      uiOutput("settings_total_size"),
      uiOutput("settings_results_per_group"),
      uiOutput("settings_results_group_num")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      uiOutput("results")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  # Set up holdover values to prevent overwriting input options during reactive update
  selected_results_per_group <- reactiveVal(10)
  selected_results_group_num <- reactiveVal(1)
  
  # Slider to specify number of observations/data points
  output$settings_total_size <- renderUI({
    tagList(
      sliderInput(inputId = "results_total_size",
                  label = "Size of Dataset",
                  min = 1,
                  max =  max_results,
                  value = 100)
    )
  })
  
  # Drop down to select the number of data points within each group
  output$settings_results_per_group <- renderUI({
    # Don't run unless results_total_size has finished initializing
    req(input$results_total_size)
    # Default number of data points per group
    choices_per_group <- 10 
    # Make 20 and 50 as options if there are enough data points
    if(input$results_total_size > 10) { choices_per_group <- c(choices_per_group, 20) }
    if(input$results_total_size > 20) { choices_per_group <- c(choices_per_group, 50) }
    names(choices_per_group) <- choices_per_group
    
    # Create the drop down, using the holdover value as the default
    tagList(
      selectInput("results_per_group",
                  label = "Results Per Group",
                  choices = choices_per_group,
                  selected = selected_results_per_group())
    )
  })
  
  output$settings_results_group_num <- renderUI({
    # Don't run unless results_per_group has finished initializing
    req(input$results_per_group)
    # Calculate the number of groups based on results_total_size and results_per_group
    choices_group_num <- 1:(ceiling(input$results_total_size / as.numeric(input$results_per_group)))
    names(choices_group_num) <- paste("Group", choices_group_num)
    
    # Create the drop down, using the holdover value as the default
    tagList(
      selectInput("results_group_num",
                  label = "Current Group",
                  choices_group_num,
                  selected = selected_results_group_num())
    )
  })
  
  output$results <- renderUI({
    # Don't run unless results_group_num has finished initializing
    req(input$results_group_num)
    # Update holdover values based on UI selections
    selected_results_per_group(input$results_per_group)
    selected_results_group_num(input$results_group_num)
    # Calculate starting and end index values, truncating end index if necessary
    item_index_start <- (as.numeric(input$results_group_num) - 1) * as.numeric(input$results_per_group) + 1
    item_index_end <- as.numeric(input$results_group_num) * as.numeric(input$results_per_group)
    if(item_index_end > input$results_total_size){ item_index_end <- input$results_total_size }
    
    # Set up the results for printing to screen
    tagList(
      h4("Underlying Values"),
      hr(),
      p("Input Values"),
      p("Size:", input$results_total_size),
      p("Results Per Group:", input$results_per_group),
      p("Current Group:", input$results_group_num),
      p("Current Group:", selected_results_group_num()),
      hr(),
      p("Calculated Values"),
      p("First Item in Current Group:", item_index_start),
      p("Last Item in Current Group:", item_index_end),
      hr(),
      h4("Results"),
      p(lapply(item_index_start:item_index_end, \(x) {span(x, ":", data_raw[x], br())} ))
    )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
