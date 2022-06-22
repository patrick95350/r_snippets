### Shiny Functionality - Show/Hide Conditional Panel
#     Uses actionButtons to show or hide a conditional panel
#
#     By Patrick Rogers, California Research Bureau
#       June 2022
#
#     Uses the following Packages
#       {shiny}

# Load Packages
library(shiny)

# Shiny UI ####
ui = fluidPage(
  actionButton(inputId = "showpanel", "Show Panel"),
  actionButton(inputId = "hidepanel", "Hide Panel"),
  conditionalPanel("output.show_panel_flag == '1'", 'Hidden Panel'),
)

# Shiny Server ####
server = function(input, output, session) {
  show_panel_flag = reactiveVal(0)
  
  observeEvent(input$showpanel, {
    show_panel_flag(1)
  })
  
  observeEvent(input$hidepanel, {
    show_panel_flag(0)
  })
  
  output$show_panel_flag = renderText({ show_panel_flag() })
  outputOptions(output, 'show_panel_flag', suspendWhenHidden=FALSE)
}

# shinyApp ####
shinyApp(ui = ui, server = server)
