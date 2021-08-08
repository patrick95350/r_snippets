### ShinyDashboard Skeleton
#     Simple starter framework for a Shiny dashboard with sidebar and tabbed boxes
#
#     By Patrick Rogers, California Research Bureau
#       Aug 2021
#
#     Uses the following Packages
#       {shiny}
#       {shinydashboard}

# Clear the workspace
remove(list=ls(all=TRUE))

# Inits
#setwd(here::here())

# Load Packages
library(shiny)
library(shinydashboard)

# Load Sourcefiles

# User Parameters

# Custom Functions ####

# Build UI ####
sidebar <- dashboardSidebar(sidebarMenu(
  menuItem(
    "Issue 1",
    tabName = "item1",
    icon = icon("train")
  ),
  menuItem(
    "Issue 2",
    tabName = "item2",
    icon = icon("car")
  ),
  menuItem(
    "Issue 3",
    tabName = "item3",
    icon = icon("fire")
  ),
  menuItem(
    "Issue 4",
    tabName = "item4",
    icon = icon("seedling")
  ),
  menuItem(
    "Issue 5",
    tabName = "item5",
    icon = icon("cloud-sun")
  )
))

body <- dashboardBody(
  tags$head(
    # Note the wrapping of the string in HTML()
    tags$style(HTML("
      /* CSS styles get tested here, before being moved to external for production. */"))
  ),
  
  tabItems(
    # Issue Tab 1 ####
    tabItem(tabName = "item1",
            h2("Issue Item 1"),
            fluidRow(
              tabBox(width = 12,
                     # Panel 1 ####
                     tabPanel(tagList(shiny::icon("chart-line"), "Panel 1"),
                              fluidRow(box(width = 12, "Some text"))),
                     # Panel 2 ####
                     tabPanel(tagList(shiny::icon("newspaper"), "Panel 2"),
                              fluidRow(box(width = 6, "Some text"),
                                       box(width = 6, "More text"))),
                     # Research Subtab
                     tabPanel(tagList(shiny::icon("book"), "Panel 3"),
                              fluidRow(box(width = 12, "Some text"),
                                       box(width = 12, "More text")))
              ))),
    
    # Issue Tab 2 ####
    tabItem(tabName = "item2",
            h2("Issue Item 2"),
            # Boxes need to be put in a row (or column)
            fluidRow(
              box(
                div("Item 2 stuff goes here.")
              ))),
    
    # Issue Tab 3 ####
    tabItem(tabName = "item3",
            h2("Issue Item 3"),
            fluidRow(
              box(
                div("Item 3 stuff goes here.")
              ))),
    
    # Issue Tab 4 ####
    tabItem(tabName = "item4",
            h2("Issue Item 4"),
            fluidRow(
              box(
                div("Item 4 stuff goes here.")
                ))),
    
    # Issue Tab 5 ####
    tabItem(tabName = "item5",
            h2("Issue Item 5"),
            fluidRow(
              box(
                div("You'll never guess what item stuff goes here!")
                )))
  )
)

ui <- dashboardPage(dashboardHeader(title = "Example Dashboard"),
                    sidebar,
                    body)

# Build Server ####

server <- function(input, output, session) {
# Nothing fancy is happening in this, just the UI skeleton
  
  
  
}

# Run! ####
shinyApp(ui, server)
