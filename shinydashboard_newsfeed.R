### Project Title
#     Description
#
#     By Patrick Rogers, California Research Bureau
#       Month 2021
#
#     Uses the following Packages
#       {shiny}
#       {shinydashboard}
#       {xml2}
#       {blastula}
#       {httr}
#
#     Uses the following data
#       data.csv

# Clear the workspace
remove(list=ls(all=TRUE))

# Inits
#setwd(here::here())

# Load Packages
library(shiny)
library(shinydashboard)
library(xml2)
library(blastula)

# Load Sourcefiles

# User Parameters
newsRSSLink <- "https://www.newsdesk.lexisnexis.com/feed/a7f6a242eb126827.rss?n=30&img=1&ext=sc"
feedLength <- 10

# Custom Functions ####
newsItem <- function(linkURL, linkText, linkDate, linkExtract, linkLicense, linkImage){
  # Inputs:
  #   linkURL       URL of the link to the newsfeed item
  #   linkText      Text of link to the newsfeed item
  #   linkDate      Date published of the newsfeed item
  #   linkExtract   Short summary or extract of the newsfeed item
  #   linkLicense   License status of the newsfeed item ("Publicly Available"/"LexisNexis Licensed")
  #   linkImage     URL of an image accompanying the newsfeed item
  #
  # Example uses the LexisNexis NewsDesk RSS feed, which follows the following XML schema:
  # <item>
  #   <title></title>
  #   <link></link>
  #   <description></description>
  #   <source url=""></source>
  #   <enclosure url="" type=""/>
  #   <pubDate></pubDate>
  #   <m:article_id></m:article_id>
  #   <m:licenses><m:license><m:name></m:name></m:license></m:licenses>
  # </item>
  
  # Build the first part of the HTML formatted newsfeed item
  itemBuild <- div(icon(linkLicense), strong(a(href=linkURL, target="_blank", linkText)))
  
  # Slightly different structure, depending on if there is an image enclosure or not
  if(is.na(linkImage)){
    itemBuild <- tagAppendChild(itemBuild, p(em("Published ", linkDate, " - "), linkExtract))
  } else {
    itemBuild <- tagAppendChild(itemBuild, p(img(src = linkImage, height = "100px", class = "newsFeedImage"),
                                             em("Published ", linkDate, " - "),
                                             linkExtract))
  }
  return(itemBuild)
}

# Build News Item List ####
feed <-
  xml_find_all(read_xml(
    x = httr::GET(url = newsRSSLink)
  ),
  ".//item")

# Parse xml into seperate vectors
links <- xml_text(xml_find_all(feed, "//item//link"))
titles <- xml_text(xml_find_all(feed, "//item//title"))
dates <- xml_text(xml_find_all(feed, "//item//pubDate"))
desc <- xml_text(xml_find_all(feed, "//item//description"))
feedLicense  <- xml_text(xml_find_all(feed, "//item//m:license"))
feedImages <- xml_attr(xml_child(xml_find_all(feed, "//item"), "enclosure"), "url")

# Set up lcoked/unlocked icons
feedLicense <- gsub(pattern = "Publicly Available", replacement = "lock-open", feedLicense)
feedLicense <- gsub(pattern = "LexisNexis Licensed", replacement = "lock", feedLicense)

# Convert into html tags
feedItems <- vector("list", feedLength)

for(i in 1:feedLength){
  feedItems[[i]] <- newsItem(links[i], titles[i], dates[i], desc[i], feedLicense[i], feedImages[i])
}

# Build UI ####
modalNewsEmail <- modalDialog(
  # Email selected news items dialog box
  title = "Email Selected News Articles",
  textInput("newsEmailAddress", label = "Email(s): ", value = NULL, placeholder = "first.last@agency.ca.gov"),
  textAreaInput("newsEmailMessage", label = "Message (optional): ", value = "Thought you would find these interesting!"),
  easyClose = TRUE,
  footer = tagList(
    modalButton("Cancel"),
    actionButton("newsEmailSend", "Send")
  )
)

sidebar <- dashboardSidebar(sidebarMenu(
  menuItem(
    "Issue 1",
    tabName = "item1",
    icon = icon("train")
  )
))

body <- dashboardBody(
  tags$head(
    # Note the wrapping of the string in HTML()
    tags$style(HTML("
      img.newsFeedImage{
      display: block;
       float: left;
       margin-right: 20px;
                    }"))
  ),
  
  tabItems(
    
    # Issue Tab 1 ####
    tabItem(tabName = "item1",
            h2("Issue Item 1"),
            fluidRow(
              tabBox(width = 12,
                     # News Feed Subtab ####
                     tabPanel(tagList(shiny::icon("newspaper"), "News Coverage"),
                              fluidRow(
                                #column(1,checkboxInput("selectall",NULL, width = "50%")),
                                #column(4, actionButton("newsSelectAll", label = "Select All", icon = icon("check-square"))),
                                #column(4,actionButton("newsEmailSelected", label = "Email", icon = icon("envelope")))
                                column(12,
                                       actionButton("newsSelectAll", label = "Select All", icon = icon("check-square")),
                                       span(HTML("&nbsp;")),
                                       actionButton("newsEmailSelected", label = "Email", icon = icon("envelope")),
                                       span(HTML("&nbsp;")),
                                       actionButton("newsDownloadSelected", label = "Download", icon = icon("file-download")),
                                       span(HTML("&nbsp;")),
                                       actionButton("newsRSSSelected", label = "RSS", icon = icon("rss-square"),
                                                    onclick = paste0("window.open('", newsRSSLink, "', '_blank')"))
                                )
                              ),
                              br(),
                              checkboxGroupInput("newsLinks", NULL,
                                                 choiceNames = feedItems,
                                                 choiceValues =
                                                   as.list(1:length(feedItems)),
                                                 width = "100%"
                              ),
                              
                              
                              # textOutput("selectedtxt")
                     ),
                     # Panel 2 ####
                     tabPanel(tagList(shiny::icon("newspaper"), "Panel 2"),
                              fluidRow(box(width = 6, "Some text"),
                                       box(width = 6, "More text"))),
                     # Research Subtab
                     tabPanel(tagList(shiny::icon("book"), "Panel 3"),
                              fluidRow(box(width = 12, "Some text"),
                                       box(width = 12, "More text")))
              )))))

ui <- dashboardPage(dashboardHeader(title = "Example Dashboard"),
                    
                    sidebar,
                    body)

# Step 2 ####

server <- function(input, output, session) {
  
  observeEvent(input$newsSelectAll, {
    if(input$newsSelectAll%%2 == 1) {
      updateCheckboxGroupInput(session, "newsLinks", choiceNames = feedItems, choiceValues = as.list(1:length(feedItems)), selected=1:10)
    } else {
      updateCheckboxGroupInput(session, "newsLinks", choiceNames = feedItems, choiceValues = as.list(1:length(feedItems)), selected=NULL)
    }
  })
  
  observeEvent(input$newsEmailSelected, {
    showModal(modalNewsEmail)
  })
  
  observeEvent(input$newsDownloadSelected, {
    showModal(modalDialog(
      title = "Download Selected Articles",
      "Download is not implemented yet.",
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  observeEvent(input$newsEmailSend, {
    test_message <- compose_email(
      body = div(div(input$newsEmailMessage),
                 div(feedItems[as.numeric(input$newsLinks)]))
      )
    
    smtp_send(
      email = test_message,
      from = "patrick95350@gmail.com",
      to = input$newsEmailAddress,
      subject = "Testing the `smtp_send()` function",
      credentials = creds(
        user = "patrick95350@gmail.com",
        provider = "gmail"
      )
    )
    
    removeModal()
  })
}

# Run! ####
shinyApp(ui, server)
