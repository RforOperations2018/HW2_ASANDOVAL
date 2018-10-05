# Homework #2

library(shiny)
library(reshape2)
library(dplyr)
library(plotly)
library(shinythemes)
library(stringr)
 
# new url   url <- "https://phl.carto.com/api/v2/sql?q=SELECT+*+FROM+shootings" 
# Upload Philly shooting victim data from Opendataphilly
shootings <- read.csv ("shootings.8.csv")

# A couple of fields in the shootings data were modified to help build the app. 
# Code_2 is a cleaner field than code
# Wound  field was cleaned and turned into broader categories
# Race field did not include Latino status, which was a separate field. It was then cleaned to be inclusive
# Sex was cleaned from M and F to Male and Female
# NA's were included where there was blank data

shootings.load <- shootings %>%
  mutate(dc_key = as.character(dc_key),
         the_geom_webmercator = as.character(the_geom_webmercator),
         location = as.factor(location))

pdf(NULL)

# Define UI for application that draws a histogram
ui <- navbarPage("Exploring Shooting Victim Data from Philadelphia", 
                 tabPanel("Plot",
                          sidebarLayout(
                            sidebarPanel(
                              # Selecting type of crime
                              selectInput("crimeSelect",
                                          "Type of Incident:",
                                          choices = sort(unique(shootings.load$Code_2)),
                                          multiple = TRUE,
                                          selectize = TRUE,
                                          selected = c("Aggravated Assualt", "Robbery", "Homicide", "Hospital Cases")),
                              
                              # Year of Incident Slider
                              sliderInput("yearSelect",
                                          "Year of Incident:",
                                          min = min(shootings.load$year, na.rm = T),
                                          max = max(shootings.load$year, na.rm = T),
                                          value = c(min(shootings.load$year, na.rm = T), max(shootings.load$year, na.rm = T)),
                                          step = 1),
                              
                              # Check box Input for whether incident occured inside
                              checkboxGroupInput(inputId = "IncidentInside",
                                                 label = "Was the Incident Inside?:",
                                                 choiceNames = list("Yes", "No"),
                                                 choiceValues = list("1", "0")
                              ),
                              
                              # Action button
                              actionButton("reset", "Reset Filters", icon = icon("refresh"))
                            ),
                            
                            # Output plot
                            mainPanel(
                              plotlyOutput("woundplotc"), 
                              plotlyOutput("sexplot"),
                              plotlyOutput("raceplot"))
                          )),
                 
                 # Data Table
                 tabPanel("Table",
                          inputPanel(
                            downloadButton("downloadData","Download Victim Data")
                          ),
                          fluidPage(DT::dataTableOutput("table"))
                 )
)

# Define server logic
server <- function(input, output, session = session) {
  # Filtered shootings data
  shootInput <- reactive({
    shootings <- shootings.load %>%
      # Slider Filter
      filter(year >= input$yearSelect[1] & year <= input$yearSelect[2])
    
    # Type of Crime Filter
    if (length(input$crimeSelect) > 0 ) {
      shootings <- subset(shootings, Code_2 %in% input$crimeSelect)
    } 
    
    # Location of Incident
    if (length(input$IncidentInside) > 0 ) {
      shootings <- subset(shootings, inside %in% input$IncidentInside)
    }
    
    return(shootings)
  })
  # Reactive melted data
  meltInput <- reactive({
    shootInput() %>%
      melt(id = "Code_2")
  })
  
  # Column plot showing types of wounds
  output$woundplotc <- renderPlotly({
    dat <- shootInput()
    ggplotly(
      ggplot(data = dat, aes(x = wound_2, fill = wound_2 )) + 
        geom_bar(position = position_dodge(width = 0.9)) +
        xlab("Area of Injury") +
        theme(axis.text.x = element_text (angle = 45,
                                          hjust = 1),
              legend.title = element_blank()) +
        guides(color = FALSE)) 
    
  })
  # Sex bar plot
  output$sexplot <- renderPlotly({
    dat <- shootInput()
    ggplotly(
      ggplot(data = dat, aes(x = sex, fill = sex)) + 
        geom_bar (position = position_dodge(width = 0.9)) +
        xlab("Sex") +
        theme(legend.title = element_blank()) +
        guides(color = FALSE))
  })
  # Race bar plot
  output$raceplot <- renderPlotly({
    dat <- shootInput()
    ggplotly(
      ggplot(data = dat, aes(x = race, fill = race)) + 
        geom_bar (position = position_dodge(width = 0.9)) +
        xlab("Race") +
        theme(legend.title = element_blank()) +
        guides(color = FALSE))
  })
  
  # Data Table
  output$table <- DT::renderDataTable({
    shootings <- shootInput()
    subset(shootings, select = c(Code_2, offender_injured, location, race, sex, dist, time))
  })
  
  # Updating the URL Bar
  observe({
    print(reactiveValuesToList(input))
    session$doBookmark()
  })
  
  onBookmarked(function(url) {
    updateQueryString(url)
  })
  
  # Download data in the datatable
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("shootings", Sys.Date(), ".csv", sep="")
    },
    
    content = function(file) {
      write.csv(shootInput(), file)
    }
  )
  
  # Reset Filter Data
  observeEvent(input$reset, {
    updateSelectInput(session, "crimeSelect", selected = c("Aggravated Assualt", "Robbery", "Homicide", "Hospital Cases"))
    updateCheckboxGroupInput(session, "IncidentInside", label = NULL, choices = NULL, selected = c("Y", "N"))
    updateSliderInput(session, "yearSelect", value = c(min(shootings.load$year, na.rm = T), max(shootings.load$year, na.rm = T)))
    showNotification("You have successfully reset the filters", type = "message")
  })
}

# Run the application 
shinyApp(ui = ui, server = server, enableBookmarking = "url")


