# Homework #2

library(shiny)
library(reshape2)
library(dplyr)
library(shinythemes)
library(stringr)
library(httr)
library(jsonlite)
library(plotly)
library(htmltools)
 
####MY ATTEMPT
ckanSQL <- function(url) {
  # MAKE REQUEST
  r <- RETRY("GET", URLencode(url))
  # EXTRACT CONTENT
  c <- content(r, "text")
  # Create Dataframe
  data.frame(jsonlite::fromJSON(c)$rows)
}
# UNIQUE VALUES FOR RESOURCE FIELD
ckanUniques <- function(field, id) {
  url <- paste0("https://phl.carto.com/api/v2/sql?q=SELECT+", field, "+FROM+", id)
  c(ckanSQL(URLencode(url)))
}

incident <- sort(ckanUniques("code", "shootings")$code)
years <- sort(ckanUniques("year", "shootings")$year)


pdf(NULL)

# Define UI for application that draws a histogram
ui <- navbarPage("Exploring Shooting Victim Data from Philadelphia", 
                 tabPanel("Plot",
                          sidebarLayout(
                            sidebarPanel(
                              # Selecting type of crime
                              selectInput("crimeSelect",
                                          "Type of Incident:",
                                          choices = incident,
                                          multiple = TRUE,
                                          selectize = TRUE,
                                          selected = c("Aggravated Assualt", "Robbery", "Homicide", "Hospital Cases")),
                              
                              # Year of Incident Slider
                              sliderInput("yearSelect",
                                          "Year of Incident:",
                                          min = min(years),
                                          max = max(years),
                                          value = c(min(years), max(years)),
                                          step = 1),
                              
                              # # Check box Input for whether incident occured inside
                              # checkboxGroupInput(inputId = "IncidentInside",
                              #                    label = "Was the Incident Inside?:",
                              #                    choiceNames = list("Yes", "No"),
                              #                    choiceValues = list("1", "0")
                              # ),
                              
                              # Action button
                              actionButton("reset", "Reset Filters", icon = icon("refresh"))
                            ),
                            
                            # Output plot
                            mainPanel(
                              plotlyOutput("codeplot", width = "100%"),
                              plotlyOutput("woundplotc", width = "100%"), 
                              plotlyOutput("raceplot", width = "100%"))
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
    shootInput <- reactive({
  url <- paste0("https://phl.carto.com/api/v2/sql?q=SELECT+*+FROM+shootings+WHERE+year+>=+'", input$yearSelect[1],"'+AND+<=+'",input$yearSelect[2],"'+AND+code+=+'",input$crimeSelect,"'")
  r <- RETRY("GET", URLencode(url))
  c <- content(r, "text")
  dat <- data.frame(jsonlite::fromJSON(c)$rows)
  # Change data types
  dat$code<- as.numeric(as.character(dat$code))
  dat$latino <- as.character(dat$latino)
  
  # Mutate data
  newdat <- dat %>%
    mutate(
      # Clean Data
      # Clean Wounds fields. This one took forever! I tried to do a case when IN function like in sql to save some 
      # lines of code, but no luck so I did it this way. I first opened the csv and manually categorized each value 
      # into a body area and then added all the quotes, equal signs, and squiggly signs in excel so I could just 
      # copy and paste it into r. I know this probably isn’t the best to clean data that is going to continually 
      # update since potentially a new cell could be spelled  aaaabbbdddoommenn  or some other incorrect way for 
      # abdomen but, this is what I could do.
      wound = case_when(
        wound == "aabdomen" ~ "Abdomen",
        wound == "abdom" ~ "Abdomen",
        wound == "abdome" ~ "Abdomen",
        wound == "abdomen" ~ "Abdomen",
        wound == "ankle" ~ "Ankle",
        wound == "arm" ~ "Arm",
        wound == "arms" ~ "Arm",
        wound == "elbow" ~ "Arm",
        wound == "forearm" ~ "Arm",
        wound == "back" ~ "Back",
        wound == "back/head" ~ "Back",
        wound == "flank" ~ "Back",
        wound == "body" ~ "Body",
        wound == "ribs" ~ "Body",
        wound == "side" ~ "Body",
        wound == "torso" ~ "Body",
        wound == "butt" ~ "Butt",
        wound == "buttock" ~ "Butt",
        wound == "buttocks" ~ "Butt",
        wound == "cheat" ~ "Chest",
        wound == "chest" ~ "Chest",
        wound == "chest/back" ~ "Chest",
        wound == "feet" ~ "Foot",
        wound == "foot" ~ "Foot",
        wound == "groin" ~ "Groin",
        wound == "testicle" ~ "Groin",
        wound == "HEAD" ~ "Head",
        wound == "cheek" ~ "Head",
        wound == "ear" ~ "Head",
        wound == "eye" ~ "Head",
        wound == "face" ~ "Head",
        wound == "face/multi" ~ "Head",
        wound == "head" ~ "Head",
        wound == "head-m" ~ "Head",
        wound == "head-md" ~ "Head",
        wound == "head/back" ~ "Head",
        wound == "head/chest" ~ "Head",
        wound == "head/mullt" ~ "Head",
        wound == "head/multi" ~ "Head",
        wound == "temple" ~ "Head",
        wound == "wrist" ~ "Hand",
        wound == "finger" ~ "Hand",
        wound == "hand" ~ "Hand",
        wound == "thumb" ~ "Hand",
        wound == "hip" ~ "Hip",
        wound == "pelvis" ~ "Hip",
        wound == "waist" ~ "Hip",
        wound == "calf" ~ "Leg",
        wound == "knee" ~ "Leg",
        wound == "leg" ~ "Leg",
        wound == "leg/buttoc" ~ "Leg",
        wound == "leg/multi" ~ "Leg",
        wound == "legs" ~ "Leg",
        wound == "LEG" ~ "Leg",
        wound == "shin" ~ "Leg",
        wound == "thigh" ~ "Leg",
        wound == "thighs" ~ "Leg",
        wound == "mukti" ~ "Multi",
        wound == "mullti" ~ "Multi",
        wound == "mult" ~ "Multi",
        wound == "mult/headi" ~ "Multi",
        wound == "multi" ~ "Multi",
        wound == "multi leg" ~ "Multi",
        wound == "multi tors" ~ "Multi",
        wound == "multi/arm" ~ "Multi",
        wound == "multi/face" ~ "Multi",
        wound == "multi/head" ~ "Multi",
        wound == "multli" ~ "Multi",
        wound == "mutli" ~ "Multi",
        wound == "mutli/head" ~ "Multi",
        wound == "neck" ~ "Neck",
        wound == "throat" ~ "Neck",
        wound == "shou" ~ "Shoulder",
        wound == "shoul" ~ "Shoulder",
        wound == "should" ~ "Shoulder",
        wound == "shouldeer" ~ "Shoulder",
        wound == "shoulder" ~ "Shoulder",
        wound == "shouldr" ~ "Shoulder",
        wound == "stom" ~ "Stomach",
        wound == "stomach" ~ "Stomach",
        wound == "unk" ~ "Unknown",
        TRUE ~ as.character(wound)
      ),
      
      # I tried to do a case when on the latino field to be in the race field by doing if latino == “1” ~ race == “Hispanic” but 
      # it didn’t work and couldn’t figure out a better way. This was my weird workaround to get latino into race. This command 
      # essentially turned race into false where latino == 1
      race = ifelse(latino == "1", race == "Hispanic", race),
      
      # I then did a case when to get it to be Hispanic and cleaned the others
      race = case_when(
        race == "A" ~ "Asian",
        race == "B" ~ "Black",
        race == "b" ~ "Black",
        race == "w" ~ "White",
        race == "W" ~ "White",
        race == "M" ~ "Multi",
        race == FALSE ~ "Hispanic",
        TRUE ~ as.character(race)
      ), 
      
      # Clean sex
      sex = ifelse(sex == "M", "Male", "Female"),
      
      # This was another tricky one. I originally tried to do a case when if code >= 100 <= 119 ~ “Homicide” but it didn’t work. This works but not ideal.
      code = case_when(
        code > 2999 ~ "Hospital Cases",
        code > 399 ~ "Aggravated Assault",
        code > 299 ~ "Robbery",
        code > 199 ~ "Rape",
        code > 99 ~ "Homicide",
        code < 100 ~ "Additional Victim",
        TRUE ~ as.character(code)
      )
    )
  return(shootings.load)
})


#### 
  # {
  # url <- "https://phl.carto.com/api/v2/sql?q=SELECT+*+FROM+shootings" 
  # # Make the Request
  # r <- RETRY("GET", URLencode(url))
  # 
  # # Extract Content
  # c <- content(r, "text")
  # 
  # # Create Dataframe
  # dat <- data.frame(jsonlite::fromJSON(c)$rows)
  # 
  # 
  # # Change data types
  # dat$code<- as.numeric(as.character(dat$code))
  # dat$latino <- as.character(dat$latino)
 
  
  
  # Upload Philly shooting victim data from Opendataphilly
 # shootings.load <- newdat
  
  
  # Filtered shootings data
 # shootInput <- reactive({
  #   shootings <- shootings.load %>%
  #     # Slider Filter
  #     filter(year >= input$yearSelect[1] & year <= input$yearSelect[2])
  #   
  #   # Type of Crime Filter
  #   if (length(input$crimeSelect) > 0 ) {
  #     shootings <- subset(shootings, code %in% input$crimeSelect)
  #   } 
  #   
  #   # Location of Incident
  #   if (length(input$IncidentInside) > 0 ) {
  #     shootings <- subset(shootings, inside %in% input$IncidentInside)
  #   }
  #   
  #   return(shootings)
  # })
  # Reactive melted data
  meltInput <- reactive({
    shootInput() %>%
      melt(id = "code")
  })
  # A plot showing the the fequency of incidents over the years
  output$codeplot <- renderPlotly({
    dat <- shootInput()
    ggplotly(
      ggplot(data = dat, aes(x = year, color = code)) + 
        geom_freqpoly() +
        guides(fill = FALSE) +
        scale_x_continuous(name = "Incident Year") +
        scale_y_continuous(name = "Counts") +
        ggtitle("Prevalent Incidents Per Year") +
        theme(legend.title = element_blank()), height = 400, width = 650)
  })
  
  # Column plot showing types of wounds
  output$woundplotc <- renderPlotly({
    dat <- shootInput()
    ggplotly(
      ggplot(data = dat, aes(x = wound, fill = as.character(fatal))) + 
        geom_bar (position = position_dodge(width = 0.7)) +
        xlab("       ") +
        ylab("Counts") +
        ggtitle("Where are Victims Injured the Most?") +
        theme(legend.position = "top",
              axis.text.x = element_text (angle = 30,
                                          hjust = 1,
                                          size = 7),
              legend.title=element_text(size = 7)) +
        guides(fill=guide_legend(title = "Was it Fatal?"), height = 400, width = 650))
 })
  

  
  # Race bar plot
  output$raceplot <- renderPlotly({
    dat <- shootInput()
    ggplotly(
      ggplot(data = dat, aes(x = race, fill = sex)) +
        geom_bar (position = position_dodge(width = 0.9)) +
        xlab("Race") +
        ylab("Counts") +
        ggtitle("Types of Victims") +
        theme(legend.title = element_blank()) +
        guides(color = FALSE), height = 400, width = 650)
  })
  
  # 
  # # A plot showing the sale price of properties
  # output$raceplot <- renderPlotly({
  #   dat <- shootInput()
  #   ggplotly()
  #   ggplot(data = dat, 
  #          aes(x = as.numeric(year), 
  #              y = as.numeric(dist), 
  #              fill = code))  + 
  #     geom_point(stroke = 0) +
  #     guides(fill = FALSE) +
  #     scale_y_continuous(name = "Districts", 
  #                        labels = comma) +
  #     scale_x_continuous(name = "Year") +
  #     theme(legend.title = element_blank())
  # })
  
  # Data Table
  output$table <- DT::renderDataTable({
    shootings <- shootInput()
    subset(shootings, select = c(code, offender_injured, location, race, sex, dist, time))
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


