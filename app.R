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
 




# A couple of fields in the shootings data were modified to help build the app. 
# Code_2 is a cleaner field than code
# Wound  field was cleaned and turned into broader categories
# Race field did not include Latino status, which was a separate field. It was then cleaned to be inclusive
# Sex was cleaned from M and F to Male and Female
# NA's were included where there was blank data

# shootings.load <- shootings %>%
#   mutate(dc_key = as.character(dc_key),
#          the_geom_webmercator = as.character(the_geom_webmercator),
#          location = as.factor(location))

pdf(NULL)

# Define UI for application that draws a histogram
ui <- navbarPage("Exploring Shooting Victim Data from Philadelphia", 
                 tabPanel("Plot",
                          sidebarLayout(
                            sidebarPanel(
                              # Selecting type of crime
                              selectInput("crimeSelect",
                                          "Type of Incident:",
                                          choices = sort(unique(shootings.load$code)),
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
                              plotlyOutput("codeplot"),
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
url <- "https://phl.carto.com/api/v2/sql?q=SELECT+*+FROM+shootings" 
# Make the Request
r <- RETRY("GET", URLencode(url))

# Extract Content
c <- content(r, "text")

# Create Dataframe
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


# Upload Philly shooting victim data from Opendataphilly
shootings.load <- newdat
server <- function(input, output, session = session) {
  # Filtered shootings data
  shootInput <- reactive({
    shootings <- shootings.load %>%
      # Slider Filter
      filter(year >= input$yearSelect[1] & year <= input$yearSelect[2])
    
    # Type of Crime Filter
    if (length(input$crimeSelect) > 0 ) {
      shootings <- subset(shootings, code %in% input$crimeSelect)
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
      melt(id = "code")
  })
  
  # Column plot showing types of wounds
  output$woundplotc <- renderPlotly({
    dat <- shootInput()
    ggplotly(
      ggplot(data = dat, aes(x = wound, fill = wound )) + 
        geom_bar(position = position_dodge(width = 0.9), na.rm = TRUE) +
        xlab("Area of Injury") +
        theme(axis.text.x = element_text (angle = 45,
                                          hjust = 1),
              legend.title = element_blank()) +
        guides(color = FALSE)) 
    
 })
  
  # A plot showing the the fequency of incidents over the years
  output$codeplot <- renderPlotly({
    dat <- shootInput()
    ggplotly(
      ggplot(data = dat, aes(x = year, color = code)) + 
      geom_freqpoly() +
      guides(fill = FALSE) +
      scale_x_continuous(name = "Incident Year") +
      scale_y_continuous(name = "Types of Incidents Per Year") +
      theme(legend.title = element_blank()))
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


