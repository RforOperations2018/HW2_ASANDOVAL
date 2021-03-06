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

ckanSQL <- function(url) {
  # MAKE REQUEST
  r <- RETRY("GET", URLencode(url))
  # EXTRACT CONTENT
  c <- content(r, "text")
  # CREATE DATAFRAME
  data.frame(jsonlite::fromJSON(c)$rows)
}
# UNIQUE VALUES FOR RESOURCE FIELD
ckanUniques <- function(field, id) {
  url <- paste0("https://phl.carto.com/api/v2/sql?q=SELECT+", field, "+FROM+", id)
  c(ckanSQL(URLencode(url)))
}


years <- sort(ckanUniques("year", "shootings")$year)
inside <- sort(ckanUniques("inside", "shootings")$inside)
# This will let my code to remain as numbers but then I can only see one of the codes.
incident.backup <- sort(ckanUniques("code", "shootings")$code)

# Weird workaround to get from numbers to words for my crime input but then I can't see my plot 
url2 <- paste0("https://phl.carto.com/api/v2/sql?q=SELECT+p.*%2C++case+when+code2+%3C100+then+'Additional+Victim'+when+code2+%3C120+then+'Homicide'+when+code2+%3C300+then+'Rape'+when+code2+%3C400+then+'Robbery'+when+code2+%3C500+then+'Aggravated+Assault'+when+code2+%3C3901+then+'Hospital+Cases'+else+null+end+as+incidents+FROM+(SELECT+*%2C+CAST(code+AS+int)+as+code2+FROM+shootings)+as+p")
r2 <- RETRY("GET", URLencode(url2))
# EXTRACT CONTENT
c2 <- content(r2, "text")
# CREATE DATAFRAME
incident <- data.frame(jsonlite::fromJSON(c2)$rows)

pdf(NULL)

# Define UI for application that draws a histogram
ui <- navbarPage("Exploring Shooting Victim Data from Philadelphia", 
                 tabPanel("Plot",
                          sidebarLayout(
                            sidebarPanel(
                              # Selecting type of crime
                              selectInput("crimeSelect",
                                          "Type of Incident:",
                                          choices = incident$incidents,
                                          multiple = TRUE,
                                          selectize = TRUE,
                                          selected = c("Aggravated Assault")),
                              
                              # Year of Incident Slider
                              sliderInput("yearSelect",
                                          "Year of Incident:",
                                          min = min(years),
                                          max = max(years),
                                          value = c(min(years), max(years)),
                                          step = 1),
                              
                              # Check box Input for whether incident occured inside
                              checkboxGroupInput(inputId = "IncidentInside",
                                                 label = "Was the Incident Inside?:",
                                                 choiceNames = list("Yes", "No"),
                                                 choiceValues = list("1", "0")),
                              
                              # Action button
                              actionButton("reset", "Reset Filters", icon = icon("refresh"))),
                            
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
    loadshoot <- reactive({
      # Build API Query with proper encodes    
  url <- paste0("https://phl.carto.com/api/v2/sql?q=SELECT+*+FROM+shootings+WHERE+year+%3E%3D+'",input$yearSelect[1],"'+AND+year+%3C%3D+'",input$yearSelect[2],"'") #+AND+code+%3D+'",input$crimeSelect,"'")
  # For crimSelect you needed to use an IN statement: https://www.w3schools.com/sql/sql_in.asp
  print(url)
  dat <- ckanSQL(url) %>%  
    # https://phl.carto.com/api/v2/sql?q=SELECT+p.*%2C++case+when+code2+%3C100+then+'Additional+Victim'+when+code2+%3C120+then+'Homicide'+when+code2+%3C300+then+'Rape'+when+code2+%3C400+then+'Robbery'+when+code2+%3C500+then+'Aggravated+Assault'+when+code2+%3C3901+then+'Hospital+Cases'+else+null+end+as+incidents+FROM+(SELECT+*%2C+CAST(code+AS+int)+as+code2+FROM+shootings)+as+p 
    
    # # Location of Incident
    # if (length(input$IncidentInside) > 0 ) {
    #   dat <- subset(dat, inside %in% input$IncidentInside)
    # }

          # Clean Data
      # Clean Wounds fields. This one took forever! I tried to do a case when IN function like in sql to save some
      # lines of code, but no luck so I did it this way. I first opened the csv and manually categorized each value 
      # into a body area and then added all the quotes, equal signs, and squiggly signs in excel so I could just 
      # copy and paste it into r. I know this probably isn’t the best to clean data that is going to continually 
      # update since potentially a new cell could be spelled  aaaabbbdddoommenn  or some other incorrect way for 
      # abdomen but, this is what I could do.
  # You could have used tolower() and/or tools::toTitlCase() Also, if you have a list of things you want to match off of you can use %in% instead ie: wound %in% c("aabdomen", "abdom", "abdome", "abdomen") lastly you can use grepl()
   mutate(wound = case_when(
        wound == "aabdomen" ~ "Abdomen",
        wound == "abdom" ~ "Abdomen",
        wound == "abdome" ~ "Abdomen",
        wound == "abdomen" ~ "Abdomen",
        wound == "ankle" ~ "Ankle",
        wound == "arm" ~ "Arm",
        wound == "arms" ~ "Arm",
        wound == "elbow" ~ "Arm",
        wound == "forearm" ~ "Arm",
        wound == "BACK" ~ "Back",
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
        TRUE ~ as.character(wound)),
        
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
        TRUE ~ as.character(race)), 
      
      # Clean sex
      sex = ifelse(sex == "M", "Male", "Female"),
      
      # Change to numeric
      code = as.numeric(code),
      # This was another tricky one. I originally tried to do a case when if code >= 100 <= 119 ~ “Homicide” but it didn’t work. This works but not ideal.
      code = case_when(
        code > 2999 ~ "Hospital Cases",
        code > 399 ~ "Aggravated Assault",
        code > 299 ~ "Robbery",
        code > 199 ~ "Rape",
        code > 99 ~ "Homicide",
        code < 100 ~ "Additional Victim",
        TRUE ~ as.character(code)))
  
  return(dat)
})

  # A plot showing the the fequency of incidents over the years
  output$codeplot <- renderPlotly({
    dat <- loadshoot()
    ggplotly(
      ggplot(data = dat, aes(x = year, color = code)) + 
        geom_freqpoly() +
        guides(fill = FALSE) +
        scale_x_continuous(name = "Incident Year") +
        scale_y_continuous(name = "Counts") +
        ggtitle("Prevalent Incidents Per Year") +
        theme(legend.title = element_blank()), height = 400, width = 650)})
  
  # Column plot showing types of wounds
  output$woundplotc <- renderPlotly({
    dat<- loadshoot()
    ggplotly(
      ggplot(data = dat, aes(x = wound, fill = as.character(fatal))) + 
        geom_bar (position = position_dodge(width = 0.7)) +
        xlab(" ") +
        ylab("Counts") +
        ggtitle("Where are Victims Injured the Most?") +
        theme(legend.position = "top",
              axis.text.x = element_text (angle = 30,
                                          hjust = 1,
                                          size = 7),
              legend.title=element_text(size = 7)) +
        guides(fill=guide_legend(title = "Was it Fatal?"), height = 400, width = 650))})
  
  # Race bar plot
  output$raceplot <- renderPlotly({
    dat<- loadshoot()
    ggplotly(
      ggplot(data = dat, aes(x = race, fill = sex)) +
        geom_bar (position = position_dodge(width = 0.9)) +
        xlab("Race") +
        ylab("Counts") +
        ggtitle("Types of Victims") +
        theme(legend.title = element_blank()) +
        guides(color = FALSE), height = 400, width = 650)})
  
  # Data Table
  output$table <- DT::renderDataTable({
    dat<- loadshoot()
    subset(dat, select = c(code, wound, offender_injured, location, race, sex, dist, time))})
  
  # Updating the URL Bar
  observe({
    print(reactiveValuesToList(input))
    session$doBookmark()})
  
  onBookmarked(function(url) {
    updateQueryString(url)})
  
  # Download data in the datatable
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("shootings", Sys.Date(), ".csv", sep="")},
    
    content = function(file) {
      write.csv(loadshoot(), file)})
  
  # Reset Filter Data
  # I didn't even get to touch this part :(
  observeEvent(input$reset, {
    updateSelectInput(session, "crimeSelect", selected = c("Aggravated Assualt", "Robbery", "Homicide", "Hospital Cases"))
    updateCheckboxGroupInput(session, "IncidentInside", label = NULL, choices = NULL, selected = c("Y", "N"))
    updateSliderInput(session, "yearSelect", value = c(min(years), max(years)))
    showNotification("You have successfully reset the filters", type = "message")
  })
}

# Run the application 
shinyApp(ui = ui, server = server, enableBookmarking = "url")