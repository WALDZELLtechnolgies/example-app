# ----Place this line at the top of the app.R file ----
options(shiny.maxRequestSize = 30 * 1024^2) # Increase file size limit to 30MB

# Set your working directory and load the data
setwd("C:/Users/JonasEmanuelBühler/OneDrive - IWP/Dokumente/Doktorat/Credtis/Digital Skills UniLu/Introduction to Shiny in R/app/data/")
europe <- readRDS("./europe.rds")

library(shiny)         # Package for shiny applications
library(dplyr)         # Package for data manipulations
library(magrittr)      # Package for pipe operator
library(ggplot2)       # Package for creating graphs
library(shinythemes)   # Package for shiny app themes

course_data <- europe %>%
  mutate(AvgTemperatureC = round((AvgTemperatureF - 32) * 5/9, 1)) # Create a new column with Avg Temperature in Celsius

# Define UI for application
ui <- fluidPage(
  
  # ----Place the below into the UI part of the app ----

  
  # Specify the selected theme to the 'theme' argument
  theme = shinytheme("flatly"),
  
  # Application title
  titlePanel("COURSE SHINY APP"),
  
  sidebarLayout(
    # Sidebar panel
    # Inside the sidebarPanel, reorder the elements to place the upload button at the end
    sidebarPanel(
      "This is the sidebar panel",
      
      # Input: A simple slider ----
      sliderInput(inputId = "year", label = "Year",
                  min = 2000,
                  max = 2019,
                  step = 1,
                  value = 2000,
                  sep = ''),
      
      # Input: A simple drop down list  ----
      selectInput(inputId = "country", label = "Country:",
                  choices = c(sort(unique(course_data$Country)))),
      
      # Input: A simple drop down list  ----
      selectInput(inputId = "city", label = "City:",
                  choices = c(sort(unique(course_data$City)))),
      
      # Input: A simple text input  ----
      textInput(inputId = "text_input", label = "Input text here:"),
      
      # Input: A simple radio button input  ----
      radioButtons(inputId = "temp_scale", label = "Temperature scale:",
                   choices = list("Fahrenheit" = "fahrenheit",
                                  "Celsius" = "celsius"),
                   selected = "fahrenheit"),
      
      # Input: Action button that subsets storm data ----
      actionButton(inputId ="button_xyz", label ="superAPP letssss GO!"), # Action button inclusion in the sidebar
      
      # Input: Upload a single RDS file ----
      fileInput(inputId ="file_xyz", label ="Upload a file (RDS)",
                multiple = FALSE,  # Allow to upload multiple files
                accept = c(".rds"))
    ),
    
    
    
    # Main panel
    mainPanel(
      "This is the main panel",
      
      textOutput(outputId = "text_output"),
      
      # Layout: Tabset with info, data, and plots tabs ----
      tabsetPanel(type = "tabs",
                  tabPanel(title = "Info",
                           h1("App Description"),
                           br(),
                           h2("jonas testttet"),
                           hr(),
                           h3("hallo liebe Welt das ist ein paragrapb"),
                           br(),
                           strong(p("hallo hallo hallo")),
                           hr(),
                           hr(),
                           p("This is the course shiny app. It is created during the course 
                           exercises using the europe.rds data: 
                           Average daily temperatures (in Fahrenheit) from cities around
                           Europe from 2000 to 2019"),
                           
                           verbatimTextOutput("data_summary")
                  ),
                  tabPanel(title = "Data",
                           br(),
                           h2("jonas testttet"),
                           br(),
                           br(),
                           dataTableOutput("data_table")
                  ),
                  tabPanel(title = "Plots",
                           fluidRow(
                             column(width = 12, plotOutput("lineplot"))
                           ),
                           fluidRow(
                             column(width = 6, plotOutput("boxplot")),
                             column(width = 6, plotOutput("lineplotTemp"))
                           )
                  )
      )
    )
  )
)

# Define server side logic
server <- function(input, output, session) {
  
  # ----Place the below into the server part of the app ----
  # Upload file and read in data
  course_data <- eventReactive(input$file_xyz, {
    readRDS(input$file_xyz$datapath)
  })
  
  country_df <- eventReactive(input$button_xyz, {
    course_data() %>%
      filter(Year >= input$year) %>%
      filter(Country == input$country)
  })
  
  city_df <- reactive({
    country_df() %>%
      filter(City == input$city) %>%
      filter(Year == input$year)
  })
  
  year_df <- reactive({
    country_df() %>%
      filter(City == input$city) %>%
      filter(Year == input$year) %>%
      group_by(Country, City, Year, Month) %>%
      summarise(MinTempF = min(AvgTemperatureF),
                MeanTempF = round(mean(AvgTemperatureF), 1),
                MaxTempF = max(AvgTemperatureF),
                MinTempC = min(AvgTemperatureC),
                MeanTempC = round(mean(AvgTemperatureC), 1),
                MaxTempC = max(AvgTemperatureC)) %>%
      ungroup()
    
  })
  
  # Output: Render a text output  ----
  output$text_output <- renderText({
    paste("Your inputs are:", input$year, input$country, input$city, input$text_input, input$temp_scale)
  })
  
  # Output: Render a print output  ----
  output$data_summary <- renderPrint({
    summary(course_data())
  })
  
  # Output: Render a (dynamic) table output  ----
  output$data_table <- renderDataTable({
    city_df()
  })
  
  # Output: Render a plot output  ----
  output$lineplot <- renderPlot({
    ggplot(data = city_df()) +
      geom_line(mapping = aes(x = Date, y = AvgTemperatureF), size = 1) +
      ylab("Average daily temperatures (in Fahrenheit)")
  })
  
  # Output: Render a plot output  ----
  output$boxplot <- renderPlot({
    ggplot(data = country_df()) +
      geom_boxplot(mapping = aes(x = Month, y = AvgTemperatureF, group = Year))
  })
  
  # Output: Render a plot output  ----
  output$lineplotTemp <- renderPlot({
    if (input$temp_scale == "fahrenheit") {
      res <- ggplot(data = year_df()) +
        geom_line(mapping = aes(x = Month, y = MinTempF), size = 1, colour = "red", linetype = "dotted") +
        geom_line(mapping = aes(x = Month, y = MeanTempF), size = 1, colour = "black") +
        geom_line(mapping = aes(x = Month, y = MaxTempF), size = 1, colour = "red", linetype = "dotted") +
        scale_x_discrete(name = "", limits = month.abb) +
        ylab("Average daily temperatures (in Fahrenheit)")
    }
    
    if (input$temp_scale == "celsius") {
      res <- ggplot(data = year_df()) +
        geom_line(mapping = aes(x = Month, y = MinTempC), size = 1, colour = "red", linetype = "dotted") +
        geom_line(mapping = aes(x = Month, y = MeanTempC), size = 1, colour = "black") +
        geom_line(mapping = aes(x = Month, y = MaxTempC), size = 1, colour = "red", linetype = "dotted") +
        scale_x_discrete(name = "", limits = month.abb) +
        ylab("Average daily temperatures (in Celsius)")
    }
    
    return(res)
  })
  
  observe({
    new_choices <- unique(course_data()$City[course_data()$Country == input$country])
    updateSelectInput(session, inputId ="city", choices = new_choices)
  })
}

# Run the application
shinyApp(ui = ui, server = server)

