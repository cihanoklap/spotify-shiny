#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(DT)
library(tidyverse)
library(lubridate)
library(plotly)
library(rsconnect)



readRDS(url("https://s3.us-east-2.amazonaws.com/spotify-shiny/spotify_data.rds"))

all_artists <- sort(unique(spotify_data$Artist))
all_regions <- sort(unique(spotify_data$Region))

all_regions = c("Argentina" = "ar",
                "Austria" = "at",
                "Australia" = "au",
                "Belgium" = "be",
                "Bolivia" = "bo",
                "Brazil" = "br",
                "Canada" = "ca",
                "Switzerland" = "ch",
                "Chile" = "cl",
                "Colombia" = "co",
                "Costa Rica" = "cr",
                "Czech Republic" = "cz",
                "Germany" = "de",
                "Denmark" = "dk",
                "Dominican Republic" = "do",
                "Ecuador" = "ec",
                "Estonia" = "ee",
                "Spain" = "es", 
                "Finland" = "fi",
                "France" = "fr",
                "United Kingdom" = "gb",
                "Global" = "global",
                "Greece" = "gr",
                "Guetemala" = "gt",
                "Hong Kong" = "hk",
                "Honduras" = "hn",
                "Hungary" = "hu",
                "Indonesia" = "id",
                "Ireland" = "ie",
                "Iceland" = "is",
                "Italy" = "it",
                "Japan" = "jp",
                "Lithuania" = "lt",
                "Luxembourg" = "lu",
                "Latvia" = "lv",
                "Mexico" = "mx",
                "Malaysia" = "my",
                "Netherlands" = "nl",
                "Norway" = "no",
                "New Zealand" = "nz",
                "Panama" = "pa",
                "Peru" = "pe",
                "Philippines" = "ph",
                "Poland" = "pl",
                "Portugal" = "pt",
                "Paraguay" = "py",
                "Sweden" = "se",
                "Singapore" = "sg",
                "Slovakia" = "sk",
                "El Salvador" = "sv",
                "Turkey" = "tr",
                "Taiwan" = "tw",
                "United States" = "us",
                "Uruguay" = "uy")

min_date <- min(spotify_data$Date)
max_date <- max(spotify_data$Date)


# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("flatly"),
   
   # Application title
   titlePanel("Spotify's Daily Song Rankings by Country & Artist", windowTitle = "Spotify's Daily Song Rankings"),
   
   # Sidebar 
   sidebarLayout(
      sidebarPanel(
        
         selectizeInput(inputId = "Artist",
                     label = "Artist",
                     choices = all_artists,
                     selected = "Ed Sheeran",
                     multiple = FALSE),
         

         selectizeInput(inputId = "Region", 
                     label = "Country:",
                     choices = all_regions, 
                     selected = "us",
                     multiple = FALSE),
         
         dateRangeInput(inputId = "Date",
                         label = "Select dates:",
                         start = "2017-01-01",
                         end = "2018-01-01",
                         min = min_date, max = max_date)
         
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotlyOutput("plot"),
         br(),
         h5(htmlOutput("description")),
         h5("Each color represents different songs. You can check the names by", 
            tags$b("hovering the points.")),
         h5(htmlOutput("feelfree"))
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
   output$plot <- renderPlotly({
     ggplotly({
        data <- subset(spotify_data,
                      Date >= input$Date[1] & Date <= input$Date[2] &
                      Region == input$Region &
                      Artist == input$Artist)
     
     
        ggplot(data, aes(x = data$Date, y = data$Position, col = data$`Track Name`)) +
          geom_point(alpha = 0.7) +
          ggtitle(paste(input$Artist, "on Spotify Top 200 Daily List")) + 
          labs(x="Date",y="Position") +
          scale_y_reverse(breaks = seq(0,200,20)) +
          theme_bw() +
          theme(plot.title = element_text(size = 14)) +
          theme(legend.position="none")
     })
   })
   
   
   output$description <- renderUI({
     paste("The plot above shows the performance of ",
           input$Artist,
           "'s songs in the country you selected.")
   })

   
   output$feelfree <- renderUI({
     paste("Feel free to play around with the variables!")
   })
}

# Run the application 
shinyApp(ui = ui, server = server)


