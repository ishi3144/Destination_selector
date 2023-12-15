#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(ggplot2)
library(ggiraph)
library(shiny)
library(tidyverse)

data <- read.csv("Touris.csv")
colnames(data) <- c("Rank","Place","State","Ideal Month","Ideal Trip Time (Days)",
                    "Average Cost", "Average Stay", "Daily Cost", "Attractions",
                    "Rating", "Drinking Age")
data <- as_tibble(data)
# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel(""),
  
  
  sidebarLayout(
    sidebarPanel(
      #place to visit
      selectInput(inputId = "State",
                  label = "Choose which State to visit",
                  choices = unique(data$State),
                  selected =,
                  multiple = TRUE),
      
      #month of visiting
      selectInput(inputId = "Month",
                  label = "Choose the month of your vacation in MM",
                  choices = c(1,2,3,4,5,6,7,8,9,10,11,12),
                  selected =  ),
      
      #age
      #sliderInput(),
      actionButton("click","Output")
      
    ),
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Destination Selector", tableOutput("Summary_t"), 
                           verbatimTextOutput("Histogram"), plotOutput("pplot1"),
                           verbatimTextOutput("title"), girafeOutput("pplot2")),
                  tabPanel("Summary", girafeOutput("summaryPlot"), 
                           verbatimTextOutput("summary"))
      )
      
    )
  )
)
# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$summaryPlot <- renderGirafe({
    gg <- data %>%
      ggplot(aes((Rating), (`Average Cost`)))+
      geom_point_interactive(aes(tooltip = Place, 
                                 data_id = Place,
                                 color = `Drinking Age` ))+
      theme_minimal()
    girafe(code = print(gg))
  })
  
  output$summary <- renderText({
    "The Summary of the given plot is as follows:
    1. This plot shows the behaviour of cost of trips versus the rating of locations.
    2. We can observe that the Average cost of the trip gradually decreases with the ratings in the range of 3.8 to 4.3
        and then increases suddenly. This implies that the average cost of the trips to the most highly rated locations 
        are pretty expensive.
    3. Using this plot we can also see the locations which are highly rated, but are reasonably priced, these locations
        lie in the region where Rating > 4.3, and Average Trip Cost <= 15,000.
        We can see, these places are:
        Amarnath, Tirupati, Varanasi,Hampi, Goa, Jaisalmer, Ujjain, Shirdi, Kasol, Kodaikanal"
  })
  

  observeEvent(input$click , {
    
    data_1 <- data %>%
      filter(data$State == input$State) 
    check <- NULL
    for (i in 1:dim(data_1)[1]){
      x <- data_1$`Ideal Month`[i]
      y <- as.integer(unlist(strsplit(x,",")))
      check[i] <- 0
      for ( j in 1:length(y)){
        if ( y[j] == input$Month){
          check[i] <- 1
        }
      }
    }
    data_2 <- data_1[check == 1,]
    check <- NULL
    for (i in 1:dim(data_2)[1]){
      data_2$'Ideal Trip Time (Days)'[i] <- 
        mean(as.numeric(unlist(strsplit(data_2$'Ideal Trip Time (Days)'[i], ","))))
    }
    
    output$Summary_t <- renderTable({
      data_2[,c(1,2,3,5,6,9,10,11)]
    })
    output$Histogram <- renderText({"The histogram is"})
    output$pplot1 <- renderPlot({
      ggplot(data_2, aes(x = Place, y = Attractions, fill = State)) +
        geom_col(color = "black") +
        labs(title = "Histogram with Character Labels on X-axis",
             x = "Places",
             y = "Number of Attractions",
             fill = "State")
    })
    output$title <- renderText({"The price vs number of attractions scatterplot 
      is as follows
      Note : The hue of the point represents the rating as given by the key"})
    output$pplot2 <- renderGirafe({
      gg <- data_2 %>%
        ggplot(aes((`Attractions`),`Average Cost`))+
        geom_point_interactive(aes(tooltip = Place, 
                                   data_id = Place,
                                   color = (Rating),
                                   size = Rating))+
        theme_minimal()
      girafe(code = print(gg))
    })
    
    
    
  })
}

# Run the application 
shinyApp(ui = ui, server=server)