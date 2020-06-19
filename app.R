library(purrr)
library(dplyr)
library(ggplot2)
library(rvest)
library(polite)
library(scales)
library(shiny)
library(glue)
library(lubridate)
source("Helpers.R")
# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Create your own age plot"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            textInput("team", "Team", "pec-zwolle"),
            textInput("teamcode", "Teamcode", "1269"),
            actionButton("myButton", "Scrape!"),
           
            textInput("rect","Rectangle 'peak age' color", "red"),
            textInput("line","Line contract length color", "black"),
            textInput("line2","Line contract length color", "black"),
            radioButtons("alpha", "See contract lines?",
                         c("Yes" = 1,
                           "No" = 0)),
            actionButton("go", "Click here to plot!")
            
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(type = "tabs",
                        tabPanel("Instruction", 
                                 h4("Instructions", align = "center"),
                                 h5("Go to transfermarkt.com and search for your favourite team.", align = "left"),
                                 h5("When you're on the page of your team, look at the URL. It looks something like this:", align = "left"),
                                 h5(" "),
                                 h5("https://www.transfermarkt.com/inter-mailand/startseite/verein/46/saison_id/2019", align = "left"),
                                 h5(" "),
                                 h5("There are two thing important: the club (inter-mailand) and the teamcode (46).", align = "left"),
                                 h5("The other number is for the season, but we ignore that. Just copy the two values and paste them in the boxes on the left (including the minus sign (-)", align = "left"),
                                 h5("Click on the 'scrape!' button and wait a little while."),
                                 h5("A (new )table on this page will appear which means the data is scraped and ready to plot!"),
                                 br(),
                                 h5("eNext, enter some colours from here:"),
                                 h5("http://sape.inf.usi.ch/quick-reference/ggplot2/colour"),
                                 h5("Or enter a hex colour code and click on 'plot!'"),
                                 br(),
                                 h5("Choose white a a colour to have no rectangle for peak age"),
                                 tableOutput("myTable")),
                        tabPanel("Age Plot", 
                                 plotOutput("scatplot")))
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    url=  reactive({
        glue("https://www.transfermarkt.com/{input$team}/leistungsdaten/verein/{input$teamcode}/reldata/%262019/plus/1")
    })
    
    myData <- reactive({
        input$myButton
        data = isolate(TransfermarktShiny(
            team_name = input$team, 
            team_num = input$teamcode))
    })
    output$myTable <- renderTable(myData())
    
    output$scatplot = renderPlot({
        if (input$go == 0)
            return()
        req(input$go)
        color1 <- isolate(input$rect)
        color2 <- isolate(input$line)
        color3 <- isolate(input$line2)
        teamname <- isolate(input$team)
        alpha <- isolate(input$alpha)
        isolate(ScatterShiny(data = myData(),
                             color1 = color1,
                             color2 = color2,
                             color3= color3,
                             teamname = teamname,
                             alpha = alpha))
        
        
    }, height = 400, width = 750 )
}

# Run the application 
shinyApp(ui = ui, server = server)
