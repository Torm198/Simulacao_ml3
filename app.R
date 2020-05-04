#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)

# Define UI for application that draws a histogram

ml3 <- function(x,a,b,c){
    c+(1-c)*1/(1+exp(-a*(x-b)))
}

inform <- function(x,a,b,c){
    p <- ml3(x,a,b,c)
    q <- 1-p
    saida <- a**2*(q/p)*((p-c)/(1-c))**2
    return(saida)
}

ui <- fluidPage(

    # Application title
    titlePanel("Simulação do Modelo Logístico de Três Paramêtros"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("a",
                        "Discriminação",
                        min = -1,
                        max = 10,
                        value = 0,
                        step = 1),
        sliderInput("b",
                    "Dificuldade",
                    min = -4,
                    max = 4,
                    value = 0),
    sliderInput('c',
                "Chance de acertar na sorte",
                min = 0,
                max = 1,
                value = 0),
    checkboxInput('info','Informação do Item')
),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        p<- ggplot(data = data.frame(x = 0), aes(x = x)) + stat_function(fun = ml3,args=list(a=input$a,b=input$b,c=input$c))+xlim(-4,4)+ylim(0,1)
        if(input$info){
            p <- p + stat_function(fun = inform,args=list(a=input$a,b=input$b,c=input$c),color='red')
        }
        p
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
