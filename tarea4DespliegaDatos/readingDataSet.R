rm(list=ls())
if(!require(shiny)) install.packages("shiny")
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(DT)) install.packages("DT")
library(ggplot2)
library(DT)
library(shiny)
data <- read.csv(file="./cheese.csv", header=T)

ui <- fluidPage(
  titlePanel("Tarea4_DataTable"),
  h5("MCC"),
  
  sidebarLayout(
    sidebarPanel(
     checkboxGroupInput("cVariables", h3("Variables"),
                              choices = names(data)),
     h4("Parámetros aPriori"),
     sliderInput("s_a", "a -> Unif ", min=1, max=10, value=c(5,8)),
     sliderInput("s_b", "b <- Norm", min=1, max=10, value=5),
     sliderInput("s_sigma", "sigma -> Unif", min=1, max=10, value=c(5, 6))
  ),
  
  mainPanel(
    tabsetPanel(type = "tabs", 
                tabPanel("datos",
                  fluidRow(
                  column(8, plotOutput("plot_data")),
                  column(12, DT::dataTableOutput("table"))
                   ) 
                ),
                tabPanel("distribuciones aPriori",
                       fluidRow(
                         column(4, plotOutput("plot_hist_A")),
                         column(4, plotOutput("plot_hist_B")),
                         column(4, plotOutput("plot_hist_Sd")),
                         column(4, plotOutput("plot_hist_Total"))
                       )
                )
               )
   
  )
))

server <- function(input, output) {
  n <- dim(data)[1]
  
  dataInput <- reactive({
    if(is.null(input$cVariables))
      return()
    aux <- data[, input$cVariables]
    aux
  })
  
  priori_a <- reactive({
    runif(n, min=input$s_a[1], max = input$s_a[2])
  })
  priori_b <- reactive({
    rnorm(n, mean=0, sd = input$s_b)
  })
  priori_sd <- reactive({
    runif(n, min=input$s_sigma[1], max = input$s_sigma[2])
  })
  
  nmes <- renderText({
    input$cVariables
    })
  
   output$table <- DT::renderDataTable(DT::datatable({
     if(is.null(input$cVariables))
       return()
    else 
      return(dataInput())
     
  }))
   
   output$plot_data <- renderPlot({
     if(is.null(input$cVariables))
       return()
     else{
       # aux1 <- dataInput()[,1]
       # aux2 <- dataInput()[,2]
      return(plot(dataInput(), main="Grafica de dispersion para quesos"))
     }
   })
   
  output$plot_hist_A <- renderPlot({
    hist(priori_a())
  })
  
  output$plot_hist_B <- renderPlot({
    hist(priori_b())
  })
  
  output$plot_hist_Sd <- renderPlot({
    hist(priori_sd())
  })
  
  output$plot_hist_Total <- renderPlot({
    hist(priori_a() * priori_b() * priori_sd())
  })
}

shinyApp(ui = ui, server = server)
