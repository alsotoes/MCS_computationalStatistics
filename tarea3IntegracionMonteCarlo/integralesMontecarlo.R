library(shiny)

ui <- fluidPage(  
  titlePanel("Tabsets"),
  sidebarLayout(
    sidebarPanel(
      radioButtons("func", "Function to integrate: ",
                   c ("4/(1+x^2)" = "f",
                      "(4-x^2)^(1/2)" = "g",
                      "6/(4-x^2)^(1/2)" = "h")),
      br(),
      
      sliderInput("n", 
                  "Number of random points:", 
                  value = 100,
                  min = 2, 
                  max = 1000),
      
      sliderInput("from", 
                  "initial value of integral:", 
                  value = 0,
                  min = 0, 
                  max = 2),
      
      sliderInput("to", 
                  "final value of integral:", 
                  value = 1,
                  min = 1, 
                  max = 4)
    ),
    
    mainPanel(
      tabsetPanel(type = "tabs", 
                  tabPanel("MonteCarlo", verbatimTextOutput("method")), 
                  tabPanel("Plot", plotOutput("plot")), 
                  tabPanel("Table", tableOutput("table"))
      )
    )
  )
)

server <- function(input, output) {
  data <- reactive({
    n <- input$n
    from <- input$from
    to <- input$to
    
    f <- function (x) {4/(1+x^2)}
    g <- function (x) {sqrt(4-x^2)}
    h <- function (x) {6/(sqrt(4-x^2))}
    
    func <- switch(input$func,
                   f = f,
                   g = g,
                   h = h)
    
    to2 <- max(func(seq(from, to)))
    
    u1 <- runif(n, from, to)
    u2 <- runif(n, from, to2)
    
    sum(func(u1) > u2)/n * ((to-from)*(to2-from))
  })
  
  
  output$plot <- renderPlot({
    #plot(runif(input$n, input$from, input$to), runif(input$n, input$from, input$to), xlab = "Uniform1", ylab = "Uniform2")
    #plot(runif(input$n), main="MonteCarlo Integration")
    curve(x^2, input$from, input$to, ylim = c(input$from, input$to))
    points(runif(input$n, input$from, input$to), runif(input$n, input$from, input$to))
  })
  
  output$method <- renderPrint({
    data()
  })
  
  output$table <- renderTable({
    data.frame(x=data())
  })
  
}

shinyApp(ui = ui, server = server)