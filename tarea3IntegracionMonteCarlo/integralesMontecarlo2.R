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
                  tabPanel("MonteCarlo", verbatimTextOutput("integral")), 
                  tabPanel("Plot", plotOutput("plot")), 
                  tabPanel("Intervalos", plotOutput("intervals"))
      )
    )
  )
)

server <- function(input, output) {
  f <- function (x) {4/(1+x^2)}
  g <- function (x) {sqrt(4-x^2)}
  h <- function (x) {6/(sqrt(4-x^2))}
  
  output$plot <- renderPlot({
    func <- switch(input$func,
           f = f,
           g = g,
           h = h)
    aux <- func(seq(input$from, input$to))
    aux[is.nan(aux)] <- 0
    mn <- min(aux)
    mx <- max(aux)
    
    curve(func(x), from=input$from, to=input$to)
    points(runif(input$n, input$from, input$to), runif(input$n, mn, mx))
  })
  
  output$integral <- renderPrint({
      func <- switch(input$func,
                     f = f,
                     g = g,
                     h = h)
      nn <- input$n
      from <- input$from
      to <- input$to
      to2 <- to - from
      
      u1 <- runif(nn, from, to)
      aux <- func(u1)
      aux[is.nan(aux)] <- 0
      to2*mean(aux)
  })
  
  output$intervals <- renderPlot({
    upper <- {}
    lower <- {}
    mC <- {}
    nn <- input$n
    
    func <- switch(input$func,
                   f = f,
                   g = g,
                   h = h)
  
    for (i in seq(1,nn)){
      u1 <- runif(i, input$from, input$to)
      to2 <- input$to - input$from
      aux <- to2*mean(func(u1))
      mC[i]  <- aux
      
      alfa = 0.05
      s = sd(mC)
      error <- qnorm(1-alfa)*s/(sqrt(i))
      left <- aux-error
      right <- aux+error
      
      lower[i] <- left
      upper[i] <- right
    }
    
    
    plot(upper[2:nn], type="l", log="x",col="gray", main=paste("Simulación MonteCarlo: ", aux))
    lines(mC[2:nn], col="black")
    lines(lower[2:nn],  col="gray")
    abline(h=aux, col="red")
    grid()
    })
}

shinyApp(ui = ui, server = server)