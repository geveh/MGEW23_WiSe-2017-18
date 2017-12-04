
library(shiny)

# Define UI for application that draws a histogram

ui <- fluidPage(
                
                numericInput("nthetavalues", label= "Choose number of priors", 
                             value = 1, 
                             min = 0, step = 1),
                numericInput("positives", label = "Choose the number of positive results",
                             value = 1,
                             min = 0, step = 1),
                numericInput("negatives", label = "Choose the number of negative results",
                             value = 0,
                             min = 0, step = 1),
                mainPanel(plotOutput("plots"), textOutput("text"))
)




# Define server logic required to draw a histogram
server <- function(input, output) {
  
  p_seq <- reactive({
    priorstep <- 1/(input$nthetavalues+1)
    pseq <- seq(0, 1, priorstep)
    pseq <- pseq[2:(length(pseq)-1)]
  })
  
  priors <- reactive({
    priorstep <- 1/(input$nthetavalues+1)
    p_seq <- seq(0, 1, priorstep)
    pseq <- p_seq[2:trunc(length(p_seq)/2)]
    p_seq <- p_seq[2:(length(p_seq)-1)]
    prior <- p_seq
    start <- length(prior)-trunc(length(prior)/2)
    end <- as.numeric(length(prior))
    prior[(start+1):end] <- rev(pseq)
    prior <- prior[1:input$nthetavalues]
    
  })
  
  likelihood <- reactive(priors()^input$positives * (1-priors())^input$negatives)
  D <- reactive(likelihood()*priors() + (1-likelihood())*(1-priors()))
  posterior <- reactive(likelihood()*priors()/D())
  
 # output$text <- renderText(priors())
  
  output$plots <- renderPlot({
  layout(matrix(c(1,2,3), 3, 1, byrow = TRUE), 
                                      widths=c(1,1), heights=c(1,1))
    plot(p_seq(), priors(), "n", xlim =  c(0,1))
    segments(x0 = p_seq(), y0 = 0, x1 =p_seq(), y1=priors())
    plot(p_seq(), likelihood(), "n", xlim =  c(0,1))
    segments(x0 = p_seq(), y0 = 0, x1 =p_seq(), y1=likelihood())
    plot(p_seq(), posterior(), "n", xlim =  c(0,1))
    segments(x0 = p_seq(), y0 = 0, x1 =p_seq(), y1=posterior())
    #text (x = 0.8, y = max(posterior), labels = paste("D =",D[1]))
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

