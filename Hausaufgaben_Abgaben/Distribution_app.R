
library(shiny)

# Define UI for application that draws a histogram

ui <- fluidPage(
                
                numericInput("nthetavalues", label= "Choose number of priors", 
                             value = 1, 
                             min = 0, step = 1),
                numericInput("positives", label = "Choose the number of positive results",
                             value = 1,
                             min = 0, step = 1),
                numericInput("negatives", label = "Choose the number of tries",
                             value = 1,
                             min = 0, step = 1),
                selectInput("distribution", label = "Choose how priors are distributed",
                            choices = c("linear" = "lin",
                                        "uniformly" = "uni",
                                        "normalized" = "norm"),
                            selected = "linear"),
                mainPanel(plotOutput("plots"), textOutput("text"))
)




# Define server logic required to draw a histogram
server <- function(input, output) {
  
  theta <- reactive({
    nthetavalues <- input$nthetavalues
    theta <- seq(0, 1, 1/(nthetavalues+1))
    theta <- theta[2:(length(theta)-1)]
  })
  
  prior <- reactive({
    if (input$distribution == "lin") {
      prior <- pmin(theta(), 1-theta())
      prior <- prior/sum(prior)
    }
    else if (input$distribution == "uni"){
      prior <- replicate(input$nthetavalues, 1/(input$nthetavalues+1))
      prior <- prior/sum(prior)
    }
    else {
      prior <- dnorm(theta(), 0.5, 1)
      prior <- prior/sum(prior)
    }
  })
  
  
  likelihood <- reactive(choose(sum(input$positives, input$negatives), input$positives)*theta()^input$positives * (1-theta())^input$negatives)
  D <- reactive(likelihood()*prior() + (1-likelihood())*(1-prior()))
  posterior <- reactive(likelihood()*prior()/D())
  
 # output$text <- renderText(priors())
  
  output$plots <- renderPlot({
  layout(matrix(c(1,2,3), 3, 1, byrow = TRUE), 
                                      widths=c(1,1), heights=c(1,1))
    plot(theta(), prior(), "n", xlim =  c(0,1))
    segments(x0 = theta(), y0 = 0, x1 =theta(), y1=prior())
    plot(theta(), likelihood(), "n", xlim =  c(0,1))
    segments(x0 = theta(), y0 = 0, x1 =theta(), y1=likelihood())
    plot(theta(), posterior(), "n", xlim =  c(0,1))
    segments(x0 = theta(), y0 = 0, x1 =theta(), y1=posterior())
    #text (x = 0.8, y = max(posterior), labels = paste("D =",D[1]))
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

