

library(shiny)
library(ggplot2)
library(reshape2)

# Define UI for application 
ui <- fluidPage(
   
   # Application title
   titlePanel("Savings Simulation"),
   
   # Sidebar with a slider input for number of bins 
   
   fluidRow(
     column(4,
       sliderInput("initial",
                   "Initial amount",
                   min = 1,
                   max = 100000,
                   value = 1000,
                   step = 500)),
     column(4,
            sliderInput("rate",
                        "Return Rate (in %)",
                        min = 0,
                        max = 20,
                        value = 5,
                        step = 0.1)),
     column(4,
              sliderInput("years",
                               label = "Years",
                               min = 0,
                               max = 50,
                               value = 10))
     
   ),
   
   fluidRow(
     column(4,
            sliderInput("contrib",
                        label = "Annual Contribution",
                        min = 0,
                        max = 50000,
                        value = 2000,
                        step = 500)),
     column(4, 
            sliderInput("growth",
                        label = "Growth Rate (in %)",
                        min = 0,
                        max = 20,
                        value = 2,
                        step = 0.1)),
            
    column(4, 
           selectInput("select",
                       label = "Facet?",
                       choices = c("No", "Yes")))
                   
    ),
   
   
   
      # Show a plot of the generated distribution
      mainPanel(
         h4("Timelines"),
         plotOutput("distPlot"),
         h4("Balances"),
         tableOutput("Balances"), width=12
      )
   )


# Define server logic required to draw a histogram
server <- function(input, output) {
   
   dat <- reactive({
     balance <- c(input$initial, rep(0, input$years))
     balance_fixed <- c(input$initial, rep(0, input$years))
     balance_growing <- c(input$initial, rep(0, input$years))
     growth_0 <- c(0,rep(input$growth,input$years-1))
     contrib_growing <- rep(input$contrib,input$years)

     
     for (y in 1:input$years) {
       contrib_growing[y+1] <- contrib_growing[y]*(1+input$growth/100)
     }
     
     for (y in 1:input$years) {

       balance[y+1] <- balance[y] * (1 + input$rate/100)
       balance_fixed[y+1] <- (balance_fixed[y] * (1 +input$rate/100)) + input$contrib
       balance_growing[y+1] <- (balance_growing[y] * (1 + input$rate/100)) + contrib_growing[y]
       
     }
     
     
     
     modality <- data.frame(
       year = 0:input$years,

       balance = balance,
       balance_fixed = balance_fixed,
       balance_growing = balance_growing
     )
     
     return(modality)

   })
   
   
   
   
  
   output$distPlot <- renderPlot({
     
     if (input$select == "No") {
       ggplot(data=dat(), aes(year)) + 
         geom_point(aes(y = balance, color= "balance")) +
         geom_point(aes(y = balance_fixed, color= "balance_fixed")) +
         geom_point(aes(y = balance_growing, color= "balance_growing")) +
         
         geom_line(aes(y = balance, color = "balance"), size=1) + 
         geom_line(aes(y = balance_fixed, color = "balance_fixed"), size=1) +
         geom_line(aes(y = balance_growing, color = "balance_growing"), size=1) + 
         ggtitle("Three Modes of Investing")
     
     }
     
     else {
       long_balance <- melt(dat(), id.vars=c("year"), 
                            variable.name="type",
                            value.name="balance")
       
       long_balance
       ggplot(data = long_balance)+
         geom_point(aes(x = year, y = balance, color=type)) +
         theme_bw() +
         geom_area(aes(x = year, y = balance, fill=type, color = type, alpha = 0.5)) +
         facet_wrap( ~ type, ncol=3)+
         ggtitle("Three Modes of Investing")
     }
   })
     
   
   
   output$Balances <- renderTable(digits = 3, {
     
     dat()
   })
   

}

# Run the application 
shinyApp(ui = ui, server = server)

