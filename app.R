setwd("E:/My Work/Univesity Bonn/Semester 4/Predictive Analysis Seminar/R code")
library(forecast)
library(tseries)
library(shiny)
library(ggplot2)

time_series_values <- c(1.129,1.127, 1.135, 1.122, 1.130,
                        1.122, 1.123, 1.121, 1.130,1.168,
                        1.44, 1.43, 1.43, 1.38, 1.36, 
                        1.33, 1.41, 1.45, 1.40, 1.43, 
                        1.28, 1.45, 1.40, 1.41, 1.44, 
                        1.43, 1.44)


### Creating the time series object
timeseries <- ts(data = time_series_values, 
                 start = 1,
                 end = 27,
                 frequency = 1,
                 deltat = 1)

timeseries_train <- timeseries[1:20]
fit <- arima(log(timeseries_train), c(8,2,0))
pred <- predict(fit, n.ahead = length(timeseries) - length(timeseries_train))
pred1 <- 2.718^pred$pred
result <- c(timeseries_train, round(pred1,2))
timeseries_test <- timeseries[21:length(timeseries)]
print(pred1)

ui <- fluidPage(theme = "style.css",
  # App title 
  titlePanel(h1("Clone Evolution Using Time Series")),
  br(),
  br(),
  # Sidebar Layout
  sidebarLayout(
    sidebarPanel(h2("Description"),
                 br(),
      p("The Demo Project Demonstrates the Use of Time Series for predicting the clones
      from the last updates. The Project uses ARIMA model and can be used in the field
      of software maintenance. It also calculates the Reduced Cost after clones after 
      hourly rates and workload is mentioned."),
      br(),
      br(),
      actionButton("st",h4("Stationary Timeseries")),
      actionButton("makepred",h4("Predictions")),
      actionButton("plotpred",h4("Plot Predictions")),
      br(),
      br(),
      br(),
      sliderInput("hourlyrate",h4("Hourly rate"), min=9, max=50,value = 10),
      br(),
      numericInput("workload",h4("Workload"), value = 10),
      br(),
      actionButton("calculateworkload",h4("Calculate Workload")),
      br(),
      br(),
      br(),
      br(),
      h6(style="text-align:right","Developed By Deepansh Pandey")
    ),
    mainPanel(
      h4(style="text-align:center", "Time Series Plot"),
      br(),
      plotOutput(outputId = "originalPlot"),
      br(),
      h4(textOutput("meanvalue")),
      br(),
      fluidRow(
        column(3,h4(style="color:green",textOutput("predvaltitle"))),
        column(5,h4(style="color:green",textOutput("predValue"))),
        column(4,h4(style="color:red",textOutput("error")))
      ),
      br(),
      fluidRow(
        column(8,h4(textOutput("effort"))),
        column(4,h4(textOutput("cost")))
      ),
      br(),
      h4(style="text-align:center", textOutput("predtitle")),
      plotOutput(outputId = "predPlot")
    )
  )
)

server <- function(input, output) {
  org <- reactiveValues(data=timeseries) 
  observeEvent(input$st,{
    org$data <- diff(diff(log(timeseries)))
  })
  
  output$originalPlot <- renderPlot({
    ggplot(org$data, aes(x=1:length(org$data))) +
    geom_line(aes(y=org$data),color="red", size=1) +
    xlab("Version") +
    ylab("Timeseries") 
   })
  
  output$meanvalue <- renderText({
    paste("Mean of the Data is - ",round(mean(org$data),2))
  })
  
  predictions <- eventReactive(input$makepred, {
    (round(pred1,2))
  })
  output$predValue <- renderText({
    predictions()
  })
  
  errortext <- eventReactive(input$makepred,
                             paste("Model Error - ",round(sum(abs(timeseries_test - round(pred1,2)))/sum(timeseries_test) * 100,2),"%"))
  
  output$error <- renderText({
    errortext()
  })
  
  predvaltext <- eventReactive(input$makepred,
                            paste("Predicted Values - "))
  
  output$predvaltitle <- renderText({
    predvaltext()
  })
  
  predtext <- eventReactive(input$plotpred,
                              paste("Prediction VS Actual Time Series"))
  
  output$predtitle <- renderText({
    predtext()
  })
  predplot <- eventReactive(input$plotpred,result)

  output$predPlot <- renderPlot({
    ggplot() +
    geom_line(data=timeseries, aes(x=1:length(timeseries), y=predplot(),color="green")) +
    geom_line(data=timeseries, aes(x=1:length(timeseries), y=timeseries,color="red")) +
    scale_color_discrete(name="Time Series",
                         labels=c("Predicted","Actual"))+
    xlab("Version") +
    ylab("Timeseries") 
      
  })


  
  efforttext <- eventReactive(input$calculateworkload,
                              paste("Effort Required - ",round(mean(pred1)*input$workload*100,0)," Hours "))
  
  output$effort <- renderText({
    efforttext()
  })
  
  costtext <- eventReactive(input$calculateworkload,
                            paste("Cost Required - $",round(sum(pred1)*input$hourlyrate*10,0),sep=""))
  
  output$cost <- renderText({
    costtext()
  })

}

shinyApp(ui = ui, server = server)