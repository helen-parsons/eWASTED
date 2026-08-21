
library(shiny)
library(mirt)
library(DT)

grm <- readRDS(file = "WASTED_GRM.rds")

ui <- fluidPage(
  titlePanel("WASTEd Questionnaire Scoring App"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("datafile", "Upload questionnaire data as a CSV", 
                accept = c(".csv")),
      actionButton("run", "Calculate WASTEd IRT Scores"),
      downloadButton("downloadScores", "Download Scores")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Item Parameters", DTOutput("itemParams")),
        tabPanel("Theta Scores", DTOutput("thetaScores")),
        tabPanel("Model Summary", verbatimTextOutput("modelSummary"))
      )
    )
  )
)

server <- function(input, output, session) {
  
  data_reactive <- reactive({
    req(input$datafile)
    read.csv(input$datafile$datapath)
  })
  
  irt_model <- eventReactive(input$run, {
    dat <- data_reactive()
    
    # Ensure items are numeric
    dat[] <- lapply(dat, as.numeric)
    
    fs <- mirt::fscores(grm, response.pattern = dat) %>% as.data.frame()
    
    # extract scores only
    theta.scores <- fs$F1
    
    #change the score to go from 0-100 
    minimum_theta <- mirt::fscores(grm, response.pattern = rep(0,14))[,1]
    maximum_theta <- mirt::fscores(grm, response.pattern = rep(4,14))[,1]
    
    scores <- 100 - (round((100*(theta.scores - minimum_theta)/(maximum_theta - minimum_theta)), digits = 0))
    
  })
  
  output$modelSummary <- renderPrint({
    req(irt_model())
    summary(irt_model())
  })
  
  output$itemParams <- renderDT({
    req(irt_model())
    params <- coef(irt_model(), IRTpars = TRUE, simplify = TRUE)$items
    datatable(params)
  })
  
  theta_values <- reactive({
    req(irt_model())
    fscores(irt_model(), method = "EAP")
  })
  
  output$thetaScores <- renderDT({
    req(theta_values())
    datatable(data.frame(ID = 1:nrow(theta_values()),
                         Theta = theta_values()))
  })
  
  output$downloadScores <- downloadHandler(
    filename = function() { "WASTED_scores.csv" },
    content = function(file) {
      write.csv(data.frame(ID = 1:nrow(theta_values()),
                           Theta = theta_values()),
                file, row.names = FALSE)
    }
  )
}

shinyApp(ui, server)
