
# version 21 Aug 2026 for upload to GitHib

library(shiny)
library(mirt)
library(DT)


### make a json file for github connection
# setwd("M:\\WMS\\CTU\\Statisticians\\Helen Parsons\\WASTED\\Analysis")
# library("rsconnect")
# rsconnect::writeManifest()


#### 1. Load model

setwd("M:\\WMS\\CTU\\Statisticians\\Helen Parsons\\WASTED\\Analysis")
fixed_model <- readRDS("WASTED_GRM.rds") # <-- in the same folder as app.R

# # Extract item names expected by the model
# expected_items <- colnames(extract.mirt(fixed_model, "data"))

### create UI

ui <- fluidPage(
  titlePanel("WASTED scoring app"),
  
  sidebarPanel(
    h4("Instructions for use"),
    tags$ol(
      tags$li("Save your responses as a CSV file so that the responses scored as 0 (leftmost response) to 4 (rightmost response)"),
      tags$li("Click 'Score Data' to compute the WASTED scores."),
      tags$li("Download the results. The scores will be the same rows as in your CSV file")
    ),
    tags$hr(),
    p(tags$b("Note:"), "Questions need to be in the same order as the questionnaire in columns A (how often fatigued) to N (able to maintain energy levels)"),
    
    # ... your existing inputs:
    #fileInput("datafile", "Upload data (in CSV format)", accept = ".csv"),
    #actionButton("score", "Score Data"),
    #downloadButton("downloadScores", "Download Scores")
  ),
  
  
  sidebarLayout(
    sidebarPanel(
      fileInput("datafile", "Upload response data (CSV)", accept = ".csv"),
      actionButton("score", "Score Data"),
      downloadButton("downloadScores", "Download Scores"),
      width = 3
    ),
    
    mainPanel(
      tabsetPanel(
        #tabPanel("Model Summary", verbatimTextOutput("modelSummary")),
        #tabPanel("Messages", verbatimTextOutput("messages")),
        #tabPanel("Item Parameters", DTOutput("itemParams")),
        tabPanel("WASTED Scores", DTOutput("thetaScores"))
      )
    )
  )
)

## set up server

server <- function(input, output, session) {
  
  messages <- reactiveVal(character())
  add_msg <- function(txt) messages(c(messages(), txt))
  
  # Show model summary
  output$modelSummary <- renderPrint({
    summary(fixed_model)
  })
  
  # Show item parameters
  output$itemParams <- renderDT({
    params <- coef(fixed_model, IRTpars = TRUE, simplify = TRUE)$items
    datatable(params)
  })
  
  # Score uploaded data
  scored_data <- eventReactive(input$score, {
    req(input$datafile)
    
    dat <- read.csv(input$datafile$datapath, header = FALSE)
    
    # Coerce to numeric
    #dat[] <- lapply(dat, function(x) suppressWarnings(as.numeric(x)))
    
    # reverse score items 8 and 14
    dat[,8] <- abs(dat[,8] - 4)
    dat[,14] <- abs(dat[,14] - 4)
    
    
    # Score with fixed model
    theta.scores.SE <- fscores(fixed_model, response.pattern = dat) %>% as.data.frame()
    theta.scores <- theta.scores.SE$F1 # just take scores, not SE data
    
    #change the score to go from 0-100 
    minimum_theta <- mirt::fscores(fixed_model, response.pattern = rep(0,14))[,1]
    maximum_theta <- mirt::fscores(fixed_model, response.pattern = rep(4,14))[,1]
    
    theta <- 100 - (round((100*(theta.scores - minimum_theta)/(maximum_theta - minimum_theta)), digits = 0))
    
    
    out <- data.frame(
      Row.Number = 1:nrow(as.matrix(theta)),
      WASTED.score = as.numeric(theta)
    )
    
    out
  })
  
  # Display theta scores
  output$thetaScores <- renderDT({
    req(scored_data())
    datatable(scored_data())
  })
  
  # Messages
  output$messages <- renderText({
    paste(messages(), collapse = "\n")
  })
  
  # Download handler
  output$downloadScores <- downloadHandler(
    filename = function() "WASTED_scores.csv",
    content = function(file) {
      write.csv(scored_data(), file, row.names = FALSE)
    }
  )
}

### run app
shinyApp(ui, server)
