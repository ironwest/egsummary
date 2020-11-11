library(gt)
library(gtsummary)
library(readr)
library(magrittr)
library(dplyr)
library(shiny)
library(openxlsx)
library(shinycssloaders)

input_file <- fileInput(
  inputId = "file",
  label =  "Choose CSV File",
  multiple = FALSE,
  accept = c("text/csv",
             "text/comma-separated-values,text/plain",
             ".csv")
)

setting_by <- selectInput("by", label = "Group By", choices = NA)

dlbutton_excel <- downloadButton("dltable_excel", "DL(Excel)")
dlbutton_csv <- downloadButton("dltable_csv", "DL(CSV)")

ui <- fluidPage(
  
  titlePanel("Summarise Your Data!"),
  
  sidebarLayout(
    sidebarPanel(
      fluidRow("This app uses gtsummary package of R"),
      hr(),
      fluidRow(
        input_file,
        setting_by
      ),
      hr(),
      fluidRow(dlbutton_excel, dlbutton_csv)
    ),
    
    mainPanel(
      shinycssloaders::withSpinner(gt::gt_output("table1"))
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  observeEvent(input$file, {
    
    
    column_vector <- dat() %>% colnames()
    
    updateSelectInput(
      session = session,
      inputId = "by",
      choices = c(NA_character_, column_vector)
    )
  })
  
  dat <- reactive({
    req(input$file)
    read_csv(input$file$datapath)
  })
  
  summary_table <- reactive({
    req(dat())
    if(input$by == "NA"){
      tbl <- tbl_summary(dat())
    }else{
      tbl <- tbl_summary(dat(), by = input$by)   
    }
    
    return(tbl)
  })
  
  output$table1 <- gt::render_gt({
    require(input$file)
    summary_table() %>% as_gt()
  })
  
  output$dltable_excel <- downloadHandler(
    filename = function() {"table1.xlsx"},
    content = function(file){
      
      
      temp <- summary_table() %>% as_tibble()
      
      wb <- createWorkbook()
      addWorksheet(wb, "summarised_table")
      writeDataTable(wb, "summarised_table", temp)
      
      saveWorkbook(wb, file)
      
    }
  )
  
  output$dltable_csv <- downloadHandler(
    filename = function() {"table1.csv"},
    content = function(file){
      
      
      temp <- summary_table() %>% as_tibble()
      write_csv(temp,file)
      
      
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)
