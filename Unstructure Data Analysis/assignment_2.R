#Sys.getenv("R_LIBS")
#.libPaths("C:/Program Files/R/R-4.0.1/library")
#.libPaths()

#rm(list = ls()); gc();


#Sys.setenv(JAVA_HOME = "C:/Program Files/Java/jre1.8.0_241")

###########################################################################################

# library setup
library(shiny) ; library(tidyverse) ; library(shinythemes)
library(readxl) ; library(DT)
library(RWeka) ; library(rJava)


# ui setup
ui <- fluidPage(
  theme = shinytheme(theme = 'simplex'),
  HTML('<h1><b> [SCH] Sensor data analytics Service </b></h1>'),
  sidebarPanel(
    h3('[1] Data load'),
    fileInput(inputId = 'file',
              label = "'csv' Please select a file'",
              multiple = F,
              buttonLabel = icon(name ='search'),
              placeholder = 'No files have been selected yet.'),
    
    h3('[2] Data preprocessing'),
    checkboxInput(inputId = 'stat',
                  label = '1. Show a datatable of the generated statistics',
                  value = FALSE),
    checkboxInput(inputId = 'peak',
                  label = '2. Show a datatable of the generated peak',
                  value = FALSE),
    checkboxInput(inputId = 'crest',
                  label = '3. Show a datatable of the generated crest',
                  value = FALSE),
    checkboxInput(inputId = 'cp',
                  label = '4. Show a datatable of the generated change point',
                  value = FALSE),
    checkboxInput(inputId = 'merge',
                  label = '5. Show a datatable of the generated finaldata ',
                  value = FALSE),
    submitButton(text = '[Execution] Check the result',
                 icon = icon(name = 'sync')),
    
    h3('[3] Exploratory data analysis'),
    selectInput(inputId = 'x',
                label = 'Select a variable to put on the X-axis.',
                choices = NULL),
    selectInput(inputId = 'y',
                label = 'Select a variable to put on the Y-axis.',
                choices = NULL),
    submitButton(text = '[Execution] Check the result',
                 icon = icon(name = 'sync')),
    
    h3('[4] Data modeling'),
    radioButtons(inputId = 'model',
                 label = 'Please select a Machine Learning Model',
                 choices = c('J48' = 'J48', 'RF' ='RF'),
                 selected = 'J48', 
                 inline = TRUE),
    submitButton(text = '[Execution] Check the result',
                 icon = icon(name = 'sync')),
    
  ),
  mainPanel(
    uiOutput(outputId = 'mainUI')
  )
)

# server setup
server <- function(input, output, session) {
  df_1 <- reactive({
    if(is.null(x = input$file)) return()
    read_excel(input$file$datapath, sheet = 1)
  })
  df_2 <- reactive({
    if(is.null(x = input$file)) return()
    read_excel(input$file$datapath, sheet = 2)
  })
  df_3 <- reactive({
    if(is.null(x = input$file)) return()
    read_excel(input$file$datapath, sheet = 3)
  })
  df_4 <- reactive({
    if(is.null(x = input$file)) return()
    read_excel(input$file$datapath, sheet = 4)
  })
  df_5 <- reactive({
    if(is.null(x = input$file)) return()
    read_excel(input$file$datapath, sheet = 5)
  })
  df_6 <- reactive({
    if(is.null(x = input$file)) return()
    training <- read_excel(input$file$datapath, sheet = 5)
    aggregate(.~activity, data= training, mean)
  })
  
  observe({
    cols <- colnames(df_5())[-1]
    updateSelectInput(session = session, inputId = 'x', choices = cols)
    updateSelectInput(session = session, inputId = 'y', choices = cols)
  })
  
  # (1) Data load
  output$head <- DT::renderDataTable({
    if(is.null(x = df_1())) return () else df_1()
  })
  output$glimpse <- renderPrint({
    if(is.null(x = df_1())) return () else glimpse(x=df_1())
  })
  output$summary <- renderText({
    print(paste0('This is the sum of [',
                 dim(df_1())[1],'] files, and the number of columns is [',
                 dim(df_1())[2],'] '))
  })
  
  output$model_rslt <- renderPrint({
    if(input$model == 'J48') {
      J48_model <- J48(as.factor(activity)~., data = df_5())
      (e_J48_model <- evaluate_Weka_classifier(J48_model, numFolds = 10, complexity = T, class = T))
    } else {
      RF <- make_Weka_classifier("weka/classifiers/trees/RandomForest") 
      RF_model <- RF(as.factor(activity)~., data = df_5())
      (e_RF_model <- evaluate_Weka_classifier(RF_model, numFolds = 10, complexity = T, class = T))
    } 
    
  })
  
  # (2) Data preprocessing
  output$processing_stat <- DT::renderDataTable({
    if(input$stat == F) return () else df_1()
  })
  output$processing_peak <- DT::renderDataTable({
    if(input$peak == F) return () else df_2()
  })
  output$processing_crest <- DT::renderDataTable({
    if(input$crest == F) return () else df_3()
  })
  output$processing_cp <- DT::renderDataTable({
    if(input$cp == F) return () else df_4()
  })
  output$processing_merge <- DT::renderDataTable({
    if(input$merge == F) return () else df_5()
  })
  output$summary_processing <- renderText({
    print(paste0('The final data set is [',
                 dim(df_5())[1],'] Rows [',
                 dim(df_5())[2],'] Columns(Features)'))
  })
  
  # (3) Exploratory data analysis
  output$plot_summary <- renderTable({
    df_6()
  })
  output$select_feature <- renderPrint({
    cat('The X column of your choise is [', input$x,'], and the Y columns is [', input$y,']')
  })
  output$plot <- renderPlot({
      ggplot(data = df_6() , mapping = aes(x = df_6()[,input$x], 
                                             y = df_6()[,input$y],
                                             color = df_6()[,'activity'])) +
      geom_point(shape = 19, size = 3)  +
      labs(title ='Scatter plot', x = input$x, y = input$y) +
      theme_bw() +
      theme(plot.title = element_text(face = 'bold', hjust = 0.5),
            legend.title = element_blank(),
            legend.position = 'bottom')
  })
  
  # (4) Data modeling
  output$d4 <- DT::renderDataTable({
    if(input$stat == F) return () else df_5()
  })

  output$mainUI <- renderUI({
    if(is.null(df_1())) {
      tagList(
        h1('[Description] Please read these.'),
        h3('[1]. You must to load the data'),
        h3('[2]. You can check the data about preprocessing result'),
        h3('[3]. You can check the summary and plot of data'),
        h3('[4]. You can check the result of data about machine learning modeling')
      )
    } else {
      tabsetPanel(
        type = 'tabs',
        
        # (1) Data load
        tabPanel(title ='(1) Data load', 
                 h3('This is the uploaded data.'),
                 dataTableOutput(outputId = 'head'),
                 h3('This is the glimpse part of the data.'),
                 verbatimTextOutput(outputId = 'glimpse'),
                 
                 textOutput(outputId = 'summary'),
                 h3('All data loading and summary views are completed.')),
        
        # (2) Data preprocessing
        tabPanel(title ='(2) Data preprocessing',
                 h2('You can check it by clicking the checkbox '),
                 h3('This datatable is based on statistics.'),
                 dataTableOutput(outputId = 'processing_stat'),
                 h3('This datatable is based on peak.'),
                 dataTableOutput(outputId = 'processing_peak'),
                 h3('This datatable is based on crest.'),
                 dataTableOutput(outputId = 'processing_crest'),
                 h3('This datatable is based on change point'),
                 dataTableOutput(outputId = 'processing_cp'),
                 h3('This datatable is final data for model'),
                 dataTableOutput(outputId = 'processing_merge'),
                 textOutput(outputId = 'summary_processing')),
        
        # (3) Exploratory data analysis       
        tabPanel(title ='(3) Exploratory data analysis',
                 h3('This is the average summary by Activity'),
                 tableOutput(outputId = 'plot_summary'),
                 h3('You can check it through Scatter Plot.'),
                 tableOutput(outputId = 'select_feature'),
                 plotOutput(outputId = 'plot')),
        
        #(4) Data modeling
        tabPanel(title ='(4) Data modeling',
                 verbatimTextOutput(outputId = 'model_rslt'))
        )
    }
  })
}

# result
shinyApp(ui = ui, server = server)


