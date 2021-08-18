  rm(list=ls())
  set.seed(1997)
  
  # if (!require(devtools)) install.packages("devtools")
  # devtools::install_github("boxuancui/DataExplorer")
  # library("DataExplorer")
  
  print("============================================================================")
  print("............ Running App ............")
  print("============================================================================")
  if (!require("shiny")) install.packages('shiny')
  if (!require("shiny")) install.packages('DT')
  if (!require("shinydashboard")) install.packages('shinydashboard')
  if (!require("shinythemes")) install.packages("shinythemes")
  if (!require("shinyjs")) install.packages("shinyjs")
  if (!require("shinyWidgets")) install.packages("shinyWidgets")
  if (!require("ggplot2")) install.packages("ggplot2")
  if (!require("dplyr")) install.packages("dplyr")
  if (!require("reactlog")) install.packages("reactlog")
  if (!require("caret")) install.packages("caret", dependencies = c("Depends", "Suggests"))
  if (!require("skimr")) install.packages("skimr")
  if (!require("mltools")) install.packages("mltools")
  if (!require("data.table")) install.packages("data.table")
  if (!require("earth")) install.packages("earth")
  if (!require("kernlab")) install.packages("kernlab")
  if (!require("shinycssloaders")) install.packages("shinycssloaders")
  if (!require("ggthemes")) install.packages("ggthemes")
  reactlog_enable()
  
  ui <- dashboardPage(
    dashboardHeader(
      title = "HIV Viral Load Suppression",
      titleWidth = 275,
      tags$li(
        class = "dropdown", style = "padding: 8px;"
      )
    ),
    dashboardSidebar(width = 275, disable = T
                     # sidebarMenu(
                     #   menuItem("Dataset & EDA", tabName = "dataset", icon = icon("database")),
                     #   menuItem("Prediction Model", tabName = "prediction", icon = icon("chart-line"))
                     # )
    ),
    dashboardBody(width = 100, height = 100,
                  useShinyjs(),
                  # tabItems(
                    # tabItem(tabName = "dataset",
                            navbarPage(title = "", id = "navigation",
                                       tabPanel("Home",  value = 'startPage',
                                                box(width = 12,
                                                    # column(2),
                                                    fluidRow(column(width=3, 
                                                           
                                                           # tags$img(src = "https://www.clipartkey.com/mpngs/m/120-1206948_transparent-aids-ribbon-png-aids-red-ribbon-transparent.png", height="50%", width="50%", align="left")
                                                           uiOutput(outputId = "image")
                                                           # imageOutput(outputId = "HomeImage")
                                                           ),
                                          column(width = 5,
                                            tags$h4(
                                         "This R shiny app is for predicting the Viral Load Suppression of HIV patients"),
                                          tags$strong(tags$h5("Description:")),
                                         tags$p("There are millions of people across the world who suffer from HIV. Many of the positive living HIV people do not know their status which is also one of the reasons for the spread of HIV. Consequently, many countries have focused on attaining the 90:90:90 goal of HIV. 90% of people living with HIV are aware of their status; 90 percent of all people with HIV infection are on long-term antiretroviral therapy, and 90 percent of those on antiretroviral therapy achieve viral load suppression. There is a particular cascade for HIV which includes multiple indicators of HIV like diseases’ status, on ART, Not on ART, Unaware, retention, Viral load suppression (VLS). So, user can simulate the dataset with all the indicators and perform exploratory analysis and predict the VLS by training the model."
                                       )
                                       ),
                                       column(2)),
                                      fluidRow(column(4), column(4, actionButton(inputId = 'startButton', label = "Start", icon = icon('play'))), column(4))
                                       )
                                      ),
                                       tabPanel("Select DataSet", value = "main",
                                                
                                                # bsModal(id = 'seletIdModal', title = "", trigger, ..., size)
                                                
                                                box(width =12,  collapsible = T,  solidHeader = T, title = "Select Data", status = "primary",
                                                    fluidRow(column(3, selectInput(inputId = "selectDataset", label = "Select Source Dataset:", choices = c("Simulate Dataset", "Upload Dataset"),  multiple = F) )),
                                                    tags$div(id = 'uploadOptions', 
                                                             fluidRow(
                                                               column(3, fileInput(inputId = "idfile", label = "Upload: (.csv only)", accept=c("text/csv","text/comma-separated-values,text/plain",".csv")))
                                                               ),
                                                             fluidRow(
                                                               column(3, actionButton(inputId = "loadFileButton", label = "Load File"))
                                                             )
                                                    ),
                                                    tags$div(id = 'simulateOptions', 
                                                             fluidRow(
                                                               column(3, numericInput(inputId = "numRows", label = "Number of Rows", value = 1000, min = 1000, step =1000, max = 10000))),
                                                             fluidRow(
                                                               column(2, 
                                                                      sliderInput(inputId = 'perentMales',label = "Percent of Males", min = 0, max = 100, value = 40, step = 1, ticks = T, animate = F),
                                                                      sliderInput(inputId = 'perentFemales',label = "Percent of Females", min = 0, max = 100, value = 40, step = 1, ticks = T, animate = F),
                                                                      sliderInput(inputId = 'perentOthers',label = "Percent of Others", min = 0, max = 100, value = 20, step = 1, ticks = T, animate = F)
                                                               ),
                                                               column(2, 
                                                                      sliderInput(inputId = 'perentHIVTrue',label = "Percent of HIV +", min = 0, max = 100, value = 30, step = 1, ticks = T, animate = F),
                                                                      sliderInput(inputId = 'perentHIVFalse',label = "Percent of HIV -", min = 0, max = 100, value = 70, step = 1, ticks = T, animate = F)                                                             
                                                               ),
                                                               column(2, 
                                                                      sliderInput(inputId = 'perentOnArtTrue',label = "Percent On Art", min = 0, max = 100, value = 50, step = 1, ticks = T, animate = F),
                                                                      sliderInput(inputId = 'perentOnArtFalse',label = "Percent not On Art", min = 0, max = 100, value = 50, step = 1, ticks = T, animate = F)                                                             
                                                               ),
                                                               # column(2, 
                                                               #        sliderInput(inputId = 'perentARTTrue',label = "Percent not on ART = 1", min = 0, max = 100, value = 40, step = 1, ticks = T, animate = F),
                                                               #        sliderInput(inputId = 'perentARTFalse',label = "Percent of ART = 0", min = 0, max = 100, value = 60, step = 1, ticks = T, animate = F)                                                             
                                                               # ),
                                                               
                                                               column(2, 
                                                                      sliderInput(inputId = 'perentUnawareTrue',label = "Percent of Unaware = 1", min = 0, max = 100, value = 40, step = 1, ticks = T, animate = F),
                                                                      sliderInput(inputId = 'perentUnawareFalse',label = "Percent of Unaware = 0", min = 0, max = 100, value = 60, step = 1, ticks = T, animate = F)                                                             
                                                               ),
                                                               column(2,
                                                                      sliderInput(inputId = 'retentionTrue',label = "Retention = 1", min = 0, max = 100, value = 40, step = 1, ticks = T, animate = F),
                                                                      sliderInput(inputId = 'retentionFalse',label = "Retention = 0", min = 0, max = 100, value = 60, step = 1, ticks = T, animate = F)
                                                               )
                                                               
                                                               
                                                             ),
                                                             
                                                             fluidRow(
                                                               column(3, actionButton(inputId = "simulateDataButton", label = "Simulate"))
                                                             )
                                                    )
                                                    ,
                                                    tags$div(id = 'viewSampleData', 
                                                             DT::dataTableOutput(outputId = "DataTable", width = 1000),
                                                            downloadButton("downloadData", "Download"),
                                                            actionButton(inputId = "resetData",label =  "Reset Data", icon = icon('retweet'))
                                                    )
                                                    )
                                       ),
                                       tabPanel("EDA - Columns", value = "screen1",
                                                tags$div(id='edaColumnsTab',
                                                         fluidRow(DT::dataTableOutput(outputId = "DataTableColumnsSummary"))
                                                )
                                                
                                       ),
                                       tabPanel("EDA - Plots", value = "screen2",
                                                tags$div(id='edaPlotTab',
                                                         fluidRow(
                                                           box(width =12, collapsible = F,  solidHeader = T, title = "Single Variable Plots", status = "primary",
                                                               fluidRow(
                                                                 column(width = 4, box( collapsible = F, solidHeader = F, status = "primary",
                                                                                        selectInput(inputId = 'xSingle', label = "Select Variable", choices = c(), multiple = F)
                                                                 )
                                                                 ),
                                                                 column(width = 8, plotOutput(outputId = 'plotSingle'))
                                                               )
                                                           )
                                                         ),
                                                         fluidRow(
                                                           box(width =12, collapsible = F,  solidHeader = T, title = "Two Variable Plots", status = "primary",
                                                               fluidRow(
                                                                 column(width = 4, box( collapsible = F, solidHeader = F, status = "primary",
                                                                                        selectInput(inputId = 'x', label = "Select X Variable", choices = c(), multiple = F),
                                                                                        selectInput(inputId = 'y', label = "Select Y Variable", choices = c(), multiple = F)
                                                                 )
                                                                 ),
                                                                 column(width = 8, plotOutput(outputId = 'plot'))
                                                               )
                                                           )
                                                         )
                                                         
                                                        
                                                )
                                                
                                       ),
                                       tabPanel("Fit Model", value = "Screen3",
                                                tags$div(id='modelFitTab',
                                                    box(title = "Prepare Data for Fittng a Model", width = 12, solidHeader = F, collapsible = T, footer = "Clicking the 'Split & Preprocess' button will split the data, impute missing values with 'knnImpute' & makes onehot encoded columns for factor variables.",
                                                      fluidRow(
                                                        column(4, selectInput(inputId = "selectYVariable", label = "Select 'y' ", choices = c()))
                                                      ),
                                                        
                                                      fluidRow(
                                                           column(4, sliderInput(inputId = 'trainPercentSlider', label = "Split Dataset into Train Test:",ticks = T,value = 0.8, min = 0.05, max = 0.95, step = 0.05  )),
                                                           column(4, htmlOutput(outputId = 'TrainTestPercentages'))
                                                           ),
                                                      fluidRow(
                                                               column(4, actionButton(inputId = 'splitButton', label = "Split & Preprocess")),
                                                               column(3,
                                                                      fluidRow(tags$strong("Train")),
                                                                      fluidRow(tags$hr()),
                                                                      fluidRow(textOutput(outputId = "trainSplitDataInd")),
                                                                      fluidRow(textOutput(outputId = "trainMissingImputeInd")),
                                                                      fluidRow(textOutput(outputId = "trainOneHotInd"))
                                                               ),
                                                               column(3,
                                                                      fluidRow(tags$strong("Test")),
                                                                      fluidRow(tags$hr()),
                                                                      fluidRow(textOutput(outputId = "testSplitDataInd")),
                                                                      fluidRow(textOutput(outputId = "testMissingImputeInd")),
                                                                      fluidRow(textOutput(outputId = "testOneHotInd"))
                                                               )
                                                               
                                                               
                                                               )),
                                                      tags$hr(),
                                                    tags$div(id = 'trainingModelSection',
                                                               
                                                    box(title = "Select Model Type", width = 12, solidHeader = F, collapsible = T,
                                                      fluidRow(
                                                        column(4,
                                                        selectInput(inputId = "selectModel", label = "Select the Model", choices = c(
                                                          # "Random Forest", "SVM-Poly", "Choose the Best"
                                                          "Linear Regression", "SVM-Linear"))
                                                        ),
                                                        column(4, actionButton(inputId = "trainButton", label = "Train"))
                                                        
                                                      )
                                                    ), 
                                                    # modelResults
                                                      tags$div(id='modelResults',                                                  
                                                        box(title = "Model Performance", width = 12, solidHeader = F, collapsible = F,
                                                        fluidRow(
                                                          column(12, textOutput(outputId = 'modelNameOutut')),
                                                          column(12, tableOutput(outputId = 'modelResultsOutut') %>% withSpinner(color="#0dc5c1"))
                                                          )
                                                        ,
                                                        fluidRow(column(1),
                                                          column(8,
                                                          tags$strong("Performance Measures - Definitions"), tags$br(),
"MSE - Root Mean Squared Error", tags$br(),
"Rsquared -  The coefficient of determination(R^2)", tags$br(),
"MAE - Mean Absolute Error", tags$br(),
"RMSESD - Standard Deviation of Root Mean Squared Error (of residuals or prediction errors)", tags$br(),
"MAE - Standard Deviation of Mean Absolute Error (of residuals or prediction errors)"
                                                          
                                                        ))
                                                    ) 
                                                    )                                                  
                                                )
                                                                                     
                                        )
                                                                                      
                                       )
                                       
                                       
                            )
                    # ),
                    # tabItem(tabName = "prediction", 'dgd')
                  # )
    )
  )
  
  server <- function(input, output, session) {
    
    
    observeEvent(input$navigation, {
      if(("All Tabs" %in% input$mainSuppress) | ("All Tabs" %in% input$screen1Suppress)| ("All Tabs" %in% input$screen2Suppress) | ("All Tabs" %in% input$screen3Suppress)){}
      else{
        if ((input$navigation == "main") & (!"This Tab" %in% input$mainSuppress))  {
          showModal(modalDialog(title = "Instructions",
                                h4("You can select & view the data in this tab"), br(),
                                "1. You can choose to simulate data / upload your own data. You can change this in the 'Select Source Dataset:' dropdown", br(),
                                "2. If you choose to simulate, you can use the sliders to alter the percentage distributions of the data",br(),
                                "3. Finally, the generated data is displayed below which can be downloaded",br(),br(),br(),
        
        checkboxGroupInput(inputId = 'mainSuppress',label = "Suppress further popups on", choices = c("This Tab", "All Tabs"), selected = c(), inline = T)
        ))}
        else if ((input$navigation == "screen1") & (!"This Tab" %in% input$screen1Suppress)){showModal(modalDialog(title = "Instructions", 
                                 h4("This EDA tab summarizes all the columns in the dataset"), br(),
                                 "1. For numerical columns, the metrics Mean, Standard Deviation, percentile values(0,25,50,75,100) are shown", br(),
                                 "2. For factor columns, the metrics Number of Unique Values& Occurance Frequency for top 4 values are shown",br(),br(),br(),
        checkboxGroupInput(inputId = 'screen1Suppress',label = "Suppress further popups on", choices = c("This Tab", "All Tabs"), selected = c(), inline = T)
        ))}
        else if ((input$navigation == "screen2") & (!"This Tab" %in% input$screen2Suppress)){showModal(modalDialog(title = "Instructions", 
                                 h4("This EDA tab allows users to look at Univariate & Bivariate plots of the dataset"), br(),
                                 "1. A Histogram is plotted in the Univariate plot if a numerial column is selected or a bar plot is plotted once a categorical column is selected. ", br(),
                                 "2. The bivariate plot shows a scatter plot if both the selected variables are numerical & it shows a box plot if one of the columns is a factor.",br(),br(),br(),
                                 checkboxGroupInput(inputId = 'screen2Suppress',label = "Suppress further popups on", choices = c("This Tab", "All Tabs"), selected = c(), inline = T)
        ))}
        else if ((input$navigation == "screen3") & (!"This Tab" %in% input$screen3Suppress)){showModal(modalDialog(title = "Instructions", 
                                 h4("Fitting a model from the Generated Datset"), br(),
                                 "1. The user can choose a y variable & the test train split percentage", br(),
                                 "2. Further proproessing steps are automatically  done in the background like imputing the missing values with 'knnImpute' & making onehot encoded columns for factor variables.",br(),
                                 "3. Finally, the user can select the model to fit from the choices avaialble & once the training is done results are displayed.",br(),br(),br(),
                                 checkboxGroupInput(inputId = 'screen3Suppress',label = "Suppress further popups on", choices = c("This Tab", "All Tabs"), selected = c(), inline = T)
        ))}
      }
      })
      
    
    
    
    observeEvent(input$startButton, {
      updateTabsetPanel(session, "navigation", selected = "main")
    })
    
    
    output$image <- renderUI({
      tags$img(src = "https://www.clipartkey.com/mpngs/m/120-1206948_transparent-aids-ribbon-png-aids-red-ribbon-transparent.png", height="75%", width="75%", align="left")
    })
    #   output$HomeImage <- renderImage({
    #   
    #   filename <- normalizePath(file.path('HIVLogo.jpeg'))
    #   
    #   # Return a list containing the filename and alt text
    #   list(src = filename, alt = "Logo") 
    # }, deleteFile = TRUE)
    
    numRows <- reactive(input$numRows)
    
    observeEvent(input$perentMales, {
      # print("% males triggered ------> ")
      # print(c(input$perentMales, input$perentFemales, input$perentOthers) )
      
      val1 = round((input$perentOthers / (input$perentOthers + input$perentFemales)) *  (100 - input$perentMales))
      val2 = 100 - val1 - input$perentMales
      
      updateSliderInput(session = session, inputId = 'perentOthers',label = "% of Others", min = 0, max = 100, value = val1, step = 1 )
      updateSliderInput(session = session, inputId = 'perentFemales',label = "% of Females", min = 0, max = 100, value = val2, step = 1 )
    })
    observeEvent(input$perentFemales, {
      # print("% females triggered ------> ")
      # print(c(input$perentMales, input$perentFemales, input$perentOthers) )
      
      
      val1 = round((input$perentOthers / (input$perentOthers + input$perentMales)) *  (100 - input$perentFemales))
      val2 = 100 - val1 - input$perentFemales
      
      updateSliderInput(session = session, inputId = 'perentOthers',label = "% of Others", min = 0, max = 100, value = val1, step = 1 )
      updateSliderInput(session = session, inputId = 'perentMales',label = "% of Males", min = 0, max = 100, value = val2, step = 1 )
    })
    observeEvent(input$perentOthers, {
      # print("% others triggered ------> ")
      # print(c(input$perentMales, input$perentFemales, input$perentOthers) )
      
      
      val1 = round((input$perentMales / (input$perentMales + input$perentFemales)) *  (100 - input$perentOthers))
      val2 = 100 - val1 - input$perentOthers
      
      updateSliderInput(session = session, inputId = 'perentMales',label = "Percent of Males", min = 0, max = 100, value = val1, step = 1 )
      updateSliderInput(session = session, inputId = 'perentFemales',label = "Percent of Females", min = 0, max = 100, value = val2, step = 1 )
    })
    
    observeEvent(input$perentHIVTrue, {updateSliderInput(session = session, inputId = 'perentHIVFalse',label = "Percent of HIV Negative", min = 0, max = 100, value = (100-input$perentHIVTrue), step = 1 )})
    observeEvent(input$perentHIVFalse, {updateSliderInput(session = session, inputId = 'perentHIVTrue',label = "Percent of HIV Positive", min = 0, max = 100, value = (100-input$perentHIVFalse), step = 1 )})
    
    # observeEvent(input$perentARTFalse, {updateSliderInput(session = session, inputId = 'perentARTTrue',label = "Percent of ART= 1", min = 0, max = 100, value = (100-input$perentARTFalse), step = 1 )})
    # observeEvent(input$perentARTTrue, {updateSliderInput(session = session, inputId = 'perentARTFalse',label = "Percent of ART = 0", min = 0, max = 100, value = (100-input$perentARTTrue), step = 1 )})
    
    
    observeEvent(input$perentOnArtFalse, {updateSliderInput(session = session, inputId = 'perentOnArtTrue',label = "Percent of not on Art", min = 0, max = 100, value = (100-input$perentOnArtFalse), step = 1 )})
    observeEvent(input$perentOnArtTrue, {updateSliderInput(session = session, inputId = 'perentOnArtFalse',label = "Percent of on Art", min = 0, max = 100, value = (100-input$perentOnArtTrue), step = 1 )})
    
    observeEvent(input$perentUnawareFalse, {updateSliderInput(session = session, inputId = 'perentUnawareTrue',label = "Percent of Unaware", min = 0, max = 100, value = (100-input$perentUnawareFalse), step = 1 )})
    observeEvent(input$perentUnawareTrue, {updateSliderInput(session = session, inputId = 'perentUnawareFalse',label = "Percent of Aware", min = 0, max = 100, value = (100-input$perentUnawareTrue), step = 1 )})
    
    observeEvent(input$retentionFalse, {updateSliderInput(session = session, inputId = 'retentionTrue',label = "Retention = 1", min = 0, max = 100, value = (100-input$retentionFalse), step = 1 )})
    observeEvent(input$retentionTrue, {updateSliderInput(session = session, inputId = 'retentionFalse',label = "Retention = 0", min = 0, max = 100, value = (100-input$retentionTrue), step = 1 )})
    
    
    
  
    
    
    
    dataset <- eventReactive((input$simulateDataButton | input$loadFileButton), {
      # print("Updating Dataset!")
      if (input$selectDataset == "Simulate Dataset") {
        n = numRows()
        data.frame( vls = round(runif(n,50,300), 2), 
                    age  = sample(seq(20, 70),size = n,replace = TRUE), 
                    sex  =  factor(sample(c("Male", "Female", "Others"),size = n,replace = TRUE,prob = c(input$perentMales/100, input$perentFemales/100, input$perentOthers/100) )), 
                    hiv =  factor(sample(c(0,1),size = n,replace = TRUE, prob = c(input$perentHIVFalse/100, input$perentHIVTrue/100))), 
                    retention =  factor(sample(c(0,1),size = n,replace = TRUE, prob = c(input$retentionFalse/100, input$retentionTrue/100))), 
                    on_art  =  factor(sample(c(0,1),size = n,replace = TRUE, prob = c(input$perentOnArtFalse/100, input$perentOnArtTrue/100))), 
                    unware  =  factor(sample(c(0,1),size = n,replace = TRUE, prob = c(input$perentUnawareFalse/100, input$perentUnawareTrue/100))), 
                    country =  factor(sample(c("India", "USA", "Germany", "Italy", "Australia"),size = n,replace = TRUE)))
      }
      else{
        
        read.csv(file = input$idfile$datapath)
        }
    })
  
    
    numeric_columns <- reactive({
      dataset() %>% filter(FALSE) %>% select_if(is.numeric) %>% colnames()
    }) 
    factor_columns <- reactive({
      dataset() %>% filter(FALSE) %>% select_if(is.factor) %>% colnames()
    })
    
    
    
    skimmed_Dataset <- reactive({
      cols <- c('skim_variable', 'skim_type', 'n_missing')
      if(length(factor_columns() > 0)){cols <- append(cols, c('factor.n_unique', 'factor.top_counts'))}
      if(length(numeric_columns() > 0)){cols <- append(cols, c('numeric.mean', 'numeric.sd', 'numeric.p0', 'numeric.p25', 'numeric.p50', 'numeric.p75', 'numeric.p100', 'numeric.hist'))}
      
      skim(dataset()) %>% 
        select(cols) %>% 
        rename(Variable = skim_variable, Type=skim_type)
      
      }) 
    
    ############################################################################################
    ###############           Reactive Rules
    ############################################################################################  
    
    
    displayDataTrigger <- reactive({
      paste(input$simulateDataButton , input$loadFileButton)
    })
    observeEvent(displayDataTrigger(), {
      
      output$DataTable  <- DT::renderDataTable(dataset(), options = list(lengthMenu = c(10, 15), pageLength = 10, scrollX = TRUE))
  
      ########### Show Column Summaries
      # output$numEDATable  <- DT::renderDataTable(numerical_summary(), options = list(searching = FALSE, paging=F, autowidth=T, info=FALSE))
      # output$factEDATable  <- DT::renderDataTable(factor_summary(), options = list(searching = FALSE, paging=F, autowidth=T, info=FALSE))
      
      output$DataTableColumnsSummary <- DT::renderDataTable(skimmed_Dataset(), options = list(searching = FALSE, paging=F, scrollX = TRUE, info=FALSE))
      
      ########### Basic EDA
      
      
      ## plotting
      updateSelectInput(session = session,inputId = 'xSingle', label = "Select Variable", choices = colnames(dataset()))
      
      updateSelectInput(session = session,inputId = 'x', label = "Select X Variable", choices = colnames(dataset()))
      updateSelectInput(session = session,inputId = 'y', label = "Select Y Variable", choices = colnames(dataset()))
      
      updateSelectInput(session = session,inputId = 'selectYVariable', label = "Select 'y'", choices = numeric_columns())
      
      
      
      
      shinyjs::showElement(id = "viewSampleData", animType = 'slide', time = 0.2 )
      shinyjs::showElement(id = "edaColumnsTab", animType = 'slide', time = 0.2 )
      shinyjs::showElement(id = "edaPlotTab", animType = 'slide', time = 0.2 )
      shinyjs::showElement(id = "modelFitTab", animType = 'slide', time = 0.2 )
      
      
    })
    
    output$downloadData <- downloadHandler(
      filename = "simulatedData.csv",
      content = function(file) {
        write.csv(dataset(), file, row.names = FALSE)
      }
    )
    
    
    makeSinglePlotTrigger <- reactive({input$xSingle})
      
    observeEvent( makeSinglePlotTrigger(), {
      
      
      if (input$xSingle %in% numeric_columns()){
        output$plotSingle <- renderPlot(
          ggplot(data = dataset(), aes_string(x=input$xSingle)) +
            geom_histogram()  + theme_stata() + scale_colour_stata()
        )
      }
      else if (input$xSingle %in% factor_columns()){
        output$plotSingle <- renderPlot(
          ggplot(data = dataset(), aes_string(x=input$xSingle, fill=input$xSingle)) +
            geom_bar(stat="count")  + theme_stata() + scale_colour_stata()
        )
      }
    })
    
    
    makeMultiplePlotTrigger <- reactive({
      paste(input$x , input$y)
    })
    
    
    observeEvent( makeMultiplePlotTrigger(), {
  
      
      if ((input$x %in% numeric_columns()) & (input$y %in% numeric_columns())){
        output$plot <- renderPlot(
          ggplot(data = dataset(), aes_string(x=input$x, y= input$y, col=input$x)) +
            geom_point() + theme_stata() #+ scale_colour_stata()
        )
      }
      else if ((input$x %in% numeric_columns()) & (input$y %in% factor_columns())){
        output$plot <- renderPlot(
          ggplot(data = dataset(), aes_string(x=input$x, y= input$y, fill=input$y)) +
            geom_boxplot()  + theme_stata() + scale_colour_stata()
        )
      }    
      else if ((input$x %in% factor_columns()) & (input$y %in% numeric_columns()) ){
        output$plot <- renderPlot(
          ggplot(data = dataset(), aes_string(x=input$x, y= input$y, fill=input$x)) +
            geom_boxplot()  + theme_stata() + scale_colour_stata()
        )
      }
      
      # else if ((input$x %in% factor_columns()) & (input$y %in% factor_columns())){
      #   output$plot <- renderPlot(
      #     ggplot(data = dataset(), aes_string(x=input$x, y= input$y)) +
      #       geom_point()
      #   )
      # }
    })
    
    
    observeEvent(input$trainPercentSlider, {
      output$TrainTestPercentages <- renderUI(
        HTML(
          paste(paste0("Train Data %: ", input$trainPercentSlider * 100, "% (", numRows() * input$trainPercentSlider, " Rows)" ), 
                paste0("Test Data %: ", (1-input$trainPercentSlider) * 100, "% (", numRows() * (1-input$trainPercentSlider), " Rows)" ), sep = "<br/>")
        )
      )
    })
    
    trainRows <- eventReactive(input$splitButton, {
      createDataPartition(dataset() %>% select(input$selectYVariable) %>% pull(), p=input$trainPercentSlider, list=FALSE)
    })
    trainData <- eventReactive(input$splitButton, {
      df  = dataset()[trainRows(), ]
      train_missingdata_model = preProcess(df, method='knnImpute')
      df = predict(train_missingdata_model, newdata = df)
      df = one_hot(as.data.table(df))
      
      df
      
    })
    testData <- eventReactive(input$splitButton, {
      df = dataset()[-trainRows(), ]
      train_missingdata_model = preProcess(df, method='knnImpute')
      df = predict(train_missingdata_model, newdata = df)
      df = one_hot(as.data.table(df))
      
      df
    })
    
    
    
    observeEvent(input$splitButton, {
      
      
        output$trainSplitDataInd <- renderText("")
        output$trainMissingImputeInd <- renderText("")
        output$trainOneHotInd <- renderText("")
        output$testSplitDataInd <- renderText("")
        output$testMissingImputeInd <- renderText("")
        output$testOneHotInd <- renderText("")
        # print("Cleared the Text")
        trainRows()
        # print("Completed finding the train Rows")
        trainData()
        # print("Completed Splitting & Preprocessing Train Data")
        output$trainSplitDataInd <- renderText("Split Data ... Done")
        output$trainMissingImputeInd <- renderText("Missing Data Imputation ... Done")
        output$trainOneHotInd <- renderText("One hot Encoding ... Done")
        # print("Updated UI Indicators for Train")
        
        testData()
        # print("Completed Splitting & Preprocessing Test Data")
        output$testSplitDataInd <- renderText("Split Data ... Done")
        output$testMissingImputeInd <- renderText("Missing Data Imputation ... Done")
        output$testOneHotInd <- renderText("One hot Encoding ... Done")
        
        
        shinyjs::showElement(id = "trainingModelSection", animType = 'slide', time = 0.2 )
      })
    
    model_name <- reactive({
    
      if (input$selectModel == "Random Forest"){'rf'}
      else if (input$selectModel == "Linear Regression"){'lm'}
      else if (input$selectModel == "SVM-Linear"){'svmLinear'}
      else if (input$selectModel == "SVM-Poly"){'svmPoly'}
      else('lm')
      
    })
    
    observeEvent(input$trainButton, {
      
      shinyjs::hideElement(id = "modelResults", animType = 'slide', time = 0.2 )
      withProgress(message = "Fitting the Model",{
      model = train(x = trainData() %>% select(-input$selectYVariable), 
                         y = trainData() %>% select(input$selectYVariable) %>% pull(), method=model_name())
      })
      
      shinyjs::showElement(id = "modelResults", animType = 'slide', time = 0.2 )
      output$modelNameOutut <- renderText(model[['modelInfo']][['label']])
      output$modelResultsOutut <- renderTable(model[['results']])
      
      
      
      
    }
    )
    
      
    
    
    observeEvent(c(input$selectDataset, input$resetData), {
      shinyjs::hideElement(id = "viewSampleData", animType = 'slide', time = 0.2 )
      shinyjs::hideElement(id = "edaColumnsTab", animType = 'slide', time = 0.2 )
      shinyjs::hideElement(id = "edaPlotTab", animType = 'slide', time = 0.2 )
      shinyjs::hideElement(id = "modelFitTab", animType = 'slide', time = 0.2 )
      shinyjs::hideElement(id = "trainingModelSection", animType = 'slide', time = 0.2 )
      shinyjs::hideElement(id = "modelResults", animType = 'slide', time = 0.2 )
      
      
      if(input$selectDataset == "Upload Dataset"){
        shinyjs::hideElement(id = "simulateOptions", animType = 'slide', time = 0.2 )
        shinyjs::showElement(id = "uploadOptions", animType = 'slide', time = 0.2 )
      }
      else{
        shinyjs::hideElement(id = "uploadOptions", animType = 'slide', time = 0.2 )
        shinyjs::showElement(id = "simulateOptions", animType = 'slide', time = 0.2 )
      }
      
    })
    
    # observeEvent(input$resetData, {
    #   shinyjs::hideElement(id = "viewSampleData", animType = 'slide', time = 0.2 )
    #   shinyjs::hideElement(id = "edaColumnsTab", animType = 'slide', time = 0.2 )
    #   shinyjs::hideElement(id = "edaPlotTab", animType = 'slide', time = 0.2 )
    #   shinyjs::hideElement(id = "modelFitTab", animType = 'slide', time = 0.2 )
    #   shinyjs::hideElement(id = "trainingModelSection", animType = 'slide', time = 0.2 )
    #   shinyjs::hideElement(id = "modelResults", animType = 'slide', time = 0.2 )
    #   
    #   
    #   if(input$selectDataset == "Upload Dataset"){
    #     shinyjs::hideElement(id = "simulateOptions", animType = 'slide', time = 0.2 )
    #     shinyjs::showElement(id = "uploadOptions", animType = 'slide', time = 0.2 )
    #   }
    #   else{
    #     shinyjs::hideElement(id = "uploadOptions", animType = 'slide', time = 0.2 )
    #     shinyjs::showElement(id = "simulateOptions", animType = 'slide', time = 0.2 )
    #   }
    #   
    # })
    
    
    
  }
  shinyApp(ui = ui, server = server)
  
