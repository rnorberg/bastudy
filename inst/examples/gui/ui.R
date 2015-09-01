library(shiny)

shinyUI(fluidPage(
  titlePanel("GUI Helper for Package `bastudy`"),
  fluidRow(
    selectInput('analType', "Select Analysis Type",
                choices = c('Naive', 'Comparison', 'Empirical Bayes'))
  ),

  fluidRow(
    h4("Upload your data here"),
    column(3,
           wellPanel(
             fileInput("before_trt", "Before", accept = c('text/csv', 'text/comma-separated-values,text/plain', '.csv'))
           )
    ),
    column(3,
           wellPanel(
             fileInput("after_trt", "After", accept = c('text/csv', 'text/comma-separated-values,text/plain', '.csv'))
           )
    ),
    column(6,
           conditionalPanel(condition = "input.analType == 'Comparison'",
                            column(6,
                                   wellPanel(
                                     fileInput("before_comp", "Comparison Data (before)", accept = c('text/csv', 'text/comma-separated-values,text/plain', '.csv'))
                                   )
                            ),
                            column(6,
                                   wellPanel(
                                     fileInput("after_comp", "Comparison Data (after)", accept = c('text/csv', 'text/comma-separated-values,text/plain', '.csv'))
                                   )
                            )
           ),
           conditionalPanel(condition = "input.analType == 'Empirical Bayes'",
                            column(6,
                                   wellPanel(
                                     fileInput("ref_data", "Reference Data", accept = c('text/csv', 'text/comma-separated-values,text/plain', '.csv'))
                                   )
                            )
           )
    )
  ),

  fluidRow(
    h4("Specify dependent, independent, and offset variables"),
    column(3,
           wellPanel(
             uiOutput("selectDepVar")
           )
    ),
    column(9,
           conditionalPanel(condition = "input.analType == 'Naive'",
                            column(4,
                                   wellPanel(
                                     numericInput('bef_duration', "Duration of Before Period", value = 1, min = 1, max = NA)
                                   )
                            ),
                            column(4,
                                   wellPanel(
                                     numericInput('af_duration', "Duration of After Period", value = 1, min = 1, max = NA)
                                   )
                            )
           ),
           conditionalPanel(condition = "input.analType == 'Empirical Bayes'",
                            column(4,
                                   wellPanel(
                                     uiOutput("selectOffestVar")
                                   )
                            ),
                            column(4,
                                   wellPanel(
                                     uiOutput("selectIndepVars")
                                   )
                            ),
                            column(4,
                                   wellPanel(
                                     uiOutput("selectKeepVars")
                                   )
                            )
           )
    )
  ),

  fluidRow(
    column(3,
           wellPanel(
             sliderInput('conf_lev', "Select Confidence Level",
                         min = 0.5, max = 1, value = 0.95, step = 0.01)
           )
    ),
    column(3,
           wellPanel(
             actionButton("goButton", "Calculate CMF")
           )
    ),
    column(6,
           wellPanel(
             verbatimTextOutput('Results')
           )
    )
  )
))
