library(shiny)

shinyServer(function(input, output, clientData, session) {

  output$selectDepVar <- renderUI({
    if(is.null(input$before_trt)) return()
    if(is.null(input$after_trt)) return()

    wellPanel(
    selectInput('depVarinput', label = 'Select Dependent Variable:',
                choices = intersect(
                  names(read.csv(input$before_trt$datapath, header=T, stringsAsFactors = F)),
                  names(read.csv(input$after_trt$datapath, header=T, stringsAsFactors = F))
                  )
                )
    )
  })

  output$selectOffestVar <- renderUI({
    if(is.null(input$before_trt)) return()
    if(is.null(input$after_trt)) return()

    wellPanel(
    selectInput('offsetVarinput', label = 'Select Offset Variable:',
                choices = intersect(
                  names(read.csv(input$before_trt$datapath, header=T, stringsAsFactors = F)),
                  names(read.csv(input$after_trt$datapath, header=T, stringsAsFactors = F))
                )
    )
    )
  })

  output$selectIndepVars <- renderUI({
    if(is.null(input$before_trt)) return()
    if(is.null(input$after_trt)) return()

    wellPanel(
    checkboxGroupInput('indepVarsinput', 'Select Independent Variables:',
                       choices = intersect(
                         names(read.csv(input$before_trt$datapath, header=T, stringsAsFactors = F)),
                         names(read.csv(input$after_trt$datapath, header=T, stringsAsFactors = F))
                       )
    )
    )
  })

  output$selectKeepVars <- renderUI({
    if(is.null(input$indepVarsinput)) return()

    checkboxGroupInput('keepVarsinput', 'Select any variables that should always be kept in the reference model:',
                       choices = input$indepVarsinput
                       )
  })
  output$Results <- renderPrint('your results here!')


})
