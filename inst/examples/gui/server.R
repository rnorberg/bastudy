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

  results_text <- eventReactive(input$goButton, {
    if(input$analType == 'Naive'){
      naive(
        before = read.csv(input$before_trt$datapath, header=T, stringsAsFactors = F),
        after = read.csv(input$after_trt$datapath, header=T, stringsAsFactors = F),
        depVar = input$depVarinput,
        db = input$bef_duration,
        da = input$af_duration,
        alpha = input$conf_lev)

    }else if(input$analType == 'Comparison'){
      CompGroup(
        compBefore = read.csv(input$before_comp$datapath, header=T, stringsAsFactors = F),
        compAfter = read.csv(input$after_comp$datapath, header=T, stringsAsFactors = F),
        before = read.csv(input$before_trt$datapath, header=T, stringsAsFactors = F),
        after = read.csv(input$after_trt$datapath, header=T, stringsAsFactors = F),
        depVar = input$depVarinput,
        alpha = input$conf_lev)

    }else if(input$analType == 'Empirical Bayes'){
      empBayes(
        reference = read.csv(input$ref_data$datapath, header=T, stringsAsFactors = F),
        before = read.csv(input$before_trt$datapath, header=T, stringsAsFactors = F),
        after = read.csv(input$after_trt$datapath, header=T, stringsAsFactors = F),
        depVar = input$depVarinput,
        offsetVar = input$offsetVarinput,
        indepVars = input$indepVarsinput,
        forceKeep = input$keepVarsinput,
        alpha = input$conf_lev)
    }
  })

  output$Results <- renderPrint({results_text()})


})
