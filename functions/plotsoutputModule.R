plotsoutputUI <- function(id) {
  ns <- NS(id)

  tagList(fluidRow(
    box(
      column(
        4,
        numericInput(
          ns('outb_Thhold'),
          'Probability to have at least n nosocomial infection:',
          value = 1,
          min = 0,
          step = 1
        ),
        checkboxInput(
          ns("pOBlegcol"),
          "Limit the number of colors in the legend",
          value = FALSE
        ),
        conditionalPanel(
          condition =
            paste0('input[\'', ns('pOBlegcol'), "\'] == 1"),
          sliderInput(
            ns('nlegcolpOB'),
            'Number of colors in the legend:',
            value = 5,
            min = 2,
            max = 10,
            step = 1
          )
        ),
        hr(),
        checkboxInput(ns("pOBoptions"),
                      "Layout options",
                      value = FALSE),
        conditionalPanel(
          condition =
            paste0('input[\'', ns('pOBoptions'), "\'] == 1"),
          selectInput(
            ns('pOBlayout'),
            "Layout",
            c(
              "as_star",
              "as_tree",
              "in_circle",
              "nicely",
              "on_grid",
              "on_sphere",
              "randomly",
              "with_dh",
              "with_fr",
              "with_gem",
              "with_graphopt",
              "with_kk",
              "with_lgl",
              "with_mds",
              "with_sugiyama"
            ),
            selected = "with_fr"
          ),
          sliderInput(
            ns('pOBvertexsize'),
            'Size of the wards:',
            value = 0.5,
            min = 0.1,
            max = 1,
            step = 0.1
          ),
          sliderInput(
            ns('pOBvertexlabelsize'),
            'Size of the names:',
            value = 0.03,
            min = 0.01,
            max = 0.1,
            step = 0.01
          ),
          sliderInput(
            ns('pOBedgearrowsize'),
            'Size of the arrows:',
            value = 0.4,
            min = 0.1,
            max = 1,
            step = 0.1
          ),
          conditionalPanel(
            condition =
              paste0('input[\'', ns('formatP1'), "\'] == \"png\""),
            column(6,
            sliderInput(
              ns('widthP1'),
              'Width (px):',
              value = 1500,
              min = 500,
              max = 2000,
              step = 50
            )),
            column(6,
            sliderInput(
              ns('heightP1'),
              'Height (px):',
              value = 1100,
              min = 500,
              max = 2000,
              step = 50
            )))
        ),
        radioButtons(
          inputId = ns("formatP1"),
          label = "Select the file type",
          choices = list("png", "pdf")
        ),
        downloadButton(outputId = ns("down_pOutbreak"), label = "Download the plot")
      ),
      column(8,
             plotOutput(ns("pOutbreak")))
    ),
    box(
      column(
        4,
        checkboxInput(
          ns("nosolegcol"),
          "Limit the number of colors in the legend",
          value = FALSE
        ),
        conditionalPanel(
          condition =
            paste0('input[\'', ns('nosolegcol'), "\'] == 1"),
          sliderInput(
            ns('nlegcolnosoHaza'),
            'Number of colors in the legend:',
            value = 5,
            min = 2,
            max = 10,
            step = 1
          )
        ),
        hr(),
        checkboxInput(ns("nosoHazaoptions"),
                      "Layout options",
                      value = FALSE),
        conditionalPanel(
          condition =
            paste0('input[\'', ns('nosoHazaoptions'), "\'] == 1"),
          selectInput(
            inputId = ns('nosoHazalayout'),
            label = "Layout",
            choices = c(
              "as_star",
              "as_tree",
              "in_circle",
              "nicely",
              "on_grid",
              "on_sphere",
              "randomly",
              "with_dh",
              "with_fr",
              "with_gem",
              "with_graphopt",
              "with_kk",
              "with_lgl",
              "with_mds",
              "with_sugiyama"
            ),
            selected = "with_fr"
          ),
          sliderInput(
            ns('nosoHazavertexsize'),
            'Size of the wards:',
            value = 0.5,
            min = 0.1,
            max = 1,
            step = 0.1
          ),
          sliderInput(
            ns('nosoHazavertexlabelsize'),
            'Size of the names:',
            value = 0.03,
            min = 0.01,
            max = 0.1,
            step = 0.01
          ),
          sliderInput(
            ns('nosoHazaedgearrowsize'),
            'Size of the arrows:',
            value = 0.4,
            min = 0.1,
            max = 1,
            step = 0.1
          ),
          conditionalPanel(
            condition =
              paste0('input[\'', ns('formatP2'), "\'] == \"png\""),
            column(6,
                   sliderInput(
                     ns('widthP2'),
                     'Width (px):',
                     value = 1500,
                     min = 500,
                     max = 2000,
                     step = 50
                   )),
            column(6,
                   sliderInput(
                     ns('heightP2'),
                     'Height (px):',
                     value = 1100,
                     min = 500,
                     max = 2000,
                     step = 50
                   )))
        ),
        radioButtons(
          inputId = ns("formatP2"),
          label = "Select the file type",
          choices = list("png", "pdf")
        ),
        downloadButton(outputId = ns("down_nosoHazard"), label = "Download the plot")
      ),
      column(8,
             # div(style = "display: inline-block;vertical-align:top;",
             plotOutput(ns("nosoHazard")))
    ),
    box(
      column(
        4,
        selectInput(
          inputId = ns("scaleInc"),
          label = "Display incidence for (scale):",
          choices = c("The whole facility" = 0,
                      "Each ward" = 1),
          selected = FALSE
        ),
        conditionalPanel(
          condition =
            paste0('input[\'', ns('scaleInc'), "\'] == 1"),
          checkboxInput(
            inputId = ns("wardInc"),
            label = "Display incidence for a specific ward",
            value = FALSE,
            width = NULL
          ),
          conditionalPanel(condition =
                             paste0('input[\'', ns('wardInc'), "\'] == 1"),
                           uiOutput(ns("ward_choiceInc")))
        ),
        selectInput(
          inputId = ns("popInc"),
          label = "Display incidence for (population):",
          choices = c(
            "Both: patients and professionals" = "P+H",
            "Patients" = "P",
            "Professionals" = "H"
          ),
          selected = FALSE
        ),
        conditionalPanel(
          condition =
            paste0('input[\'', ns('iterInc'), "\'] == 0"),
          checkboxInput(
            inputId = ns("display_sdInc"),
            label = "Display standard deviation",
            value = FALSE,
            width = NULL
          )
        ),
        checkboxInput(
          inputId = ns("iterInc"),
          label = "Display incidence for only one simulation",
          value = FALSE,
          width = NULL
        ),
        conditionalPanel(condition =
                           paste0('input[\'', ns('iterInc'), "\'] == 1"),
                         uiOutput(ns("iter_choiceInc"))),
        hr(),
        radioButtons(
          inputId = ns("formatP3"),
          label = "Select the file type",
          choices = list("png", "pdf")
        ),
        downloadButton(outputId = ns("down_Incidence"), label = "Download the plot")
      ),
      column(8,
             plotOutput(ns("plotIncidence")))
    ),
    box(
      column(
        4,
        selectInput(
          inputId = ns("scaleTest"),
          label = "Display number of tests for (scale):",
          choices = c("The whole facility" = 0,
                      "Each ward" = 1),
          selected = FALSE
        ),
        conditionalPanel(
          condition =
            paste0('input[\'', ns('scaleTest'), "\'] == 1"),
          checkboxInput(
            inputId = ns("wardTest"),
            label = "Display number of tests for a specific ward",
            value = FALSE,
            width = NULL
          ),
          conditionalPanel(condition =
                             paste0('input[\'', ns('wardTest'), "\'] == 1"),
                           uiOutput(ns(
                             "ward_choiceTest"
                           )))
        ),
        selectInput(
          inputId = ns("popTest"),
          label = "Display number of tests for (population):",
          choices = c(
            "Both: patients and professionals" = "P+H",
            "Patients" = "P",
            "Professionals" = "H"
          ),
          selected = FALSE
        ),
        checkboxInput(
          inputId = ns("iterTest"),
          label = "Display number of tests for only one simulation",
          value = FALSE,
          width = NULL
        ),
        conditionalPanel(condition =
                           paste0('input[\'', ns('iterTest'), "\'] == 1"),
                         uiOutput(ns(
                           "iter_choiceTest"
                         ))),
        checkboxInput(
          inputId = ns("agrtest"),
          label = "Median over aggregated period",
          value = FALSE,
          width = NULL
        ),
        conditionalPanel(condition =
                           paste0('input[\'', ns('agrtest'), "\'] == 1"),
                         uiOutput(ns(
                           "daysint_choiceTest"
                         ))),
        hr(),
        radioButtons(
          inputId = ns("formatP4"),
          label = "Select the file type",
          choices = list("png", "pdf")
        ),
        downloadButton(outputId = ns("down_nTest"), label = "Download the plot")
      ),
      column(8,
             plotOutput(ns("plottest")))
    )
  ))
}

plotsoutput <-
  function(input,
           output,
           session,
           model,
           variable,
           ndays) {
    # model()
    ns <- session$ns

    #########
    ######### Network plot
    #########

    mypOutbreak <- function() {
      if (input$pOBlegcol == 1)
        maxcolors <- input$nlegcolpOB
      else
        maxcolors <- FALSE

      plot_pOutbreak(
        trajmwss = model(),
        pop_size_P = variable$pop_size_P,
        matContact = variable$matContact,
        outb_Thhold = input$outb_Thhold,
        layout = input$pOBlayout,
        vertexsize = input$pOBvertexsize,
        vertexlabelsize = input$pOBvertexlabelsize,
        edgearrowsize = input$pOBedgearrowsize,
        maxcolors = maxcolors,
        addtitle = TRUE,
        verbose = FALSE
      )

    }

    output$pOutbreak <- renderPlot({
      mypOutbreak()
    })

    # downloadHandler contains 2 arguments as functions, namely filename, content
    output$down_pOutbreak <- downloadHandler(
      filename =  function() {
        paste("outbreak_probability", input$formatP1, sep = ".")
      },
      # content is a function with argument file. content writes the plot to the device
      content = function(file) {
        if (input$formatP1 == "png")
          png(file, res = 150,
              width = input$widthP1,
              height = input$heightP1
              ) # open the png device
        else
          pdf(file) # open the pdf device

        mypOutbreak()

        # draw the plot
        dev.off()  # turn the device off
      }
    )

    ## a plot function
    mynosoHazard <- function() {
      if (input$nosolegcol == 1)
        maxcolors <- input$nlegcolnosoHaza
      else
        maxcolors <- FALSE

      plot_nosoHazard(
        trajmwss = model(),
        ward_names = variable$ward_names,
        pop_size_P = variable$pop_size_P,
        LS = variable$LS,
        matContact = variable$matContact,
        layout = input$nosoHazalayout,
        vertexsize = input$nosoHazavertexsize,
        vertexlabelsize = input$nosoHazavertexlabelsize,
        edgearrowsize = input$nosoHazaedgearrowsize,
        addtitle = TRUE,
        maxcolors = maxcolors,
        verbose = FALSE
      )
    }


    output$nosoHazard <- renderPlot({
      mynosoHazard()
    })

    # downloadHandler contains 2 arguments as functions, namely filename, content
    output$down_nosoHazard <- downloadHandler(
      filename =  function() {
        paste("nosocomial_hazard", input$formatP2, sep = ".")
      },
      # content is a function with argument file. content writes the plot to the device
      content = function(file) {
        if (input$formatP2 == "png")
          png(file, res = 150,
              width = input$widthP2,
              height = input$heightP2) # open the png device
        else
          pdf(file) # open the pdf device

        mynosoHazard()

        # draw the plot
        dev.off()  # turn the device off

      }
    )

    #########
    ######### Cumulative incidences
    #########

    output$iter_choiceInc <- renderUI({
      selectInput(
        inputId = ns("iter_inc"),
        label = "Select a simulation",
        choices = seq(length(model()))
      )
    })

    output$ward_choiceInc <- renderUI({
      selectInput(
        inputId = ns("ward_inc"),
        label = "Select a ward",
        choices = variable$ward_names
      )
    })


    myIncidence <- function() {
      ward = FALSE

      if (isTRUE(input$iterInc) |  length(input$display_sdInc) == 0)
        display_sdInc <- FALSE
      else
        display_sdInc <- input$display_sdInc

      if (input$scaleInc == 1 &
          isTRUE(input$wardInc) & length(input$ward_inc) > 0)
        ward = input$ward_inc

      if (input$iterInc == 1 & length(input$iter_inc) > 0)
        iter = input$iter_inc %>% as.numeric
      else
        iter = FALSE

      if (input$popInc == "P+H" | length(input$popInc) == 0)
        pop = FALSE
      else
        pop = input$popInc

      scale = input$scaleInc
      display_sd = display_sdInc

      plot_incidence(
        trajmwss = model(),
        scale = scale,
        pop = pop,
        iter = iter,
        ward = ward,
        display_sd = display_sdInc
      )
    }

    output$plotIncidence <- renderPlot({
      myIncidence()
    })

    # downloadHandler contains 2 arguments as functions, namely filename, content
    output$down_Incidence <- downloadHandler(
      filename =  function() {
        paste("daily_incidence", input$formatP3, sep = ".")
      },
      # content is a function with argument file. content writes the plot to the device
      content = function(file) {
        if (input$formatP3 == "png")
          png(file) # open the png device
        else
          pdf(file) # open the pdf device

        myIncidence()

        # draw the plot
        dev.off()  # turn the device off
      }
    )

    #########
    ######### Daily test boxplot
    #########

    output$iter_choiceTest <- renderUI({
      selectInput(
        inputId = ns("iter_test"),
        label = "Select a simulation",
        choices = seq(length(model()))
      )
    })

    output$ward_choiceTest <- renderUI({
      selectInput(
        inputId = ns("ward_test"),
        label = "Select a ward",
        choices = variable$ward_names
      )
    })

    output$daysint_choiceTest <- renderUI({
      numericInput(
        ns('daysint'),
        'Calculate median of daily number of test over n-days periods',
        value = 1,
        min = 0,
        max = ndays(),
        step = 1
      )
    })

    myTestcounter <- function() {
      ward = FALSE

      if (input$scaleTest == 1 &
          isTRUE(input$wardTest) & length(input$ward_test) > 0)
        ward = input$ward_test


      if (input$iterTest == 1 & length(input$iter_test) > 0)
        iter = input$iter_test %>% as.numeric
      else
        iter = FALSE

      if (input$popTest == "P+H")
        pop = NULL
      else
        pop = input$popTest


      if (input$agrtest == 0 | length(input$daysint) == 0)
        daysint = 1
      else
        daysint = input$daysint

      scale = input$scaleTest %>% as.numeric

      plot_testcount(
        trajmwss = model(),
        scale = scale,
        pop = pop,
        iter = iter,
        ward = ward,
        daysint = daysint
      )

    }

    output$plottest <- renderPlot({
      myTestcounter()

    })

    # downloadHandler contains 2 arguments as functions, namely filename, content
    output$down_nTest <- downloadHandler(
      filename =  function() {
        paste("daily_test_counter", input$formatP4, sep = ".")
      },
      # content is a function with argument file. content writes the plot to the device
      content = function(file) {
        if (input$formatP4 == "png")
          png(file) # open the png device
        else
          pdf(file) # open the pdf device

        myTestcounter()

        # draw the plot
        dev.off()  # turn the device off
      }
    )

  }
