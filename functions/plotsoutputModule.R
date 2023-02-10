plotsoutputUI <- function(id) {
  ns <- NS(id)

  tagList(fluidRow(
    box(
      title =  h2("Nosocomial hazard", align="center"),
      plotOutput(ns("nosoHazard")),

      downloadButton(outputId = ns("down_nosoHazard"), label = "Download the plot"),
      align = "center"
    ),
    box(
      title =  h2("Daily incidence (entire facility)", align="center"),
      plotOutput(ns("plotIncidence")),
      downloadButton(outputId = ns("down_Incidence"), label = "Download the plot"),
      align = "center"
    ),
    box(title =  h2("Daily number of tests", align="center"),
        plotOutput(ns("plottest")),
        downloadButton(outputId = ns("down_nTest"), label = "Download the plot"),
        align = "center"

    ),
    box(title =  h2("Peak incidence by service", align="center"),
        plotOutput(ns("pPeak")),
        downloadButton(outputId = ns("down_pPeak"), label = "Download the plot"),
        align = "center"
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
    ######### Peak incidence plot
    #########

    mypPeak <- function() {
      simple_plot_peak = function(trajmwss) {
        trajmwss <- lapply(seq(length(trajmwss)), function(sim) {
          trajmwss[[sim]][, `:=`(iteration, sim)]
          trajmwss[[sim]]
        })
        trajmwss %<>% do.call(rbind, .)
        trajmwss[, `:=`(incP = (sum(incPA + incPM + incPS)),
                        incPsymp = (sum(incPM + incPS)),
                        incH = (sum(incHA + incHM + incHS)),
                        incHsymp = (sum(incHM + incHS)),
                        inc = (sum(incPA + incPM + incPS + incHA + incHM + incHS))),
                 by = c("iteration", "time")]

        trajmwss[, `:=`(incP = c(0,diff(incP)),
                        incPsymp = c(0,diff(incPsymp)),
                        incH = c(0,diff(incH)),
                        incHsymp = c(0,diff(incHsymp)),
                        inc = c(0,diff(inc))),
                 by = c("node", "iteration")]

        trajmwss[, `:=`(mean_incP = mean(incP), sd_incP = sd(incP),
                        mean_incPsymp = mean(incPsymp), sd_incPsymp = sd(incPsymp),
                        mean_incH = mean(incH), sd_incH = sd(incH), mean_incHsymp = mean(incHsymp),
                        sd_incHsymp = sd(incHsymp), mean_inc = mean(inc),
                        sd_inc = sd(inc)), by = c("node","time")]

        trajmwss = trajmwss %>%
          select(node, mean_incP, mean_incH) %>%
          group_by(node) %>%
          summarise(maxP = max(mean_incP), maxH = max(mean_incH)) %>%
          rename(Patients = maxP, Professionals = maxH) %>%
          melt("node")

        p = ggplot(trajmwss) +
          geom_col(aes(node, value, fill = variable), position = "dodge") +
          geom_errorbar(aes(node, value, ymin=value-0.1, ymax=value+0.1, group = variable),
                        position = "dodge") + #TODO replace with actual mean+-sd
          labs(x = "Service", y = "Peak daily incidence", fill = "")

        return(p)
      }

      simple_plot_peak(trajmwss = model())

    }

    output$pPeak <- renderPlot({
      mypPeak()
    })

    # downloadHandler contains 2 arguments as functions, namely filename, content
    output$down_pPeak <- downloadHandler(
      filename =  function() {
        paste("outbreak_probability", "png", sep = ".")
      },
      # content is a function with argument file. content writes the plot to the device
      content = function(file) {
        png(file, res = 150,
            width = 500,
            height = 500
        ) # open the png device

        mypPeak()

        # draw the plot
        dev.off()  # turn the device off
      }
    )


    #########
    ######### Network plot
    #########

    ## a plot function
    mynosoHazard <- function() {
      # if (input$nosolegcol == 1)
      #   maxcolors <- input$nlegcolnosoHaza
      # else
      #   maxcolors <- FALSE
      maxcolors = 5 #paramByDefaultVSimp

      plot_nosoHazard(
        trajmwss = model(),
        ward_names = variable$ward_names,
        pop_size_P = variable$pop_size_P,
        LS = variable$LS,
        matContact = variable$matContact,
        layout = "with_fr",  #input$nosoHazalayout, #paramByDefaultVSimp
        vertexsize = 0.5,  #input$nosoHazavertexsize, #paramByDefaultVSimp
        vertexlabelsize = 0.03, #input$nosoHazavertexlabelsize, #paramByDefaultVSimp
        edgearrowsize = 0.4, #input$nosoHazaedgearrowsize, #paramByDefaultVSimp
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
        paste("nosocomial_hazard", "png" , sep = ".") #input$formatP2 replace by "png" #paramByDefaultVSimp
      },
      # content is a function with argument file. content writes the plot to the device
      content = function(file) {
        png(file)

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

    #QL edit here to use simple version
    myIncidence <- function() {

      #redefine plot_incidence()

      simple_plot_incidence = function(trajmwss) {
        trajmwss <- lapply(seq(length(trajmwss)), function(sim) {
          trajmwss[[sim]][, `:=`(iteration, sim)]
          trajmwss[[sim]]
        })
        trajmwss %<>% do.call(rbind, .)
        trajmwss[, `:=`(incP = (sum(incPA + incPM + incPS)),
                        incPsymp = (sum(incPM + incPS)),
                        incH = (sum(incHA + incHM + incHS)),
                        incHsymp = (sum(incHM + incHS)),
                        inc = (sum(incPA + incPM + incPS + incHA + incHM + incHS))),
                 by = c("iteration", "time")]

        trajmwss[, `:=`(incP = c(0,diff(incP)),
                        incPsymp = c(0,diff(incPsymp)),
                        incH = c(0,diff(incH)),
                        incHsymp = c(0,diff(incHsymp)),
                        inc = c(0,diff(inc))),
                 by = c("node", "iteration")]

        trajmwss[, `:=`(mean_incP = mean(incP), sd_incP = sd(incP),
                        mean_incPsymp = mean(incPsymp), sd_incPsymp = sd(incPsymp),
                        mean_incH = mean(incH), sd_incH = sd(incH), mean_incHsymp = mean(incHsymp),
                        sd_incHsymp = sd(incHsymp), mean_inc = mean(inc),
                        sd_inc = sd(inc)), by = c("time")]

        p <- ggplot(trajmwss) +
          geom_line(aes(time, mean_incH, colour = "Professionals")) +
          geom_line(aes(time, mean_incP, colour = "Patients")) +
          xlab("Time (day)") +
          ylab("Median daily incidence") +
          labs(colour = "") +
          geom_errorbar(aes(time, mean_incP,
                            ymin = ifelse(mean_incP - sd_incP >= 0,
                                          mean_incP - sd_incP, 0),
                            ymax = mean_incP + sd_incP, colour = "Patients"),
                        alpha = 0.75, width = 0.2, position = position_dodge(0.9)) +
          geom_errorbar(aes(time, mean_incH,ymin = ifelse(mean_incH - sd_incH >= 0,
                                                          mean_incH - sd_incH, 0),
                            ymax = mean_incH + sd_incH, colour = "Professionals"),
                        alpha = 0.75, width = 0.2, position = position_dodge(0.9))

        return(p)
      }

      simple_plot_incidence(trajmwss = model())

    }

    output$plotIncidence <- renderPlot({
      myIncidence()
    })

    # downloadHandler contains 2 arguments as functions, namely filename, content
    output$down_Incidence <- downloadHandler(
      filename =  function() {
        paste("daily_incidence", "png", sep = ".") #edited to remove call to input$formatP3
      },
      # content is a function with argument file. content writes the plot to the device
      content = function(file) {
        png(file) # open the png device

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

      plot_testcount(
        trajmwss = model(),
        scale = 0, #scale,
        pop = NULL, #pop,
        iter = FALSE, #iter,
        ward = FALSE, #ward,
        daysint = 7 #daysint
      )

    }

    output$plottest <- renderPlot({
      myTestcounter()

    })

    # downloadHandler contains 2 arguments as functions, namely filename, content
    output$down_nTest <- downloadHandler(
      filename =  function() {
        paste("daily_test_counter", "png" , sep = ".") #input$formatP4 change in "png" for VSimp
      },
      # content is a function with argument file. content writes the plot to the device
      content = function(file) {
        #if (input$formatP4 == "png")
        png(file) # open the png device
        #else
        #  pdf(file) # open the pdf device

        myTestcounter()

        # draw the plot
        dev.off()  # turn the device off
      }
    )

  }
