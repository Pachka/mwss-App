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
      title =  h2("Daily imported and nosocomial cases (entire facility)", align="center"),
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
           ndays) {
    # model()
    ns <- session$ns

    #########
    ######### Peak incidence plot
    #########

    mypPeak <- function() {
      simple_plot_peak = function(trajmwss) {
        n_it <- seq(length(trajmwss))

        # add iteration
        trajmwss <- lapply(n_it, function(sim) {
          trajmwss[[sim]][, `:=`(iteration = sim)]
          trajmwss[[sim]]
        })

        # group into unique data.table
        trajmwss %<>% do.call(rbind, .)

        #  cummulative daily incidence per node per iteration
        trajmwss[, `:=`(incP = (sum(incPA, incPM,  incPS)),
                        incH = (sum(incHA, incHM, incHS))),
                 by = c("iteration", "time", "node")]

        #  daily incidence per node per iteration
        trajmwss[, `:=`(d_incP = diff(c(0,incP)),
                        d_incH = diff(c(0,incH))),
                 by = c("node", "iteration")]

        #  remove unused columns
        trajmwss %<>% .[, c("time","node", "iteration", "d_incP", "d_incH"), with=FALSE]


        #  renames variables
        setnames(trajmwss, "d_incP", "Patients")
        setnames(trajmwss, "d_incH", "Healthcare workers")

        #  remove unused columns
        trajmwss %<>% melt(., id.vars = c("time" , "node" , "iteration"))

        # Compute average daily incidence over simulations
        trajmwss[, `:=`(mean = mean(value),
                        sd = sd(value)),
                 by = c("node","time", "variable")]

        #  remove unused columns
        trajmwss %<>% .[, c("time","node", "variable", "value", "mean","sd"), with=FALSE]

        # select max daily incidence
        trajmwss[, `:=`(maxInc = max(mean)),
                 by = c("node", "variable")]

        #  remove unused columns
        trajmwss %<>% .[, c("node", "variable", "mean","sd", "maxInc"), with=FALSE]
        trajmwss %<>% unique

        p <- ggplot(trajmwss) +
          geom_col(aes(node, maxInc, fill = variable), position = "dodge") +
          geom_errorbar(aes(node, maxInc, ymin=maxInc-0.1, ymax=maxInc+0.1, group = variable),
                        position = "dodge") + #TODO replace with actual mean+-sd
          labs(x = "Service", y = "Peak daily incidence", fill = "")

        print(p)
        return(p)
      }

      simple_plot_peak(trajmwss = model()[["trajmwss"]])

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
        trajmwss = model()[["trajmwss"]],
        ward_names = model()[["ward_names"]],
        pop_size_P = model()[["pop_size_P"]],
        LS = model()[["LS"]],
        matContact = model()[["matContact"]],
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
        choices = seq(length(model()[["trajmwss"]]))
      )
    })

    output$ward_choiceInc <- renderUI({
      selectInput(
        inputId = ns("ward_inc"),
        label = "Select a ward",
        choices = model()[["ward_names"]]
      )
    })

    #QL edit here to use simple version

    myIncidence <- function(){

    simple_plot_incidence = function(trajmwss){

      n_it <- seq(length(trajmwss))

      # add iteration
      trajmwss <- lapply(n_it, function(sim) {
        trajmwss[[sim]][, `:=`(iteration = sim)]
        trajmwss[[sim]]
      })

      trajmwss %<>% do.call(rbind, .)

      setDT(trajmwss)
      # incidence by pop
      # trajmwss[, `:=`(incP = (sum(incPA, incPM, incPS)),
      #                 incH = (sum(incHA, incHM, incHS))),
      #          by = c("iteration", "node","time")]

      trajmwss[, `:=`(casImpP = sum(admE, admEA, admES, admIA, admIM, admIS),
                      casImpH = infHout),
               by = c("iteration", "node","time")]

      trajmwss %<>% .[, c("time","node", "iteration", "casImpP", "casImpH", "infP", "infH"), with=FALSE]

      trajmwss[, `:=`(casImpP = c(0,diff(casImpP)),
                      casImpH = c(0,diff(casImpH)),
                      infP = c(0,diff(infP)),
                      infH = c(0,diff(infH))),
               by = c("node", "iteration")]

      trajmwss %<>% melt(., id.vars = c("time" , "node" , "iteration"))

      trajmwss[, `:=`(value = sum(value)),
               by = c("time", "iteration", "variable")]

      trajmwss %<>% .[, c("time", "variable", "value"), with=FALSE]

      trajmwss[, `:=`(mean = mean(value),
                      sd = sd(value)),
               by = c("time", "variable")]


      p <- ggplot(trajmwss, aes(x=time, y=value, group=variable, color = variable, fill = variable)) +
        geom_smooth(span = 0.5) +
        geom_point(data = trajmwss[mean != 0], aes(x=time, y=mean, group=variable, color = variable)) +
        xlab("Time (day)") +
        ylab("Avearge daily number of cases") +
        scale_fill_manual(values =  c("#f6ec23", "#f68323", "#6495ED", "#CCCCFF"),
                          name = "",
                          limits = c("casImpP", "infP","casImpH", "infH"),
                          labels = c("Imported (patients)",
                                     "Nosocomial (patients)",
                                     "Imported (healthcare workers)",
                                     "Nosocomial (healthcare workers)"
                          )) +
        scale_colour_manual(values =  c("#f6ec23", "#f68323", "#6495ED", "#CCCCFF"),
                            name = "",
                            limits = c("casImpP", "infP","casImpH", "infH"),
                            labels = c("Imported (patients)",
                                       "Nosocomial (patients)",
                                       "Imported (healthcare workers)",
                                       "Nosocomial (healthcare workers)"
                            )
        )

      print(p)
      return(p)
    }

    simple_plot_incidence(trajmwss = model()[["trajmwss"]])
    }

    output$plotIncidence <- renderPlot({
      myIncidence()
    })

    # downloadHandler contains 2 arguments as functions, namely filename, content
    output$down_Incidence <- downloadHandler(
      filename =  function() {
        paste("daily_cases", "png", sep = ".") #edited to remove call to input$formatP3
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
        choices = seq(length(model()[["trajmwss"]]))
      )
    })

    output$ward_choiceTest <- renderUI({
      selectInput(
        inputId = ns("ward_test"),
        label = "Select a ward",
        choices = model()[["ward_names"]]
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
        trajmwss = model()[["trajmwss"]],
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
        paste("daily_test_counter", "png", sep = ".")
      },
      # content is a function with argument file. content writes the plot to the device
      content = function(file) {
        png(file) # open the png device

        myTestcounter()

        # draw the plot
        dev.off()  # turn the device off
      }
    )

  }
