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
      title =  h2("Daily cases over the entire facility", align="center"),
      footer = "29 wards, 500 patients and 900 healthcare workers. Four stochastic simulations are illustrated here among the 50. The orange shadow is the 95% prediction interval estimated from the 50 simulations.",
      plotOutput(ns("plotIncidence3")),
      downloadButton(outputId = ns("down_Incidence"), label = "Download the plot"),
      align = "center"
    ),
    box(
      title =  h2("Daily number of tests", align="center"),
      plotOutput(ns("plottest")),
      downloadButton(outputId = ns("down_nTest"), label = "Download the plot"),
      align = "center"

    ),
    box(
      title =  h2("Peak incidence by service", align="center"),
      footer = "For each population, the five highest peaks are displayed in color.",
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
        # select maximal incidence in each node for each simulation
        trajmwss[, `:=`(maxInc = max(value)),
                 by = c("node", "variable", "iteration")]

        trajmwss %<>% unique
        trajmwss %<>% .[, c("node", "variable", "maxInc"), with=FALSE]

        # calculate mean peak and sd per node over all simulations
        trajmwss[, `:=`(mean = mean(maxInc),
                        sd = sd(maxInc)),
                 by = c("node", "variable")]

        trajmwss %<>% .[, c("node", "variable","mean","sd"), with=FALSE]
        trajmwss %<>% unique

        p <- ggplot(trajmwss) +
          facet_wrap(~variable) +
          geom_col(aes(node, mean), fill = "grey", position = "dodge") +
          geom_col(data = trajmwss[order(mean), tail(.SD,5), by = variable],
                   aes(node, mean, fill = variable),
                   position = "dodge") +
          geom_errorbar(aes(node,
                            mean,
                            ymin = ifelse((mean-sd) < 0, 0, (mean-sd)),
                            ymax = mean+sd,
                            group = variable),
                        position = "dodge") +
          labs(x = "Service", y = "Peak daily incidence") +
          theme_bw() +
          theme(axis.text.x = element_text(angle=45, hjust=1, size = 5)) +
          guides(fill = "none")
        #try displaying top 10 only maybe, then rest in grey? to clearly show where highest burden is

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
            width = 700,
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

    myIncidence <- function(x){

      simple_plot_incidence1 = function(trajmwss){

        n_it <- seq(length(trajmwss))

        # add iteration
        trajmwss <- lapply(n_it, function(sim) {
          trajmwss[[sim]][, `:=`(iteration = sim)]
          trajmwss[[sim]]
        })

        trajmwss %<>% do.call(rbind, .)

        setDT(trajmwss)

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

      simple_plot_incidence2 = function(trajmwss){
        n_it <- seq(length(trajmwss))

        # add iteration
        trajmwss <- lapply(n_it, function(sim) {
          trajmwss[[sim]][, `:=`(iteration = sim)]
          trajmwss[[sim]]
        })

        trajmwss %<>% do.call(rbind, .)

        setDT(trajmwss)

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

        trajmwss[, node := NULL]

        trajmwss %<>% unique

        # New facet label names for supp variable
        var_labs <- c("Patients: imported cases", "Healthcare workers: imported cases",
                      "Patients: nosocomial cases", "Healthcare workers: nosocomial cases")
        names(var_labs) <- trajmwss$variable %>% unique

        # Create the plot
        p <- ggplot(trajmwss) +
          geom_line(aes(x=time, y=value, group=factor(iteration), colour = factor(iteration)), linewidth = 0.1, alpha = 0.5) +
          # geom_smooth(aes(x=time, y=value), span = 0.5) +
          facet_wrap(. ~ variable,
                     labeller = labeller(variable = var_labs)) +
          theme(legend.position = "none") + theme_bw()

        print(p)
        return(p)
      }

      simple_plot_incidence3 = function(trajmwss){
        n_it <- seq(length(trajmwss))

        # add iteration
        trajmwss <- lapply(n_it, function(sim) {
          trajmwss[[sim]][, `:=`(iteration = sim)]
          trajmwss[[sim]]
        })

        trajmwss %<>% do.call(rbind, .)

        setDT(trajmwss)

        trajmwss[, `:=`(casImpP = sum(admE, admEA, admES, admIA, admIM, admIS),
                        casImpH = infHout)]

        trajmwss %<>% .[, c("time","node", "iteration", "casImpP", "casImpH", "infP", "infH"), with=FALSE]

        trajmwss[, `:=`(casImpP = c(0,diff(casImpP)),
                        casImpH = c(0,diff(casImpH)),
                        infP = c(0,diff(infP)),
                        infH = c(0,diff(infH))),
                 by = c("node", "iteration")]

        trajmwss[, `:=`(infections = casImpP + casImpH + infP + infH)]

        trajmwss %<>% .[, c("time","node", "iteration", "infections"), with=FALSE]

        # trajmwss %<>% melt(., id.vars = c("time" , "node" , "iteration"))

        trajmwss[, `:=`(infections = sum(infections)),
                 by = c("time", "iteration")]

        trajmwss[, node := NULL]

        trajmwss %<>% unique

        trajmwss[, `:=`(mean = mean(infections),
                        sd = sd(infections),
                        yhat_lower = quantile(infections, 0.025),
                        yhat_upper = quantile(infections, 0.925)),
                 by = c("time")]

        p <- ggplot(trajmwss) +
          # geom_point() +
          geom_ribbon(aes(x=time, ymin = yhat_lower, ymax = yhat_upper),
                      alpha = 0.3,
                      na.rm = TRUE,
                      fill = "#f68323") +
          geom_line(data = trajmwss[iteration %in% 1:4],
                    aes(x=time, y=infections, group=factor(iteration),
                        color = factor(iteration)), linewidth = .2) +
          xlab("Time (day)") +
          ylab("Avearge daily number of cases") +
          theme(panel.background = element_rect(fill = "white", colour = "grey50")) +
          guides(color="none")

        print(p)
        return(p)
      }

      if(x == 1)
        simple_plot_incidence1(trajmwss = model()[["trajmwss"]])

      if(x == 2)
        simple_plot_incidence2(trajmwss = model()[["trajmwss"]])

      if(x == 3)
        simple_plot_incidence3(trajmwss = model()[["trajmwss"]])

    }

    output$plotIncidence1 <- renderPlot({
      myIncidence(1)
    })


    output$plotIncidence2 <- renderPlot({
      myIncidence(2)
    })

    output$plotIncidence3 <- renderPlot({
      myIncidence(3)
    })

    # downloadHandler contains 2 arguments as functions, namely filename, content
    output$down_Incidence <- downloadHandler(
      filename =  function() {
        paste("daily_cases", "png", sep = ".") #edited to remove call to input$formatP3
      },
      # content is a function with argument file. content writes the plot to the device
      content = function(file) {
        png(file) # open the png device

        myIncidence(3)

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
      ) + theme_bw()

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
