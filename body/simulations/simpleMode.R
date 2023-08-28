simpleMode <- tabPanel(
  "Simple mode",
  icon = icon("sliders-h",
              verify_fa = FALSE),
  hr(),
  box(
    width = 6,
    loadTestdtUI("loadtest"),
    updateParamsUI_simp("disease"),
    updateImportationUI_simp("level_importation")
  ),
  box(
    width = 6,
    # title = "Clustering",
    # solidHeader = T,
    plot_network_UI("network_plot_simple")
    # plotOutput("network_plot")
  ),
  hr(),
  fluidRow(
    box(
      width = 12,
      helper(
        shiny_tag = uiOutput("CSprotocolsUI"),
        icon = "question-circle",
        colour = "orange",
        type = "markdown",
        title = "",
        content = "HelpBoxSC",
        size = "m",
        buttonLabel = "Okay",
        easyClose = TRUE,
        fade = FALSE
      )
    ),
    box(
      title = "Simulations parameters",
      solidHeader = T,
      width = 12,
      status = "primary",
      numericInput(
        'n_days',
        'Number of simulated days',
        value = 60,
        min = 1,
        step = 1
      ),
      uiOutput("runbutton"),
      # display load spinner when shiny is busy
      conditionalPanel(
        condition = "$(\'html\').hasClass(\'shiny-busy\')",
        tags$div("Simulation in progress. This may take a while...",
                 id = "loadmessage")
      )
    )
  ),
  conditionalPanel(
    "output.simoutput == true",
    valueboxoutputUI("simulation"),
    plotsoutputUI("simulationPlots")
  )
)
