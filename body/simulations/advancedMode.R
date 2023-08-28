advancedMode <- tabPanel(
  "Advanced mode",
  icon = icon("sliders-h",
              verify_fa = FALSE),
  hr(),
  fluidRow(
    box(
      title = "General settings",
      solidHeader = T,
      conditionalPanel(
        "output.atleastoneward_advanced == true",
        div(DTOutput("structure_hospital"), style = "font-size: 70%;")
      )
      ,
      h5("Add, edit or delete a ward/building"),
      div(style="display:inline-block",
          actionButton("addW_button", "",
                       tags$i(
                         class = "fa fa-plus",
                         style = "color: rgb(0,166,90)"
                       )
          )
      ),
      conditionalPanel(
        "output.atleastoneward_advanced == true",
        div(style="display:inline-block",
            actionButton("editW_button", "",
                         tags$i(
                           class = "fa fa-edit",
                           style = "color: rgb(0,166,90)"
                         )
            )
        ),
        div(style="display:inline-block",
            actionButton("deleteW_button", '',
                         tags$i(
                           class = "fa fa-minus",
                           style = "color: rgb(255,0,0)"
                         )
            ))
      ),
      actionButton("upload_general_button",
                   tags$i(
                     class = "fa fa-upload"
                   )
      ),
      downloadButton('download_general',"")
    ),
    conditionalPanel(
      "output.atleastoneward_advanced == true",
      box(
        title = "Health care workers shared time",
        solidHeader = T,
        # width = 12,
        h5(
          "The work time of HCWS is not constrain to 100%.
                 Some HCWS can have a part time (appearing in yellow) and other can to exceeding hours (appearing in red).
                 Neverthess 1% of work time must correspond to the same duration for all HCWS."
        ),
        br(),
        div(dataTableOutput("contacts"), style = "font-size:70%"),
        editbuttonUI("editplanning", "Modify time distribution"),
        actionButton("upload_HCWS_time_button",
                     tags$i(
                       class = "fa fa-upload"
                     )
        ),
        downloadButton('download_Hplanning',"")
      )
    )
  ),
  fluidRow(
    conditionalPanel(
      "output.atleastoneward_advanced == true",
      box(
        width = 6,
        plot_network_UI("network_plot_advanced")
      )
    )
  ),
  hr(),
  fluidRow(
    box(
      width = 12,
      helper(
        shiny_tag = uiOutput("CSprotocolsUI_adv"),
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
      updateParamsUI_simp("disease_adv"),
      updateImportationUI_simp("level_importation_adv"),
      numericInput(
        'n_days_adv',
        'Number of simulated days',
        value = 60,
        min = 1,
        step = 1
      ),
      uiOutput("runbutton_adv"),
      # display load spinner when shiny is busy
      conditionalPanel(
        condition = "$(\'html\').hasClass(\'shiny-busy\')",
        tags$div("Simulation in progress. This may take a while...",
                 id = "loadmessage_adv")
      )
    )
  ),
  conditionalPanel(
    "output.simadv_output == true",
    valueboxoutputUI("simulation_adv"),
    plotsoutputUI("simulationPlots_adv")
  )
)
