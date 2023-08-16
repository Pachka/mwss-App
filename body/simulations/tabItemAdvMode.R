###################
###################
### Simulations ###
###################
###################


tabItemAdvanced <- function() {
  tabItem("Vadv",
          tabsetPanel(
            id = "tabsADV",
            tabPanel(
              title = "How to use",
              icon = icon("question-circle",
                          verify_fa = FALSE),
              br(),
              HTML("In this mode, you can explore basic epidemiological scenarios on predefined theoretical healthcare structures. It is not possible to edit the epidemiological parameters associated with studied viruses or the list of available  surveillance and control strategies that can be evaluated."),
              h3("Initialization/Parameters"),
              HTML(
                "The first step is to select a hospital structure and a pathogen. Users may choose between three possible hospital structures and three possible pathogens. All three simulated hospital structures include 29 wards distributed over five buildings, and connected through healthcare workers to form a ward network, which is displayed as a graph. This network’s level of clustering varies: in the “high clustering” level, healthcare workers mostly work within a given building, while in the “low clustering” level, their assignments are not tied to the location of wards into specific buildings. The three possible pathogens were chosen to simulate three respiratory viruses: SARS-CoV-2, influenza and respiratory syncytial virus (RSV)."
              ),
              h3("Control and Surveillance"),
              HTML(
                "Different surveillance and control strategies can be selected from a list. Details on how these practices are implemented are provided through the question mark button to the right of the ‘Control and surveillance’ box."),
              h3("Simulations"),
              HTML(
              "By default, the simulation duration is set to 60 days, but that duration can be modified. Once you click the \"Run\" button, the model will run 50 stochastic simulations. Be patient, it can take time, do not close the window at any time, it would close the session and you would lose your selections and simulations."
              ),
              h3("Simulation output"),
              HTML(
              "Once simulations are run, results are provided through different outputs: summary statistics, graphs, and a detailed report (pdf format). All figures are downloadable in png format. Be careful and save the results you would like to keep : when a new simulation is run, all outputs and results are overwritten.")
              # img(
              #   src = 'compartmentalModel.png',
              #   title = "Multilevel compartmental model",
              #   width = "70%"
              # )
            ),
            tabPanel(
              title = "Simulations",
              icon = icon("sliders-h",
                          verify_fa = FALSE),
              # div(
              #   style = "display: inline-block;vertical-align:top;",
              #   fileInput(
              #     "loadparams",
              #     "Upload a pre-recorded set of parameters",
              #     buttonLabel = "Browse RDA file",
              #     accept = c("rda", ".Rda")
              #   )
              # ),
              # div(
              #   style = "display: inline-block;vertical-align:top;",
              #   conditionalPanel(
              #     "output.paramsUploaded == true",
              #     actionButton(
              #       inputId = "applyParamsLoad",
              #       label = "Upload",
              #       icon = icon("upload",
              #                   verify_fa = FALSE),
              #       style = "color: #fff; background-color: red; border-color: #fff; padding: 5px 5px 5px 5px; margin: 10px 5px 5px 5px; "
              #     )
              #   )
              # ),
              # div(style = "display: inline-block;vertical-align:top;",
              #     downloadParamsUI("dwloadParams")),
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
                plotOutput("network_plot")
              ),
              hr(),
              fluidRow(
                box(
                  width = 12,

                  helper(
                    shiny_tag = checkboxGroupInput(
                      "CSprotocols",
                      "Control and surveillance:",
                      c(
                        "Impose isolation/contact restrictions to detected patients?" = "ISO",
                        "Implement random tests at regular intervals for patients?" = "testPat",
                        "Implement random tests at regular intervals for professionals?" = "testProf",
                        "Implement a test at patient admission?" = "SA"
                      )
                    ),
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
                  # Only show this panel if the plot type is a histogram
                  # numericInput(
                  #   'n_sim',
                  #   'Number of simulations',
                  #   value = 50,
                  #   min = 1,
                  #   step = 1
                  # ),
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
          ))

}
