###################
###################
### Simulations ###
###################
###################


tabItemSim <- function() {
  tabItem("Vsimp",
          tabsetPanel(
            id = "tabsSIM",
            tabPanel(
              title = "How to use",
              icon = icon("question-circle",
                          verify_fa = FALSE),
              h3("Simulation panel"),
              HTML(
                "In this panel, you can select a variant and run various surveillance and control scenarios to assess their impact on the disease spread.
                <br>
                In the upper part of this panel, you can use either the scrolling list, to select a SARS-CoV-2 variants,
                or the buttons 'Browse' and 'Upload' to load a previously saved set of parameters.
                Parameters loaded using the scrolling list were defined based on the literature (see Reference tab) and
                parameter estimation statistical approaches.
                <br>
                  Be careful, uploading new parameters will erase any modified parameters. Think about regularly saving your inputs using the 'Download' button.
                <br>
                  Using the 'Download' button allows saving the set of parameters in RDA file.
                This file contains a list of parameters and can be opened in the MWSS-App as well as directly using the R software.
                <br>
                The epidemiological parameters defined for each variant are displayed in the 'More parameters' panel.
                They have been defined based on literature review and expert opinion. They should not require modification, but to meet different user needs, the possibility to adjust each parameter has been implemented."
              ),
              h3("Surveillance and control"),
              HTML(
                "At this stage of development, the following measures have been implemented:
                  <ol>
                    <li>contact restriction for patients with positive test (ISO compartment at the subpopulation level). You can adjust the average duration of this restriction.</li>
                    <li>regular screening in patient and/or professionals populations. You can adjust the frequency of test-screening events, as well as the targeted population and subpopulation (immunity-based).</li>
                    <li>systematic screening of patients at the admission. This measure implies the absence of contact with most of the professionals and all patients before test result. You can adjust the various parameters such as the type of test used, the level of infection control in patient/professional interactions, etc.</li>
                  </ol>"
              ),
              h3("Simulation parameters"),
              HTML(
                "You can define the duration of the simulation (days) and the number of simulations before running the model.
                The number of simulations should be defined as a tradeoff between running time and standard deviation of the output.
                During a run, any click is prevented by the app.
                Once you clicked the \"Run\" button, be patient, simulations can take time.
                Do not close the window."
              ),
              h3("Simulation output"),
              HTML(
                "Various output are proposed.
                You can download a synthetic report and raw data ready to be imported and explore in R.
                All figures are editable and downloadable either in png of pdf.
                "
              ),
              br(),
              br(),
              img(
                src = 'compartmentalModel.png',
                title = "Multilevel compartmental model",
                width = "70%"
              )
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
                width = 12,
                loadTestdtUI("loadtest"),
                updateParamsUI_simp("disease"),
              ),
              hr(),
              fluidRow(
                box(
                  width = 12,
                  checkboxGroupInput(
                    "CSprotocols",
                    "Control and surveillance:",
                    c(
                      "Impose isolation/contact restrictions to detected patients?" = "ISO",
                      "Implement random tests at regular intervals? for patients?" = "testPat",
                      "Implement random tests at regular intervals? for professionals?" = "testProf",
                      "Implement a test at patient admission?" = "SA"
                    )
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
                  # checkboxInput(
                  #   "simP",
                  #   "Do you want to use simulated prevalences and proportion of vaccinated individuals?",
                  #   value = FALSE,
                  #   width = NULL
                  # ),
                  # conditionalPanel(
                  #   condition = "input.simP == 1",
                  #   dateInput(
                  #     "startSimP",
                  #     "Simulate prevalences and vacc. proportions from:",
                  #     value = "2022-01-01",
                  #     format = "dd/mm/yy"
                  #   )
                  # ),
                  # conditionalPanel(
                  #   "output.atleastoneward == true" ,
                  #   conditionalPanel(
                  #     condition = "$(\'html\').hasClass(\'shiny-busy\')",
                  #     # tags$div(class = "loader"),
                  #     tags$div(class = "prevent_click")
                  #   ),
                  #   actionButton(
                  #     "runmodelVsimp",
                  #     "Run",
                  #     # span("Run", id = "UpdateAnimate", class = "loading dots"),
                  #     icon = icon("play",
                  #                 verify_fa = FALSE),
                  #     style = "color: #fff; background-color: #063567; border-color: #2e6da4"
                  #   ),
                  #   div(
                  #     style = "display: inline-block;vertical-align:top;",
                  #     conditionalPanel(
                  #       "output.simoutput == true",
                  #       synthreportUI("report_exp"),
                  #       exporttrajUI("export_traj")
                  #     )
                  #   )
                  # ),
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
