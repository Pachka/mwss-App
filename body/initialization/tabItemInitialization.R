######################
######################
### Initialization ###
######################
######################

tabItemInitialization <- function() {
  tabItem(tabName = "INI",
          tabsetPanel(
            id = "tabsSTR",
            tabPanel(
              title = "How to use",
              icon = icon("question-circle",
                          verify_fa = FALSE),
              h3(
                "Organizational structure of the healthcare facility and epidemiological impact."
              ),
              p(
                "
                Contact networks, responsible for disease introduction and spread in healthcare facilities, connect
                several subpopulations: medical staff, patients and visitors, potentially structured in subgroups such as:  departments, wards, and rooms.
                In the frame of evaluating nosocomial transmission of Sars-CoV-2 in healthcare facilities,
                MWSS considers direct transmission routes through effective contacts among those populations structured in wards.
                Healthcare workers can be contaminated either at work: by infectious patients or infectious professionals, or in the community.
                Patients can be contaminated by infectious patients and professionals but also by infectious visitors."
              ),
              # https://doi.org/10.1016/j.jtbi.2008.07.001
              h3("Inform mwss about your own healthcare system"),
              HTML(
                "
                In this panel, you can inform the social network structure of your system.
                That structure will influence the spread of the pathogen after its introduction.
                <br>
                In the <b>'Structure'</b> tab, inform mwss about the structure of the system you want to represent.
                <br>
                In the <b>'General structure'</b> box, use buttons to add, edit or delete wards from your system.
                For each ward, the system requires a unique name, a number of patients (beds/capacity), a number of professionals,
                the average length of stay of patients (in days) and the average daily number of visitors.
                When wards are added to the system, they appear in the 'Connectivity' network plot (the size of the node being
                the total population size including both patients and professionals) and professionals are added to the
                <b>'Health care workers shared time'</b> table assuming full time (100%) in the associated ward."
              ),
              h4("Professionals shared between multiple wards"),
              HTML(
                "The time spent by professionals into each ward can be adjusted using the 'Modify time distribution' button.
                Total working time of a professional can be more or less than 100%,
                nevertheless the `total` column will be highlighting those particular cases respectively in red and green to avoid mistake.
                When professionals are spending time in multiple wards, a connection between
                those wards graphically appears on the connectivity network plot.
                <br>
                <br>
                In the <b>'Parameters'</b> tab, inform mwss about the epimediological settings of the system you want to represent.
                In this tab, use the left part to inform mwss about the specificities in your facility related to professionals (<b>sick leave</b>: SL and <b>extended sick leave</b>: ESL)
          and patients (<b>intensive care</b>: IC and <b>potential comorbidities or resistance</b>).
          For example, children are a lot less likely to develop severe symptoms, while older individuals are more likely to develop severe symptoms.
          Use the right part to characterize contacts between populations (quantity, duration and level of infection control).
          For example, infection control of children during visits could be lower than with professionals.
          Finaly, inform mwss about the used test (delay before result and targeted population).
          Two types of tests are proposed:
        <ol>
        <li>antigen detection rapid diagnostic test (Ag-RDT), and</li>
        <li>real-time reverse transcription polymerase chain reaction assay (RT-PCR).</li>
        </ol>
        In essence, both types of test are defined by their specificity, sensibility and duration (define in the 'More parameters' panel), it can be any type test,
        nevertheless, in its stage of developement, mwss only provides the possibility of discriminating two types of tests
        used either for patient or professional screening, or for symptomatic patients confirmation.
        <br>
        Use the right side of the tab to inform on the  probability of being tested and detection/reaction time in case of symptoms for both patients and professionals.
                "
              ),
              h3("Save your structure"),
              HTML(
                "In the upper part of the <b>'Structure'</b> tab, you can use the green button: 'Download' to save a registered structure,
                 the associated buttons 'Browse' and 'Upload' to load a previously saved structure and the 'Clear' button to reset the tool.
                 Please, note that uploading a dataset or clearing the structure will erase everything that has been previously recorded.
                 The 'Download' button will save the dataset under a Rda format (R Data file) associated with the R program.
                 This file can be loaded in the R-shiny application 'MWSS-App' or in R.
                 <br>
                 In the same way, in the <b>'Parameters'</b> tab, use the upper part to save and load your settings/parameteres.

                "
              ),
              h3("No copy of your entries is saved anywhere, neither on the cloud nor on our servers, remember to download it locally for
                     later use."),
              br(),
              br(),
              img(src = 'compartmentalModel-structure.png',
                  title = "Multilevel compartmental model",
                  width = "80%")
            ),
            tabPanel(
              title = "Structure",
              icon = icon("hospital-user",
                          verify_fa = FALSE),
              fluidRow(
                div(
                  style = "display: inline-block;vertical-align:top;margin-left: 25px",
                  # offset = 0,
                    fileInput("loadwards",
                              "", # FIX ME: Explain in How to use: Upload a pre-recorded set of wards/buildings
                              buttonLabel = "Browse RDA file",
                              accept = c("rda", ".Rda")
                              )
                    ),
                div(
                  style = "display: inline-block;vertical-align:top;",
                  conditionalPanel(
                    "output.fileUploaded == true",
                    actionButton(
                      inputId = "uploadSTR",
                      label = "Upload",
                      icon = icon("upload",
                                  verify_fa = FALSE),
                      style = "color: #fff; background-color: red; border-color: #fff; width:130; margin: 20px 5px 5px 5px;"
                    )
                  )
                ),
                div(
                  style = "display: inline-block;vertical-align:top;",
                  conditionalPanel(
                    "output.atleastoneward == true",
                    # h5("Download the current structure.", style = "margin: 15px 5px 5px 5px; "),
                    downloadButton("downloadData",
                                   "Save",
                                   style = "color: #fff; background-color: #27ae60; border-color: #fff; width:130; margin: 20px 5px 5px 5px;"),
                    resetreactivesUI("resetall")
                  )
                ),
                # div(
                #   style = "display: inline-block;vertical-align:top;",
                #   br(),
                #   loadTestdtUI("loadtest")
                # ),
              ),
              fluidRow(
                box(
                  title = "General structure",
                  solidHeader = T,
                  conditionalPanel(
                    "output.atleastoneward == true",
                    div(DT::DTOutput("facilitystr"), style = "font-size: 70%;")
                  ),
                  h5("Add, edit or delete a ward/building"),
                  div(style = "display: inline-block;vertical-align:top;",
                      addbuttonUI("addward", "")),
                  div(style = "display: inline-block;vertical-align:top;",
                      wardEditUI("editward", "")),
                  div(style = "display: inline-block;vertical-align:top;",
                      wardRemoveUI("removeward", ""))
                ),
                # box(
                #   title = "Connectivity",
                #   solidHeader = T,
                #   plotOutput("network_plot")
                # )
                ),
              hr(),
              fluidRow(
                conditionalPanel(
                  "output.atleastoneward == true",
                  box(
                    title = "Health care workers shared time",
                    solidHeader = T,
                    width = 12,
                    h5(
                      "The work time of HCWS is not constrain to 100%.
                 Some HCWS can have a part time (appearing in yellow) and other can to exceeding hours (appearing in red).
                 Neverthess 1% of work time must correspond to the same duration for all HCWS."
                    ),
                    br(),
                    div(dataTableOutput("contacts"), style = "font-size:70%"),
                    editbuttonUI("editplanning", "Modify time distribution")
                  )
                )
              )
            ),
            tabPanel(
              title = "Parameters",
              icon = icon("sliders-h",
                          verify_fa = FALSE),
              fluidRow(
                br(),
                div(
                  style = "display: inline-block;vertical-align:top;margin-left: 25px",
                  # offset = 0,
                    fileInput("loadsettings",
                              "",
                              buttonLabel = "Browse RDA file",
                              accept = c("rda", ".Rda"))),
                div(
                  style = "display: inline-block;vertical-align:top;",
                  actionButton(
                    inputId = "uploadsettings",
                    label = "Upload",
                    icon = icon("upload",
                                verify_fa = FALSE),
                    style = "color: #fff; background-color: red; border-color: #fff; width:130; margin: 20px 5px 5px 5px;"
                  )
                ),
                div(
                  style = "display: inline-block;vertical-align:top;",
                  downloadButton("downloadsettings",
                                 "Save",
                                 style = "color: #fff; background-color: #27ae60; border-color: #fff; width:130; margin: 20px 5px 5px 5px;")
                )
                ),
              fluidRow(
                box(
                  title = "Specificities of your facility for COVID-19 management",
                  solidHeader = T,
                  column(
                    width = 6,
                    h4("Health care workers (HCWs)"),
                    br(),
                    sliderInput(
                      "pSL",
                      label = 'Probability that HCWs developping mild symptoms take sick leave',
                      min = 0,
                      max = 100,
                      value = 30,
                      post  = " %"
                    ),
                    sliderInput(
                      "pESL",
                      label = 'Probability that HCWS developping severe symptoms take extended sick leave',
                      min = 0,
                      max = 100,
                      value = 100,
                      post  = " %"
                    ),
                    #
                    conditionalPanel(
                      condition = "input.pSL > 0",
                      sliderInput(
                        "tSLs",
                        label = 'On average, how many days do sick leave and extended sick leave last?',
                        min = 0,
                        max = 90,
                        value = c(14, 28),
                        post = " days"
                      )
                    ),
                    sliderInput(
                      "pSLT",
                      # label = 'Probability to take sick leave after a positive test',
                      label = 'Probability that non-symptomatic HCWs with a positive test take sick leave.',
                      min = 0,
                      max = 100,
                      value = 10,
                      post  = " %"
                    ),
                    numericInput(
                      'tw',
                      'Average number of working hours per fulltime professional per week (hours)',
                      value = 35,
                      min = 1,
                      max = 70,
                      step = 1
                    )
                  ),
                  column(
                    width = 6,
                    h4("Patients"),
                    sliderInput(
                      "pIC",
                      label = 'When developing severe symptoms, what is the probability of transfer to another facility (eg. intensive care outside of the institut)?',
                      min = 0,
                      max = 100,
                      value = 30,
                      post  = " %"
                    ),
                    conditionalPanel(
                      condition = "input.pIC > 0",
                      numericInput(
                        'tIC',
                        'Average number of days outside the facility (eg. in intensive care)',
                        value = 15,
                        min = 1,
                        step = 0.5
                      )
                    ),
                    helper(
                      shiny_tag = checkboxInput(
                        "comorbidities",
                        "Do your patients have comorbidities or resistance?",
                        value = FALSE,
                        width = NULL
                      ),
                      icon = "question-circle",
                      colour = NULL,
                      type = "markdown",
                      title = "",
                      content = "HelpBoxComorbidities",
                      size = "m",
                      buttonLabel = "Okay",
                      easyClose = TRUE,
                      fade = FALSE
                    ),
                    conditionalPanel(
                      condition = "input.comorbidities == 1",
                      helper(
                        numericInput(
                          "rsymp",
                          label = paste(
                            'Ratio adjusting probability of symptoms for patients compared to general population (professionals)'
                          ),
                          min = 0,
                          value = 1,
                          step = 0.01
                        ),
                        icon = "triangle-exclamation",
                        colour = "orange",
                        type = "inline",
                        content = textOutput("rsympInfo")
                      ),
                      helper(
                        numericInput(
                          "rsev",
                          label = paste(
                            'Ratio adjusting probability of severity if symptoms for patients compared to general population (professionals)'
                          ),
                          min = 0,
                          value = 1,
                          step = 0.01
                        ),
                        icon = "exclamation-triangle",
                        colour = "orange",
                        type = "inline",
                        content = paste(
                          textOutput("rsevInfo")
                        ) # FIX ME give an example
                      )
                    )
                  )
                ),
                box(
                  title = "Person-to-person contacts within your facility",
                  solidHeader = T,
                  column(
                    width = 6,
                    h4("Patients-to-Patient"),
                    sliderInput(
                      "n_ctcP_PW",
                      label = 'How many patients on average does each HCW come in contact with on a daily basis?',
                      min = 0,
                      max = 15,
                      value = 4
                    ),
                    conditionalPanel(
                      condition = "input.n_ctcP_PW > 0",
                      timeInput(
                        "t_ctcP_PW",
                        'Average duration of those contacts (H:M)',
                        seconds = FALSE,
                        value = strptime("00:30", "%R")
                      ),
                      radioButtons(
                        "epsPPW",
                        "During those contacts, how would you characterize the level of infection control?",
                        choiceNames =
                          list("low", "regular", "high"),
                        choiceValues =
                          list(0.2, 0.5, 0.8),
                        inline = TRUE
                      )
                    ),
                    hr(),
                    h4("HCW-to-HCW"),
                    sliderInput(
                      "n_ctcH_H",
                      label = 'How many HCW on average does each HCW come in contact with on a daily basis?',
                      min = 0,
                      max = 15,
                      value = 5
                    ),
                    conditionalPanel(
                      condition = "input.n_ctcH_H > 0",
                      timeInput(
                        "t_ctcH_H",
                        'Average duration of those contacts (H:M)',
                        seconds = FALSE,
                        value = strptime("00:03", "%R")
                      ),
                      radioButtons(
                        "epsHHW",
                        "During those contacts, how would you characterize the level of infection control?",
                        choiceNames =
                          list("low", "regular", "high"),
                        choiceValues =
                          list(0.2, 0.5, 0.8),
                        inline = TRUE
                      )
                    )
                  ),
                  column(
                    width = 6,
                    h4("Patients-to-HCWs"),
                    sliderInput(
                      "n_ctcH_PW",
                      label = 'How many HCW on average does each patient come in contact with on a daily basis?',
                      min = 0,
                      max = 15,
                      value = 4
                    ),
                    conditionalPanel(
                      condition = "input.n_ctcH_PW > 0",
                      timeInput(
                        "t_ctcH_PW",
                        'Average duration of those contacts (H:M)',
                        seconds = FALSE,
                        value = strptime("00:15", "%R")
                      ),
                      radioButtons(
                        "epsHPW",
                        "During those contacts, how would you characterize the level of infection control for patients?",
                        choiceNames =
                          list("low", "regular", "high"),
                        choiceValues =
                          list(0.2, 0.5, 0.8),
                        inline = TRUE
                      ),
                      radioButtons(
                        "epsPHW",
                        "During those contacts, how would you characterize the level of infection control for professionals?",
                        choiceNames =
                          list("low", "regular", "high"),
                        choiceValues =
                          list(0.2, 0.5, 0.8),
                        inline = TRUE
                      )
                    ),
                    hr(),
                    h4("Patients-to-Visitors"),
                    timeInput(
                      "t_ctcV_PW",
                      'Average duration of one visit (H:M)',
                      seconds = FALSE,
                      value = strptime("00:20", "%R")
                    ),
                    radioButtons(
                      "epsVPW",
                      "During visits, how would you characterize the level of infection control for patients?",
                      choiceNames =
                        list("low", "regular", "high"),
                      choiceValues =
                        list(0.2, 0.5, 0.8),
                      inline = TRUE
                    )
                  )
                ),
                box(
                  width = 12,
                box(
                  title = "What kind of test are you using",
                  # solidHeader = TRUE,
                  radioButtons(
                    "testPW",
                    "To systematically test patients?",
                    choiceNames =
                      list("Ag-RDT", "RT-PCR"),
                    choiceValues =
                      list("Ag-RDT", "RT-PCR"),
                    inline = TRUE
                  ),
                  radioButtons(
                    'testH',
                    "To systematically test professionals?",
                    choiceNames =
                      list("Ag-RDT", "RT-PCR"),
                    choiceValues =
                      list("Ag-RDT", "RT-PCR"),
                    inline = TRUE
                  ),
                  radioButtons(
                    'testsymp',
                    "To test symptomatic individuals (either patients or professionals)?",
                    choiceNames =
                      list("Ag-RDT", "RT-PCR"),
                    choiceValues =
                      list("Ag-RDT", "RT-PCR"),
                    inline = TRUE
                  )
                ),
                box(
                  title = "Test of symptomatic individuals",
                  # solidHeader = TRUE,
                  sliderInput(
                    "ptestPWsymp",
                    label = 'Probability to test symptomatic patients',
                    min = 0,
                    max = 100,
                    value = 100
                  ),
                  conditionalPanel(
                    'input.ptestPWsymp > 0',
                    numericInput(
                      'tbeftestPsymp',
                      'Average duration between first symptoms and test for symptomatic patients (hours)',
                      value = 2,
                      min = 0.5,
                      step = 0.5
                    )
                  ),
                  sliderInput(
                    "ptestHsymp",
                    label = 'Probability to test symptomatic professionals',
                    min = 0,
                    max = 100,
                    value = 85
                  ),
                  conditionalPanel(
                    'input.ptestHsymp > 0',
                    numericInput(
                      'tbeftestHsymp',
                      'Average duration between first symptoms and test for symptomatic professionals (hours)',
                      value = 24,
                      min = 0.5,
                      step = 0.5
                    )
                  )
                )
              ))
            )
          )
  )
}
