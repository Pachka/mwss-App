###################
##################
### Parameters ###
##################
##################


tabItemParams <- function() {
  tabItem(
    "PARAMS",
    tabsetPanel(
      id = "tabsPARAMS",
      header = tagList(
        br(),
        div(style = "display: inline-block;vertical-align:top;",
            updateParamsUI("variant")),
        div(
          style = "display: inline-block;vertical-align:top;",
          fileInput(
            "loadparams",
            "Upload a pre-recorded set of parameters",
            buttonLabel = "Browse RDA file",
            accept = c("rda", ".Rda")
          )
        ),
        div(
          style = "display: inline-block;vertical-align:top;",
          conditionalPanel(
            "output.paramsUploaded == true",
            actionButton(
              inputId = "applyParamsLoad",
              label = "Upload",
              icon = icon("upload"),
              style = "color: #fff; background-color: red; border-color: #fff; padding: 5px 5px 5px 5px; margin: 10px 5px 5px 5px; "
            )
          )
        ),
        div(style = "display: inline-block;vertical-align:top;",
            downloadParamsUI("dwloadParams")),
        hr()
      ),
      tabPanel(
        title = "How to use",
        icon = icon("question-circle",
                    verify_fa = FALSE),
        h3("Epidemiological parameters"),
        HTML(
          "In this panel, you can adjust the epidemiological parameters.
          Those parameters will drive the spread and introduction of the pathogen in the healthcare system structured in connected subpopulations.
          "
        ),
        h3("Upload a baseline set of parameters"),
        HTML(
          "In the upper part of this panel, you can use either the scrolling list, to select a SARS-CoV-2 variants,
          or the buttons 'Browse' and 'Upload' to load a previously saved set of parameters.
          Parameters loaded using the scrolling list were defined based on the literature (see Reference tab) and
          parameter estimation statistical approaches.
          <br>
          Be careful, uploading new parameters will erase any modified parameters. Think about regularly saving your inputs using the 'Download' button.
          <br>
          Using the 'Download' button allows saving the set of parameters in RDA file.
          This file contains a list of parameters and can be opened in the MWSS-App as well as directly using the R software."
        ),
        h3("Adjust parameters"),
        h4("Epidemiological parameters"),
        HTML(
          "In this tab, use the left part to inform mwss about the specificities in your facility related to professionals (<b>sick leave</b>: SL and <b>extended sick leave</b>: ESL)
          and patients (<b>intensive care</b>: IC and <b>potential comorbidities or resistance</b>).
          For example, children are a lot less likely to develop severe symptoms, while older individuals are more likely to develop severe symptoms.
          Use the right part to characterize contacts between populations (quantity, duration and level of infection control).
          For example, infection control of children during visits could be lower than with professionals.
          <br>
          <b>All parameters are pre-entered and may be left as they are.</b>"
        ),
        h4("Test-related parameters"),
        HTML(
        "In this tab, use the left part to inform mwss about the used test (specificity, sensibility, delay before result and targeted population).
        Two types of tests are proposed:
        <ol>
        <li>antigen detection rapid diagnostic test (Ag-RDT), and</li>
        <li>real-time reverse transcription polymerase chain reaction assay (RT-PCR).</li>
        </ol>
        In essence, both types of test are defined by their specificity, sensibility and duration, it can be any type test,
        nevertheless, in its stage of developement, mwss only provides the possibility of discriminating two types of tests
        used either for patient or professional screening, or for symptomatic patients confirmation.
        <br>
        Use the right side of the tab to inform on the detection/reaction time in case of symptoms for both patients and professionals.
        "
        ),
        h4("Immunity-related parameters"),
        HTML(
          "
          MWSS considers three immunity levels:
        <ol>
        <li> individuals without any immunity (neither vaccinated nor recovered: NI), </li>
        <li> individuals with a low immunity level (considering either an old vaccine injection or recovery: LI), and </li>
        <li> individuals with a high immunity level (considering either a recent vaccine injection or recovery: HI). </li>
        </ol>
        <br>
        Those three levels of immunity impact the probabilities of both contracting the disease, and developing mild or severe symptoms.
        <br>
          By default, all patients and professionals are considered as fully susceptible (neither vaccinated nor recovered).
          In this tab, you can specify the initial immunity state population.
          Your population will be randomly sampled based on probability weights defined for each immunity level.
          You can choose to use national proportions as probabilities, or set you own probabilities.
          You can also choose to use different probabilities for different wards (for example,
          you may face different immunity levels in paediatric, geriatric or psychiatric wards).
          "
        ),
        h4("Expert corner"),
        HTML(
          "
          This tab is mainly reserved to epidemiologists, nevertheless if you whish to, you have access to all the model parameters here.
          <br>
          Using this tab, you can change the daily incidence, the basic reproduction number (R0) and the average disease duration.
          Those parameters will affect the probability of contamination of professionals in the community (outside of work), as well as
          the probability to receive a infectious visitor.
          You can also adjust the national proportions and duration of each immunity and epidemiological stages,
          as well as the probability of receiving a vaccination dose.
          Finally, you can adjust the immunity efficiency.
          Epidemiological stages considered in this model are: susceptible (S), exposed but non contagious (E), exposed and contagious either before symptoms (ES) or before asymptomatic stage (EA), infectious either asymptomatic (IA), with mild symptoms (IM) or with severe symptoms (IS).
          "
        ),
        br(),
        br(),
        img(src = 'compartmentalModel-epid.png',
            title = "Multilevel compartmental model",
            width = "100%")
      ),
      tabPanel(
        title = "Epidemiological parameters",
        icon = icon("sliders-h",
                    verify_fa = FALSE),
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
              sliderInput(
                "pdieIC",
                label = 'Probability of dying in intensive care',
                min = 0,
                max = 100,
                value = 0.5,
                step = 0.1,
                post = " %"
              ),
              helper(
                checkboxInput(
                  "comorbidities",
                  "Do your patients have comorbidities or resistance?",
                  value = FALSE,
                  width = NULL
                ),
                icon = icon("question-circle",
                            verify_fa = FALSE),
                colour = "red",
                type = "markdown",
                content = "HelpBoxComorbidities"
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
                  icon = icon("exclamation-triangle",
                              verify_fa = FALSE),
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
                  icon = icon("exclamation-triangle",
                              verify_fa = FALSE),
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
          )
        )
      ),
      tabPanel(
        title = "Test-related parameters",
        icon = icon("sliders-h",
                    verify_fa = FALSE),
        fluidPage(
          box(
            width = 9,
            title = "Test effectiveness",
            solidHeader = TRUE,
            column(
              width = 4,
              radioButtons(
                "testPW",
                "What kind of test are you using to systematically test patients?",
                choiceNames =
                  list("Ag-RDT", "RT-PCR"),
                choiceValues =
                  list("Ag-RDT", "RT-PCR"),
                inline = TRUE
              ),
              radioButtons(
                'testH',
                "What kind of test are you using to systematically test professionals?",
                choiceNames =
                  list("Ag-RDT", "RT-PCR"),
                choiceValues =
                  list("Ag-RDT", "RT-PCR"),
                inline = TRUE
              ),
              radioButtons(
                'testsymp',
                "What kind of test are you using to test symptomatic individuals (either patients or professionals)?",
                choiceNames =
                  list("Ag-RDT", "RT-PCR"),
                choiceValues =
                  list("Ag-RDT", "RT-PCR"),
                inline = TRUE
              )
            ),
            br(),
            column(
              width = 4,
              conditionalPanel(
                'input.testPW == "Ag-RDT" | input.testH == "Ag-RDT" | input.testsymp == "Ag-RDT"',
                h5(
                  "Here you can adjust sensitivity and specificity of the Ag-RDT tests."
                ),
                numericInput(
                  'sensAg',
                  'Sensitivity',
                  value = 0.85,
                  max = 1,
                  min = 0,
                  step = 0.01
                ),
                numericInput(
                  'speAg',
                  'Specificity',
                  value = 0.95,
                  max = 1,
                  min = 0,
                  step = 0.01
                ),
                timeInput(
                  'tAg',
                  'Average delay between test and action (H:M)',
                  seconds = FALSE,
                  value = strptime("00:30", "%R")
                )
              )
            ),
            column(
              width = 4,
              conditionalPanel(
                'input.testPW == "RT-PCR" | input.testH == "RT-PCR" | input.testsymp == "RT-PCR"',
                h5(
                  "Here you can adjust sensitivity and specificity of the RT-PCR tests."
                ),
                numericInput(
                  'sensPCR',
                  'Sensitivity',
                  value = 0.85,
                  max = 1,
                  min = 0,
                  step = 0.01
                ),
                numericInput(
                  'spePCR',
                  'Specificity',
                  value = 0.95,
                  max = 1,
                  min = 0,
                  step = 0.01
                ),
                timeInput(
                  'tPCR',
                  'Duration from test to action (H:M)',
                  seconds = FALSE,
                  value = strptime("00:30", "%R")
                )
              )
            )
          ),
          box(
            width = 3,
            title = "Test of symptomatic individuals",
            solidHeader = TRUE,
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
        )
      ),
      tabPanel(
        title = "Immunity-related parameters",
        icon = icon("sliders-h",
                    verify_fa = FALSE),
        fluidRow(
          box(
            title = "Here you can define the initial immunity status of patients and professionals within your facility",
            width = 12,
            solidHeader = T,
            conditionalPanel(
              "output.atleastoneward == true",
              column(
                12,
                setIMMstateUI(
                  "ImmstateP",
                  "Patients"
                )
              ),
              column(
                12,
                setIMMstateUI(
                  "ImmstateH",
                  "Healthcare workers"
                )
              )
            )
          ),
          conditionalPanel(
            "output.atleastoneward == true",
            box(
              title = "Visualize the immunity levels of each ward",
              width = 12,
              solidHeader = T,
              column(5,
              # div(style = "display: inline-block;vertical-align:top;",
                  div(DT::DTOutput("IMMstateTab"), style = "font-size: 70%;")),
              column(7,
              # div(
              #   style = "display: inline-block;vertical-align:top;",
                selectInput(
                  "popimm_plot",
                  label = "Display initial immunity state for (population):",
                  choices = c(
                    "Both: patients and professionals" = "P+H",
                    "Patients" = "P",
                    "Professionals" = "H"
                  )
                ),
              sliderInput("piesize",
                          "Size of the pies",
                          min=5, max = 50, value = 30),
              sliderInput("labelpos",
                          "Position of the ward names",
                          min=0, max = 10, value = 3),
              # sliderInput("alphalabelpos",
              #             "Position of the ward names (angle)",
              #             min = 1, max = 10, value = 2),
              h4('Legend: Red: proportion of non immune individuals;
              Orange: proportion of individuals with low immunity; 
                 Gree: proportion of individuals with high immunity.'),
                plotOutput("imm_plot")),
              # div(
              #   style = "display: inline-block;vertical-align:top;"
                  # )
            )
          )
        )
      ),
      tabPanel(
        title = "Expert corner",
        icon = icon("sliders-h",
                    verify_fa = FALSE),
        fluidRow(
          box(
            width = 2,
            title = 'Characteristics of the variant in the community',
            numericInput(
              'I',
              'Daily incidence for 100,000 persons',
              # FIX ME: add help (https://www.gouvernement.fr/info-coronavirus/carte-et-donnees)
              value = 185,
              min = 0,
              max = 100000
            ),
            numericInput(
              'R0',
              'Basic reproduction number',
              # FIX ME: add help  https://www.gouvernement.fr/info-coronavirus/carte-et-donnees
              value = 1.29,
              min = 0,
              step = 0.01
            ),
            numericInput(
              'd',
              'Average disease duration',
              # FIX ME: add help (https://www.gouvernement.fr/info-coronavirus/carte-et-donnees)
              value = 10,
              min = 0,
              max = 150
            ),
            hr(),
            h5("Ratio adjusting the excretion rate based on epidemiological stage"),
            numericInput(
              'rEA',
              'Exposed pre-asymptomatic',
              value = 0.35,
              min = 0,
              max = 1
            ),
            numericInput(
              'rES',
              'Exposed pre-symptomatic',
              value = 1,
              min = 0,
              max = 1
            ),
            numericInput(
              'rIA',
              'Infectious asymptomatic',
              value = 0.35,
              min = 0,
              max = 1
            ),
            numericInput(
              'rIM',
              'Infectious with mild symptoms',
              value = 1,
              min = 0,
              max = 1
            ),
            numericInput(
              'rIS',
              'Infectious with severe symptoms',
              value = 1,
              min = 0,
              max = 1
            )
          ),
          box(
            width = 5,
            column(
              6,
              helper(
                sliderInput(
                  'pLI_NL',
                  'Proportion of the population with low immunity (probability to have low immunity level at the admission)',
                  value =  0.20,
                  min = 0,
                  max = 1,
                  step = 0.01
                ),
                icon = icon("exclamation-triangle",
                            verify_fa = FALSE),
                colour = "orange",
                type = "inline",
                content = paste(
                  "This proportion defines the national levels of immunity of the 'Immunity-related parameters' panel"
                )
                ),
              sliderInput(
                'rinfLI',
                'Low immunity impact (probability to be infected compared to non immune individuals)',
                value =  0.70,
                min = 0,
                max = 1,
                step = 0.01
              ),
              sliderInput(
                'hNI2LI',
                'Daily probability to acquire low level of immunity when non immune',
                value =  1 / 30,
                min = 0,
                max = 1,
                step = 0.01
              ),
              numericInput(
                'tLI',
                'Average duration of low immunity (days)',
                value = 60,
                min = 1,
                step = 1
              )
            ),
            column(
              6,
              helper(
                sliderInput(
                  'pHI_NL',
                  'Proportion of the population with high immunity (probability to have high immunity level at the admission)',
                  value =  0.50,
                  min = 0,
                  max = 1,
                  step = 0.01
                ),
                icon = icon("exclamation-triangle",
                            verify_fa = FALSE),
                colour = "orange",
                type = "inline",
                content = paste(
                  "This proportion defines the national levels of immunity of the 'Immunity-related parameters' panel"
                )),
              sliderInput(
                'rinfHI',
                'High immunity impact (probability to be infected compared to non immune individuals)',
                value =  0.50,
                min = 0,
                max = 1,
                step = 0.01
              ),
              sliderInput(
                'hLI2HI',
                'Daily probability to acquire high level of immunity when lowly immune',
                value =  1 / 60,
                min = 0,
                max = 1,
                step = 0.01
              ),
              numericInput(
                'tHI',
                'Average duration of high immunity (days)',
                value = 150,
                min = 1,
                step = 1
              ),
              style = 'border-left: 1px solid'
            )
          ),
          box(
            width = 5,
            column(
              6,
              h4("Probability to develop symptoms"),
              sliderInput(
                'psympNI',
                'With no history of infection or vaccination',
                value = 0.5,
                min = 0,
                max = 1,
                step = 0.01
              ),
              sliderInput(
                'psympLI',
                'With low immunity', # old (>3 month) history of infection or vaccination',
                value = 0.2,
                min = 0,
                max = 1,
                step = 0.01
              ),
              sliderInput(
                'psympHI',
                'With high immunity',# recent (<= 3 month) history of infection or vaccination',
                value = 0.2,
                min = 0,
                max = 1,
                step = 0.01
              )
            ),
            column(
              6,
              h4(
                "Probability to develop severe symptoms when symptomatic (conditional probability)"
              ),
              sliderInput(
                'psevNI',
                'With no history of infection or vaccination',
                value = 0.5,
                min = 0,
                max = 1,
                step = 0.01
              ),
              sliderInput(
                'psevLI',
                'With low immunity', # old (>3 month) history of infection or vaccination',
                value = 0.3,
                min = 0,
                max = 1,
                step = 0.01
              ),
              sliderInput(
                'psevHI',
                'With high immunity', # recent (<= 3 month) history of infection or vaccination',
                value = 0.1,
                min = 0,
                max = 1,
                step = 0.01
              ),
              style = 'border-left: 1px solid'
            )
          )
        ),
        diseasetimelineUI("covid"),
      ),
      tabPanel(
        title = "References",
        icon = icon("book")
      )
    )
  )
}
