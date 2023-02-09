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
      tabPanel(
        title = "How to use",
        icon = icon("question-circle",
                    verify_fa = FALSE),
        h3("Epidemiological parameters"),
        HTML(
          "This tab is mainly reserved to epidemiologists, nevertheless if you whish to, you have access to all the model parameters here.
          In this panel, you can define the epidemiological parameters.
          Those parameters will drive the spread and introduction of the pathogen in the healthcare system structured in connected subpopulations.
          <b>All parameters are pre-entered and may be left as they are.</b>
          "
        ),
        h3("Define parameters"),
        h4("Epidemiological parameters"),
        HTML(
          "
          Using this tab, you can change the daily incidence, the basic reproduction number (R0) and the average disease duration.
          Those parameters will affect the probability of contamination of professionals in the community (outside of work), as well as the probability to receive a infectious visitor.
          This tab, also allows to define other features such as the probability of developping symptoms or the excretion rate define for each epidemiological stage.
          Epidemiological stages considered in this model are: susceptible (S), exposed but non contagious (E), exposed and contagious either before symptoms (ES) or before asymptomatic stage (EA), infectious either asymptomatic (IA), with mild symptoms (IM) or with severe symptoms (IS).
          <br>
             MWSS also considers three immunity levels:
        <ol>
        <li> individuals without any immunity (neither vaccinated nor recovered: NI), </li>
        <li> individuals with a low immunity level (considering either an old vaccine injection or recovery: LI), and </li>
        <li> individuals with a high immunity level (considering either a recent vaccine injection or recovery: HI). </li>
        </ol>
        <br>
        Those three levels of immunity impact the probabilities of both contracting the disease, and developing mild or severe symptoms.
        In this tab, you can adjust the immunity impact (percentage of protection assigned to each immunity level)."
        ),
        h4("Test-related parameters"),
        HTML(
        "In this tab, the specificity, sensibility and duration of the tests can be adjusted.
        "
        ),
        h4("Immunity-related parameters"),
        HTML(
          "
          By default, all patients and professionals are considered as fully susceptible (neither vaccinated nor recovered).
          In this tab, you can specify the initial immunity state population.
          Your population will be randomly sampled based on probability weights defined for each immunity level.
          You can choose to use national proportions as probabilities, or set you own probabilities.
          You can also choose to use different probabilities for different wards (for example,
          you may face different immunity levels in paediatric, geriatric or psychiatric wards).
          <br>
          The national proportions and duration of each immunity and epidemiological stages,
          as well as the probability of receiving a vaccination dose can be modify within the 'More advanced parameters' box at the bottom of the page.

          "
        ),
        br(),
        br(),
        img(src = 'compartmentalModel-epid.png',
            title = "Multilevel compartmental model",
            width = "80%")
      ),
      tabPanel(
        title = "Epidemiological parameters",
        icon = icon("viruses",
                    verify_fa = FALSE),
        fluidRow(
          box(
            # width = 2,
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
              'Average disease duration (days)',
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
            # width = 5,
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
            ),
            sliderInput(
              "pdieIC",
              label = 'Probability of dying in intensive care',
              min = 0,
              max = 100,
              value = 0.5,
              step = 0.1,
              post = " %"
            )
          )
        ),
        diseasetimelineUI("covid"),
      ),
      tabPanel(
        title = "Test-related parameters",
        icon = icon("vial-virus",
                    verify_fa = FALSE),
        fluidPage(
          box(
            # width = 9,
            title = "Test effectiveness",
            solidHeader = TRUE,
            column(
              width = 6,
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
              width = 6,
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
          )
        )
      ),
      tabPanel(
        title = "Immunity-related parameters",
        icon = icon("shield",
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
          ),

          box(
            title = "More advanced parameters",
            width = 12,
            solidHeader = T,
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
                icon = "exclamation-triangle",
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
                icon = "exclamation-triangle",
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
          )
        )
      ),
      tabPanel(
        title = "References",
        icon = icon("book",
                    verify_fa = FALSE)
      )
    )
  )
}
