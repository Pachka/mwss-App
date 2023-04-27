##################################
##################################
### App presentation ###
##################################
##################################

tabItemPresentation <- function() {
  tabItem("PRS",
          img(
            src = 'banner.svg',
            title = "MWSS-App",
            width = "100%"
          ),
            column(12, align="center", offset = 3,
                   actionButtonStyled("versionSimple", "Simple mode", icon = NULL, width = NULL,
                                      btn_type = "button", type = "warning", class = "btn-lg"),
                   actionButtonStyled("versionAdv", "Advanced mode", icon = NULL, width = NULL,
                                      btn_type = "button", type = "warning", class = "btn-lg"),
                   actionButtonStyled("versionExp", "Expert mode", icon = NULL, width = NULL,
                                      btn_type = "button", type = "warning", class = "btn-lg"),
         tags$style(type='text/css', "#button { vertical-align- middle; height- 50px; width- 100%; font-size- 30px;}")
                   ),
         br(),
         br(),
         br(),
          p(
            "MWSS-App contains three sections (left panel):"),
          p("The first one allows to initialize the tool.
          In this section, describe the healthcare structure with its different wards and how healthcare workers are shared between them.
          In short and technical words, this is where you parameterize the demographic model.
          You will also describe epidemiological settings inherent to your facility."),
          p("The second section allows to run simulation according to different monitoring and control scenarios.
          The predictions can be visualized and downloaded in the section."),
          p("The last section concerns more advanced parameters used in the epidemiological model.
          This section is divided into three sub-sections detailling the epidemiological characteristics of a specific population ('Epidemiological parameters'),
          the characteristics of the tests used ('Test-related parameters') and
          the levels of immunity in the different sub-populations studied (patients and healthcare workers in each department; 'Immunity-related parameters')."
          ),
          p("Each section begins by the 'How to use' page that provides tips for users.")
          )
}
