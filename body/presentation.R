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
            "The MWSS-App enables the simulation of nosocomial outbreaks of a pathogen within a hospital. In the model, the hospital is composed of interconnected wards hosting patients and health-care workers. A series of interventions for surveillance and control are implemented to enable the evaluation of their impact on pathogen transmission."),
         p(
           "The application can be used under three modes: simple, advanced or expert. The appearance of the user interface changes depending on the selected mode. The different settings and options available in each mode are detailed in a dedicated 'How to use' tab. You can change the mode on the presentation page of the MWSS-App."),
         p(
           "The purpose of the simple mode is to allow the exploration of basic epidemiological scenarios on pre-defined theoretical healthcare structures, with a simplified user interface. Sets of parameter values are pre-entered for a list of viruses and of monitoring and surveillance strategies."),
         p(
           "Advanced and expert modes are under development. In the advanced mode, the user will be able to design their healthcare structure, including a list of wards and interconnections between them through healthcare worker sharing. They will also be able to document/parametrize care organization, and baseline hygiene and epidemic management. In the expert mode, all model parameters will be accessible, including epidemiological characteristics, efficacy of available surveillance tools, and levels of immunity in the studied sub-populations (patients and healthcare workers in each ward). This mode will provide users access to the model's full flexibility and adaptability.")
          )
}
