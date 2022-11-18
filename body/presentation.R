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
          br(),br(),br(),
          h4(tags$div(HTML("
          MWSS-App contains four sections (left panel):
<br>
<br>
          The first one allows to initialize the tool.
          In this section, you will describe the healthcare structure with its different wards and how healthcare workers are shared between them.
          In short and technical words, this is where you parameterize the demographic model.
          You will also describe epidemiological settings inherent to your facility.
<br>
<br>
          The second section allows to run simulation according to different monitoring and control scenarios.
          The predictions can be visualized and downloaded in the section.
         
<br>
          The last section concerns the parameters of the epidemiological model.
          This section is divided into four sub-sections. These concern the epidemiological characteristics of a specific population ('Epidemiological parameters'),
          the characteristics of the tests used ('Test-related parameters'),
          the levels of immunity in the different sub-populations studied (patients and healthcare workers in each department; 'Immunity-related parameters').
          The last subsection displays complementary parameters used by the model, there were taken from the literature and should not require any modification.
<br>

 ")))
          )
}
