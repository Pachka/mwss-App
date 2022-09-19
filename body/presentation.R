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
          The first one is about the healthcare structure.
          It is in this section that you will define the structure of the different wards and the assignments of healthcare workers to the different wards.
          In short and technical words, this is where you parameterize the demographic model.
<br>
<br>
          The second section concerns the parameters of the epidemiological model.
          This section is divided into four sub-sections. These concern the epidemiological characteristics of a specific population ('Epidemiological parameters'),
          the characteristics of the tests used ('Test-related parameters'),
          the levels of immunity in the different sub-populations studied (patients and healthcare workers in each department; 'Immunity-related parameters').
          The last subsection displays complementary parameters used by the model, there were taken from the literature and should not require any modification.
<br>
<br>
          The last section concerns the monitoring and control scenarios.
          It is also in this section that the simulations will be performed and that you will be able to visualize and download the predictions.
          ")))
          )
}
