#################
#################
### Structure ###
#################
#################

tabItemStructure <- function() {
  tabItem(tabName = "STR",
          tabsetPanel(
            id = "tabsSTR",
            header = tagList(
              div(style = "display: inline-block;vertical-align:top;",
                  fileInput("loadwards",
                            "", # FIX ME: Explain in How to use: Upload a pre-recorded set of wards/buildings
                            buttonLabel = "Browse RDA file",
                            accept = c("rda", ".Rda"))),
              div(
                style = "display: inline-block;vertical-align:top;",
                conditionalPanel(
                  "output.fileUploaded == true",
                  actionButton(
                    inputId = "uploadSTR",
                    label = "Upload",
                    icon = icon("upload"),
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
                                 "Download",
                                 style = "color: #fff; background-color: #27ae60; border-color: #fff; width:130; margin: 20px 5px 5px 5px;"),
                  resetreactivesUI("resetall")
                )
              ),
              div(
                style = "display: inline-block;vertical-align:top;",
                br(),
                loadTestdtUI("loadtest")
              ),
              hr()
            ),
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
                "
              ),
              h3("Save your structure"),
              p(
                "In the upper part of this panel, you can use the green button: 'Download' to save a registered structure,
                 the associated buttons 'Browse' and 'Upload' to load a previously saved structure and the 'Clear' button to reset the tool.
                 Please, note that uploading a dataset or clearing the structure will erase everything that has been previously recorded.
                 The 'Download' button will save the dataset under a Rda format (R Data file) associated with the R program.
                 This file can be loaded in the R-shiny application 'MWSS-App' or in R.
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
              icon = icon("sliders-h",
                          verify_fa = FALSE),
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
                    wardRemoveUI("removeward", "")),
                conditionalPanel(
                  "output.atleastoneward == true",
                  hr(),
                  h4("Health care workers shared time"),
                  h5(
                    "The work time of HCWS is not constrain to 100%.
                 Some HCWS can have a part time (appearing in yellow) and other can to exceeding hours (appearing in red).
                 Neverthess 1% of work time must correspond to the same duration for all HCWS."
                  ),
                  br(),
                  div(dataTableOutput("contacts"), style = "font-size:70%"),
                  editbuttonUI("editplanning", "Modify time distribution")
                ),
              ),
              box(
                title = "Connectivity",
                solidHeader = T,
                plotOutput("network_plot")
              )
            )
          ))



}
