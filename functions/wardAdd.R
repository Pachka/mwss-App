addbuttonUI <- function(id, titletext){
  ns <- NS(id)
  actionButton(ns("addbut"), titletext,
               tags$i(
                 class = "fa fa-plus",
                 style = "color: rgb(0,166,90)"
               )
  )
}


wardAddPanel <- function(input, output, session) {

  ns <- session$ns

  observeEvent(input$addbut, {
    showModal(
      modalDialog(
        title = "Here you can add new wards or buildings",
        textInput("wardname", "Ward/building name", ""),
        tags$p(
          "Note: Each ward/building has collective areas and dedicated medical staff. \n
          Two wards cannot have the same name.  \n
          If you try to use the same name the 'Add ward' button will not appear."
        ),
        conditionalPanel(
          "input.wardname != ''",
          numericInput(
            'pop_size_P',
            'Number of beds',
            value = 1,
            min = 1,
            step = 1
          ),
          numericInput(
            'pop_size_H',
            'Number of healthcare workers',
            value = 1,
            min = 1,
            step = 1
          ),
          numericInput(
            'LS',
            'Average patient length of stay (days)',
            value = 1,
            min = 1,
            step = 1
          ),
          tags$p(
            "Note: Minimal length of stay is 1 day."
          ),
          numericInput(
            'nVisits',
            'Average number of visitors per day',
            value = 0,
            min = 0,
            step = 1
          ),
          tags$p(
            "Note: The daily number of visitors will be divided by the number of patients to obtain an average number of visit per day per patients."
          )
        ),
        uiOutput("addBut"),
        easyClose = TRUE
      )
    )
  })
}
