wardEditUI <- function(id, titletext) {
  ns <- NS(id)

  conditionalPanel(
    "output.atleastoneward_advanced == true",
  actionButton(
    ns("editbut"),
    titletext,
    tags$i(class = "fa fa-edit",
           style = "color: rgb(0,166,90)")
  )
  )
}

wardEdit  <- function(input, output, session, variable) {

  # Show modal when button is clicked.
  observeEvent(input$editbut, {

    ward_namesChoices <- variable$ward_names

    showModal(modalDialog(
          selectizeInput("wardtoEdit", "Select a ward/building",
                                 choices = ward_namesChoices,
                                 options = list(
                                   placeholder = 'Select ward',
                                   onInitialize = I('function() { this.setValue(""); }')
                                 )),
          conditionalPanel(
            "input.wardtoEdit != ''",
            numericInput(
              'pop_size_PNEW',
              'Number of beds',
              value = 1,
              min = 1,
              step = 1
            ),
            numericInput(
              'pop_size_HNEW',
              'Number of healthcare workers',
              value = 1,
              min = 1,
              step = 1
            ),
            h5("Changing the number of professionals will reset the plannings of all healthcare workers of the ward at 100% in the ward. If you already adjust the professional planning, consider adding/removing specific professionals within the professional planning panel."),
            numericInput(
              'LSNEW',
              'Average patient length of stay (days)',
              value = 1,
              min = 1,
              step = 1
            ),
            numericInput(
              'nVisitsNEW',
              'Average number of visits per day',
              value = 0,
              min = 0,
              step = 1
            ),
          actionButton("wardEditOk", "Modify ward/building")),
        title = "Here you can edit ward or building structure",
        easyClose = TRUE
      )
    )
  })



}
