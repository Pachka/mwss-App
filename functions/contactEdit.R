contactEdit  <- function(input, output, session, variable) {
  observeEvent(input$editbut, {

    ward_namesChoices <- variable$ward_names
    professionalsChoices <- variable$Hplanning$professionals

    showModal(modalDialog(
      title = "Here you can edit healthcare workers plannings",
      selectizeInput("wardcontactedit",
                     "Select a ward to filter the list of professionals",
                     choices = ward_namesChoices,
                     options = list(
                       placeholder = 'Select a ward',
                       onInitialize = I('function() { this.setValue(""); }')
                     )),
      selectizeInput("HCWScontactedit", "Caregivers name",
                     choices = professionalsChoices,
                     options = list(
                       placeholder = 'Select worker',
                       onInitialize = I('function() { this.setValue(""); }')
                     )),
      conditionalPanel(
        "input.HCWScontactedit != ''",
        div(DT::DTOutput("contactEditTab"), style = "font-size: 70%;"),
        selectizeInput("selectedW", "Change time spend in one of the Ward/building",
                       choices = ward_namesChoices,
                       options = list(
                         placeholder = 'Select ward',
                         onInitialize = I('function() { this.setValue(""); }')
                       )),
        conditionalPanel(
          "input.selectedW != ''",
          numericInput(
            'ptimeinWard',
            '% of work time in the ward',
            value = 0,
            min = 0,
            step = 1
          ),
          actionButton("planningEdit", "Modify planning"))
      ),
      easyClose = TRUE
    ))
  })
}
