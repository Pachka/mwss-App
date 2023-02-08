updateParamsUI_simp <- function(id) {
  ns <- NS(id)
  selectizeInput(ns("disease_id"), "Pathogen",
                 choices = setNames(
                   list("Covid", "Influenza"),
                   list("SARS-CoV-2", "Influenza")
                   ),
                 selected = NULL,
                 options = list(
                   placeholder = 'Select disease',
                   onInitialize = I('function() { this.setValue(""); }')
                 )
  )
}

updateParams_simp  <- function(input, output, session, variable){
  ns <- session$ns

  observeEvent(input$disease_id, {

      # ask for confirmation
      ask_confirmation(
        inputId = "confirmdiseasechange",
        title = "Want to confirm ?",
        type = "warning",
        btn_labels = c("Cancel", "Confirm"),
        text = "Note that choosing another disease will erase the current set of parameters."
      )

      observeEvent(eventExpr = input$confirmdiseasechange,
                   handlerExpr = {
                     if (isTRUE(input$confirmdiseasechange)) {

                         # structure
                         variable$gdata = build_gdata()

                     }}, ignoreNULL = FALSE)
  })

  }
