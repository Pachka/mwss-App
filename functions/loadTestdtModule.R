loadTestdtUI <- function(id) {
  ns <- NS(id)

  selectizeInput(ns("structure"), "Structure",
                 choices = setNames(
                   list("1", "2"),
                   list("Network 1", "Network 2")
                 ),
        selected = NULL,
        options = list(
          placeholder = 'Select hospital structure',
          onInitialize = I('function() { this.setValue(""); }')
        )
      )
}

loadTestdt <- function(input, output, session, variable) {
  ns <- session$ns

  observeEvent(input$structure, {
    if(input$structure %in% c("1", "2")){
    # ask for confirmation
    ask_confirmation(
      inputId = "confirmuploadtestdt",
      title = "Want to confirm ?",
      type = "warning",
      btn_labels = c("Cancel", "Confirm"),
      text = "Note that loading the test dataset will erase the current structure."
    )

    observeEvent(eventExpr = input$confirmuploadtestdt,
                 handlerExpr = {
                   if (isTRUE(input$confirmuploadtestdt)) {
                     load(paste0("./data/structure",input$structure,".rda"))

                     if (exists("saveInputs")) {
                       # structure
                       variable$ward_names = saveInputs$ward_names
                       variable$pop_size_P = saveInputs$pop_size_P
                       variable$pop_size_H = saveInputs$pop_size_H
                       variable$nVisits = saveInputs$nVisits
                       variable$LS = saveInputs$LS
                       # Contacts
                       variable$Hplanning = saveInputs$Hplanning
                       variable$matContact = saveInputs$matContact
                       # Immunity
                       variable$IMMstate = saveInputs$IMMstate
                       # Epidemiological states // infections
                       variable$EPIstate = saveInputs$EPIstate
                     }
                   }}, ignoreNULL = FALSE)
}
                 })

  }
