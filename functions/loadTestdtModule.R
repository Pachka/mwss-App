loadTestdtUI <- function(id) {
  ns <- NS(id)

  tagList(
    fluidRow(
      box(
        selectInput(
          ns("structure"), "Structure:",
          c("Network 1" = "1",
            "Network 2" = "2")),
        actionButton(
          ns("testds"),
          "Load example dataset",
          icon = icon("download",
                      verify_fa = FALSE),
          style = "color: #fff; background-color: #063567; border-color: #2e6da4"
        )
      )
    )
  )
}

loadTestdt <- function(input, output, session, variable) {
  ns <- session$ns

  observeEvent(input$testds, {
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

                 })

  }
