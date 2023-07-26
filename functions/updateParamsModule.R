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
if(input$disease_id %in% c("Covid", "Influenza")){
                         variable$disease = input$disease_id
}

  })




  }
