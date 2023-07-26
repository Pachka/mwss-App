resetreactivesUI <- function(id) {
  ns <- NS(id)

  actionButton(ns("reset"), "Clear",
               style = "color: #fff; border-color: #fff; width:130; margin: 20px 5px 5px 5px;")

}

resetreactives <- function(input, output, session, variable) {
  observeEvent(input$reset, {
      # Wards structure
      variable$ward_names = character()
      variable$pop_size_P = numeric()
      variable$pop_size_H = numeric()
      variable$nVisits = numeric()
      variable$LS = numeric()
      # Contacts
      variable$Hplanning = NULL
      variable$matContact = NULL
      # Immunity
      variable$IMMstate = NULL
      # Epidemiological states // infections
      # variable$EPIstate = NULL
      variable$gdata = NULL
  })
}
