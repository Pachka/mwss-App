updateImportationUI_simp <- function(id, variable) {
  ns <- NS(id)

  tagList(
  selectizeInput(ns("importation_lev"), "Level of importation",
                 choices = setNames(
                   list(0, 50/100000, 185/100000),
                   list("No importation (index case)",
                        "Low level importation",
                        "High level importation")
                   ),
                 selected = NULL,
                 options = list(
                   placeholder = 'Select scenario',
                   onInitialize = I('function() { this.setValue(""); }')
                 )
  ),
  conditionalPanel(condition = "input.importation_lev == '0'",
                   ns = ns,
                   selectInput(ns("index_c"),
                               "Index case in ward:",
                               build_network()$ward_names))
  )

}

updateImportation_simp  <- function(input, output, session, variable){
  ns <- session$ns

  observeEvent(list(input$importation_lev, input$index_c), {
    if(!is.na(as.numeric(input$importation_lev))){
      # structure
      variable$imp_lev <- as.numeric(input$importation_lev)

      if(input$importation_lev == "0" & !is.null(input$index_c))
        variable$EPIstate <- data.frame(
          ward = input$index_c,
          pop = c("P"),
          imm = c("NI"),
          epi = c("E"),
          n = c(1)
        )
    }
  })


}
