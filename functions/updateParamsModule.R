updateParamsUI <- function(id) {
  ns <- NS(id)
  selectizeInput(ns("variant_id"), "SARS-CoV-2 variants",
                 choices = setNames(
                   list("A", "B", "G", "D", "E", "O1", "O2"),
                   list("Alpha", "Beta", "Gamma", "Delta", "Epsilon", "Omicron (BA.1)", "Omicron (BA.2)")),
                 selected = "Omicron (BA.1)",
                 options = list(
                   placeholder = 'Select variant',
                   onInitialize = I('function() { this.setValue(""); }')
                 )
  )
}

updateParams  <- function(input, output, session){
  ns <- session$ns

  observeEvent(input$variant_id, {

    rdsfiles <- list.files("./data", pattern = "\\.rds$")

    if(paste0(input$variant_id, ".rds") %in% rdsfiles){
      # ask for confirmation
        ask_confirmation(
          inputId = "confirmVariantchange",
          title = "Want to confirm ?",
          type = "warning",
          btn_labels = c("Cancel", "Confirm"),
          text = "Note that choosing another variant will erase the current set of parameters."
        )

      observeEvent(eventExpr = input$confirmVariantchange,
                   handlerExpr = {
                     if(isTRUE(input$confirmVariantchange)){
                       gdata <- readRDS(paste0("./data/", input$variant_id, ".rds"))

                       # update sliders input values
                       for(slidersInput in c("psympNI", "psympLI", "psympHI", "psevNI", "psevLI", "psevHI",
                                             "pIC", "pdieIC",
                                             "hNI2LI", "hLI2HI",
                                             "rinfLI", "rinfHI"))
                         updateSliderInput(session = .subset2(session, "parent"),
                                           slidersInput,
                                           value = gdata[[slidersInput]])

                       for(slidersInput in c("pLI", "pHI"))
                         updateSliderInput(session = .subset2(session, "parent"),
                                           paste0(slidersInput, "_NL"),
                                           value = gdata[[slidersInput]])

                       # update times input values
                       # for(timesInput in c( ))
                       #   updateSliderInput(session = .subset2(session, "parent"),
                       #                     timesInput,
                       #                     value = gdata[[timesInput]])

                       # update numeric input values
                       for(numInput in c("I", "d", "R0",
                                         "tIC",
                                         "tE", "tEA", "tES", "tIA", "tIM", "tIS", "tLI","tHI",
                                         "rsymp", "rsev",
                                         # Ratio adjusting the excretion rates based on epidemiological stage
                                         "rEA","rES", "rIA", "rIM","rIS", 
                                         "sensAg", "speAg", "sensPCR", "spePCR"))
                         updateNumericInput(session = .subset2(session, "parent"),
                                            numInput,
                                            value = gdata[[numInput]])

                       # update sick leave duration input values
                       updateNumericInput(session = .subset2(session, "parent"),
                                          "tSLs",
                                          value = c(gdata[["tSL"]],gdata[["tESL"]]))

                       updateSliderInput(session = .subset2(session, "parent"),
                                         "pSL",
                                         value = gdata$pSL)

                     }
                   }, ignoreNULL = FALSE)

    } else {
      if(input$variant_id != ""){
        # Show a simple modal
        shinyalert(title = paste("Epidemiological parameters for this variant haven't been integrated yet.
                                 Set values by yourself or choose another variant."),
                   type = "info",
                   size = "l")
      }
    }
  })

}
