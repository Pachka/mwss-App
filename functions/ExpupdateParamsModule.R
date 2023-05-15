updateParamsUI <- function(id) {
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

updateParams  <- function(input, output, session){
  ns <- session$ns


  observeEvent(input$disease_id, {

    rdsfiles <- list.files("./data", pattern = "\\.rds$")

    if(paste0(input$disease_id, ".rds") %in% rdsfiles){
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
                     if(isTRUE(input$confirmdiseasechange)){
                       gdata <- readRDS(paste0("./data/", input$disease_id, ".rds"))

                       # update sliders input values
                       for(slidersInput in c("psympNI", "psympLI", "psympHI", "psevNI", "psevLI", "psevHI",
                                             # "pIC",
                                             "pdieIC",
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
                                         # "tIC",
                                         "tE", "tEA", "tES", "tIA", "tIM", "tIS", "tLI","tHI",
                                         # "rsymp", "rsev",
                                         # Ratio adjusting the excretion rates based on epidemiological stage
                                         "rEA","rES", "rIA", "rIM","rIS",
                                         "sensAg", "speAg", "sensPCR", "spePCR"))
                         updateNumericInput(session = .subset2(session, "parent"),
                                            numInput,
                                            value = gdata[[numInput]])

                       # update sick leave duration input values
                       # updateNumericInput(session = .subset2(session, "parent"),
                       #                    "tSLs",
                       #                    value = c(gdata[["tSL"]],gdata[["tESL"]]))

                       # updateSliderInput(session = .subset2(session, "parent"),
                       #                   "pSL",
                       #                   value = gdata$pSL)

                     }
                   }, ignoreNULL = FALSE)

    } else {
      if(input$disease_id != ""){
        # Show a simple modal
        shinyalert(title = paste("Epidemiological parameters for this disease haven't been integrated yet.
                                 Set values by yourself or choose another pathogen."),
                   type = "info",
                   size = "l")
      }
    }


  })


}
