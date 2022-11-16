uploadParamsUI <- function(id) {
  ns <- NS(id)

  tagList(fileInput(ns("loadparams"), "Upload a pre-recorded set of parameters",
                    accept = c("rda", ".Rda")),
          conditionalPanel("output.paramsUploaded == true",
                           actionButton(inputId = ns("applyParamsLoad"),
                                        label = "Load new set of parameters",
                                        icon = icon("play",
                                                    verify_fa = FALSE),
                                        style = "color: #fff; background-color: red; border-color: #fff;width:130;padding: 5px 5px 5px 5px;margin: -20px 5px 5px 15px; ")
          )
          )

}

uploadParams  <- function(input, output, session, gdata){

  # conditional display
  output$paramsUploaded <- reactive({
    return(!is.null(input$loadparams))
  })

  outputOptions(output,
                'paramsUploaded',
                suspendWhenHidden = FALSE)

  # ask for confirmation
  observeEvent(input$applyParamsLoad, {
    ask_confirmation(
      inputId = "confirmationloadparams",
      title = "Want to confirm ?",
      type = "warning",
      btn_labels = c("Cancel", "Confirm"),
      text = "Note that loading a set of parameters will erase the current set of parameters"
    )
  })

  # ask for confirmation
  observeEvent(eventExpr = input$confirmationloadparams,
               handlerExpr = {
                 if(isTRUE(input$confirmationloadparams)){
                   req(input$loadparams)

                   load(input$loadparams$datapath)
                 }
               }, ignoreNULL = FALSE)

}
