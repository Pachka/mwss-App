downloadParamsUI <- function(id) {
  ns <- NS(id)

  tagList(h5("", style = "margin: 15px 5px 5px 5px; "),
          downloadButton(ns("params"), "Save",
                         style = "color: #fff; background-color: #27ae60; border-color: #fff; padding: 5px 5px 5px 5px;margin: 10px 5px 5px 5px; "),
  )

}

downloadParams  <- function(input, output, session, gdata){
  ns <- session$ns

  output$params <- downloadHandler(

    filename = function() {
      paste("params_", Sys.Date(), ".rda", sep="")
    },
    content = function(filename) {

      if(endsWith(filename, ".rda") | endsWith(filename, ".Rda"))
        save(gdata, file = filename) else
          save(gdata, file = paste0(filename, ".rda"))
    }
  )


}
