exporttrajUI <- function(id) {
  ns <- NS(id)

  tagList(
    downloadButton(ns("downloadoutput"), "Export data in .Rda (R readable file)")
    )
}

# Export simulated trajectories at a Rda format
exporttraj <- function(input, output, session, model) {
  # model()
  ns <- session$ns

  ####### Generating downloadable reports
  output$downloadoutput <- downloadHandler(

    filename = function() {
      paste("MWSS_ouput-", Sys.Date(), ".Rda", sep = "")
    },
    content = function(file) {

      trajmwss <- model()[["trajmwss"]]
      save(trajmwss, file = file)
    }
  )

}
