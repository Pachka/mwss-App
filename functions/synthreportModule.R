synthreportUI <- function(id) {
  ns <- NS(id)

  tagList(
    downloadButton(ns("downloadReport"), "Generate report"))
}

synthreport <- function(input, output, session, model, variable, n_days, n_sim, CSprotocols) {
  # model()
  ns <- session$ns

  ####### Generating downloadable reports
  output$downloadReport <- downloadHandler(

    filename = function() {
      paste("MWSS_report-", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {

      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)

      # Set up parameters to pass to Rmd document
        params <- list(control_traj = model(),
                       data = variable(),
                       n_days = n_days(),
                       n_sim = n_sim(),
                       CSprotocols = CSprotocols())

      # save(params, file = "paramsReport.rda")

      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport,
                        output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )

    }
