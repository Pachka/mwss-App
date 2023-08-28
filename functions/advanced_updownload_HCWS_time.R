

observeEvent(input$upload_HCWS_time_button, {
  showModal(modalDialog(
    title = "Upload data",
    tagList(
      fileInput("upload_HCWS_time",
                "",
                # list(icon("upload"),"Import your file")),
                accept=c('.xlsx', '.xls', '.csv', '.txt'),
                multiple = F)
    )
  )
  )
})

observeEvent(input$upload_HCWS_time,{

  file <- input$upload_HCWS_time
  ext <- tools::file_ext(file$datapath)
  req(file)

  if(is.null(file))
    return(NULL)

  file_contents <- read.table(file$datapath,
                              sep = ";",
                              dec = ".",
                              quote = "",
                              header=TRUE,
                              colClasses = c("character", rep("numeric", length(data_advanced$ward_names)))
  )

  required_columns <- c("professionals", data_advanced$ward_names)
  column_names <- colnames(file_contents)

  # print(file_contents)

  shiny::validate(
    need(ext %in% c('xls', 'csv', 'txt', 'xlsx'), message = "Incorrect file type"),
    need(required_columns %in% column_names, message = paste(paste(required_columns[!(required_columns %in% column_names)], "column is missing"), collapse = "\n "))
  )

  data_advanced$Hplanning <- file_contents

})


output$download_Hplanning <- downloadHandler(

  filename = function(){"data_template.csv"},
  content = function(fname){

    template <- data_advanced$Hplanning
    setDT(template)

    setcolorder(template, c("professionals", data_advanced$ward_names))
    write.table(template, fname, sep = ";", row.names = F, quote = FALSE)

  })

