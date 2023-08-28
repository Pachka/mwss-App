
observeEvent(input$upload_general_button, {
  showModal(modalDialog(
    title = "Upload data",
    tagList(
      fileInput("upload_general",
                "",
                # list(icon("upload"),"Import your file")),
                accept=c('.xlsx', '.xls', '.csv', '.txt'),
                multiple = F)
    )
  )
  )
})

observeEvent(input$upload_general,{

  file <- input$upload_general
  ext <- tools::file_ext(file$datapath)
  req(file)

  if(is.null(file))
    return(NULL)

  file_contents <- read.table(file$datapath,
                              sep = ";",
                              dec = ".",
                              quote = "",
                              header=TRUE,
                              colClasses = c("character", "integer", "integer", "integer", 'numeric')
  )

  required_columns <- c("ward_names", "pop_size_P", "pop_size_H", "nVisits", "LS")
  column_names <- colnames(file_contents)

  # print(file_contents)

  shiny::validate(
    need(ext %in% c('xls', 'csv', 'txt', 'xlsx'), message = "Incorrect file type"),
    need(required_columns %in% column_names, message = paste(paste(required_columns[!(required_columns %in% column_names)], "column is missing"), collapse = "\n "))
  )

  data_advanced$ward_names <- file_contents$ward_names %>% as.character()
  data_advanced$pop_size_P <- file_contents$pop_size_P %>% as.numeric()
  data_advanced$pop_size_H <- file_contents$pop_size_H %>% as.numeric()
  data_advanced$nVisits <- file_contents$nVisits %>% as.numeric()
  data_advanced$LS <- file_contents$LS %>% as.numeric()

  Hplanning <- matrix(0,
                      nrow = sum(file_contents$pop_size_H),
                      ncol = length(file_contents$ward_names),
                      dimnames = list(
                                      sapply(seq(length(data_advanced$ward_names)), function(x){
                                        paste0("HCWS_",
                                               seq(data_advanced$pop_size_H[x]),
                                               "_",
                                               data_advanced$ward_names[x])
                                      }) %>%
                                        unlist,
                                      file_contents$ward_names
                      )
  ) %>% as.data.frame


  for(i in seq(length(data_advanced$ward_names))){
    if(i == 1)
      Hplanning[1 : data_advanced$pop_size_H[i], i] <- 100 else
    Hplanning[(1 + sum(data_advanced$pop_size_H[1 : (i-1)])) : (data_advanced$pop_size_H[i] + sum(data_advanced$pop_size_H[1 : (i-1)])), i] <- 100
  }

  Hplanning$professionals <- sapply(seq(length(data_advanced$ward_names)), function(x){
    paste0("HCWS_",
           seq(data_advanced$pop_size_H[x]),
           "_",
           data_advanced$ward_names[x])
  }) %>%
    unlist

  data_advanced$Hplanning <- Hplanning

  })

output$download_general <- downloadHandler(

  filename = function(){"data_template.csv"},
  content = function(fname){
    if(length(data_advanced$ward_names) > 0){
      template <- data.frame(
        ward_names = data_advanced$ward_names,
        pop_size_P = data_advanced$pop_size_P,
        pop_size_H = data_advanced$pop_size_H,
        nVisits = data_advanced$nVisits,
        LS = data_advanced$LS)
    }
    else
      template <- data.frame(
        ward_names = c("NAME_1", "NAME_2"),
        pop_size_P = runif(2,1,30) %>% round,
        pop_size_H = runif(2,1,15) %>% round,
        nVisits = runif(2,1,30) %>% round,
        LS = runif(2,1,7) %>% round)

    write.table(template, fname, sep = ";", row.names = F, quote = FALSE)

  })

