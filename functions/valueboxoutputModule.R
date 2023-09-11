valueboxoutputUI <- function(id) {
  ns <- NS(id)

  tagList(
    splitLayout(
      valueBoxOutput(ns("nosoH"), width = NULL),
      valueBoxOutput(ns("nosoP"), width = NULL),
      valueBoxOutput(ns("nSev"), width = NULL)
    ),
    splitLayout(
      valueBoxOutput(ns("ntestP"), width = NULL),
      valueBoxOutput(ns("ntestH"), width = NULL),
      valueBoxOutput(ns("ISO"), width = NULL),
      valueBoxOutput(ns("SL"), width = NULL)
    )
  )
}

valueboxoutput <- function(input, output, session, model) {

  output$nosoH <- renderValueBox({
    valueBox(
      ifelse("mwss" %in% class(model()[["trajmwss"]]),
             paste (keyoutput(model()[["trajmwss"]],
                             scale = 0,
                             focus = "infections")$H$quantiles_noso[["50%"]] %>% ceiling," (",
                   keyoutput(model()[["trajmwss"]],
                             scale = 0,
                             focus = "infections")$H$quantiles_out[["50%"]] %>% ceiling
                   ,")"),
             ""),
      HTML("Number of nosocomial (imported)  <br/> infections among professionals"),
      icon = icon("user-md", verify_fa = FALSE),
      color = "red",
      width = NULL
    )
  })

  output$nosoP <- renderValueBox({
    valueBox(
      ifelse("mwss" %in% class(model()[["trajmwss"]]),
             paste (keyoutput(model()[["trajmwss"]],
                     scale = 0,
                     focus = "infections")$P$quantiles_noso[["50%"]] %>% ceiling," (",
                   keyoutput(model()[["trajmwss"]],
                             scale = 0,
                             focus = "infections")$P$quantiles_intro[["50%"]] %>% ceiling
                   ,")"),
             ""),
      HTML("Number of nosocomial (imported)  <br/> infections among patients"),
      icon = icon("bed",
                  verify_fa = FALSE),
      color = "red",
      width = NULL
    )
  })

  # number of severe cases
  output$nSev <- renderValueBox({
    valueBox(
      ifelse(
        "mwss" %in% class(model()[["trajmwss"]]),
        keyoutput(model()[["trajmwss"]],
                scale = 0,
                focus = "incidence")$incidence[, incPS] %>%  median %>% ceiling,
        ""
      ),
      HTML("Number of severe cases <br/>among patients"),
      icon = icon("fire",
                  verify_fa = FALSE),
      color = "red",
      width = NULL
    )
  })

  # number of test
  output$ntestP <- renderValueBox({
    valueBox(
      ifelse(
        "mwss" %in% class(model()[["trajmwss"]]),
          keyoutput(model()[["trajmwss"]],
                scale = 0,
                focus = "test")$quantilesP[["50%"]] %>% ceiling,
        ""
      ),
      HTML("Number of tests of patients <br/> "),
      icon = icon("exclamation-triangle",
                  verify_fa = FALSE),
      color = "yellow",
      width = NULL
    )
  })

  output$ntestH <- renderValueBox({
    valueBox(
      ifelse(
        "mwss" %in% class(model()[["trajmwss"]]),
        keyoutput(model()[["trajmwss"]],
                scale = 0,
                focus = "test")$quantilesH[["50%"]] %>% ceiling,
        ""
      ),
      HTML("Number of tests of professionals <br/> "),
      icon = icon("exclamation-triangle",
                  verify_fa = FALSE),
      color = "yellow",
      width = NULL
    )
  })


  # number of severe cases
  output$ISO <- renderValueBox({

    valueBox(
      ifelse(
        "mwss" %in% class(model()[["trajmwss"]]),
        ifelse("ISO" %in% class(model()[["trajmwss"]]),
               keyoutput(model()[["trajmwss"]],
                         scale = 0)$ISO$quantiles[["50%"]] %>% ceiling, 0),
        ""
      ),
      HTML(
        "Maximal number of beds <br/>simultaneously under confinement"
      ),
      icon = icon("bed",
                  verify_fa = FALSE),
      color = "green",
      width = NULL
    )
  })

  # number of severe cases
  output$SL <- renderValueBox({
    valueBox(
      ifelse(
        "mwss" %in% class(model()[["trajmwss"]]),
        keyoutput(model()[["trajmwss"]],
                scale = 0)$SL$quantiles[["50%"]] %>% ceiling,
        ""
      ),
      HTML(
        "Maximal number of professionals <br/>simultaneously in sick leave"
      ),
      icon = icon("user-md", verify_fa = FALSE),
      color = "green",
      width = NULL
    )

  })

}
