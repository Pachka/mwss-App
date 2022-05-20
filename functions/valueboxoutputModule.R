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
      ifelse("mwss" %in% class(model()),
             keyoutput(model(),
                     scale = 0,
                     focus = "infections")$H$quantiles_noso[["50%"]],
             ""),
      HTML("Number of nosocomial  <br/> infection among professionals"),
      icon = icon("user-md"),
      color = "red",
      width = NULL
    )
  })

  output$nosoP <- renderValueBox({
    valueBox(
      ifelse("mwss" %in% class(model()),
             keyoutput(model(),
                     scale = 0,
                     focus = "infections")$P$quantiles_noso[["50%"]],
             ""),
      HTML("Number of nosocomial  <br/> infection among patients"),
      icon = icon("bed"),
      color = "red",
      width = NULL
    )
  })

  # number of severe cases
  output$nSev <- renderValueBox({
    valueBox(
      ifelse(
        "mwss" %in% class(model()),
        keyoutput(model(),
                scale = 0,
                focus = "incidence")$incidence[, incPS] %>%  median,
        ""
      ),
      HTML("Number of severe cases <br/>among patients"),
      icon = icon("fire"),
      color = "red",
      width = NULL
    )
  })

  # number of test FIX ME split patient / HCWS
  output$ntestP <- renderValueBox({
    valueBox(
      ifelse(
        "mwss" %in% class(model()),
          keyoutput(model(),
                scale = 0,
                focus = "test")$quantilesP[["50%"]],
        ""
      ),
      HTML("Number of tests of patients <br/> "),
      icon = icon("exclamation-triangle"),
      color = "yellow",
      width = NULL
    )
  })

  output$ntestH <- renderValueBox({
    valueBox(
      ifelse(
        "mwss" %in% class(model()),
        keyoutput(model(),
                scale = 0,
                focus = "test")$quantilesH[["50%"]],
        ""
      ),
      HTML("Number of tests of professionals <br/> "),
      icon = icon("exclamation-triangle"),
      color = "yellow",
      width = NULL
    )
  })


  # number of severe cases
  output$ISO <- renderValueBox({

    valueBox(
      ifelse(
        "mwss" %in% class(model()),
        ifelse("ISO" %in% class(model()),
               keyoutput(model(),
                         scale = 0)$ISO$quantiles[["50%"]], 0),
        ""
      ),
      HTML(
        "Maximal number of beds <br/>simulataneously under confinement"
      ),
      # "Maximal number of beds simulataneously under confinement",
      icon = icon("bed"),
      color = "green",
      width = NULL
    )
  })

  # number of severe cases
  output$SL <- renderValueBox({
    valueBox(
      ifelse(
        "mwss" %in% class(model()),
        keyoutput(model(),
                scale = 0)$SL$quantiles[["50%"]],
        ""
      ),
      HTML(
        "Maximal number of professionals <br/>simulataneously in sick leave"
      ),
      icon = icon("user-md"),
      color = "green",
      width = NULL
    )

  })

}
