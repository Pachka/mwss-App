diseasetimelineUI <-
  function(id) {
    ns <- NS(id)

    tagList(fluidRow(box(
      width = 12,
      column(
        2,
        numericInput(
          ns('tE'),
          'Average duration of non contagious incubating period (days)',
          value = 5,
          min = 0,
          step = 0.5
        ),
        numericInput(
          ns('tEA'),
          'Average duration of contagious incubating period for future asymptomatic (days)',
          value = 2,
          min = 0,
          step = 0.5
        ),
        numericInput(
          ns('tIA'),
          'Average duration of infectious period when asymptomatic (days)',
          value = 7,
          min = 0,
          step = 0.5
        )
      ),
      column(
        2,
        numericInput(
          ns('tES'),
          'Average duration of contagious incubating period for future symptomatic (days)',
          value = 2,
          min = 0,
          step = 0.5
        ),
        numericInput(
          ns('tIM'),
          'Average duration of infectious period with mild symptoms (days)',
          value = 8,
          min = 0,
          step = 0.5
        ),
        numericInput(
          ns('tIS'),
          'Average duration of infectious period with severe symptoms (days)',
          value = 9,
          min = 0,
          step = 0.5
        )
      ),
      column(8,
             plotOutput(ns("disease_states")))
    )))

  }

diseasetimeline  <-
  function(input,
           output,
           session) {
    ns <- session$ns

    output$disease_states <- renderPlot({
      # create a dataset
      state <-
        c(rep("tE" , 3), rep("tEi" , 3) , rep("tI" , 3)) %>% factor(., levels = c("tI", "tEi", "tE"))

      condition <-
        rep(c("Asymptomatic" , "Mild symptoms" , "Severe symptoms") , 3) %>%
        factor(., levels = rev(c(
          "Asymptomatic" , "Mild symptoms" , "Severe symptoms"
        )))

      value <-
        c(rep(input$tE , 3) ,
          input$tEA,
          rep(input$tES , 2) ,
          input$tIA,
          input$tIM,
          input$tIS)

      data <- data.frame(state, condition, value)


      ggplot(data, aes(
        fill = as.factor(state),
        y = value,
        x = condition
      )) +
        geom_bar(position = "stack", stat = "identity") + coord_flip() +
        xlab("") + ylab("Duration (days)") +
        scale_fill_manual(
          values = c("#8b2312", "#E69F00", "#128b23"),
          name = "Epidemiological\nstate",
          breaks = c("tI", "tEi", "tE"),
          labels = c(
            "Infectious",
            "Incubating infectious",
            "Incubating non infectious"
          )
        )


    })

    return(list(
      tE = reactive({input$tE}),
      tEA = reactive({input$tEA}),
      tES = reactive({input$tES}),
      tIA = reactive({input$tIA}),
      tIM = reactive({input$tIM}),
      tIS = reactive({input$tIS})
    ))

  }
