setIMMstateUI <-
  function(id,
           population) {

    ns <- NS(id)

    radiobuttonChoices <- c(
      "Use the national levels of immunity for the simulated variant" = "NL",
      "Modify levels of immunity globally" = "ML",
      "I"
    )
    names(radiobuttonChoices)[3] <-
      paste("Fix the immunity levels of",
            tolower(population),
            "in each ward")

    tagList(
      radioButtons(ns('immuStat'),
                   population,
                   choices = radiobuttonChoices,
                   character(0)),
      conditionalPanel(
        paste0('input[\'', ns('immuStat'), "\'] == \'NL\'"),
        uiOutput(ns("valuebox_pNI_NL")),
        uiOutput(ns("valuebox_pLI_NL")),
        uiOutput(ns("valuebox_pHI_NL")),
        fluidRow(
          actionButton(inputId = ns("setImmState_NL"),
                       label = "Apply",
                       icon = icon("play")),
          br()
        )
      ),
      conditionalPanel(
        paste0('input[\'', ns('immuStat'), "\'] == \'ML\'"),
        div(
          style = "display: inline-block;vertical-align:top;",
          knobInput(
            inputId = ns("pHI"),
            label = "Recent infection/vaccination history (high immunity)",
            value = 0,
            post = "%",
            displayPrevious = TRUE,
            lineCap = "round",
            fgColor = "#D9FF66",
            inputColor = "#C6FF1A"
          )
        ),
        div(
          style = "display: inline-block;vertical-align:top;",
          knobInput(
            inputId = ns("pLI"),
            label = "Old infection/vaccination history (low immunity)",
            value = 0,
            post = "%",
            displayPrevious = TRUE,
            lineCap = "round",
            fgColor = "#FFD966",
            inputColor = "#FFC61A"
          )
        ),
        div(
          style = "display: inline-block;vertical-align:top;",
          knobInput(
            inputId = ns("pNI"),
            label = "No infection/vaccination history",
            value = 0,
            post = "%",
            displayPrevious = TRUE,
            lineCap = "round",
            fgColor = "#FF6666",
            inputColor = "#FF1A1A",
            readOnly = T
          )
        ),
        fluidRow(
          actionButton(inputId = ns("setImmState_Faclev"),
                       label = "Apply",
                       icon = icon("play")),
          br()
        )
      ),
      conditionalPanel(
        paste0('input[\'', ns('immuStat'), "\'] == \'I\'"),
        uiOutput(ns("Wids_lookup")),
        conditionalPanel(
          paste0('input[\'', ns('wardImmEdit'), "\'] != \'\'"),
          fluidRow(
          div(
            style = "display: inline-block;vertical-align:top;",
            knobInput(
              inputId = ns("pHI_W"),
              label = "Recent infection/vaccination history (high immunity)",
              value = 0,
              post = "%",
              displayPrevious = TRUE,
              lineCap = "round",
              fgColor = "#D9FF66",
              inputColor = "#C6FF1A"
            )
          ),
          div(
            style = "display: inline-block;vertical-align:top;",
            knobInput(
              inputId = ns("pLI_W"),
              label = "Old infection/vaccination history (low immunity)",
              value = 0,
              post = "%",
              displayPrevious = TRUE,
              lineCap = "round",
              fgColor = "#FFD966",
              inputColor = "#FFC61A"
            )
          ),
          div(
            style = "display: inline-block;vertical-align:top;",
            knobInput(
              inputId = ns("pNI_W"),
              label = "No infection/vaccination history",
              value = 0,
              post = "%",
              displayPrevious = TRUE,
              lineCap = "round",
              fgColor = "#FF6666",
              inputColor = "#FF1A1A",
              readOnly = T
            )
          )),
          fluidRow(
          actionButton(inputId = ns("setImmState_Wlev"),
                       label = "Apply",
                       icon = icon("play")),
          br()
          )
        )
      )
    )

  }

setIMMstate  <-
  function(input,
           output,
           session,
           variable,
           pop,
           pLI_NL,
           pHI_NL
           ) {

    ns <- session$ns

    #### Set national proportions for all wards

    output$valuebox_pNI_NL <- renderUI({

    if(pop == "P")
      title = "of patients with no infection/vaccination history"
      if(pop == "H")
        title = "of professionals with no infection/vaccination history"

      valueBox(
        paste((1 - pHI_NL() - pLI_NL())*100, "%"),
        title,
        icon = NULL,
        color = "red",
        width = 4,
        href = NULL
      )

    })

    output$valuebox_pLI_NL <- renderUI({

      if(pop == "P")
        title = "of patients with old infection/vaccination history"
      if(pop == "H")
        title = "of professionals with old infection/vaccination history"

      valueBox(
        paste(pLI_NL()*100, "%"),
        title,
        icon = NULL,
        color = "orange",
        width = 4,
        href = NULL
      )

    })
    output$valuebox_pHI_NL <- renderUI({

      if(pop == "P")
        title = "of patients with recent infection/vaccination history"
      if(pop == "H")
        title = "of professionals with recent infection/vaccination history"

      valueBox(
        paste(pHI_NL()*100, "%"),
        title,
        icon = NULL,
        color = "green",
        width = 4,
        href = NULL
      )

    })

    observeEvent(input$setImmState_NL, {
      nW <- length(variable$ward_names)

      IMMstate <- lapply(seq(nW), function(W){
          if(pop == "P")
            popsize <- variable$pop_size_P[W]
          if(pop == "H")
            popsize <- variable$pop_size_H[W]

          IMMstate_W <- sample(c("NI", "LI", "HI"),
                               popsize,
                               prob = c(1 - pLI_NL() - pHI_NL(),
                                        pLI_NL(),
                                        pHI_NL()),
                               replace = T) %>% table %>% data.frame
          setnames(IMMstate_W, ".", "imm")
          setnames(IMMstate_W, "Freq", "n")

          if(!identical(IMMstate_W$imm, c("NI", "LI", "HI"))){
            missingIMM <- which(!c("NI", "LI", "HI") %in% IMMstate_W$imm)
            IMMstate_W <- rbind(IMMstate_W, expand.grid(imm = c("NI", "LI", "HI")[missingIMM], n = 0))
          }


          IMMstate_W$ward <- variable$ward_names[W]
          IMMstate_W$pop <- pop

          IMMstate_W

    }) %>% do.call(rbind, .)

      variable$IMMstate  %<>% .[which(.$pop != pop),] %>% rbind(., IMMstate)

    })

    #### Set unique proportions for all wards

    observeEvent(input$pLI, {
      updateKnobInput(session,
                      "pNI",
                      value = 100  - input$pLI - input$pHI)

      if ((input$pLI + input$pHI) >= 100) {
        updateKnobInput(session,
                        "pHI",
                        value = 100 - input$pLI)
      }

    })

    observeEvent(input$pHI, {
      updateKnobInput(session,
                      "pNI",
                      value = 100 - input$pLI - input$pHI)

      if ((input$pLI + input$pHI) >= 100){
        updateKnobInput(session,
                        "pLI",
                        value = 100 - input$pHI)
      }
    })


    #### Update IMMstate

    observeEvent(input$setImmState_Faclev, {
      nW <- length(variable$ward_names)

      IMMstate <- lapply(seq(nW), function(W){

        if(pop == "P")
          popsize <- variable$pop_size_P[W]
        if(pop == "H")
          popsize <- variable$pop_size_H[W]

        IMMstate_W <- sample(c("NI", "LI", "HI"),
                             popsize,
                             prob = c(input$pNI/100,
                                      input$pLI/100,
                                      input$pHI/100),
                             replace = T) %>% table %>% data.frame
        setnames(IMMstate_W, ".", "imm")
        setnames(IMMstate_W, "Freq", "n")

        if(!identical(IMMstate_W$imm, c("NI", "LI", "HI"))){
          missingIMM <- which(!c("NI", "LI", "HI") %in% IMMstate_W$imm)
          IMMstate_W <- rbind(IMMstate_W, expand.grid(imm = c("NI", "LI", "HI")[missingIMM], n = 0))
        }

        IMMstate_W$ward <- variable$ward_names[W]
        IMMstate_W$pop <- pop

        IMMstate_W

      }) %>% do.call(rbind, .)

      variable$IMMstate  %<>% .[which(.$pop != pop),] %>% rbind(., IMMstate)

    })

    #### Set proportions for specific wards

    # select the ward

    output$Wids_lookup <- renderUI({

      ward_namesChoices = variable$ward_names

      selectizeInput(
        ns("wardImmEdit"),
        "Select a ward",
        choices = ward_namesChoices,
        options = list(
          placeholder = 'Select ward',
          onInitialize = I('function() { this.setValue(""); }')
        )
      )

    })

    # select proportions

    observeEvent(input$pLI_W, {
      updateKnobInput(session,
                      "pNI_W",
                      value = 100  - input$pLI_W - input$pHI_W)

      if ((input$pLI_W + input$pHI_W) >= 100) {
        updateKnobInput(session,
                        "pHI_W",
                        value = 100 - input$pLI_W)
      }

    })

    observeEvent(input$pHI_W, {
      updateKnobInput(session,
                      "pNI_W",
                      value = 100 - input$pLI_W - input$pHI_W)

      if ((input$pLI_W + input$pHI_W) >= 100) {
        updateKnobInput(session,
                        "pLI_W",
                        value = 100 - input$pHI_W)
      }
    })

    #### Set national proportions for all wards

    observeEvent(input$setImmState_Wlev, {
      W <- which(variable$ward_names == input$wardImmEdit)

        if(pop == "P")
          popsize <- variable$pop_size_P[W]

        if(pop == "H")
          popsize <- variable$pop_size_H[W]

        IMMstate <- sample(c("NI", "LI", "HI"),
                             popsize,
                             prob = c(input$pNI_W/100,
                                      input$pLI_W/100,
                                      input$pHI_W/100),
                             replace = T) %>% table %>% data.frame
        setnames(IMMstate, ".", "imm")
        setnames(IMMstate, "Freq", "n")

        if(!identical(IMMstate$imm, c("NI", "LI", "HI"))){
          missingIMM <- which(!c("NI", "LI", "HI") %in% IMMstate$imm)
          IMMstate <- rbind(IMMstate, expand.grid(imm = c("NI", "LI", "HI")[missingIMM], n = 0))
        }

        IMMstate$ward <- variable$ward_names[W]
        IMMstate$pop <- pop

        variable$IMMstate %<>% .[which(.$pop != pop | .$ward != variable$ward_names[W]),] %>% rbind(., IMMstate)

    })

  }
