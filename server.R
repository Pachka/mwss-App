# Define server logic required to draw a histogram
server <- function(input, output, session) {
  ## Set helpers
  # uses 'helpfiles' directory by default
  # in this example, we use the withMathJax parameter to render formula
  observe_helpers(
    session = shiny::getDefaultReactiveDomain(),
    help_dir = "helpfiles",
    withMathJax = FALSE
  )

  ###############################
  ####  Tables initialization ###
  ###############################

  # make reactive to record wards
  data <- shiny::reactiveValues(
    # Wards structure
    ward_names = character(),
    pop_size_P = numeric(),
    pop_size_H = numeric(),
    nVisits = numeric(),
    LS = numeric(),
    # Contacts
    Hplanning = NULL,
    matContact = NULL,
    # Immunity
    IMMstate = NULL,
    # Epidemiological states // infections
    EPIstate = NULL,
    # gdata = NULL,
    disease = NULL,
    imp_lev = NULL
  )


  # make reactive to record wards
  data_advanced <- shiny::reactiveValues(
    # Wards structure
    ward_names = character(),
    pop_size_P = numeric(),
    pop_size_H = numeric(),
    nVisits = numeric(),
    LS = numeric(),
    # Contacts
    Hplanning = NULL,
    matContact = NULL,
    # Immunity
    IMMstate = NULL,
    # Epidemiological states // infections
    EPIstate = NULL,
    # gdata = NULL,
    disease = NULL,
    imp_lev = NULL
  )


  #####################################
  #######    Simulation panel    ######
  #####################################

  ## go to simulation tabs using buttons in the presentation page
  observeEvent(input$versionSimple, {

    updateTabsetPanel(session,
                      "sidebarMenu",
                      selected = "simulation_sidetab")

    updateTabsetPanel(session,
                      "tabsSIMULATIONS",
                      selected = "Simple mode")

  })

  observeEvent(input$tabsSIMULATIONS, {
    if(input$tabsSIMULATIONS == "Advanced mode")
    shinyalert(title = paste("Advanced mode is under development."),
               type = "info",
               size = "l")

  })


  observeEvent(input$versionAdv, {

    updateTabsetPanel(session,
                      "sidebarMenu",
                      selected = "simulation_sidetab")

    updateTabsetPanel(session,
                      "tabsSIMULATIONS",
                      selected = "Advanced mode")

  })

  observeEvent(input$versionExp, {
      shinyalert(title = paste("Expert mode is under development."),
                 type = "info",
                 size = "l")
  })

  ### ### ### ### ###
  ### SIMPLE MODE ###
  ### ### ### ### ###

  # load test data
  clustering <- callModule(module = loadTestdt,
                           id = "loadtest",
                           variable = data)

  # update contact matrix
  observe({
    if (!is.null(data$Hplanning) & length(data_advanced$ward_names) > 1){
      contacts <- data$Hplanning

      matContact <- lapply(data$ward_names, function(W) {
        W <- contacts$professionals %>% endsWith(., paste0("_", W)) %>% contacts[., ]
        W %<>% .[, data$ward_names] %>% colSums
        W %<>% divide_by(sum(.)) %>% multiply_by((100))
        W
      }) %>% do.call(rbind, .)

      rownames(matContact) <- colnames(matContact)
      data$matContact <- matContact
    }
  })

  # display control and surveillance options
  output$CSprotocolsUI <- renderUI({
    checkboxGroupInput(
      "CSprotocols",
      "Control and surveillance:",
      c(
        "Impose isolation/contact restrictions to detected patients?" = "ISO",
        "Implement random tests at regular intervals for patients?" = "testPat",
        "Implement random tests at regular intervals for professionals?" = "testProf",
        "Implement a test at patient admission?" = "SA"
      )
    )
  })

  # display control and surveillance options
  output$CSprotocolsUI_adv <- renderUI({
    checkboxGroupInput(
      "CSprotocols_adv",
      "Control and surveillance:",
      c(
        "Impose isolation/contact restrictions to detected patients?" = "ISO",
        "Implement random tests at regular intervals for patients?" = "testPat",
        "Implement random tests at regular intervals for professionals?" = "testProf",
        "Implement a test at patient admission?" = "SA"
      )
    )
  })

  ###
  # Display network -- FIX ME turn into module to be used with data and data_avdanced
  ###
  output$network_plot <- renderPlot({
    num_nodes <- data$ward_names %>% length

    if(num_nodes == 1) {
      my_sociomatrix <- matrix(rep(0, num_nodes * num_nodes),
                               # edge values
                               nrow = num_nodes,
                               #nrow must be same as ncol
                               ncol = num_nodes)

      diag(my_sociomatrix) <- 0

      net <- as.network(
        x = my_sociomatrix,
        # the network object
        directed = TRUE,
        # specify whether the network is directed
        loops = FALSE,
        # do we allow self ties (should not allow them)
        matrix.type = "adjacency" # the type of input
      )

      network.vertex.names(net) <- data$ward_names

      vertex_size <-
        as.numeric(data$pop_size_P) + as.numeric(data$pop_size_H)

      plot.network(
        net,
        # our network object
        vertex.col = "grey",
        # color nodes by gender
        vertex.cex = (vertex_size / max(vertex_size)) * 3,
        # size nodes by their age
        displaylabels = T,
        # show the node names
        label.pos = 0 # display the names directly over nodes
      )

    }

    if(num_nodes > 1) {
      plot_connectivity(
        matContact = data$matContact,
        size = as.numeric(data$pop_size_P) + as.numeric(data$pop_size_H),
        vertexcexrate = 3,
        vertexcol = c(rep("red",3),
                      rep("blue",4),
                      rep("white",5),
                      rep("yellow",8),
                      rep("orange",9)),
        verbose = FALSE
      )
    }

  })


  ### ### ### ### ### ###
  ###  ADVANCED MODE  ###
  ### ### ### ### ### ###


  ####
  # Condition based on non empty data
  ####

  output$atleastoneward_advanced <- reactive({
    return(length(data_advanced$ward_names) > 0)
  })

  outputOptions(output,
                'atleastoneward_advanced',
                suspendWhenHidden = FALSE)

  ###
  ### Body
  ###

  ############## Renders  ##############

  ###
  # Display structure of the facility
  ###

  output$structure_hospital <- renderDT({

    facilitystr <- data.frame(
      "Ward" = data_advanced$ward_names,
      "Patients" = data_advanced$pop_size_P,
      "Professionals" = data_advanced$pop_size_H,
      "Length of stay" = data_advanced$LS,
      "Daily visits" = data_advanced$nVisits,
      check.names = FALSE
    )

    DT::datatable(
      facilitystr,
      rownames = F,
      editable = FALSE,
      escape = FALSE
    )

  })

  ###
  # Display professionals time sharing
  ###

  output$contacts <- DT::renderDT({
    if (!is.null(data_advanced$Hplanning)) {
      TS <- data_advanced$Hplanning
      TS %>% setDT
      wardN <- data_advanced$ward_names
      TS$total <- TS[, ..wardN] %>% rowSums

      setcolorder(TS, c("professionals", wardN, "total"))

      datatable(TS,
                rownames = FALSE) %>% formatStyle('total',
                                                  backgroundColor = styleInterval(c(99.9, 100.1),
                                                                                  c('green', 'white', 'red')),
                                                  fontWeight = 'bold')
    }

  })

  ###
  # Display network statistics
  ###

  ############## Actions  ##############

  ###############################
  ####    Add wards           ###
  ###############################

  ###
  ### Use add Button
  ###

  ## Open remote window: when addward is activated (see body_ui)
  observeEvent(input$addW_button, {
    showModal(
      modalDialog(
        title = "Here you can add new wards or buildings",
        textInput("wardname", "Ward/building name", ""),
        tags$p(
          "Note: Each ward/building has collective areas and dedicated medical staff. \n
          Two wards cannot have the same name.  \n
          If you try to use the same name the 'Add ward' button will not appear."
        ),
        conditionalPanel(
          "input.wardname != ''",
          numericInput(
            'pop_size_P',
            'Number of beds',
            value = 1,
            min = 1,
            step = 1
          ),
          numericInput(
            'pop_size_H',
            'Number of healthcare workers',
            value = 1,
            min = 1,
            step = 1
          ),
          numericInput(
            'LS',
            'Average patient length of stay (days)',
            value = 1,
            min = 1,
            step = 1
          ),
          tags$p(
            "Note: Minimal length of stay is 1 day."
          ),
          numericInput(
            'nVisits',
            'Average number of visitors per day',
            value = 0,
            min = 0,
            step = 1
          ),
          tags$p(
            "Note: The daily number of visitors will be divided by the number of patients to obtain an average number of visit per day per patients."
          )
        ),
        uiOutput("addBut"),
        easyClose = TRUE
      )
    )
  })

  ## Conditional display of add button: If a new ward name is written in the dedicated field, the add button appears
  output$addBut <- renderUI({
    if (!(input$wardname %in% data_advanced$ward_names) & input$wardname != "") {
      actionButton("validateWadd", "Add ward/building")
    }
  })

  ## Add a ward
  # update reactive values
  observeEvent(input$validateWadd, {

      #####
      ##### Update structure data
      #####

      data_advanced$ward_names %<>% c(., input$wardname)
      data_advanced$pop_size_P %<>% c(., input$pop_size_P)
      data_advanced$pop_size_H %<>% c(., input$pop_size_H)
      data_advanced$nVisits %<>% c(., input$nVisits)
      data_advanced$LS %<>% c(., input$LS)

      #####
      ##### Update professionals plannings
      #####
      if (is.null(data_advanced$Hplanning)) {
        data_advanced$Hplanning <-
          data.table(professionals = paste0(
            "HCWS_",
            seq(input$pop_size_H),
            paste0("_", input$wardname)
          ),
          ward = 100)
        setnames(data_advanced$Hplanning, "ward", input$wardname)

      } else {
        newW <-
          data.table(professionals = paste0(
            "HCWS_",
            seq(input$pop_size_H),
            paste0("_", input$wardname)
          ),
          ward = 100)
        setnames(newW, "ward", input$wardname)

        data_advanced$Hplanning %<>% rbind(., newW, fill = TRUE)

        data_advanced$Hplanning[is.na(data_advanced$Hplanning)] <- 0
      }

  })

  ###############################
  ####    Edit wards         ####
  ###############################

  ###
  ### Use edit Button
  ###

  ## Open remote window: when editward is activated (see body_ui)
  # Show modal when button is clicked.
  observeEvent(input$editW_button, {

    ward_namesChoices <- data_advanced$ward_names

    showModal(modalDialog(
      selectizeInput("wardtoEdit", "Select a ward/building",
                     choices = ward_namesChoices,
                     options = list(
                       placeholder = 'Select ward',
                       onInitialize = I('function() { this.setValue(""); }')
                     )),
      conditionalPanel(
        "input.wardtoEdit != ''",
        numericInput(
          'pop_size_PNEW',
          'Number of beds',
          value = data_advanced$pop_size_P[which(data_advanced$ward_names == input$wardtoEdit)],
          min = 1,
          step = 1
        ),
        numericInput(
          'pop_size_HNEW',
          'Number of healthcare workers',
          value = 1,
          min = 1,
          step = 1
        ),
        h5("Changing the number of professionals will reset the plannings of all healthcare workers of the ward at 100% in the ward. If you already adjust the professional planning, consider adding/removing specific professionals within the professional planning panel."),
        numericInput(
          'LSNEW',
          'Average patient length of stay (days)',
          value = 1,
          min = 1,
          step = 1
        ),
        numericInput(
          'nVisitsNEW',
          'Average number of visits per day',
          value = 0,
          min = 0,
          step = 1
        ),
        actionButton("wardEditOk", "Modify ward/building")),
      title = "Here you can edit ward or building structure",
      easyClose = TRUE
    )
    )
  })

  # Get changed data from table on popup and store in dataframe
  observeEvent(input$wardtoEdit, {
    if (input$wardtoEdit != "") {
      selectedW <- which(data_advanced$ward_names == input$wardtoEdit)

      updateNumericInput(session, 'pop_size_PNEW', value = data_advanced$pop_size_P[selectedW])
      updateNumericInput(session, 'pop_size_HNEW', value = data_advanced$pop_size_H[selectedW])
      updateNumericInput(session, 'LSNEW', value = data_advanced$LS[selectedW])
      updateNumericInput(session, 'nVisitsNEW', value = data_advanced$nVisits[selectedW])
    }

  })

  ## Validate edits
  # Update reactives
  observeEvent(input$wardEditOk, {
    isolate({
      selectedW <- which(data_advanced$ward_names == input$wardtoEdit)

      matContneedsUpdate <-
        data_advanced$pop_size_H[selectedW] != input$pop_size_HNEW
      #####
      ##### Update structure data
      #####

      data_advanced$pop_size_P[selectedW] <- input$pop_size_PNEW
      data_advanced$pop_size_H[selectedW] <- input$pop_size_HNEW
      data_advanced$LS[selectedW] <- input$LSNEW
      data_advanced$nVisits[selectedW] <- input$nVisitsNEW

      #####
      ##### Update professionals planning
      #####

      if (matContneedsUpdatematContneedsUpdate) {

        data_advanced$Hplanning <-
          data_advanced$Hplanning[!endsWith(data_advanced$Hplanning$professionals,
                                   paste0("_", input$wardtoEdit)), ]

        newW <-
          data.table(professionals = paste0(
            "HCWS_",
            seq(input$pop_size_HNEW),
            paste0("_", input$wardtoEdit)
          ),
          ward = 100)
        setnames(newW, "ward", input$wardtoEdit)

        data_advanced$Hplanning %<>% rbind(., newW, fill = TRUE)

        data_advanced$Hplanning[is.na(data_advanced$Hplanning)] <- 0

      }

    })
  })

  ###############################
  ####    Remove ward        ####
  ###############################

  ###
  ### Use remove Button
  ###
  ## Open remote window: when editward is activated (see body_ui)
  observeEvent(input$deleteW_button,{
    # Show modal when button is clicked.

    ward_namesChoices <- data_advanced$ward_names

    showModal(modalDialog(
      selectizeInput("wardtoremove", "Select the ward/building to remove",
                     choices = ward_namesChoices,
                     options = list(
                       placeholder = 'Select ward',
                       onInitialize = I('function() { this.setValue(""); }')
                     )),
      conditionalPanel(
        "input.wardtoremove != ''",
        actionButton("wardRemoveOk", "Definitly remove ward/building")),
      title = "Here you can remove ward or building from your structure",
      easyClose = TRUE
    )
    )
  })

  ## Validate Removal
  # Update reactives
  observeEvent(input$wardRemoveOk, {
    isolate({
      selectedW <- which(data_advanced$ward_names == input$wardtoremove)
      #####
      ##### Update structure data
      #####

      data_advanced$ward_names %<>% .[-selectedW]
      data_advanced$pop_size_P %<>% .[-selectedW]
      data_advanced$pop_size_H %<>% .[-selectedW]
      data_advanced$LS %<>% .[-selectedW]
      data_advanced$nVisits %<>% .[-selectedW]

      #####
      ##### Update professionals planning
      #####

      data_advanced$Hplanning <-
        data_advanced$Hplanning[!endsWith(data_advanced$Hplanning$professionals,
                                 paste0("_", input$wardtoremove)), ]

      data_advanced$Hplanning[, c(input$wardtoremove) := NULL]

    })

  })



  ####################################################
  ####    Edit professional time sharing table    ####
  ####################################################

  ## Open remote window: when editward is activated (see body_ui)
  callModule(module = contactEdit,
             id = "editplanning",
             variable = data_advanced)

  # update HCWS list based on the selected ward
  observeEvent(input$wardcontactedit, {

    if (input$wardcontactedit != "") {
      selectedW <- which(data_advanced$ward_names == input$wardcontactedit)

      professionalsChoices <- data_advanced$Hplanning$professionals %>%
        .[endsWith(., input$wardcontactedit)]

      updateSelectizeInput(session,
                           'HCWScontactedit',
                           choices = professionalsChoices)
    }

  })


  output$contactEditTab <- DT::renderDT({
    DT::datatable(
      data_advanced$Hplanning[professionals == input$HCWScontactedit],
      rownames = F,
      editable = FALSE,
      escape = FALSE
    )

  })

  observeEvent(input$selectedW, {
    if (input$selectedW != "") {
      updateNumericInput(session,
                         'ptimeinWard',
                         value = data_advanced$Hplanning[professionals == input$HCWScontactedit,
                                                mget(input$selectedW)])
    }

  })


  ## Validate edits
  # Update reactives
  observeEvent(input$planningEdit, {
    data_advanced$Hplanning[which(data_advanced$Hplanning$professionals == input$HCWScontactedit),
                   c(input$selectedW)] <- input$ptimeinWard
  })


  # update contact matrix

  observe({
    if (!is.null(data_advanced$Hplanning) & length(data_advanced$ward_names) > 1 ){
      contacts <- data_advanced$Hplanning

      matContact <- lapply(data_advanced$ward_names, function(W) {
        W <- contacts$professionals %>% endsWith(., paste0("_", W)) %>% contacts[., ]
        W %<>% as.data.frame %>% .[, data_advanced$ward_names] %>% colSums
        W %<>% divide_by(sum(.)) %>% multiply_by((100))
        W
      }) %>% do.call(rbind, .)


      rownames(matContact) <- colnames(matContact)
      data_advanced$matContact <- matContact
    }
  })

  #####################################
  #######    Parameters panel    ######
  #####################################

  ## Choose disease, update parameters baseline values
  callModule(module = updateParams_simp,
             id = "disease",
             variable = data)

  callModule(module = updateParams_simp,
             id = "disease_adv",
             variable = data_advanced)

  ## Choose level of importation, update parameters baseline values
  callModule(module = updateImportation_simp,
             id = "level_importation",
             variable = data)

  callModule(module = updateImportation_simp,
             id = "level_importation_adv",
             variable = data_advanced)


  ###
  ### Download set of parameters
  ###

  callModule(
    module = downloadParams,
    id = "dwloadParams",
    gdata = list(
      sensAg = input$sensAg,
      speAg = input$speAg,
      sensPCR = input$sensPCR,
      tbeftestHsymp = input$tbeftestHsymp,
      tHI = input$tHI,
      spePCR = input$spePCR,
      tbeftestPsymp = input$tbeftestPsymp,
      I = input$I,
      R0 = input$R0,
      d = input$d,
      rEA = input$rEA,
      rES = input$rES,
      rIA = input$rIA,
      rIM = input$rIM,
      rIS = input$rIS,
      tLI = input$tLI,
      tAg = input$tAg,
      tPCR = input$tPCR,
      popimm_plot = input$popimm_plot,
      piesize = input$piesize,
      labelpos = input$labelpos,
      pHI_NL = input$pHI_NL,
      hNI2LI = input$hNI2LI,
      ptestHsymp = input$ptestHsymp,
      ptestPWsymp = input$ptestPWsymp,
      rinfLI = input$rinfLI,
      pLI_NL = input$pLI_NL,
      rinfHI = input$rinfHI,
      hLI2HI = input$hLI2HI,
      psympNI = input$psympNI,
      psympLI = input$psympLI,
      psympHI = input$psympHI,
      psevNI = input$psevNI,
      psevLI = input$psevLI,
      psevHI = input$psevHI,
      pdieIC = input$pdieIC
    )
  )

  #### Upload dataset
  # Layout load button once the file is uploaded
  ####

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

  # load params after confirmation
  observeEvent(
    eventExpr = input$confirmationloadparams,
    handlerExpr = {
      if (isTRUE(input$confirmationloadparams)) {
        req(input$loadparams)

        load(input$loadparams$datapath)

        if (exists("gdata")) {
          # # update sliders input values
          for (slidersInput in c(
            "piesize", "labelpos", 'pHI_NL', 'hNI2LI',
            "ptestHsymp",
            "ptestPWsymp",
            'rinfLI',
            'pLI_NL',
            'rinfHI',
            'hLI2HI',
            'psympNI',
            'psympLI',
            'psympHI',
            'psevNI',
            'psevLI',
            'psevHI',
            "pdieIC"
          ))
          updateSliderInput(session,
                            slidersInput,
                            value = gdata[[slidersInput]])

          # update times input values
          for (timesInput in c(
            'tAg',
            'tPCR'
          ))
            updateTimeInput(session, timesInput, value = as.POSIXct(gdata[[timesInput]]))

          # update numeric input values
          for (numInput in c(
            'sensAg',
            'speAg',
            'sensPCR',
            'tbeftestHsymp',
            'tHI',
            'spePCR',
            'tbeftestPsymp',
            'I',
            'R0',
            'd',
            'rEA',
            'rES','rIA','rIM','rIS',
            'tLI'
          ))
          updateNumericInput(session,
                             numInput,
                             value = gdata[[numInput]])

          # update select input values
          for (selectInput in c(
            "popimm_plot"
          ))
            updateSelectInput(session, selectInput,
                              selected = gdata[[selectInput]])
        } # FIX ME: add warnings if gdata is not loaded


      }
    },
    ignoreNULL = FALSE
  )

  ## Helpers for patient ratios

  output$rsympInfo <- renderText({
    c(
      "For the professionals, the percent probabilities to develop symptoms are currently set at",
      input$psympNI * 100,"% for non immune,",
      input$psympLI * 100,"% for individuals with old vaccination or infection history and",
      input$psympHI * 100,"% for those with recent vaccination or infection history. \n \n
      With a ratio of 0.5, those probabilities for the patients will respectivelly be:",
      input$psympNI * 100 * 0.5,"%,",input$psympLI * 100 * 0.5,"% and",input$psympHI * 100 * 0.5, "%."
    )
  })

  output$rsevInfo <- renderText({
    c(
      "For the professionals, the contional percent probabilities to develop severe symptoms when symptomatic are currently set at",
      input$psevNI * 100,"% for non immune,",
      input$psevLI * 100,"% for individuals with old vaccination or infection history and",
      input$psevHI * 100,"% for those with recent vaccination or infection history. \n \n
      With a ratio of 0.5, those probabilities for the patients will respectivelly be:",
      input$psevNI * 100 * 0.5,"%,",input$psevLI * 100 * 0.5,"% and",input$psevHI * 100 * 0.5, "%."
    )
  })

  ## Set ImmState

  # initialize ImmState
  observeEvent(input$tabsPARAMS, {
    if (input$tabsPARAMS == "Immunity-related parameters" &
        is.null(data$IMMstate) &
        length(data$ward_names) > 0) {
      nW <- length(data$ward_names)
      data$IMMstate <- data.frame(
        ward = rep(data$ward_names, 2),
        pop = c(rep("P", nW), rep("H", nW)),
        imm = "NI",
        n = c(data$pop_size_P, data$pop_size_H)
      ) %>% rbind(expand.grid(
        ward = data$ward_names,
        pop = c("P", "H"),
        imm = c("LI", "HI"),
        n = 0
      ))
    }

    # If a new ward has been added
    if (input$tabsPARAMS == "Immunity-related parameters" &
        !is.null(data$IMMstate) &
        NA %in% match(data$ward_names, data$IMMstate$ward)) {
      missingW <- which(is.na(match(
        data$ward_names, data$IMMstate$ward
      )))
      nW <- length(missingW)

      missingImm <- data.frame(
        ward = rep(data$ward_names[missingW], 2),
        pop = c(rep("P", nW), rep("H", nW)),
        imm = "NI",
        n = c(data$pop_size_P[missingW], data$pop_size_H[missingW])
      ) %>% rbind(expand.grid(
        ward = data$ward_names[missingW],
        pop = c("P", "H"),
        imm = c("LI", "HI"),
        n = 0
      ))

      data$IMMstate %<>% rbind(., missingImm)

    }
  })

  # FIX ME: set the proportion based on parameters
  # https://www.gouvernement.fr/info-coronavirus/carte-et-donnees#suivi_de_la_vaccination_-_nombre_de_personnes_vaccinees

  callModule(
    module = setIMMstate,
    id = "ImmstateP",
    variable = data,
    pop = "P",
    pLI_NL = reactive(input$pLI_NL),
    pHI_NL = reactive(input$pHI_NL)
  )

  callModule(
    module = setIMMstate,
    id = "ImmstateH",
    variable = data,
    pop = "H",
    pLI_NL = reactive(input$pLI_NL),
    pHI_NL = reactive(input$pHI_NL)
  )

  output$IMMstateTab <- DT::renderDT({
    DT::datatable(
      data$IMMstate,
      rownames = F,
      editable = FALSE,
      escape = FALSE
    )
  })

  output$imm_plot <- renderPlot({
    if (!is.null(req(data$IMMstate))) {
      #  get the network

      g <- plot_connectivity(
        matContact = data$matContact,
        size = as.numeric(data$pop_size_P) + as.numeric(data$pop_size_H),
        netobj = TRUE,
        verbose = FALSE
      ) %>% intergraph::asIgraph(.)

      #  get the network

      if (input$popimm_plot == "P+H")
        pop <- c("P", "H")
      else
        pop <- input$popimm_plot


      values <- lapply(data$ward_names, function(x) {
        if (input$popimm_plot == "P+H") {
          sapply(c("NI", "LI", "HI"), function(imSt) {
            data$IMMstate[data$IMMstate$ward == x &
                            data$IMMstate$imm == imSt, "n"] %>% sum
          })
        } else
          sapply(c("NI", "LI", "HI"), function(imSt) {
            data$IMMstate[data$IMMstate$ward == x &
                            data$IMMstate$imm == imSt &
                            data$IMMstate$pop == input$popimm_plot, "n"] %>% sum
          })

      })

      # default for all
      V(g)$pie.color = list(c("#FF1A1A", "#FFC61A", "#C6FF1A"))

      plot(
        g,
        layout = layout_nicely(g),
        vertex.shape = "pie",
        vertex.pie = values,
        vertex.size = input$piesize,
        vertex.label = data$ward_names,
        vertex.label.dist = input$labelpos,
        vertex.label.degree = pi / 2
      )
    }
  })

  ## Set expert corner params

  observeEvent(input$pHI_NL, {
    updateSliderInput(session, 'pLI_NL', max = 1 - input$pHI_NL)
  })

  observeEvent(input$pLI_NL, {
    updateSliderInput(session, 'pHI_NL', max = 1 - input$pLI_NL)
  })

  dtimeline <- callModule(module = diseasetimeline,
                          id = "covid")

  observeEvent(input$tAg, {
    if((as.numeric(strftime(input$tAg, "%M")) / 60 + as.numeric(strftime(input$tAg, "%H"))) /
       24 == 0)
      showModal(modalDialog(
        title = "Important message",
        "This is an important message!"
      ))

  })


  #####################################
  #######    Simulations panel   ######
  #####################################

  # set maximal number of HCWS in the SA based on smallest ward
  observe({
    if (length(data$pop_size_H) > 0)
      updateNumericInput(session, 'nH_SA', max = min(data$pop_size_H))
  })

  ########### RUN MODEL ###########

  ###### SIMPLE

  ## condition of display of run button

  output$atleastoneward <- reactive({
    return(length(data$ward_names) > 0)
  })

  outputOptions(output,
                'atleastoneward',
                suspendWhenHidden = FALSE)

  output$runbutton <- renderUI({
      conditionalPanel(
        "output.atleastoneward == true" ,
        conditionalPanel(
          condition = "$(\'html\').hasClass(\'shiny-busy\')",
          # tags$div(class = "loader"),
          tags$div(class = "prevent_click")
        ),
        actionButton(
          "runmodelVsimp",
          "Run",
          # span("Run", id = "UpdateAnimate", class = "loading dots"),
          icon = icon("play",
                      verify_fa = FALSE),
          style = "color: #fff; background-color: #063567; border-color: #2e6da4"
        ),
        div(
          style = "display: inline-block;vertical-align:top;",
          conditionalPanel(
            "output.simoutput == true",
            downloadButton("Markdown", "Generate report")
          )
        )
      )
  })

  runmodel <- eventReactive(input$runmodelVsimp, {

    if(is.null(data$imp_lev)|is.null(data$disease)){
        showModal(modalDialog(
          title = "Important message",
          "Select a pathogen and a scenario of importation!"
        )) } else
          if(input$n_days == 0){
            showModal(modalDialog(
              title = "Important message",
              "The number of simulated days must be up to 0."
            )) } else {

    ward_names <- data$ward_names
    pop_size_P <- data$pop_size_P
    pop_size_H <- data$pop_size_H
    nVisits <- data$nVisits
    LS <- data$LS
    LS[LS == 0] <- 1

    matContact <- data$matContact

    n_days <- input$n_days %>% seq

    IMMstate = data$IMMstate
    EPIstate = data$EPIstate

    # print(EPIstate)

    gdata = build_gdata(disease = data$disease,
                        I = data$imp_lev)

    # Isolation
    if ('ISO' %in% input$CSprotocols) {
      gdata[['pISO']] = 1
    } else
      gdata[['pISO']] = 0

    # Testing for patients
    if ('testPat' %in% input$CSprotocols) {
      gdata[['tbtwtestP']] = 7
      gdata[['ptestPWNI']] = 0.75
      gdata[['ptestPWLI']] = 0.5
      gdata[['ptestPWHI']] = 0.1
    } else {
      gdata[['ptestPWNI']] = 0
      gdata[['ptestPWLI']] = 0
      gdata[['ptestPWHI']] = 0
    }
    # Testing for professionals
    if ('testProf' %in% input$CSprotocols) {
      gdata[['tbtwtestH']] = 14
      gdata[['ptestHNI']] = 0.75
      gdata[['ptestHLI']] = 0.5
      gdata[['ptestHHI']] = 0.2
      gdata[['pSLT']] = 0.2
    } else{
      gdata[['ptestHNI']] = 0
      gdata[['ptestHLI']] = 0
      gdata[['ptestHHI']] = 0
    }
    # Screening area
    if ('SA' %in% input$CSprotocols) {
      SA = TRUE
      # nH_SA = input$nH_SA
      nH_SA = 1
      gdata[['tSA']] = 2/24
      gdata[['ptestPSAsymp']] = 1
      gdata[['ptestPSANI']] = 0.75
      gdata[['ptestPSALI']] = 0.5
      gdata[['ptestPSAHI']] = 0.25
      gdata[['ttestSA']] = 2/24
      gdata[['n_ctcH_PSA']] = 2
      gdata[['t_ctcH_PSA']] = (10/60)/24
      gdata[['epsHPSA']] = 0.5
      gdata[['epsPHSA']] = 0.5
      gdata[['n_ctcP_PSA']] = 0
      gdata[['t_ctcP_PSA']] = (5/60)/24
      gdata[['epsPPSA']] = 0.5
    } else{
      SA = FALSE
      nH_SA = NULL
    }

    # save(ward_names, file = "tmpdata/ward_names.Rda")
    # save(pop_size_P, file = "tmpdata/pop_size_P.Rda")
    # save(pop_size_H, file = "tmpdata/pop_size_H.Rda")
    # save(nVisits, file = "tmpdata/nVisits.Rda")
    # save(LS, file = "tmpdata/LS.Rda")
    # save(matContact, file = "tmpdata/matContact.Rda")
    # save(IMMstate, file = "tmpdata/IMMstate.Rda")
    # save(EPIstate,  file = "tmpdata/EPIstate.Rda")
    # save(SA,  file = "tmpdata/SA.Rda")
    # save(nH_SA,  file = "tmpdata/nH_SA.Rda")
    # save(gdata,  file = "tmpdata/gdata.Rda")
    # save(n_days,  file = "tmpdata/n_days.Rda")

    mwssmodel <- mwss(
      ward_names,
      pop_size_P,
      pop_size_H,
      nVisits,
      LS,
      matContact = matContact,
      IMMstate = IMMstate,
      EPIstate = EPIstate,
      SA = SA,
      nH_SA = nH_SA,
      gdata = gdata,
      tSim =  n_days,
      verbose = FALSE
    )

    # trajmwss <- multisim(mwssmodel, input$n_sim, ward_names)
    trajmwss <- multisim(mwssmodel, 50, ward_names)

    scenarios <- input$CSprotocols

    trajmwss_data <- list(
      trajmwss = trajmwss,
      ward_names = ward_names,
      pop_size_P = pop_size_P,
      pop_size_H = pop_size_H,
      nVisits = nVisits,
      LS = LS,
      matContact = matContact,
      IMMstate = IMMstate,
      EPIstate = EPIstate,
      clustering = clustering$clustering,
      disease = data$disease,
      gdata = gdata,
      scenarios = scenarios
        )

    # save(trajmwss_data,  file = "tmpdata/trajmwss_data.Rda")

    return(trajmwss_data)}
  })


  output$simoutput <- reactive({
    return("mwss" %in% class(runmodel()[["trajmwss"]]))
  })

  outputOptions(output,
                'simoutput',
                suspendWhenHidden = FALSE)

  callModule(module = valueboxoutput,
             id = "simulation",
             model = runmodel)

  callModule(module = plotsoutput,
             id = "simulationPlots",
             model = runmodel,
             ndays = reactive(input$n_days))

  callModule(module = exporttraj,
             id = "export_traj",
             model = runmodel)

  callModule(module = plot_network_serv,
             id = "network_plot_advanced",
             variable = data_advanced,
             buildings = FALSE)

  callModule(module = plot_network_serv,
             id = "network_plot_simple",
             variable = data,
             buildings = TRUE)

  output$Markdown <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = "Report.pdf",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).

      tempReport <- file.path(tempdir(), "epi_report.Rmd")
      file.copy("epi_report.Rmd", tempReport, overwrite = TRUE)

      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      output <- rmarkdown::render(
        input = tempReport,
        # Set up parameters to pass to Rmd document
        params = runmodel()
      )
      file.copy(output, file)
    })


####### advanced

output$runbutton_adv <- renderUI({
  conditionalPanel(
    "output.atleastoneward_advanced == true",
    conditionalPanel(
      condition = "$(\'html\').hasClass(\'shiny-busy\')",
      tags$div(class = "prevent_click")
    ),
    actionButton(
      "runmodelVadv",
      "Run",
      icon = icon("play",
                  verify_fa = FALSE),
      style = "color: #fff; background-color: #063567; border-color: #2e6da4"
    ),
    div(
      style = "display: inline-block;vertical-align:top;",
      conditionalPanel(
        "output.simadv_output == true",
        downloadButton("Markdown_adv", "Generate report")
      )
    )
  )
})

runmodel_adv <- eventReactive(input$runmodelVadv, {

  if(is.null(data_advanced$imp_lev)|is.null(data_advanced$disease)){
    showModal(modalDialog(
      title = "Important message",
      "Select a pathogen and a scenario of importation!"
    )) } else
      if(input$n_days_adv == 0){
        showModal(modalDialog(
          title = "Important message",
          "The number of simulated days must be up to 0."
        )) } else {

          ward_names <- data_advanced$ward_names
          pop_size_P <- data_advanced$pop_size_P
          pop_size_H <- data_advanced$pop_size_H
          nVisits <- data_advanced$nVisits
          LS <- data_advanced$LS
          LS[LS == 0] <- 1

          matContact <- data_advanced$matContact

          n_days <- input$n_days_adv %>% seq

          IMMstate = data_advanced$IMMstate
          EPIstate = data_advanced$EPIstate

          # print(EPIstate)

          gdata = build_gdata(disease = data_advanced$disease,
                              I = data_advanced$imp_lev)

          # Isolation
          if ('ISO' %in% input$CSprotocolsUI_adv) {
            gdata[['pISO']] = 1
          } else
            gdata[['pISO']] = 0

          # Testing for patients
          if ('testPat' %in% input$CSprotocolsUI_adv) {
            gdata[['tbtwtestP']] = 7
            gdata[['ptestPWNI']] = 0.75
            gdata[['ptestPWLI']] = 0.5
            gdata[['ptestPWHI']] = 0.1
          } else {
            gdata[['ptestPWNI']] = 0
            gdata[['ptestPWLI']] = 0
            gdata[['ptestPWHI']] = 0
          }
          # Testing for professionals
          if ('testProf' %in% input$CSprotocolsUI_adv) {
            gdata[['tbtwtestH']] = 14
            gdata[['ptestHNI']] = 0.75
            gdata[['ptestHLI']] = 0.5
            gdata[['ptestHHI']] = 0.2
            gdata[['pSLT']] = 0.2
          } else{
            gdata[['ptestHNI']] = 0
            gdata[['ptestHLI']] = 0
            gdata[['ptestHHI']] = 0
          }
          # Screening area
          if ('SA' %in% input$CSprotocolsUI_adv) {
            SA = TRUE
            # nH_SA = input$nH_SA
            nH_SA = 1
            gdata[['tSA']] = 2/24
            gdata[['ptestPSAsymp']] = 1
            gdata[['ptestPSANI']] = 0.75
            gdata[['ptestPSALI']] = 0.5
            gdata[['ptestPSAHI']] = 0.25
            gdata[['ttestSA']] = 2/24
            gdata[['n_ctcH_PSA']] = 2
            gdata[['t_ctcH_PSA']] = (10/60)/24
            gdata[['epsHPSA']] = 0.5
            gdata[['epsPHSA']] = 0.5
            gdata[['n_ctcP_PSA']] = 0
            gdata[['t_ctcP_PSA']] = (5/60)/24
            gdata[['epsPPSA']] = 0.5
          } else{
            SA = FALSE
            nH_SA = NULL
          }

          mwssmodel <- mwss(
            ward_names,
            pop_size_P,
            pop_size_H,
            nVisits,
            LS,
            matContact = matContact,
            IMMstate = IMMstate,
            EPIstate = EPIstate,
            SA = SA,
            nH_SA = nH_SA,
            gdata = gdata,
            tSim =  n_days,
            verbose = FALSE
          )

          # trajmwss <- multisim(mwssmodel, input$n_sim, ward_names)
          trajmwss <- multisim(mwssmodel, 50, ward_names)

          scenarios <- input$CSprotocols

          trajmwss_data <- list(
            trajmwss = trajmwss,
            ward_names = ward_names,
            pop_size_P = pop_size_P,
            pop_size_H = pop_size_H,
            nVisits = nVisits,
            LS = LS,
            matContact = matContact,
            IMMstate = IMMstate,
            EPIstate = EPIstate,
            clustering = NULL,
            disease = data_advanced$disease,
            gdata = gdata,
            scenarios = scenarios
          )

          # save(trajmwss_data,  file = "tmpdata/trajmwss_data.Rda")

          return(trajmwss_data)}
})



output$simadv_output <- reactive({
  return("mwss" %in% class(runmodel_adv()[["trajmwss"]]))
})

outputOptions(output,
              'simadv_output',
              suspendWhenHidden = FALSE)


callModule(module = valueboxoutput,
           id = "simulation_adv",
           model = runmodel_adv)

callModule(module = plotsoutput,
           id = "simulationPlots_adv",
           model = runmodel_adv,
           ndays = reactive(input$n_days_adv))
###

source("functions/advanced_updownload.R", local = TRUE)
source("functions/advanced_updownload_HCWS_time.R", local = TRUE)


output$Markdown_adv <- downloadHandler(
  # For PDF output, change this to "report.pdf"
  filename = "Report.pdf",
  content = function(file) {
    # Copy the report file to a temporary directory before processing it, in
    # case we don't have write permissions to the current working dir (which
    # can happen when deployed).


    tempReport <- file.path(tempdir(), "epi_report.Rmd")
    file.copy("epi_report.Rmd", tempReport, overwrite = TRUE)

    # Knit the document, passing in the `params` list, and eval it in a
    # child of the global environment (this isolates the code in the document
    # from the code in this app).
    output <- rmarkdown::render(
      input = tempReport,
      # Set up parameters to pass to Rmd document
      params = runmodel_adv()
    )
    file.copy(output, file)
  })
} # end server function
