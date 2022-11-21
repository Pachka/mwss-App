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
    gdata = NULL
  )
  
  # load test
  callModule(module = loadTestdt,
             id = "loadtest",
             variable = data)
  
  # reset inputs
  callModule(module = resetreactives,
             id = "resetall",
             variable = data)
  
  
  observe({
    if (!is.null(data$Hplanning)){
      contacts <- data$Hplanning
      
      matContact <- lapply(data$ward_names, function(W) {
        W <- contacts$professionals %>% endsWith(., paste0("_", W)) %>% contacts[., ]
        W %<>% .[, mget(c(names(.)[names(.) != "professionals"]))] %>% colSums
        W %<>% divide_by(sum(.)) %>% multiply_by((100))
        W
      }) %>% do.call(rbind, .)
      
      rownames(matContact) <- colnames(matContact)
      data$matContact <- matContact
    }
  })
  
  #####################################
  #######    Structure panel     ######
  #####################################
  
  ###
  ### Sidebar
  ###
  
  #### Upload dataset
  # Layout load button once the file is uploaded
  ####
  
  # conditional display
  output$fileUploaded <- reactive({
    return(!is.null(input$loadwards))
  })
  
  outputOptions(output,
                'fileUploaded',
                suspendWhenHidden = FALSE)
  
  # ask for confirmation
  observeEvent(input$uploadSTR, {
    ask_confirmation(
      inputId = "myconfirmation",
      title = "Want to confirm ?",
      type = "warning",
      btn_labels = c("Cancel", "Confirm"),
      text = "Note that loading a dataset will erase the currently recorded structure."
    )
  })
  
  # ask for confirmation
  observeEvent(
    eventExpr = input$myconfirmation,
    handlerExpr = {
      if (isTRUE(input$myconfirmation)) {
        req(input$loadwards)
        
        load(input$loadwards$datapath)
        
        if (exists("saveInputs")) {
          # structure
          data$ward_names = saveInputs$ward_names
          data$pop_size_P = saveInputs$pop_size_P
          data$pop_size_H = saveInputs$pop_size_H
          data$nVisits = saveInputs$nVisits
          data$LS = saveInputs$LS
          # Contacts
          data$Hplanning = saveInputs$Hplanning
          data$matContact = saveInputs$matContact
          # Immunity
          data$IMMstate = saveInputs$IMMstate
          # Epidemiological states // infections
          data$EPIstate = saveInputs$EPIstate
        } #FIX ME: warn me if the object is missing
      }
    },
    ignoreNULL = FALSE
  )
  
  #### Download dataset
  # Layout download button once at least one ward has been added
  ####
  
  output$atleastoneward <- reactive({
    return(length(data$ward_names) > 0)
  })
  
  outputOptions(output,
                'atleastoneward',
                suspendWhenHidden = FALSE)
  
  ###
  # Download dataset
  ###
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".rda", sep = "")
    },
    content = function(filename) {
      saveInputs <- list(
        ward_names = data$ward_names,
        pop_size_P = data$pop_size_P,
        pop_size_H = data$pop_size_H,
        nVisits = data$nVisits,
        LS = data$LS,
        # Contacts
        Hplanning = data$Hplanning,
        matContact = data$matContact,
        # Immunity
        IMMstate = data$IMMstate,
        # Epidemiological states // infections
        EPIstate = data$EPIstate
      )
      
      if (endsWith(filename, ".rda") | endsWith(filename, ".Rda"))
        save(saveInputs, file = filename)
      else
        save(saveInputs, file = paste0(filename, ".rda"))
    }
  )
  
  ####
  #### Download settings
  #### Upload dataset
  # Layout load button once the file is uploaded
  ####
  
  # ask for confirmation
  observeEvent(input$uploadsettings, {
    ask_confirmation(
      inputId = "settingsconfirmation",
      title = "Want to confirm ?",
      type = "warning",
      btn_labels = c("Cancel", "Confirm"),
      text = "Note that loading a dataset will erase the currently recorded epidemiological settings"
    )
  })
  
  # ask for confirmation
  observeEvent(
    eventExpr = input$settingsconfirmation,
    handlerExpr = {
      if (isTRUE(input$settingsconfirmation)) {
        req(input$loadsettings)
        
        load(input$loadsettings$datapath)
        
        if (exists("savesettings")) {
          # update slider input values
          for(slidersInputset in c("pSL","pESL","tSLs","pSLT","pIC","n_ctcP_PW","n_ctcH_H","n_ctcH_PW", "ptestHsymp", "ptestPWsymp"))
            updateSliderInput(session,
                              slidersInputset,
                              value = savesettings[[slidersInputset]])
          
          # update times input values
          for(timesInputset in c("t_ctcP_PW","t_ctcH_H","t_ctcH_PW","t_ctcV_PW"))
            updateTimeInput(session, timesInputset, value = as.POSIXct(savesettings[[timesInputset]]))
          
          # update numeric input values
          for(numInput in c('tw','tIC',"rsymp","rsev", "tbeftestHsymp", "tbeftestPsymp"))
            updateNumericInput(session,
                               numInput,
                               value = savesettings[[numInput]])
          
          # update radio button input values
          for(RadioButtInput in c("epsPPW","epsHHW","epsHPW","epsPHW","epsVPW", "testPW", 'testH', 'testsymp'))
            updateRadioButtons(session,
                               RadioButtInput,
                               selected = savesettings[[RadioButtInput]])
          
          # update checkbox input values
          for(checkboxtInput in c("comorbidities"))
            updateCheckboxInput(session,
                                checkboxtInput,
                                value = savesettings[[checkboxtInput]])
        }
      }
    },
    ignoreNULL = FALSE
  )
  
  #### Download dataset
  ####
  
  ###
  # Download dataset
  ###
  
  output$downloadsettings <- downloadHandler(
    filename = function() {
      paste("settings-", Sys.Date(), ".rda", sep = "")
    },
    content = function(filename) {
      savesettings <- list(
        pSL = input$pSL,
        pESL = input$pESL,
        pSLT = input$pSLT,
        tSLs = input$tSLs,
        tw = input$tw,
        pIC = input$pIC,
        tIC = input$tIC,
        rsymp = input$rsymp,
        rsev = input$rsev,
        n_ctcP_PW = input$n_ctcP_PW,
        t_ctcP_PW = input$t_ctcP_PW,
        epsPPW = input$epsPPW,
        n_ctcH_H = input$n_ctcH_H,
        t_ctcH_H = input$t_ctcH_H,
        epsHHW = input$epsHHW,
        comorbidities = input$comorbidities,
        n_ctcH_PW = input$n_ctcH_PW,
        t_ctcH_PW = input$t_ctcH_PW,
        epsHPW = input$epsHPW,
        epsPHW = input$epsPHW,
        t_ctcV_PW = input$t_ctcV_PW,
        epsVPW = input$epsVPW,
        testPW = input$testPW,
        testH = input$testH,
        testsymp = input$testsymp
      )
      
      if (endsWith(filename, ".rda") | endsWith(filename, ".Rda"))
        save(savesettings, file = filename)
      else
        save(savesettings, file = paste0(filename, ".rda"))
    }
  )
  
  
  ###
  ### Body
  ###
  
  ############## Renders  ##############
  
  ###
  # Display structure of the facility
  ###
  
  output$facilitystr <- DT::renderDT({
    facilitystr <- data.frame(
      "Ward" = data$ward_names,
      "Patients" = data$pop_size_P,
      "Professionals" = data$pop_size_H,
      "Length of stay" = data$LS,
      "Daily visits" = data$nVisits,
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
    if (!is.null(data$Hplanning)) {
      TS <- data$Hplanning
      wardN <- data$ward_names
      TS$total <- TS[, mget(wardN)] %>% rowSums
      
      datatable(TS,
                rownames = FALSE) %>% formatStyle('total',
                                                  backgroundColor = styleInterval(c(99.9, 100.1),
                                                                                  c('green', 'white', 'red')),
                                                  fontWeight = 'bold')
    }
    
  })
  
  ###
  # Display network
  ###
  
  output$network_plot <- renderPlot({
    num_nodes <- length(data$ward_names)
    
    if (num_nodes == 1) {
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
    
    if (num_nodes > 1) {
      plot_connectivity(
        data$matContact,
        as.numeric(data$pop_size_P) + as.numeric(data$pop_size_H),
        verbose = FALSE
      )
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
  callModule(wardAddPanel, "addward")
  
  ## Conditional display of add button: If a new ward name is written in the dedicated field, the add button appears
  output$addBut <- renderUI({
    if (!(input$wardname %in% data$ward_names) & input$wardname != "") {
      actionButton("addWbutton", "Add ward/building")
    }
  })
  
  ## Add a ward
  # update reactive values
  observeEvent(input$addWbutton, {
    isolate({
      #####
      ##### Update structure data
      #####
      
      data$ward_names %<>% c(., input$wardname)
      data$pop_size_P %<>% c(., input$pop_size_P)
      data$pop_size_H %<>% c(., input$pop_size_H)
      data$nVisits %<>% c(., input$nVisits)
      data$LS %<>% c(., input$LS)
      
      #####
      ##### Update professionals plannings
      #####
      if (is.null(data$Hplanning)) {
        data$Hplanning <-
          data.table(professionals = paste0(
            "HCWS_",
            seq(input$pop_size_H),
            paste0("_", input$wardname)
          ),
          ward = 100)
        setnames(data$Hplanning, "ward", input$wardname)
      } else {
        newW <-
          data.table(professionals = paste0(
            "HCWS_",
            seq(input$pop_size_H),
            paste0("_", input$wardname)
          ),
          ward = 100)
        setnames(newW, "ward", input$wardname)
        
        data$Hplanning %<>% rbind(., newW, fill = TRUE)
        
        data$Hplanning[is.na(data$Hplanning)] <- 0
      }
    })
  })
  
  ###############################
  ####    Edit wards         ####
  ###############################
  
  ###
  ### Use edit Button
  ###
  
  ## Open remote window: when editward is activated (see body_ui)
  callModule(module = wardEdit,
             id = "editward",
             variable = data)
  
  # Get changed data from table on popup and store in dataframe
  observeEvent(input$wardtoEdit, {
    if (input$wardtoEdit != "") {
      selectedW <- which(data$ward_names == input$wardtoEdit)
      
      updateNumericInput(session, 'pop_size_PNEW', value = data$pop_size_P[selectedW])
      updateNumericInput(session, 'pop_size_HNEW', value = data$pop_size_H[selectedW])
      updateNumericInput(session, 'LSNEW', value = data$LS[selectedW])
      updateNumericInput(session, 'nVisitsNEW', value = data$nVisits[selectedW])
    }
    
  })
  
  ## Validate edits
  # Update reactives
  observeEvent(input$wardEditOk, {
    isolate({
      selectedW <- which(data$ward_names == input$wardtoEdit)
      
      matContneedsUpdate <-
        data$pop_size_H[selectedW] != input$pop_size_HNEW
      #####
      ##### Update structure data
      #####
      
      data$pop_size_P[selectedW] <- input$pop_size_PNEW
      data$pop_size_H[selectedW] <- input$pop_size_HNEW
      data$LS[selectedW] <- input$LSNEW
      data$nVisits[selectedW] <- input$nVisitsNEW
      
      #####
      ##### Update professionals planning
      #####
      
      if (matContneedsUpdate) {
        data$Hplanning <-
          data$Hplanning[!endsWith(data$Hplanning$professionals,
                                   paste0("_", input$wardtoEdit)), ]
        
        newW <-
          data.table(professionals = paste0(
            "HCWS_",
            seq(input$pop_size_HNEW),
            paste0("_", input$wardtoEdit)
          ),
          ward = 100)
        setnames(newW, "ward", input$wardtoEdit)
        
        data$Hplanning %<>% rbind(., newW, fill = TRUE)
        
        data$Hplanning[is.na(data$Hplanning)] <- 0
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
  callModule(module = wardRemove,
             id = "removeward",
             variable = data)
  
  ## Validate Removal
  # Update reactives
  observeEvent(input$wardRemoveOk, {
    isolate({
      selectedW <- which(data$ward_names == input$wardtoremove)
      #####
      ##### Update structure data
      #####
      
      data$ward_names %<>% .[-selectedW]
      data$pop_size_P %<>% .[-selectedW]
      data$pop_size_H %<>% .[-selectedW]
      data$LS %<>% .[-selectedW]
      data$nVisits %<>% .[-selectedW]
      
      #####
      ##### Update professionals planning
      #####
      
      data$Hplanning <-
        data$Hplanning[!endsWith(data$Hplanning$professionals,
                                 paste0("_", input$wardtoremove)), ]
      
      data$Hplanning[, c(input$wardtoremove) := NULL]
    })
    
  })
  
  
  ####################################################
  ####    Edit professional time sharing table    ####
  ####################################################
  
  ## Open remote window: when editward is activated (see body_ui)
  callModule(module = contactEdit,
             id = "editplanning",
             variable = data)
  
  # update HCWS list based on the selected ward
  observeEvent(input$wardcontactedit, {
    if (input$wardcontactedit != "") {
      selectedW <- which(data$ward_names == input$wardcontactedit)
      
      professionalsChoices <- data$Hplanning$professionals %>%
        .[endsWith(., input$wardcontactedit)]
      
      updateSelectizeInput(session,
                           'HCWScontactedit',
                           choices = professionalsChoices)
    }
    
  })
  
  output$contactEditTab <- DT::renderDT({
    DT::datatable(
      data$Hplanning[professionals == input$HCWScontactedit],
      rownames = F,
      editable = FALSE,
      escape = FALSE
    )
    
  })
  
  observeEvent(input$selectedW, {
    if (input$selectedW != "") {
      updateNumericInput(session,
                         'ptimeinWard',
                         value = data$Hplanning[professionals == input$HCWScontactedit,
                                                mget(input$selectedW)])
    }
    
  })
  
  
  ## Validate edits
  # Update reactives
  observeEvent(input$planningEdit, {
    data$Hplanning[which(data$Hplanning$professionals == input$HCWScontactedit),
                   c(input$selectedW)] <- input$ptimeinWard
    
  })
  
  
  #####################################
  #######    Parameters panel    ######
  #####################################
  
  ## Choose variant, update parameters baseline values
  callModule(module = updateParams,
             id = "variant")
  
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
        data$matContact,
        as.numeric(data$pop_size_P) + as.numeric(data$pop_size_H),
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
  
  #####################################
  #######    Simulations panel   ######
  #####################################
  
  # set maximal number of HCWS in the SA based on smallest ward
  observe({
    if (length(data$pop_size_H) > 0)
      updateNumericInput(session, 'nH_SA', max = min(data$pop_size_H))
  })
  
  
  ########### RUN MODEL ###########
  
  
  observe({
    ## Set test parameters
    # Patients
    if (input$testPW == "Ag-RDT") {
      senW = input$sensAg
      speW = input$speAg
      ttestPW = (as.numeric(strftime(input$tAg, "%M")) / 60 + as.numeric(strftime(input$tAg, "%H"))) /
        24
    } else {
      senW = input$sensPCR
      speW = input$spePCR
      ttestPW = (as.numeric(strftime(input$tPCR, "%M")) / 60 + as.numeric(strftime(input$tPCR, "%H"))) /
        24
    }
    
    # Professionals
    if (input$testH == "Ag-RDT") {
      senH = input$sensAg
      speH = input$speAg
      ttestHW = (as.numeric(strftime(input$tAg, "%M")) / 60 + as.numeric(strftime(input$tAg, "%H"))) /
        24
    } else {
      senH = input$sensPCR
      speH = input$spePCR
      ttestHW = (as.numeric(strftime(input$tPCR, "%M")) / 60 + as.numeric(strftime(input$tPCR, "%H"))) /
        24
    }
    
    # Symptomatics
    if (input$testsymp == "Ag-RDT") {
      sensymp = input$sensAg
      spesymp = input$speAg
      ttestsymp = (as.numeric(strftime(input$tAg, "%M")) / 60 + as.numeric(strftime(input$tAg, "%H"))) /
        24
    } else {
      sensymp = input$sensPCR
      spesymp = input$spePCR
      ttestsymp = (as.numeric(strftime(input$tPCR, "%M")) / 60 + as.numeric(strftime(input$tPCR, "%H"))) /
        24
    }
    
    # Screening area
    if ('SA' %in% input$CSprotocols) {
      if (input$testSA == "Ag-RDT") {
        senSA = input$sensAg
        speSA = input$speAg
      } else {
        senSA = input$sensPCR
        speSA = input$spePCR
      }
    } else{
      senSA = 0
      speSA = 0
    }
    
    # Screening area
    
    ptestPWNI = input$ptestPWNI # probability to test NI patients in the ward
    ptestPWLI = input$ptestPWLI # probability to test PI patients in the ward
    ptestPWHI = input$ptestPWHI # probability to test FI patients in the ward
    ptestHNI = input$ptestHNI # probability to test NI HCWS
    ptestHLI = input$ptestHLI # probability to test PI HCWS
    ptestHHI = input$ptestHHI # probability to test FI HCWS
    
    if (!('screenstrP' %in% input$regscreenPop)) {
      ptestPWNI = 0 # probability to test NI patients in the ward
      ptestPWLI = 0 # probability to test PI patients in the ward
      ptestPWHI = 0 # probability to test FI patients in the ward
    }
    
    if (!('screenstrH' %in% input$regscreenPop)) {
      ptestHNI = 0 # probability to test NI HCWS
      ptestHLI = 0 # probability to test PI HCWS
      ptestHHI = 0 # probability to test FI HCWS
    }
    
    
    pISO <- ifelse('ISO' %in% input$CSprotocols,
                   input$pISO,
                   0)
    
    gdata <- build_gdata(
      ##### Infection
      n_ctcH_PSA = input$n_ctcH_PSA,
      t_ctcH_PSA = (as.numeric(strftime(
        input$t_ctcH_PSA, "%M"
      )) / 60 + as.numeric(strftime(
        input$t_ctcH_PSA, "%H"
      ))) / 24,
      n_ctcP_PSA = input$n_ctcH_PSA,
      t_ctcP_PSA = (as.numeric(strftime(
        input$t_ctcP_PSA, "%M"
      )) / 60 + as.numeric(strftime(
        input$t_ctcP_PSA, "%H"
      ))) / 24,
      n_ctcH_PW = input$n_ctcH_PW,
      t_ctcH_PW = (as.numeric(strftime(
        input$t_ctcH_PW, "%M"
      )) / 60 + as.numeric(strftime(
        input$t_ctcH_PW, "%H"
      ))) / 24,
      n_ctcP_PW = input$n_ctcP_PW,
      t_ctcP_PW = (as.numeric(strftime(
        input$t_ctcP_PW, "%M"
      )) / 60 + as.numeric(strftime(
        input$t_ctcP_PW, "%H"
      ))) / 24,
      n_ctcH_H = input$n_ctcH_H,
      t_ctcH_H = (as.numeric(strftime(
        input$t_ctcH_H, "%M"
      )) / 60 + as.numeric(strftime(
        input$t_ctcH_H, "%H"
      ))) / 24,
      t_ctcV_PW = (as.numeric(strftime(
        input$t_ctcV_PW, "%M"
      )) / 60 + as.numeric(strftime(
        input$t_ctcV_PW, "%H"
      ))) / 24,
      # daily incidence (https://www.gouvernement.fr/info-coronavirus/carte-et-donnees)
      I = input$I / 100000,
      # disease duration (days)
      d = 10,
      #  basic reproduction number
      R0 = input$R0,
      # https://www.gouvernement.fr/info-coronavirus/carte-et-donnees
      tw = input$tw,
      tSA  = (as.numeric(strftime(input$tSA, "%M")) / 60 + as.numeric(strftime(input$tSA, "%H"))) /
        24,
      # average duration before full admission (in screening area for clinical exam, administrative procedure, etc)
      tIC  = input$tIC,
      # average duration of stay in intensive care
      tSL  = input$tSLs[1],
      # average duration of sick leave
      tESL = input$tSLs[2],
      # average duration of extended sick leave
      tE  = dtimeline$tE(),
      # duration epidemiological state E
      tEA = dtimeline$tEA(),
      # duration epidemiological state EA
      tES = dtimeline$tES(),
      # duration epidemiological state ES
      tIA = dtimeline$tIA(),
      # duration epidemiological state IA
      tIM = dtimeline$tIM(),
      # duration epidemiological state IM
      tIS = dtimeline$tIS(),
      # duration epidemiological state IS
      tLI = input$tLI,
      # duration of partial immunity before return to non immune status
      tHI = input$tHI,
      # duration of full immunity before return to partial immune status
      # patient protection
      epsPPSA = input$epsPPSA %>% as.numeric,
      epsHPSA = input$epsHPSA %>% as.numeric,
      epsHPW = input$epsHPW %>% as.numeric,
      epsPPW = input$epsPPW %>% as.numeric,
      epsVPW = input$epsVPW %>% as.numeric,
      # healthcare workers protection
      epsPHSA = input$epsPHSA %>% as.numeric,
      #from patient in SA
      epsPHW = input$epsPHW %>% as.numeric,
      #from patient in W
      epsHHW = input$epsHHW %>% as.numeric,
      #from HW in W
      ## Test in ward
      ttestSA = (as.numeric(strftime(
        input$ttestSA, "%M"
      )) / 60 + as.numeric(strftime(
        input$ttestSA, "%H"
      ))) / 24,
      # test duration in screening area
      ttestPW = ttestPW,
      # test duration in ward for screening patients
      ttestHW = ttestHW,
      # test duration in ward for screening professionals
      ttestsymp = ttestsymp,
      # test duration for symptomatic
      
      tbtwtestP = input$tbtwtestP,
      # duration between two tests for patient
      tbtwtestH = input$tbtwtestH,
      # duration between two tests for HCWS
      
      tbeftestPsymp = input$tbeftestPsymp / 24,
      # duration before test of symp patient
      tbeftestHsymp = input$tbeftestHsymp / 24,
      # duration before test of symp HCWS
      
      psympNI = input$psympNI,
      # probability to be symptomatic when non immune
      psympLI = input$psympLI,
      # probability to be symptomatic when partially immune
      psympHI = input$psympHI,
      # probability to be symptomatic when fully immune
      
      psevNI = input$psevNI,
      # probability to develop severe symptoms when non immune
      psevLI = input$psevLI,
      # probability to develop severe symptoms when partially immune
      psevHI = input$psevHI,
      # probability to develop severe symptoms when fully immune
      
      pSL = input$pSL / 100,
      # probability to take sick leave
      pESL = input$pESL / 100,
      # probability to take extended sick leave
      pSLT = input$pSLT / 100,
      # probability to take EL/ESL after positive test
      
      pISO = pISO,
      # probability to be isolated if confirmed
      
      pIC = input$pIC / 100,
      # probability to be transfer in intensive care
      pdieIC = input$pdieIC / 100,####################### FIX HERE
      # probability to die in intensive care
      
      ###################################
      pLI = input$pLI_NL,
      # probability to be PI at the admission (proportion of PI in the population)
      pHI = input$pHI_NL,
      # probability to be FI at the admission (proportion of FI in the population)
      hNI2LI = input$hNI2LI,
      # daily probability to become partially immune
      hLI2HI = input$hLI2HI,
      # daily probability to become fully immune
      
      rinfLI = input$rinfLI,
      # partial immunity efficiency % FIX ME better explain that this is the ratio of reduction of probability to be infected compared to non immune
      rinfHI = input$rinfHI,
      # partial immunity efficiency % FIX ME better explain that this is the ratio of reduction of probability to be infected compared to non immune
      
      rsymp = input$rsymp,
      # Ratio adjusting probability of symptoms for patients compared to general population (professionals)
      rsev = input$rsev,
      # Ratio adjusting probability of severity if symptoms for patients compared to general population (professionals)
      
      # Ratio adjusting the excretion rates based on epidemiological stage
      rEA = input$rEA,
      rES = input$rES,
      rIA = input$rIA,
      rIM = input$rIM,
      rIS = input$rIS,
      
      ptestPSAsymp = input$ptestPSAsymp,
      # probability to test symptomatic patients in the screening area
      ptestPSANI = input$ptestPSANI,
      # probability to test NI patients in the screening area
      ptestPSALI = input$ptestPSALI,
      # probability to test PI patients in the screening area
      ptestPSAHI = input$ptestPSAHI,
      # probability to test FI patients in the screening area
      
      ptestPWsymp = input$ptestPWsymp / 100,
      # probability to test symptomatic patients in the ward
      ptestPWNI = ptestPWNI,
      # probability to test NI patients in the ward
      ptestPWLI = ptestPWLI,
      # probability to test PI patients in the ward
      ptestPWHI = ptestPWHI,
      # probability to test FI patients in the ward
      
      ptestHsymp = input$ptestHsymp / 100,
      # probability to test symptomatic HCWS in the ward
      ptestHNI = ptestHNI,
      # probability to test NI HCWS
      ptestHLI = ptestHLI,
      # probability to test PI HCWS
      ptestHHI = ptestHHI,
      # probability to test FI HCWS
      
      senSA = senSA,
      speSA = speSA,
      senW = senW,
      speW = speW,
      senH = senH,
      speH = senH,
      sensymp = sensymp,
      spesymp = spesymp
    )
    
    data$gdata <- gdata
  })
  
  runmodel <- eventReactive(input$runmodel, {
    
    ward_names <- data$ward_names
    pop_size_P <- data$pop_size_P
    pop_size_H <- data$pop_size_H
    nVisits <- data$nVisits
    LS <- data$LS
    LS[LS == 0] <- 1
    
    # Screening area
    if ('SA' %in% input$CSprotocols) {
      SA = TRUE
      nH_SA = input$nH_SA
    } else{
      SA = FALSE
      nH_SA = NULL
    }
    
    matContact <- data$matContact
    
    n_days <- input$n_days %>% seq
    
    IMMstate = data$IMMstate
    EPIstate = data$EPIstate
    
    gdata = data$gdata
    
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
    
    trajmwss <- multisim(mwssmodel, input$n_sim, ward_names)
    
    return(trajmwss)
  })
  
  output$simoutput <- reactive({
    return("mwss" %in% class(runmodel()))
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
             variable = data,
             ndays = reactive(input$n_days))
  
  callModule(module = synthreport,
             id = "report_exp",
             model = runmodel,
             variable = reactive(data),
             n_days = reactive(input$n_days),
             n_sim = reactive(input$n_sim))
  
  callModule(module = exporttraj,
             id = "export_traj",
             model = runmodel)
} # end server function
