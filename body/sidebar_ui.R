###############
### sidebar ###
###############


sidebar <- dashboardSidebar(

  sidebarMenu(
    id = "sidebarMenu",

    menuItem("Presentation", icon = icon("book",
                                         verify_fa = FALSE), tabName = "PRS"),

    #################
    #################
    ### Structure ###
    #################
    #################

    menuItem("Initialization", icon = icon("gears",
                                      verify_fa = FALSE), tabName = "INI"),


    ###################
    ###################
    ### Simulations ###
    ###################
    ###################

    menuItem("Simulation", icon = icon("play",
                                       verify_fa = FALSE), tabName = "SIM"),
    
    ##################################
    ##################################
    ### Epidemiological parameters ###
    ##################################
    ##################################


    menuItem("Parameters", icon = icon("fas fa-sliders-h", verify_fa = FALSE), tabName = "PARAMS"),

    #############
    #############
    ### About ###
    #############
    #############

    menuItem("About", icon = icon("sticky-note", verify_fa = FALSE), tabName = "ABOUT") # icon could also be "book

  )



)
