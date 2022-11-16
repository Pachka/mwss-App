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

    menuItem("Structure", icon = icon("users",
                                      verify_fa = FALSE), tabName = "STR"),

    ##################################
    ##################################
    ### Epidemiological parameters ###
    ##################################
    ##################################


    menuItem("Parameters", icon = icon("fas fa-sliders-h", verify_fa = FALSE), tabName = "PARAMS"),

    ###################
    ###################
    ### Simulations ###
    ###################
    ###################

    menuItem("Simulation", icon = icon("play",
                                       verify_fa = FALSE), tabName = "SIM"),

    #############
    #############
    ### About ###
    #############
    #############

    menuItem("About", icon = icon("sticky-note", verify_fa = FALSE), tabName = "ABOUT") # icon could also be "book

  )



)
