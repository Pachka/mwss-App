###############
### sidebar ###
###############


sidebar <- dashboardSidebar(

  sidebarMenu(
    id = "sidebarMenu",

    menuItem("Presentation", icon = icon("book"), tabName = "PRS"),

    #################
    #################
    ### Structure ###
    #################
    #################

    menuItem("Structure", icon = icon("users"), tabName = "STR"),

    ##################################
    ##################################
    ### Epidemiological parameters ###
    ##################################
    ##################################


    menuItem("Parameters", icon = icon("fas fa-sliders-h"), tabName = "PARAMS"),

    ###################
    ###################
    ### Simulations ###
    ###################
    ###################

    menuItem("Simulation", icon = icon("play"), tabName = "SIM"),

    #############
    #############
    ### About ###
    #############
    #############

    menuItem("About", icon = icon("sticky-note"), tabName = "ABOUT") # icon could also be "book

  )



)
