###############
### sidebar ###
###############


sidebar <- dashboardSidebar(

  sidebarMenu(
    id = "sidebarMenu",

    menuItem("Presentation",
             icon = icon("book", verify_fa = FALSE),
             tabName = "PRS"),


    ################
    ### VERSIONS ###
    ################

    ###
    ### Sidebar
    ###
    menuItem("Simulations", icon = icon("play",
                                                verify_fa = FALSE),
                     tabName = "simulation_sidetab"),

    #############
    #############
    ### About ###
    #############
    #############

    menuItem("About", icon = icon("sticky-note", verify_fa = FALSE), tabName = "ABOUT") # icon could also be "book

  )



)
