###############
### sidebar ###
###############


sidebar <- dashboardSidebar(

  sidebarMenu(
    id = "sidebarMenu",

    menuItem("Presentation", icon = icon("book",
                                         verify_fa = FALSE), tabName = "PRS"),


    ################
    ### VERISONS ###
    ################

    menuItem("Version simple", icon = icon("play",
                                       verify_fa = FALSE), tabName = "Vsimp"),



    menuItem("Version expert", icon = icon("play",
                                           verify_fa = FALSE), tabName = "Vexp",
             menuSubItem("Initialization", icon = icon("gears", verify_fa = FALSE), tabName = "INI"),
             menuSubItem('Simulations', tabName = 'SIMexp'),
             menuSubItem("More parameters", icon = icon("fas fa-sliders-h", verify_fa = FALSE), tabName = "PARAMS")),

    #############
    #############
    ### About ###
    #############
    #############

    menuItem("About", icon = icon("sticky-note", verify_fa = FALSE), tabName = "ABOUT") # icon could also be "book

  )



)
