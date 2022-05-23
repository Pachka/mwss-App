# Title
header <- shinydashboard::dashboardHeader(
  title = "Disease spread estimate",

  tags$li(a(href = 'https://www.cnam.eu/site-en/',
            img(src = 'logoCNAM.png',
                title = "Conservatoire national des arts et metiers", height = "30px"),
            style = "padding-top:10px; padding-bottom:10px;"),
          class = "dropdown"),

  tags$li(a(href = 'https://www.pasteur.fr/en',
            img(src = 'logoPasteur.png',
                title = "Pasteur Institut", height = "30px"),
            style = "padding-top:10px; padding-bottom:10px;"),
          class = "dropdown"),

  tags$li(a(href = 'https://www.inserm.fr/en/home/',
            img(src = 'logoInserm.png',
                title = "Institut national de la santé et de la recherche médicale", height = "30px"),
            style = "padding-top:10px; padding-bottom:10px;"),
          class = "dropdown"),

  tags$li(a(href = 'https://www.universite-paris-saclay.fr/en',
            img(src = 'logoUPS.svg',
                title = "Universite Paris-Saclay", height = "30px"),
            style = "padding-top:10px; padding-bottom:10px;"),
          class = "dropdown")
)
