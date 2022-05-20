addbuttonUI <- function(id, titletext){
  ns <- NS(id)
  actionButton(ns("addbut"), titletext,
               tags$i(
                 class = "fa fa-plus",
                 style = "color: rgb(0,166,90)"
               )
  )
}

editbuttonUI <- function(id, titletext){
  ns <- NS(id)
  actionButton(ns("editbut"), titletext,
               tags$i(
                 class = "fa fa-edit",
                 style = "color: rgb(0,166,90)"
               )
  )
}


deletebuttonUI <- function(id, titletext){
  ns <- NS(id)
  actionButton(ns("deletebut"), titletext,
               tags$i(
                 class = "fa fa-minus",
                 style = "color: rgb(255,0,0)"
               )
  )
}

