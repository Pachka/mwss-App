wardRemoveUI <- function(id, titletext) {
  ns <- NS(id)

  conditionalPanel(
    "output.atleastoneward == true",
  actionButton(ns("deletebut"), titletext,
               tags$i(
                 class = "fa fa-minus",
                 style = "color: rgb(255,0,0)"
               )
  )
  )
}

wardRemove  <- function(input, output, session, variable){

  observeEvent(input$deletebut,{
    # Show modal when button is clicked.

      ward_namesChoices <- variable$ward_names

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

    }
