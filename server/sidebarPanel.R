siedbarPanelconfig <- function(id){
  observeEvent(input$sidebarItemExpanded, {
    if(input$sidebarItemExpanded == "STR")
      updateTabItems(session, NS(id, "sidebarMenu"), selected = "hiddenSTR") else
        if(input$sidebarItemExpanded == "EPI")
          updateTabItems(session, NS(id, "sidebarMenu"), selected = "hiddenEPI") else
            if(input$sidebarItemExpanded == "SIM")
              updateTabItems(session, NS(id, "sidebarMenu"), selected = "hiddenSIM")

  })
}
