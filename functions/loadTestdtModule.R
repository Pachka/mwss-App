loadTestdtUI <- function(id) {
  ns <- NS(id)

  selectizeInput(ns("structure"), "Level of clustering",
                 choices = setNames(
                   list("1", "2", "3"),
                   list("High clustering",
                        "Medium clustering",
                        "Low clustering")
                 ),
                 selected = NULL,
                 options = list(
                   placeholder = 'Select hospital structure',
                   onInitialize = I('function() { this.setValue(""); }')
                 )
  )
}

loadTestdt <- function(input, output, session, variable) {
  ns <- session$ns

  toreturn <- reactiveValues()

  observeEvent(input$structure, {
    if(input$structure %in% c("1", "2", "3")){

      # Highly clustered
      if(input$structure == "1"){
        toreturn$clustering <- 'highly clustered'
        network_input <- build_network(within_clust_lev = 0.9, between_clust_lev = 0.01, clust_ratio_inout = 0.95)}
      # Medium clustered
      if(input$structure == "2"){
        toreturn$clustering <- 'medium clustered'
        network_input <- build_network(within_clust_lev = 0.4, between_clust_lev = 0.05, clust_ratio_inout = 0.7)}
      # Homogeneous
      if(input$structure == "3"){
        toreturn$clustering <- 'homogeneous'
        network_input <- build_network(within_clust_lev = 0.2, between_clust_lev = 0.1, clust_ratio_inout = 0.2)}

      if (exists("network_input")) {
        # structure
        variable$ward_names = network_input$ward_names
        variable$pop_size_P = network_input$pop_size_P
        variable$pop_size_H = network_input$pop_size_H
        variable$nVisits = network_input$nVisits
        variable$LS = network_input$LS
        # Contacts
        variable$Hplanning = NULL
        variable$matContact = network_input$matContact
        # Immunity
        # variable$IMMstate = NULL
        # Epidemiological states // infections
        # variable$EPIstate = NULL
      }
    }


    })

  return(toreturn)

}
