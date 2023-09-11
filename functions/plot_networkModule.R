plot_network_UI <- function(id) {
  ns <- NS(id)

  plotOutput(ns("network_plot"))

}

# Export simulated trajectories at a Rda format
plot_network_serv <- function(input, output, session, variable, buildings) {
  # model()
  ns <- session$ns

  output$network_plot <- renderPlot({

    if((variable$ward_names %>% length) > 0){
    num_nodes <- variable$ward_names %>% length

    if(num_nodes == 1) {
      my_sociomatrix <- matrix(rep(0, num_nodes * num_nodes),
                               # edge values
                               nrow = num_nodes,
                               #nrow must be same as ncol
                               ncol = num_nodes)

      diag(my_sociomatrix) <- 0

      net <- as.network(
        x = my_sociomatrix,
        # the network object
        directed = TRUE,
        # specify whether the network is directed
        loops = FALSE,
        # do we allow self ties (should not allow them)
        matrix.type = "adjacency" # the type of input
      )

      network.vertex.names(net) <- variable$ward_names

      vertex_size <-
        as.numeric(variable$pop_size_P) + as.numeric(variable$pop_size_H)

      plot.network(
        net,
        # our network object
        vertex.col = "grey",
        # color nodes by gender
        vertex.cex = (vertex_size / max(vertex_size)) * 3,
        # size nodes by their age
        displaylabels = T,
        # show the node names
        label.pos = 0 # display the names directly over nodes
      )

    }

    if(num_nodes > 1) {

      if(buildings)
      plot_connectivity(
        matContact = variable$matContact,
        size = as.numeric(variable$pop_size_P) + as.numeric(variable$pop_size_H),
        vertexcexrate = 3,
        vertexcol = c(rep("red",3),
                      rep("blue",4),
                      rep("white",5),
                      rep("yellow",8),
                      rep("orange",9)),
        verbose = FALSE
      ) else
        plot_connectivity(
          matContact = variable$matContact,
          size = as.numeric(variable$pop_size_P) + as.numeric(variable$pop_size_H),
          vertexcexrate = 3,
          verbose = FALSE
        )
    }
    }

  })

}
