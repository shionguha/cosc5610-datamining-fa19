library('igraph')

# Independent Cascade model
#   - graph: igraph object
#   - activated: list of initially activated nodes
IC <- function (g, activated) {
  # Defining the graph layout to preserve it
  # the same throughout the visualization
  l <- layout.fruchterman.reingold(g)
  # Setting the activated nodes
  V(g)$activated <- F
  for (v in activated) {
    V(g)[v]$activated <- T
  }
  # Marking all activations (edges) as "not yet tried"
  E(g)$tried <- F
  possible.activations = ecount(g)
  # The process goes on until there are possible activations
  while(possible.activations > 0) {
    # Network visualization (at each simulation step)
    V(g)$color <- ifelse(V(g)$activated, "red", "lightblue")
    plot(g, layout=l, edge.width=E(g)$weight*5)
    # Iterating through activated nodes
    for(v in V(g)) {
      if(V(g)[v]$activated) {
        # Finding activations for each note that have not been tried yet
        for(w in neighbors(g, V(g)[v]$name, mode="out")) {
          e <- E(g)[from(V(g)[v]$name) & to(V(g)[w]$name)]
          if (! e$tried) {
            # Activation attempt
            if (runif(1, 0, 1) <= e$weight) {
              V(g)[w]$activated <- T
            }
            e$tried <- T
            possible.activations <- possible.activations - 1
          }
        }
      }
    }
  }
  # Network visualization after the process has terminated
  V(g)$color <- ifelse(V(g)$activated, "red", "lightblue")
  plot(g, layout=l, edge.width=E(g)$weight*5)
}

