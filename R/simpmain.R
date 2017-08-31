#' Simplify network
#'
#' This function allows you to remove multiple edges, self-loops and keeps only main component.
#' @param seed_graph igraph object of the network to be simplified.
#' @return seed_graph_maincomp is an igraph object with the simplified main component of the input graph.

simpmain=function(seed_graph){
  seed_graph_simp=igraph::simplify(seed_graph)
  seed_comp=igraph::components(seed_graph_simp)#divide el grafo en subgrafos
  seed_graph_maincomp=igraph::induced_subgraph(seed_graph_simp,seed_comp[[1]]==1)
  seed_graph_maincomp
}
