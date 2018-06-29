#' Find set of node ids in graph
#'
#' This function allows you to get a vector of node ids matching your input vector with strings of node identifiers. Identifiers not found in the network are ignored.
#' @param seed_graph igraph object of the network to be searched.
#' @param seedvec vector with strings of node identifiers.
#' @return rowindex vector of integers with the found node ids
#' @export

seedrows=function(seed_graph,seedvec){
  vertexlist=unlist(igraph::vertex_attr(seed_graph))
  rowindex=which(is.element(vertexlist,seedvec))
}
