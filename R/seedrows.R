#' Find set of node ids in graph
#'
#' This function allows you to get a vector of node ids matching your input vector with strings of node identifiers. Identifiers not found in the network are ignored.
#' @param seed_graph igraph object of the network to be searched.
#' @param seedvec vector with strings of node identifiers.
#' @return rowindex vector of integers with the found node ids
#' @export

seedrows=function(seed_graph,seedvec){
  vertexlist=igraph::vertex_attr(seed_graph)
  seedrows=rep(0,length(seedvec))

  for (i in 1:length(seedvec)){
    if (any(vertexlist[[1]]==seedvec[i])){
      seedrows[i]=which(vertexlist[[1]]==seedvec[i], arr.ind=TRUE)
    }
  }

  rowindex=seedrows[seedrows>0]
  rowindex=rowindex[order(rowindex)]
  rowindex
}
