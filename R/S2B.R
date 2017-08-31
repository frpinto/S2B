#' S2B main function
#'
#' This function computes S2B scores, with specificity scores.
#' @param seed_graph igraph object of the network to be searched.
#' @param index1 vector with integer node identifiers.
#' @param index2 vector with integer node identifiers.
#' @param nrep number of randomizations (shuffle edges maintaining node degree) to compute specificity score 1
#' @param nrep2 number of randomizations (shuffle seed identity) to compute specificity score 2
#' @return s2btable - dataframe with with S2B and specificity scores for each node in the graph
#' @return seedmat1 - matrix with one row for each node in the network and a column for each seed in index1. If the matrix element (i,j) is 1, then index1[j] was connected to the other seed set through a shortest path going through node i, and shorter than meandist
#' @return seedmat2 - matrix similar to seedmat1 but reffering to index 2 seeds
#' @return maxS2B - theoretical maximum of absolute S2B scores for this network and seed sets
#' @export

S2B=function(seed_graph,index1,index2,nrep,nrep2){
  meandist=igraph::mean_distance(seed_graph)
  bt=subS2B(seed_graph,index1,index2,meandist)
  pbt=rep(0,gorder(seed_graph))
  nscore=rep(0,gorder(seed_graph))
  deglist=igraph::degree(seed_graph)
  if (nrep2>0){
    rbt_matrix2=matrix(nrow=length(bt$allcount),ncol=nrep2)
    for (i in 1:nrep2){
      rindex1=sample(igraph::gorder(seed_graph),length(index1),replace=FALSE)
      rindex2=sample(igraph::gorder(seed_graph),length(index2),replace=FALSE)
      rbt=subS2B(seed_graph,rindex1,rindex2,meandist)
      nscore[rbt$allcount<bt$allcount]=nscore[rbt$allcount<bt$allcount]+1
      rbt_matrix2[,i]=rbt$allcount
    }
    nscore=nscore/nrep2
  } else {
    rbt_matrix2=matrix()
  }
  if (nrep>0){
    rbt_matrix=matrix(nrow=length(bt$allcount),ncol=nrep)
    for (i in 1:nrep){
      rg=igraph::sample_degseq(deglist,method="vl")
      rbt=subS2B(rg,index1,index2,meandist)
      pbt[rbt$allcount<bt$allcount]=pbt[rbt$allcount<bt$allcount]+1
      rbt_matrix[,i]=rbt$allcount
    }
    pbt=pbt/nrep
  } else {
    rbt_matrix=matrix()
  }
  bigvertexlist=igraph::vertex_attr(seed_graph)
  allstat=data.frame(protein=bigvertexlist[[1]],bcount=bt$allcount,score=pbt, nscore=nscore) ####
  s2btable=makes2btable(allstat,seed_graph,index1,index2)
  list(s2btable=allstat,seedmat1=bt$smat1,seedmat2=bt$smat2,maxS2B=bt$maxS2B)
}
