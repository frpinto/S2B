#' S2B auxiliary function
#'
#' This function computes S2B scores, without specificity scores
#' @param seed_graph igraph object of the network to be searched.
#' @param index1 vector with integer node identifiers.
#' @param index2 vector with integer node identifiers.
#' @param meandist length threshold above which shortest paths are not included in S2B computation
#' @return allcount - vector with S2B scores for each node in the graph
#' @return smat1 - matrix with one row for each node in the network and a column for each seed in index1. If the matrix element (i,j) is 1, then index1[j] was connected to the other seed set through a shortest path going through node i, and shorter than meandist
#' @return smat2 - matrix similar to smat1 but reffering to index 2 seeds
#' @return maxS2B - theoretical maximum of absolute S2B scores for this network and seed sets

subS2B=function(seed_graph,index1,index2,meandist){
  betweencount=rep(0,igraph::gorder(seed_graph)) #lista del tama??o de seed_graph
  seedmat1=matrix(data=0,nrow=igraph::gorder(seed_graph),ncol=length(index1))
  seedmat2=matrix(data=0,nrow=igraph::gorder(seed_graph),ncol=length(index2))
  sp1=igraph::distances(seed_graph,v=index1,to=igraph::V(seed_graph))
  sp2=igraph::distances(seed_graph,v=index2,to=igraph::V(seed_graph))
  sp1[sp1==Inf]=igraph::vcount(seed_graph)
  sp2[sp2==Inf]=igraph::vcount(seed_graph)
  sp=sp1[,index2]
  maxbc=sum(sp>0 & sp<meandist)
  betweensub=union(index1,index2)
  for (i in 1:length(index1)){
    for (j in 1:length(index2)){
      m=sp1[i,index2[j]]
      if (m<meandist){
        sumsp=sp1[i,]+sp2[j,]
        nodelist=which(sumsp==m) # aquellos que cumplan la condicion, seran nodos presentes en un sh_path y seran a??adidos a nodelist
        betweencount[nodelist]=betweencount[nodelist]+1
        seedmat1[nodelist,i]=1
        seedmat2[nodelist,j]=1
      }else {
        nodelist=c(index1[i],index2[j])
        betweencount[nodelist]=betweencount[nodelist]+1
      }
    }
  }
  betweencount[index1]=betweencount[index1]-length(index2)
  betweencount[index2]=betweencount[index2]-length(index1)
  betweencount[intersect(index1,index2)]=betweencount[intersect(index1,index2)]+1
  betweencount=betweencount/maxbc
  list(allcount=betweencount,smat1=seedmat1,smat2=seedmat2,maxS2B=maxbc)
}
