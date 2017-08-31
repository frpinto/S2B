#' identifies nodes with direct neigbohrs in seeds lists
#'
#' Function called by makes2btable to annotate S2B results
#' @param g igraph object with the network analyzed by S2B
#' @param A_index vector with seed ids
#' @param B_index vector with seed ids
#' @param nrep number of network randomizations (shufle egdes maintaining degree) used to compute specificity score
#' @return data frame with number of direct neighbors in both seed lists, the related number of crossbridges and its specificity score
#' @export

multineibstat2=function(g,A_index,B_index,nrep){

  pn1=A_index
  for (i in 1:length(A_index)){
    pn1=c(pn1,igraph::neighbors(g,A_index[i]))
  }
  pn1_tab=tabulate(pn1,nbins=igraph::gorder(g))

  pn2=B_index
  for (i in 1:length(B_index)){
    pn2=c(pn2,igraph::neighbors(g,B_index[i]))
  }
  pn2_tab=tabulate(pn2,nbins=igraph::gorder(g))

  prod12=pn1_tab*pn2_tab

  pvec=rep(0,igraph::gorder(g))
  deglist=igraph::degree(g)
  for (k in 1:nrep){
    rg=igraph::sample_degseq(deglist,method="vl")
    rpn1=A_index
    for (i in 1:length(A_index)){
      rpn1=c(rpn1,igraph::neighbors(rg,A_index[i]))
    }
    rpn1_tab=tabulate(rpn1,nbins=igraph::gorder(g))

    rpn2=B_index
    for (i in 1:length(B_index)){
      rpn2=c(rpn2,igraph::neighbors(rg,B_index[i]))
    }
    rpn2_tab=tabulate(rpn2,nbins=igraph::gorder(g))

    rprod12=rpn1_tab*rpn2_tab
    pvec=pvec+(prod12>rprod12)+0.5*(prod12==rprod12)
  }
  pvec=pvec/nrep
  pn_multi=which(prod12>=1)
  data.frame(neib=pn_multi,freq1=pn1_tab[pn_multi],freq2=pn2_tab[pn_multi],spec=pvec[pn_multi],prod=prod12[pn_multi],stringsAsFactors=F)
}
