#' Organize table of results for S2B function
#'
#' Function called by S2B
#' @param complete_results data frame with S2B and specificity scores
#' @param apid_main igraph object with the network analyzed by S2B
#' @param A_index vector with seed ids
#' @param B_index vector with seed ids
#' @return data frame with S2B results and node annotation
makes2btable=function(complete_results,apid_main,A_index,B_index){

  multi_neib2=multineibstat2(apid_main,A_index,B_index,1)
  #symblist=vertex_attr(Hs_uniprot2symb_graph(apid_main))
  s2b_apid_2=data.frame(uniprot=complete_results[,1],S2B=complete_results[,2],S2Bspec=complete_results[,3],S2Bspec2=complete_results[,4],DA="candidate",ALSneib=0,SMAneib=0,bridges=0,bridgespec=0,stringsAsFactors=F)
  s2b_apid_2$DA[A_index]="Disease 1"
  s2b_apid_2$DA[B_index]="Disease 2"
  s2b_apid_2$DA[intersect(A_index,B_index)]="Disease 1/Disease 2"
  s2b_apid_2$ALSneib[multi_neib2$neib]=multi_neib2$freq1
  s2b_apid_2$SMAneib[multi_neib2$neib]=multi_neib2$freq2
  s2b_apid_2$bridges[multi_neib2$neib]=multi_neib2$prod
  s2b_apid_2$bridgespec[multi_neib2$neib]=multi_neib2$spec
  s2b_apid_2

}
