#' S2B threshold
#'
#' This function defines the S2B threshold that separates nodes with high/low S2B score
#' @param s2bvec vector of S2B scores obtained with S2B or subS2B.
#' @return s2bt - numeric value of the S2B threshold

s2bthreshold=function(s2bvec){
  n=length(s2bvec)
  x=max(s2bvec)*(1:n)/n
  y=sort(s2bvec)
  distvec=sqrt((x-max(s2bvec))^2+y^2)
  s2bt=y[which.min(distvec)]
  s2bt
}