% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/S2B.R
\name{S2B}
\alias{S2B}
\title{S2B main function}
\usage{
S2B(seed_graph, index1, index2, nrep, nrep2)
}
\arguments{
\item{seed_graph}{igraph object of the network to be searched.}

\item{index1}{vector with integer node identifiers.}

\item{index2}{vector with integer node identifiers.}

\item{nrep}{number of randomizations (shuffle edges maintaining node degree) to compute specificity score 1}

\item{nrep2}{number of randomizations (shuffle seed identity) to compute specificity score 2}
}
\value{
s2btable - dataframe with with S2B and specificity scores for each node in the graph

seedmat1 - matrix with one row for each node in the network and a column for each seed in index1. If the matrix element (i,j) is 1, then index1[j] was connected to the other seed set through a shortest path going through node i, and shorter than meandist

seedmat2 - matrix similar to seedmat1 but reffering to index 2 seeds

maxS2B - theoretical maximum of absolute S2B scores for this network and seed sets
}
\description{
This function computes S2B scores, with specificity scores.
}
