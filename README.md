# S2B

The goal of S2B (Specific Specific Betweenness) is to prioritize nodes that are simultaneously associated with two diseases/processes, knowing a set of seed nodes associated with each disease/process separately. For that, S2B uses a biological network and searches for shortest paths connecting seeds from one of the diseases/processes to seeds of the other disease/process. Nodes that appear more frequently in these specific shortest paths have a higher S2B score. To avoid the selection of highly connected nodes that are not specifically related to both diseases/processes, S2B employs network and seed randomization procedures to evaluate the specificity of S2B scores.

## Installation

You can install S2B from github with:


``` r
# install.packages("devtools")
devtools::install_github("frpinto/S2B")
```

## Example

An S2B analysis needs as inputs 1) a biological interaction network and 2) two sets of seed proteins/genes. The package includes the necessary input variables to test S2B use:
* a3h2_graph.RData - contains a human protein-protein physical interaction network derived from [APID](apid.dep.usal.es/) and from [HuRI](http://interactome.baderlab.org/). This variable is an igraph object. Node names are UniProt identifiers.
* ALS_vec.RData - a vector of UniProt identifiers of proteins previously associated with Amyotrophic Lateral Sclerosis (ALS)
* SMA_vec.RData - a vector of UniProt identifiers of proteins previously associated with Spinal Muscular Atrophy (SMA)

The input network should have a single connected component without self loops or multiple edges linking the same pair of nodes. The function simpmain is included to extract the main connected component of the input network and removing self loops and multiple edges: 

``` r
a3h2_main = simpmain(a3h2_graph)
```

The function seedrows is necessary to check if the members of the seed sets are part of the network and finding their indexes in the nodes list:

``` r
als_index = seedrows(a3h2_main, ALS_vec)
sma_index = seedrows(a3h2_main, SMA_vec)
```

Finally, the S2B function can be applied:


``` r
results = S2B(a3h2_main,als_index,sma_index,100,100)
```
The last two inputs are the number of network and seed randomizations applied to estimate the specificity of S2B scores.
