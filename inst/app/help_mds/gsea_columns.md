# Columns description 

* pathway – name of the pathway as in ‘names(pathway)‘
* pval – an enrichment p-value
* padj – a BH-adjusted p-value
* ES – enrichment score, same as in Broad GSEA implementation
* NES – enrichment score normalized to mean enrichment of random samples of the same size
* nMoreExtreme‘ – a number of times a random gene set had a more extreme enrichment score value
* size – size of the pathway after removing genes not present in ‘names(stats)‘
* leadingEdge – vector with indexes of leading edge genes that drive the enrichment
