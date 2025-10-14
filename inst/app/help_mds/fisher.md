# Enrichment analysis - Fisher test

Over representation analysis is a widely used approach to determine whether known biological functions or processes are over-represented (= enriched) in an experimentally-derived gene list, e.g. a list of differentially expressed genes (DEGs).

Fisher test is widely used to measure the gene-enrichment in annotation terms : when members of two independent groups can fall into one of two mutually exclusive categories, Fisher Exact test is used to determine whether the proportions of those falling into each category differs by group.

**A hypothetical example**

In human genome background (30,000 gene total), 40 genes are involved in p53 signaling pathway. A given gene list has found that 3 out of 300 belong to p53 signaling pathway. Then  we ask the question if 3/300 is more than random chance comparing to the human background of 40/30000.

A 2x2 contingency table is built on above numbers:

|                | User Genes          |  Genome |
|----------------|:-------------------:|:-------:|
|In pathway      |3                    |40       |
|Not in pathway  |297                  |29960    |

<br/>
Fisher Exact P-Value =  0.008. Since P-Value <= 0.01, this user gene list is specifically associated (enriched) in p53 signaling pathway than random chance.


