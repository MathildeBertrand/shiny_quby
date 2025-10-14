# Volcano plot description

Volcano plots are commonly used to display the results of RNA-seq or other omics experiments. A volcano plot is a type of scatterplot that shows statistical significance (P value or FDR) versus magnitude of change (log2 fold change). It enables quick visual identification of genes with large fold changes that are also statistically significant. These may be the most biologically significant genes. In a volcano plot, the most upregulated genes are towards the right, the most downregulated genes are towards the left, and the most statistically significant genes are towards the top.

Feature in orange are up-regulated in group2 vs group1, whereas features in blue are donw-regulated in group2 vs group1, according to the selected log2 fold change and p-value threshold.
You can zoom on the plot and the feature names will appear on `Feature selection` plot below volcano plot. Then, just click on a feature to see the CPM in your samples.

You can click on the `Save` button to save deregulated features and enable enrichment analysis with Fisher test in the side bar panel.
