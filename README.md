APPLI shiny analyse RNA Seq QUBY from DAC ICM : 
>* https://gitlab.com/icm-institute/dac/public/quby_tutorials/-/blob/main/Quby_Bulk.md?ref_type=heads
>* https://gitlab.com/icm-institute/dac/web/qubyrsq

Repo avec correction bug sur les ACP, modification de certains plots et correction de la sélection des pathways pour enrichissement sur KEGG

# QubyRSQ: Interactive web-based application for trascriptomics data exploration and analysis

QubyRSQ is developed at ICM Data Analysis Core using the R-Shiny framework. Its goal is to facilitate RNAseq analysis via a user-friendly graphical interface and its usage does not require any programming experience. 

## 1. Installation

* Install R

https://cran.r-project.org/


### Dependencies that need to be installed manually

Open R and run the following commands:

```
install.packages("devtools") 
install.packages("remotes")
library("devtools")
install.packages("BiocManager")
BiocManager::install("DESeq2")
BiocManager::install("edgeR")
BiocManager::install("clusterProfiler")
BiocManager::install("org.Hs.eg.db")
BiocManager::install("org.Mm.eg.db")
BiocManager::install("org.Cf.eg.db")
BiocManager::install("org.Rn.eg.db")
BiocManager::install("EnsDb.Hsapiens.v86")
BiocManager::install("EnsDb.Mmusculus.v79")
BiocManager::install("EnsDb.Rnorvegicus.v79")

```

### Install QubyRSQ

```
remotes::install_gitlab("icm-institute/iconics/quby_rsq_open", subdir = "QubyRSQ")

```

### Downgrade shinydashboardPlus

`shinydashboardPlus` will be installed as a dependecy with QubyRSQ, however, due to major changes in shinydashboardPlus in >= 2.0.0, we need to get back to a previous version:


```
devtools::install_version("shinydashboardPlus", version="0.7.5",repos = "http://cran.us.r-project.org")

```

### Troubleshooting

A possible error during the installation:

```
(converted from warning) cannot remove prior installation of package ‘rlang’
```

Remove `rlang` package from Rlibraries. Use `find.package("rlang")` in R to find out where your package is located.

## 2. Launch the application

```
library(QubyRSQ)
run_quby_rsq()
```

## 3. Upload the data

* raw read counts: a simple file (csv, tsv, txt): rows corresponds to genes and columns corresponds to samples. First colum must contain the gene names, and the column name must be **Gene_name**. Blank space and special characters, other then `-`(dash), `_`(underscore) and `.`(dot), should generally be avoided in sample names as well as in file names. 

* sample plan: a simple file (csv, tsv, txt): this is the design information: first column must be `SampleID`, followed by a column with group assignments, and one or more additional columnswith other information about the samples: different grouping variables or covariates (age, sex, batch, treatment etc.)

* currently supported organisms: human, mouse, rat, pig, dog 

* click on `Upload and Normalize`: normalization is done using `cpm()` function from {edgeR} package and `vst()` or `varianceStabilizingTransformation()` from {DESeq2} package

* if your sample plan contains multiple columns beside the SampleID (eg. different groupingsof your samples), you can chose on which column you will want to base your analysis. 


## 4. Data exploration

### 4a. Expression data
* Global overview: select the unit (raw, cpm or vst) and visualize the global expression data for each sample. You can also download the expression table in any of the selected units. 
* Feature-specific: explore gene expressions by individual genes: by sample or by groups, see how the gene ranks among the other genes on the Data tab (Quantile ranks)


### 4b. Sample stratification

Run a Principal Component Analysis, and check weather you can identify any outliers among your samples. Click on `Run PCA` an see how your samples group together base on the first three principal components. Use `Add shapes by` to bring another variable onto the plot. You can exclude some of the potential outliers to see if it improves your classification. If you have multiple groups, you can run the PCA on a subset of the groups by excluding the others. If you suspect a batch effect in your data, you can perform a batch correction by adding the batch variable in `Correct batch effect by` and then re-running the analysis. 


## 5. Differential expression analysis

Differential analysis is done on two groups (group 1 vs group 2). Two methods are proposed DESeq2 and edgeR. Both methods can include covariates (if any available from the sample plan)

* Data Selection: on this tab we select the two groups to compare. From a statistical point of view, each group should have at least three individuals (three replicates). The function will work with less, however the results will lack credibility. Previously identified outliers can be excluded at this point. Before the differential analysis, a filter is applied on the expression data, to exclude genes that are not expressed. The default filter is: 1 CPM in at least 1 sample. 

* Analysis: on this tab we select the method: DESeq2 or edgeR, add covariates and filter the results by FDR (adjusted p-value), or p-value and log2 Fold Change. 

* For any further analysis (Enrichment or Set), the differential analysis needs to be saved by clicking on `Save results`. 

* Volcano plot: up- and down-regulated features displayed by their log2(FC) and the -log10(FDR) 

* Differentially expressed features - Summary: a summary of the up/down/not regulated features in form of a barplot

* Zoom on the volcano plot: use your mouse to draw a rectangle on the volcano plot (above) to discover the name of the corresponding genes of each of the points. 

* Selected feature: Click on the dot before the gene name on the zoomed plot to display the expression of the selected gene for each sample in form of a barplot at the right. 

* MA plot: similar to the volcano plot, this plot displays genes according to their expression. On the left hand site are the genes with the lowest, and on the right hand site the genes with the highest expressions. 

* Differential analysis summary: explore or download the results of the differential analysis. You can select the `Full data table`, or subset it to `Regulated features only`.

* Heatmap: gene expression profile for each sample considering the differentially expressed genes. Click on the wheel to change the parameters of the display. 
## 6. Set intersections

Search for common genes between the results of two or multiple differential analysis. According to the number of intersected sets, a Venn-diagram or an Upset-plot will be displayed. 

## 7. Enrichment analysis

* Over representation analysis

* Gene Set Enrichment Analysis (GSEA)
