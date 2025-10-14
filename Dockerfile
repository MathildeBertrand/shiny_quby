FROM rocker/shiny-verse:4.0.3
MAINTAINER beata.gyorgy@icm-institute.org

RUN apt update && apt install -y libbz2-dev liblzma-dev libglpk-dev default-jdk default-jre

RUN install2.r shinydashboard shinycssloaders shinyjs shinyhelper shinyAce bsplus shinyBS shinydashboardPlus shinyalert shinyWidgets rintrojs \
DT data.table rJava reshape ggplot2 ggiraph RColorBrewer scatterplot3d plotly pheatmap heatmaply VennDiagram xlsx remotes ggupset \
BiocManager msigdbr FactoMineR ggnewscale ggridges \
markdown scales sessioninfo && \
R -e "BiocManager::install(c('clusterProfiler','edgeR','DESeq2','org.Hs.eg.db','org.Mm.eg.db','org.Rn.eg.db','org.Dr.eg.db','org.Ss.eg.db', 'ensembldb','EnsDb.Mmusculus.v79','EnsDb.Hsapiens.v86','EnsDb.Rnorvegicus.v79','Gviz','enrichplot'))"

ENV LD_LIBRARY_PATH=/usr/lib/jvm/jre/lib/amd64:/usr/lib/jvm/jre/lib/amd64/default/usr/lib/jvm/jre/lib/amd64:/usr/lib/jvm/jre/lib/amd64/default

RUN ["installGithub.r", "krassowski/complex-upset@d4f87557c1465a5600147d94e2d971b1c65fa8da"]

RUN install2.r waiter

COPY Rprofile.site /usr/lib/R/etc
RUN mkdir /usr/app
COPY app /usr/app

EXPOSE 3838

CMD ["R","-e","shiny::runApp('/usr/app',host='0.0.0.0',port=3838)"]

