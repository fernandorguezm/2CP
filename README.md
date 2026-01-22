# Scripts used for RNA-seq processing of the manuscript 2-Cys peroxiredoxins and chaperone cpHSP70 act concertedly in chloroplast biogenesis in Arabidopsis seedlings

We can find two folders:
- Data: where we find the DEGs, fold-change values of the comparisons, and lists of AGIs used to do the plots. 
- Scripts: Scripts used for RNA-seq processing of the manuscript "2-Cys peroxiredoxins and chaperone cpHSP70 act concertedly in chloroplast biogenesis in Arabidopsis seedlings". The scripts used to process raw RNA-seq data from fastq format to gtf files belong to Francisco Romero Campero (see https://github.com/fran-romero-campero/miscellanomics/tree/master/RNA-seq). This includes the generation of reference genome indexes, quality analysis, mapping of reads to the reference genome, and transcript quantification, as well as differential expression analysis in Ballgown.
  * 1_Gene_Ontology.R: Script used for enrichment in terms of gene ontology and barplot generation.
  * 2_Venn_diagrams.R: Script used for Venn diagram generation comparing green and albino DEGs.
  * 3_Clustering_heatmap.R: Script used for the generation of HSPs heatmaps using the list of AGIs (../Data/Lists) and fold-changes (../Data/Fold-changes)
