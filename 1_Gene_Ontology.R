# Author: Fernando Rodríguez Marín
# Contact: fernando.rodriguez@ibvf.csic.es

library(clusterProfiler)
library(org.At.tair.db)
library(TxDb.Athaliana.BioMart.plantsmart28)
library(enrichplot)
library(ggplot2)
library(officer)
library(rvg)
library(xlsx)


genes <- as.data.frame(genes(TxDb.Athaliana.BioMart.plantsmart28))
genes_names <- genes$gene_id
length(genes_names)

DEGs_green <- read.csv(file = "../Data/DEGs/DEGs_2cpabgreenSUC.vs.WTSUC.csv", sep = ";")
DEGs_albino <- read.csv("../Data/DEGs/DEGs_2cpabalbinoSUC.vs.WTSUC.csv", sep = ";")

## Creating a function to represent GO barplots 

Generate_barplot_GO <- function(gene.set, name, size_letters){
  
ego <- enrichGO(gene = gene.set, OrgDb = org.At.tair.db, ont = "BP", pvalueCutoff = 0.05, 
                universe = genes_names, keyType = "TAIR")
ego.res <- as.data.frame(ego)

write.xlsx(ego.res, paste("../Figures/", name, ".xlsx", sep = ""), sheetName = "Sheet1", 
           col.names = TRUE, row.names = FALSE, append = FALSE)

plot <- barplot(ego, showCategory=10, color = "pvalue") + 
  scale_fill_continuous(low="#b83326", high="#5773c0", name = "p-value",
                        guide=guide_colorbar(reverse=T)) +
  xlab(label = "Number of genes") +
  theme(axis.text.y = element_text(angle = 0, size = size_letters),
        axis.text.x = element_text(angle = 0, size = size_letters),
        axis.title.x = element_text(size = size_letters),
        text = element_text(size = size_letters),
        panel.background = element_rect(fill = "transparent",
                                        colour = NA_character_), # necessary to avoid drawing panel outline
        panel.grid.major = element_blank(), # get rid of major grid
        panel.grid.minor = element_blank(), # get rid of minor grid
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_), # necessary to avoid drawing plot outline
        legend.background = element_blank(),
        legend.box.background = element_blank(),
        rect = element_rect(fill = NA),
        legend.key = element_rect(fill = "transparent"))

anyplot <- dml(ggobj = plot,
               .bg = "white",
               .pointsize = 10,
               .editable = TRUE)

doc <- read_pptx()
doc <- add_slide(doc, "Title and Content", "Office Theme")
doc <- ph_with(doc, anyplot, location = ph_location_fullsize())
fileout <- paste("../Figures/", name, ".pptx", sep = "")
print(doc,  target = fileout)

}

## GO enrichment exclusive upregulated genes in green phenotype

Generate_barplot_GO(gene.set = setdiff(c(na.omit(DEGs_green$activated.genes)), c(na.omit(DEGs_albino$activated.genes))),
                    name = "Exclusive_up_green",
                    size_letters = 20)

## GO enrichment exclusive upregulated genes in albino phenotype

Generate_barplot_GO(gene.set = setdiff(c(na.omit(DEGs_albino$activated.genes)),c(na.omit(DEGs_green$activated.genes))),
                    name = "Exclusive_up_albino",
                    size_letters = 20)

## GO enrichment exclusive downregulated genes in green phenotype

Generate_barplot_GO(gene.set = setdiff(c(na.omit(DEGs_green$repressed.genes)), c(na.omit(DEGs_albino$repressed.genes))),
                    name = "Exclusive_down_green",
                    size_letters = 20)

## GO enrichment exclusive downregulated genes in albino phenotype

Generate_barplot_GO(gene.set = setdiff(c(na.omit(DEGs_albino$repressed.genes)),c(na.omit(DEGs_green$repressed.genes))),
                    name = "Exclusive_down_albino",
                    size_letters = 20)

## GO enrichment common upregulated genes

Generate_barplot_GO(gene.set = intersect(c(na.omit(DEGs_green$activated.genes)), c(na.omit(DEGs_albino$activated.genes))),
                    name = "Common_up",
                    size_letters = 20)

## GO enrichment common downregulated genes

Generate_barplot_GO(gene.set = intersect(c(na.omit(DEGs_albino$repressed.genes)),c(na.omit(DEGs_green$repressed.genes))),
                    name = "Common_down",
                    size_letters = 20)

## GO enrichment upregulated genes in green phenotype

Generate_barplot_GO(gene.set =c(na.omit(DEGs_green$activated.genes)),
                    name = "Up_green",
                    size_letters = 20)

## GO enrichment upregulated genes in albino phenotype

Generate_barplot_GO(gene.set = c(na.omit(DEGs_albino$activated.genes)),
                    name = "Up_albino",
                    size_letters = 20)

## GO enrichment downregulated genes in green phenotype

Generate_barplot_GO(gene.set =c(na.omit(DEGs_green$repressed.genes)),
                    name = "Down_green",
                    size_letters = 20)

## GO enrichment downregulated genes in albino phenotype

Generate_barplot_GO(gene.set =c(na.omit(DEGs_albino$repressed.genes)),
                    name = "Down_albino",
                    size_letters = 20)
