# Author: Fernando Rodríguez Marín
# Contact: fernando.rodriguez@ibvf.csic.es

library(VennDiagram)
library(eulerr)
library(MetBrewer)
library(ggplot2)
library(officer)
library(rvg)

DEGs_green <- read.csv(file = "../Data/DEGs/DEGs_2cpabgreenSUC.vs.WTSUC.csv", sep = ";")
DEGs_albino <- read.csv("../Data/DEGs/DEGs_2cpabalbinoSUC.vs.WTSUC.csv", sep = ";")

## Generating Venn diagram of DEGs

VennDiag <- euler(c("A" =  length(setdiff(c(na.omit(DEGs_green$DEGs)), c(na.omit(DEGs_albino$DEGs)))), 
                    "B"= length(setdiff(c(na.omit(DEGs_albino$DEGs)), c(na.omit(DEGs_green$DEGs)))), 
                    "A&B"= length(intersect(c(na.omit(DEGs_albino$DEGs)), c(na.omit(DEGs_green$DEGs))))))
plot <- plot(VennDiag, fontsize=1000, cex=1000, alpha=1,
     fill=c("#9AB888","#D2D2D2", "#B7C5AD"),
     edges = list(col = c("black","black", "black"), lex = 2), 
     quantities = list(cex = 4.5),
     labels = c("", "", ""))

anyplot <- dml(ggobj = plot,
               .bg = "white",
               .pointsize = 10,
               .editable = TRUE)

anyplot <- ggplotify::as.ggplot(plot)
doc <- read_pptx()
doc <- add_slide(doc, "Title and Content", "Office Theme")
doc <- ph_with(doc, anyplot, location = ph_location_fullsize())
fileout <- "../Figures/Venndiagram_green_albino_DEGs.pptx"
print(doc,  target = fileout)


## Generating Venn diagram of upregulated genes

VennDiag <- euler(c("A" =  length(setdiff(c(na.omit(DEGs_green$activated.genes)), c(na.omit(DEGs_albino$activated.genes)))), 
                    "B"= length(setdiff(c(na.omit(DEGs_albino$activated.genes)), c(na.omit(DEGs_green$activated.genes)))), 
                    "A&B"= length(intersect(c(na.omit(DEGs_albino$activated.genes)), c(na.omit(DEGs_green$activated.genes))))))
plot <- plot(VennDiag, fontsize=1000, cex=1000, alpha=1,
             fill=c("#9AB888","#D2D2D2", "#B7C5AD"),
             edges = list(col = c("black","black", "black"), lex = 2), 
             quantities = list(cex = 4.5),
             labels = c("", "", ""))

anyplot <- dml(ggobj = plot,
               .bg = "white",
               .pointsize = 10,
               .editable = TRUE)

anyplot <- ggplotify::as.ggplot(plot)
doc <- read_pptx()
doc <- add_slide(doc, "Title and Content", "Office Theme")
doc <- ph_with(doc, anyplot, location = ph_location_fullsize())
fileout <- "../Figures/Venndiagram_green_albino_Up.pptx"
print(doc,  target = fileout)


## Generating Venn diagram of Downregulated genes

VennDiag <- euler(c("A" =  length(setdiff(c(na.omit(DEGs_green$repressed.genes)), c(na.omit(DEGs_albino$repressed.genes)))), 
                    "B"= length(setdiff(c(na.omit(DEGs_albino$repressed.genes)), c(na.omit(DEGs_green$repressed.genes)))), 
                    "A&B"= length(intersect(c(na.omit(DEGs_albino$repressed.genes)), c(na.omit(DEGs_green$repressed.genes))))))
plot <- plot(VennDiag, fontsize=1000, cex=1000, alpha=1,
             fill=c("#9AB888","#D2D2D2", "#B7C5AD"),
             edges = list(col = c("black","black", "black"), lex = 2), 
             quantities = list(cex = 4.5),
             labels = c("", "", ""))

anyplot <- dml(ggobj = plot,
               .bg = "white",
               .pointsize = 10,
               .editable = TRUE)

anyplot <- ggplotify::as.ggplot(plot)
doc <- read_pptx()
doc <- add_slide(doc, "Title and Content", "Office Theme")
doc <- ph_with(doc, anyplot, location = ph_location_fullsize())
fileout <- "../Figures/Venndiagram_green_albino_down.pptx"
print(doc,  target = fileout)
