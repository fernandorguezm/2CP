# Author: Fernando Rodríguez Marín
# Contact: fernando.rodriguez@ibvf.csic.es

library(tidyverse)
library(cluster)
library(pheatmap)
library(ggplotify)
library(officer)
library(rvg)

##########################################################################
##########################################################################
######################### HEATMAP FOLD-CHANGE ############################
##########################################################################
##########################################################################

Genes <- read.csv("../Data/Lists/HSP20-like.txt", sep = "\t", header=F)

FC_greensuc <- read.csv("../Data/Fold-changes/green_vs_WT.csv", sep = ";")

Genes <- na.omit(unique(Genes$V2))
fold.change <- data.frame(Genes)

i <- 1
for (i in 1:length(Genes)) {

  fold.change[i,2] <- FC_greensuc[FC_greensuc$X==Genes[i],]$logFC

}

rownames(fold.change) <- fold.change[,1]
fold.change <- fold.change[,-1, drop =F]
colnames(fold.change) <- c("2cpab green vs WT suc")

Annotation <-  read.csv("../Data/Lists/HSP20-like.txt", sep = "\t", header=F)
head(Annotation)

fold.change$Localization <- Annotation$V3

i <- 1
j <- 1
for (i in 1:nrow(fold.change)) {
  for (j in 1:nrow(Annotation)) {
    if (Annotation[j,]$V2==rownames(fold.change)[i]) {
      rownames(fold.change)[i] <- paste(rownames(fold.change)[i], Annotation[j,]$V1)
    }
  }
}


paletteLength <- 100


myColor <- c(colorRampPalette(c("white", "#b83326"))(paletteLength))

pht <- pheatmap(fold.change[,1, drop =F],
         fontsize = 20,
         breaks = seq(0,6, length.out=(paletteLength + 1)),
         cluster_cols = F,
         color=myColor,
         display_numbers = as.matrix(round((fold.change[,1, drop =F]),2)),
         border_color = "black",
         number_color = "black",
         annotation_row = fold.change[,2, drop=F]
)


pht <- ggplotify::as.ggplot(pht)
anyplot <- dml(ggobj = pht,
               .bg = "white",
               .pointsize = 10,
               .editable = TRUE)

doc <- read_pptx()
doc <- add_slide(doc, "Title and Content", "Office Theme")
doc <- ph_with(doc, anyplot, location = ph_location_fullsize())
fileout <- "../Figures/Heatmap_HSP20like.pptx"
print(doc,  target = fileout)

#### HSP70 family

Genes <- read.csv("../Data/Lists/HSP70.txt", sep = "\t", header=F)

FC_greensuc <- read.csv("../Data/Fold-changes/green_vs_WT.csv", sep = ";")

Genes <- na.omit(unique(Genes$V2))
fold.change <- data.frame(Genes)

i <- 1
for (i in 1:length(Genes)) {
  
  fold.change[i,2] <- FC_greensuc[FC_greensuc$X==Genes[i],]$logFC
  
}

rownames(fold.change) <- fold.change[,1]
fold.change <- fold.change[,-1, drop =F]
colnames(fold.change) <- c("2cpab green vs WT suc")

Annotation <-  read.csv("../Data/Lists/HSP70.txt", sep = "\t", header=F)
head(Annotation)

fold.change$Localization <- Annotation$V3

i <- 1
j <- 1
for (i in 1:nrow(fold.change)) {
  for (j in 1:nrow(Annotation)) {
    if (Annotation[j,]$V2==rownames(fold.change)[i]) {
      rownames(fold.change)[i] <- paste(rownames(fold.change)[i], Annotation[j,]$V1)
    }
  }
}


paletteLength <- 100


myColor <- c(colorRampPalette(c("white", "#b83326"))(paletteLength))

pht <- pheatmap(fold.change[,1, drop =F],
                fontsize = 20,
                breaks = seq(0,6, length.out=(paletteLength + 1)),
                cluster_cols = F,
                color=myColor,
                display_numbers = as.matrix(round((fold.change[,1, drop =F]),2)),
                border_color = "black",
                number_color = "black",
                annotation_row = fold.change[,2, drop=F]
)
pht <- ggplotify::as.ggplot(pht)
anyplot <- dml(ggobj = pht,
               .bg = "white",
               .pointsize = 10,
               .editable = TRUE)

doc <- read_pptx()
doc <- add_slide(doc, "Title and Content", "Office Theme")
doc <- ph_with(doc, anyplot, location = ph_location_fullsize())
fileout <- "../Figures/Heatmap_HSP70.pptx"
print(doc,  target = fileout)


#### HSP90family

Genes <- read.csv("../Data/Lists/HSP90.txt", sep = "\t", header=F)

FC_greensuc <- read.csv("../Data/Fold-changes/green_vs_WT.csv", sep = ";")

Genes <- na.omit(unique(Genes$V2))
fold.change <- data.frame(Genes)

i <- 1
for (i in 1:length(Genes)) {
  
  fold.change[i,2] <- FC_greensuc[FC_greensuc$X==Genes[i],]$logFC
  
}

rownames(fold.change) <- fold.change[,1]
fold.change <- fold.change[,-1, drop =F]
colnames(fold.change) <- c("2cpab green vs WT suc")

Annotation <-  read.csv("../Data/Lists/HSP90.txt", sep = "\t", header=F)
head(Annotation)

fold.change$Localization <- Annotation$V3

i <- 1
j <- 1
for (i in 1:nrow(fold.change)) {
  for (j in 1:nrow(Annotation)) {
    if (Annotation[j,]$V2==rownames(fold.change)[i]) {
      rownames(fold.change)[i] <- paste(rownames(fold.change)[i], Annotation[j,]$V1)
    }
  }
}


paletteLength <- 100


myColor <- c(colorRampPalette(c("white", "#b83326"))(paletteLength))

pht <- pheatmap(fold.change[,1, drop =F],
                cluster_cols = F,
                breaks = seq(0,6, length.out=(paletteLength + 1)),
                fontsize = 20,
                color=myColor,
                display_numbers = as.matrix(round((fold.change[,1, drop =F]),2)),
                border_color = "black",
                number_color = "black",
                annotation_row = fold.change[,2, drop=F]
)
pht <- ggplotify::as.ggplot(pht)
anyplot <- dml(ggobj = pht,
               .bg = "white",
               .pointsize = 10,
               .editable = TRUE)

doc <- read_pptx()
doc <- add_slide(doc, "Title and Content", "Office Theme")
doc <- ph_with(doc, anyplot, location = ph_location_fullsize())
fileout <- "../Figures/Heatmap_HSP90.pptx"
print(doc,  target = fileout)


#### HSP10 family

Genes <- read.csv("../Data/Lists/HSP100.txt", sep = "\t", header=F)

FC_greensuc <- read.csv("../Data/Fold-changes/green_vs_WT.csv", sep = ";")

Genes <- na.omit(unique(Genes$V2))
fold.change <- data.frame(Genes)

i <- 1
for (i in 1:length(Genes)) {
  
  fold.change[i,2] <- FC_greensuc[FC_greensuc$X==Genes[i],]$logFC
  
}

rownames(fold.change) <- fold.change[,1]
fold.change <- fold.change[,-1, drop =F]
colnames(fold.change) <- c("2cpab green vs WT suc")

Annotation <-  read.csv("../Data/Lists/HSP100.txt", sep = "\t", header=F)
head(Annotation)

fold.change$Localization <- Annotation$V3

i <- 1
j <- 1
for (i in 1:nrow(fold.change)) {
  for (j in 1:nrow(Annotation)) {
    if (Annotation[j,]$V2==rownames(fold.change)[i]) {
      rownames(fold.change)[i] <- paste(rownames(fold.change)[i], Annotation[j,]$V1)
    }
  }
}


paletteLength <- 100


myColor <- c(colorRampPalette(c("white", "#b83326"))(paletteLength))

pht <- pheatmap(fold.change[,1, drop =F],
                cluster_cols = F,
         breaks = seq(0,6, length.out=(paletteLength + 1)),
                fontsize = 20,
                color=myColor,
                display_numbers = as.matrix(round((fold.change[,1, drop =F]),2)),
                border_color = "black",
                number_color = "black",
                annotation_row = fold.change[,2, drop=F]
)
pht <- ggplotify::as.ggplot(pht)
anyplot <- dml(ggobj = pht,
               .bg = "white",
               .pointsize = 10,
               .editable = TRUE)

doc <- read_pptx()
doc <- add_slide(doc, "Title and Content", "Office Theme")
doc <- ph_with(doc, anyplot, location = ph_location_fullsize())
fileout <- "../Figures/Heatmap_HSP100.pptx"
print(doc,  target = fileout)


