library(devtools)
library(ggbiplot)
library(ggfortify)
library(gplots)
library(pvclust)
library(plyr)
library(reshape)
library(scater)
library(edgeR)
library(scran)
library(d3heatmap)
library(DESeq2)
library(statmod)
library(genefilter)
library(limSolve)

deconvolution_normalization <- function(data, target_dir, plots){
  
  # Load Gene expression data
  gene_exp_data <- read.table(data,header = TRUE)
  
  # Target directory path
  dir_path = target_dir
  
  # Initialize SCESet
  sce <- newSCESet(countData=gene_exp_data)
  
  # Identify ERCC spikes
  is.spike <- grepl("^ERCC", rownames(sce))
  
  # Quality control metrics
  sce <- calculateQCMetrics(sce, feature_controls=list(ERCC=is.spike))
  isSpike(sce) <- "ERCC"
  
  # Mention whether Spike in ERCC
  is.ercc <- isSpike(sce, type="ERCC")
  
  # Plot for smooth scatter
  ave.counts <- rowMeans(counts(sce))
  numcells <- nexprs(sce, byrow=TRUE)
  
  z <- list(ave.counts, numcells)
  return(z)
  
  if(plots == TRUE){
    smoothScatter(log10(ave.counts), numcells, xlab=expression(Log[10]~"average count"),
                  ylab="Number of expressing cells")
    points(log10(ave.counts[is.ercc]), numcells[is.ercc], col="red", pch=16, cex=0.5)
  }
}