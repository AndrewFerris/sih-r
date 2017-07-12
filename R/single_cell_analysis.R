#' @name deconvolution_normalization
#' 
#' @title Deconvolution Normalization for Bioinformatics Pre-Processing
#' 
#' @description A function used for pre-processing standard bioinformatics file formats into a workable format in the R environment.
#' 
#' @param data A text delimited data file in standard bioinformatics format.
#' @param target_dir The specific local target directory to write out plotting to.
#' @param plots A logical paramater specifying whether to return plotting.
#' 
#' @return ave.counts The average counts from the SCEset
#' @return numcells The number of expressions from the SCEset
#' 
#' @import ggbiplot ggfortify gplots pvclust plyr reshape scater edgeR scran d3heatmap DESeq2 statmod genefilter limSolve
#' 
#' @author Anushi Shah
#' 
#' @export
NULL

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