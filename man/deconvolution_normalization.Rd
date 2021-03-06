% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/single_cell_analysis.R
\name{deconvolution_normalization}
\alias{deconvolution_normalization}
\title{Deconvolution Normalization for Bioinformatics Pre-Processing}
\arguments{
\item{data}{A text delimited data file in standard bioinformatics format.}

\item{target_dir}{The specific local target directory to write out plotting to.}

\item{plots}{A logical paramater specifying whether to return plotting.}
}
\value{
ave.counts The average counts from the SCEset

numcells The number of expressions from the SCEset
}
\description{
A function used for pre-processing standard bioinformatics file formats into a workable format in the R environment.
}
\author{
Anushi Shah
}
