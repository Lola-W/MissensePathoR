% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/enrichSNP.R
\name{enrichSNP}
\alias{enrichSNP}
\title{Pathway Enrichment Analysis Using g:Profiler}
\usage{
enrichSNP(diffOut)
}
\arguments{
\item{diffOut}{A TopTags object containing differential expression results
with columns 'FDR' and 'logFC'.}
}
\value{
A list containing enrichment results for up-regulated, down-regulated,
and all significantly thresholded genes.
}
\description{
This function performs pathway enrichment analysis on SNP data, identifying
up- and down-regulated genes based on thresholded over-representation analysis.
It utilizes g:Profiler (Raudvere et al. 2019) for functional mapping.
}
\details{
The function applies a significance threshold (FDR <= 0.05) to categorize genes
into up-regulated and down-regulated groups based on log fold change (logFC).
In edge cases where fewer than one gene is identified as significantly
differentially expressed, the function adjusts the gene lists accordingly:
- If no genes meet the significance criteria, the function stops and issues a warning.
- If only a few genes are identified as significant, the function expands the gene list
  based on fold changes(logFC) to ensure a minimum number(3) of genes are analyzed.
- Special handling is done to ensure a balanced representation of up- and down-regulated genes.

Pathway enrichment analysis is conducted using the g:Profiler tool,
specifically employing the GOST (gene set over-representation testing) method.
}
\examples{
\dontrun{
  enrichOut <- enrichSNP(diffOut)
}
}
\references{
Raudvere, Uku, et al. (2019). "G: Profiler: A Web Server for Functional Enrichment
Analysis and Conversions of Gene Lists (2019 Update)." Nucleic Acids Research,
47(W1): W191–98.
}
