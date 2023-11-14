#' VCF Sample Data from the MissensePathoR Package
#'
#' This dataset contains combined VCF data for Hela cell replicates across four time points
#' (0, 1, 4, and 8 hours) after introducing H2O2, processed with the `readVCF` function from
#' the MissensePathoR package. Chromosomes 1-5 are included, representing a subset of the
#' human genome.
#'
#' @source Generated from VCF files processed using the MissensePathoR package.
#' From GSE113171 from Gene Expression Omnibus
#'
#' @references
#' Rendleman J, Cheng Z, Maity S, et al. New insights into the cellular
#' temporal response to proteostatic stress. Elife. 2018;7:e39054.
#' doi: 10.7554/eLife.39054.
#'
#' @format A data table with 9 columns:
#' \describe{
#'   \item{CHROM}{Chromosome number where the variant is located.}
#'   \item{POS}{The position of the variant on the chromosome.}
#'   \item{ID}{The identifier of the variant, if available.}
#'   \item{REF}{The reference allele at the position.}
#'   \item{ALT}{The alternate allele observed in the sample.}
#'   \item{QUAL}{Quality score for the variant calling.}
#'   \item{FILTER}{Filter status of the variant calling.}
#'   \item{group}{The experimental group, here indicating the time point after H2O2 introduction.}
#'   \item{sample_name}{The name of sample, here is the replicate from the experiment.}
#' }
#' @examples
#' \dontrun{
#'  data(vcfSample)
#'  head(vcfSample)
#' }
"vcfSample"

#' AlphaMissense Sample Dataset
#'
#' A sample dataset containing missense variant predictions from the AlphaMissense
#' community dataset resource. This dataset is a small extract from a larger
#' collection of 71 million missense variant predictions that saturate the human proteome.
#' Each variant is characterized by a single nucleotide change resulting in an altered
#' amino acid. This sample includes information on the chromosomal location, reference and
#' alternate alleles, genome build, protein identifiers, and the predicted pathogenicity
#' and class of the variants.
#'
#' @format A data table with 6 variables:
#' \describe{
#'   \item{CHROM}{Chromosome of the variant (with 'chr' prefix).}
#'   \item{POS}{Position of the variant on the chromosome.}
#'   \item{REF}{Reference allele.}
#'   \item{ALT}{Alternate allele.}
#'   \item{genome}{Genome build, here 'hg38'.}
#'   \item{uniprot_id}{UniProt identifier for the protein.}
#'   \item{transcript_id}{Transcript identifier.}
#'   \item{protein_variant}{Amino acid change resulting from the variant.}
#'   \item{am_pathogenicity}{Pathogenicity score for the variant.}
#'   \item{am_class}{Classification of the variant, e.g., 'likely_benign'.}
#' }
#' @source
#' Jun Cheng et al., "Accurate proteome-wide missense variant effect prediction
#' with AlphaMissense." Science 381, eadg7492 (2023). DOI: 10.1126/science.adg7492.
#' The larger dataset includes 22.8 million variants classified as likely pathogenic
#' and 40.9 million as likely benign.
#' @keywords datasets
#' @examples
#' data("AlphaMissenseSample")
#' head(AlphaMissenseSample)
"AlphaMissenseSample"

#' Example Predicted Pathogenicity Scores
#'
#' This dataset contains a sample of predicted pathogenicity scores obtained from the
#' `predictPathoScore` function. It includes a subset of variants with their respective
#' pathogenicity predictions and classifications.
#'
#' @format A data table with 3848 rows and 13 columns.
#' \describe{
#'   \item{sample_name}{\code{character} Sample identifier}
#'   \item{group}{\code{character} Group classification}
#'   \item{CHROM}{\code{character} Chromosome where the variant is located}
#'   \item{POS}{\code{integer} Position of the variant on the chromosome}
#'   \item{REF}{\code{character} Reference allele}
#'   \item{ALT}{\code{character} Alternate allele representing the variant}
#'   \item{genome}{\code{character} Genome build (e.g., hg38)}
#'   \item{uniprot_id}{\code{character} UniProt identifier for the associated protein}
#'   \item{transcript_id}{\code{character} Transcript identifier}
#'   \item{protein_variant}{\code{character} Protein variant notation}
#'   \item{am_pathogenicity}{\code{numeric} AlphaMissense pathogenicity score}
#'   \item{am_class}{\code{character} Pathogenicity classification}
#'   \item{QUAL}{\code{numeric} Quality score of the variant call}
#' }
#' @source Generated using `predictPathoScore` function from the MissensePathoR package.
#' @examples
#' data(predScoreSample)
#' head(predScoreSample)
"predScoreSample"



