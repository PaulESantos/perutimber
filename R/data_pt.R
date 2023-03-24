#' List of Plant Species Name accordingly with the Catalogue of the timber forest
#' species of the Amazon and the Peruvian Yunga.
#'
#' The 'tab_perutimber' contains records belonging to all the timber forest
#' species the Amazon and the Peruvian Yunga.
#'
#' @name tab_perutimber
#' @docType data
#' @format A tibble with the following columns:
#'   \describe{\item{id_cat}{The fixed species id of the input taxon in Catalogue of the timber forest species of the Amazon and the Peruvian Yunga - CTSAPY.}
#'   \item{input_genus}{A character vector. The input genus of the corresponding species name listed in CTSAPY.}
#'   \item{input_epitheton}{A character vector. The specific epithet of the corresponding species name listed in CTSAPY.}
#'   \item{rank}{A character vector. The taxonomic rank: "species","subspecies", "variety", of the corresponding species name listed in CTSAPY.}
#'   \item{input_subspecies_epitheton}{A character vector. If the indicated rank is below species, the subspecies epithet input of the corresponding species name listed in CTSAPY.}
#'   \item{taxonomic_status}{A character vector. description if a taxon is classified as ‘accepted’, ‘synonym’, ‘no opinion’. According to the Taxonomic Name Resolution Service - TNRS.}
#'   \item{accepted_name}{A character vector. The list of the accepted plant taxa names according to the Taxonomic Name Resolution Service - TNRS.}
#'   \item{accepted_family}{A character vector. The corresponding family name of the accepted_name.}
#'   \item{accepted_name_author}{A character vector. The corresponding author name of the accepted_name, staying empty if the taxonomic_status is "Synonym" or "No opinion".}
#'   \item{tnrs_name_id}{A character vector. The accepted_name_id according to the Taxonomic Name Resolution Service - TNRS output.}
#'   \item{accepted_name_url}{A character vector. The accepted_name information url.}
#'   \item{source}{A character vector. The accepted_name information source.}}
#'
#' @references
#' Catalogue of the timber forest species of the Amazon and the Peruvian Yunga.
#' Boyle, B. L., Matasci, N., Mozzherin, D., Rees, T., Barbosa, G. C.,
#'  Kumar Sajja, R., & Enquist, B. J. (2021). Taxonomic Name Resolution Service,
#'  version 5.0. In Botanical Information and Ecology Network. https://tnrs.biendata.org/ ,
#'  Accessed Mar 21, 2023.
#'
#' @keywords datasets
#' @examples
#'
#' data(tab_perutimber)
#' str(tab_perutimber)
#'
"tab_perutimber"

#' List of species name in tab_perutimber separeted by category
#'
#' The 'perutimber_sps_class' includes all species separeted by genus, epithet, author,
#' subspecies, variety, and id (position in the
#' \code{tab_perutimber}).
#'
#' @name perutimber_sps_class
#' @docType data
#' @format A data.frame.
#' @keywords datasets
#' @examples
#'
#' data(perutimber_sps_class)
#'
"perutimber_sps_class"
#' List of the number positions of the first 3 letters of the species name in
#' the tab_perutimber
#'
#' The 'tab_perutimber_position' reports the
#' position (in term of number of rows) of the first three letters (triphthong)
#' for the plant names stored in the variable 'accepted_name' of the table
#' 'tab_perutimber'. This indexing system speeds up of the search on the
#' largest list using the package.
#'
#'
#' @name tab_perutimber_position
#' @docType data
#' @format A data frame with 399 observations on the following 3 variables.
#' \describe{
#'  \item{position}{A character vector. It is the position of
#' the first 3 letters of the species name in the tab_perutimber.}
#' \item{triphthong}{A character vector. First 3 letters of the species
#' name in the tab_perutimber.}
#' \item{genus}{A character vector.
#' Corresponding Genus name.} }
#'
#' @keywords datasets
#' @examples
#'
#' data(tab_perutimber_position)
#' str(tab_perutimber_position)
"tab_perutimber_position"

