#' Search species name present in the  Catalogue of the timber forest species of
#' the Amazon and the Peruvian Yunga
#'
#' @description
#' This function takes a species list and tries to match a name in the Catalogue
#'  of the timber forest species of the Amazon and the Peruvian Yunga, checking
#'  if the name is listed in tha dataset.
#'
#' @param splist A character vector specifying the input taxon, each element
#' including genus and specific epithet and, potentially, infraspecific rank,
#' infraspecific name and author name.
#' Only valid characters are allowed (see \code{\link[base:validEnc]{base::validEnc}}).
#'
#' @param max_distance match when comparing the submitted name with the closest
#' name matches in the species listed in the "Catalogue of the timber forest
#' species of the Amazon and the Peruvian Yunga". The distance used is a generalized
#' Levenshtein distance that indicates the total number of insertions, deletions,
#' and substitutions allowed to match the two names. It can be expressed as an
#' integer or as the fraction of the binomial name.
#' For example, a name with length 10, and a max_distance = 0.1, allow only one
#' change (insertion, deletion, or substitution). A max_distance = 2, allows two
#' changes.
#'
#' @return A logical vector, TRUE indicates whether the species name is present in the
#'  'Catalogue of the timber forest species of the Amazon and the Peruvian Yunga',
#'   considering exact and fuzzy matching of species names. If the species name is
#'   not listed, the logical vector is FALSE.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Search for multiple species vector
#' splist <- c("Euterpe precatoria var. precatorio",
#'             "Welfia alfredi",
#'             "Hibiscus abelmoschus var. betulifolius")
#' get_perutimber_data(splist)
#'
#' # Search for multiple species data.frame
#' # base
#' df_splist <- data.frame(splist = splist)
#' df_splist$peutimber <- search_perutimber(df_splist$splist)
#' # dplyr
#' # df_splist <- data.frame(splist = splist)
#' # dplyr::mutate(df_splist,
#' #               perutimber = search_perutimber(splist))
#'
#' }
#'

search_perutimber <- function(splist, max_distance = 0.2){
  result <- pt_sps_search(splist = splist, max_distance = max_distance)
  return(as.character(!is.na(result$accepted_name)))
}


