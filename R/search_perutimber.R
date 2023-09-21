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
#' @return A character vector that can have three different output values. The first
#' value, "Present," indicates whether the species name is fully matched with the
#' names listed in the 'Catalogue of the timber forest species of the Amazon and the
#' Peruvian Yunga'. The second value, "P_updated_name," provides a fuzzy matching of
#' species names. If a species name is not listed in the catalogue, the third value
#' returned will be an empty string.
#'
#' @export
#'
#' @examples
#'
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
#'
search_perutimber <- function(splist, max_distance = 0.2){
  result <- search_pt(splist = splist, max_distance = max_distance)
  compara <- result$accepted_name == result$name_submitted
  output_result <- vector()
  for(i in seq_along(splist)){

    if( isTRUE(compara[i] == TRUE)){
      output_result[i] <- "Present"
    }
    else if(isTRUE(compara[i] == FALSE)){
      output_result[i]<- "P_updated_name"
    }
    else if(is.na(compara[i])){
      output_result[i] <- ""
    }
  }
  return(output_result)
}


