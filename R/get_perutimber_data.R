#' @title Get perutimber data
#'
#' @description
#' This function takes a species list and tries to match a name in the Catalogue
#' of the timber forest species of the Amazon and the Peruvian Yunga, subseting
#' information for each species. If the name_submitted is a valid name, it will
#' be the duplicated in accepted_name column, else the accepted_name column will
#' display the closest name given the maximum distance defined in `max_distance`
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
#' @return A table with the accepted name and catalog data of the species.
#'
#' @export
#'
#' @examples
#' splist <- c("Euterpe precatoria var. precatorio",
#'             "Welfia alfredi",
#'             "Hibiscus abelmoschus var. betulifolius")
#' get_perutimber_data(splist)
#'
get_perutimber_data <- function(splist, max_distance = 0.2){
  sps_result <- search_pt(splist = splist, max_distance = max_distance)
  submitted_names = sps_result[!is.na(sps_result$accepted_name), 1]#,
  names_accepted <-  sps_result[!is.na(sps_result$accepted_name), 8]#
  output_matrix <- matrix(nrow = length(submitted_names), ncol = 10 )
  for (i in seq_along(names_accepted)) {
    sps_x <- as.matrix(perutimber::perutimber_data[perutimber::perutimber_data$accepted_name == names_accepted[i],])
    output_matrix[i,] <- sps_x
  }
  colnames(output_matrix) <- c( "accepted_name", "accepted_name_author", "accepted_family",
                                "common_name", "accepted_name_rank", "habit",
                                "plant_height_m", "regions", "elevation_m",
                                "uses")
  result <- cbind(data.frame(names_submitted = submitted_names),
                  output_matrix)
  rownames(result) <- NULL
  return(result)
}
