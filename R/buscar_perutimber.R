#' Search plant names according to the Catalogue of the timber forest species of
#' the Amazon and the Peruvian Yunga.
#'
#' Allow plant search plant taxa names listed in the "Catalogue of the timber
#' forest species of the Amazon and the Peruvian Yunga". Connects to the data
#' listed in the catalog and validates if  species its present, removing
#' orthographic errors in plant names.
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
#' @param show_correct If TRUE, a column is added to the final result indicating
#' whether the binomial name was exactly matched (TRUE), or if it is misspelled
#' (FALSE).
#'
#'
#' @details
#'
#' The function tries to match a names in the "Catalogue of the timber forest
#' species of the Amazon and the Peruvian Yunga", which has a corresponding
#' accepted valid name (accepted_name). If the input name is a valid name,
#' it will be the duplicated in accepted_name column.
#'
#' The algorithm will first try to exactly match the binomial names provided in
#' `splist`. If no match is found, it will try to find the closest name given the
#' maximum distance defined in `max_distance`.
#' Note that only binomial names with valid characters are allowed in this
#' function.
#'
#' @return A data frame
#' See \code{\link[perutimber:tab_perutimber]{perutimber::tab_perutimber}} for more
#' details.
#'
#' @references Vásquez Martínez and Rojas Gonzáles (2022) titled "Catálogo de las
#' especies forestales maderables de la Amazonía y la Yunga Peruana" in Revista
#' Forestal del Perú 37(3, Número Especial): 5-138
#' https://revistas.lamolina.edu.pe/index.php/rfp/article/view/1956.
#'
#' @keywords internal
search_pt <- function(splist,
                        max_distance = 0.2,
                        show_correct = FALSE,
                        genus_fuzzy = FALSE,
                        grammar_check = FALSE,
                        progress_bar = FALSE) {
  #hasData() # Check if LCVP is installed
  # Defensive function here, check for user input errors
  if (is.factor(splist)) {
    splist <- as.character(splist)
  }
  .names_check(splist, "splist")

  # Fix species name
  splist_std <- .names_standardize(splist)

  # Classify splist
  splist_class <- .splist_classify(splist_std)

  # Check binomial
  .check_binomial(splist_class, splist)

  # Now match
  matching <- .match_algorithm(splist_class,
                               max_distance,
                               progress_bar = progress_bar,
                               genus_fuzzy = genus_fuzzy,
                               grammar_check = grammar_check)

  # Elaborate the return object
  ## Return Null if it did not find anything
  if (all(is.na(matching))) {
    result_final <- NULL
    ## Return the matrix with matched species
  } else {
    comb_match <- matching[, -(1:2), drop = FALSE]
    # keep homonyms to the warning
    ho_pos <- ncol(comb_match)
    homonyms <- as.logical(comb_match[, ho_pos])
    homonyms[is.na(homonyms)] <- FALSE
    comb_match <- comb_match[, -ho_pos, drop = FALSE]

    comb_match <- as.matrix(apply(comb_match, 2, as.logical))

    if (ncol(comb_match) == 1) { # If only one column, need to be transposed
      comb_match <- t(comb_match)
    }
    # Transform in data.frame
    comb_match <- as.data.frame(comb_match)
    names_col <-
      colnames(perutimber::perutimber_sps_class)[-c(1,
                                                    ncol(perutimber::perutimber_sps_class))]

    colnames(comb_match) <- paste(names_col, "match", sep = "_")

    result_final <- data.frame("name_submitted" = splist,
                               perutimber::tab_perutimber[matching[, 1], , drop = FALSE])

    # Add whether the searched name matched each class,
    # will be used in the summary function
    attributes(result_final)$match.names <- comb_match
    # Remove row names
    rownames(result_final) <- NULL
    # Warning more than one match
    if (any(homonyms)) {
      warning(
        paste0(
          "More than one name was matched for some species. ",
          "Only the first 'Accepted' (if present) name was returned. ",
          "Consider using the function fuzzy_search ",
          "to return all names for these species:\n",
          paste(result_final[homonyms, 1], collapse = ", ")
        ),
        call. = FALSE
      )
      attributes(result_final)$matched_mult <- result_final[homonyms, 1]
    }
  }

  # If no match, give a warning
  if (is.null(result_final)) {
    warning(paste0("No match found for the species list provided.",
                   " Try increasing the 'max_distance' argument."))
  } else {
    if (show_correct) {

      result_final$Correct <- rowSums(comb_match[, 1:2, drop = FALSE]) == 2
    }
  }
  return(result_final)
}
