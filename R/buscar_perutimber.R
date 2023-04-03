
lcvp_search <- function(splist,
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

    result_final <- data.frame("Search" = splist,
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
          "Only the first 'accepted' (if present) name was returned. ",
          "Consider using the function lcvp_fuzzy_search ",
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
