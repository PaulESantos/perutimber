#' Code for the algorithms to exactly and fuzzy match names
#' Author: Bruno Vilela
#' The matching algorithm
#' @keywords internal
.pt_match_algorithm  <- function(splist_class, max.distance) {
  # N species
  n_sps <- nrow(splist_class)
  # N classes
  n_class <- ncol(perutimber::perutimber_sps_class)
  # Save results
  exact <- matrix(ncol = n_class, nrow = n_sps)
  # Loop across species
  for (i in 1:n_sps) {
    splist_class_i <- splist_class[i,]
    # Search genus position
    pos_genus_pre <- .pt_group_ind(splist_class_i[2],
                                     perutimber::tab_perutimber_position$genus,
                                     max.distance,
                                     only_one = FALSE,
                                     closest = TRUE)
    pos_genus <- .pt_genus_search(pos_genus_pre)
    if (!any(is.na(pos_genus))) {
      # Try exact match first
      exact[i,] <- .pt_exact_match(splist_class_i,
                                pos_genus,
                                n_class)
      # Try fuzzy
      if (any(is.na(exact[i, ]))) {
        exact[i,] <- .pt_fuzzy_match(splist_class_i,
                                  pos_genus,
                                  max.distance,
                                  n_class)
      }
    } else {
      # Fuzzy if did not find the genus
      exact[i, ] <- .pt_fuzzy_match(splist_class_i,
                                 pos_genus = NULL,
                                 max.distance,
                                 n_class)
    }

  }
  return(exact)
}




#-------------------------------------------------------#
#' Exact match function
#' @keywords internal
.pt_exact_match <- function(splist_class_i,
                         pos_genus,
                         n_class,
                         fuzzy = FALSE) {
  # Look the categories that are equal
  sp_pos <- apply(perutimber::perutimber_sps_class[pos_genus,-n_class,
                                       drop = FALSE],
                  1,
                  function(x) {
                    x == splist_class_i
                  })

  # Identify the actual number positions
  choosen <- which(sp_pos[3,])
  # Work for the when fuzzy
  if (fuzzy) {
    choosen <- 1:ncol(sp_pos)
  }
  n_choosen <- length(choosen)
  if (n_choosen == 0) {
    # No match found
    return(rep(NA, n_class))
  } else {
    # if there is more than one matched genus and epithet
    # keep the one with more matches across subcategories
    if (n_choosen > 1) {
      choosen <- choosen[which.max(colSums(sp_pos[, choosen]))]
    }

    # Pick matched species ID
    matched_sp <-
      perutimber::perutimber_sps_class[pos_genus, , drop = FALSE][choosen, "id"]

    # Concatenate with the matching info
    matched_result <- c(matched_sp, sp_pos[, choosen])

    return(matched_result)
  }
}

#-------------------------------------------------------#
#' Fuzzy matching function
#' @keywords internal
.pt_fuzzy_match <- function(splist_class_i,
                         pos_genus = NULL,
                         max.distance,
                         n_class) {
  # If we did not find an approximation of the genus
  fuzzy_match <- NULL
  if (!is.null(pos_genus)) {
    # Use the `agrep` function with the max.distance parameter
    name1 <- paste(splist_class_i[2], splist_class_i[3])
    name2 <- paste(perutimber::perutimber_sps_class[pos_genus, 2],
                   perutimber::perutimber_sps_class[pos_genus, 3])
    fuzzy_match <- agrep(name1,
                         name2,
                         max.distance = max.distance)
  }
  if (is.null(pos_genus) | length(fuzzy_match) == 0) {
    pos_genus <- 1:nrow(perutimber::perutimber_sps_class)
    # Use the `agrep` function with the max.distance parameter
    name1 <- paste(splist_class_i[2], splist_class_i[3])
    name2 <- paste(perutimber::perutimber_sps_class[, 2],
                   perutimber::perutimber_sps_class[, 3])
    fuzzy_match <- agrep(name1,
                         name2,
                         max.distance = max.distance)
  }

  if (length(fuzzy_match) == 0) {
    # No match found
    return(rep(NA, n_class))
  } else {
    # Keep the closest
    dist_names <- utils::adist(name1, name2[fuzzy_match])
    which_closest <- which(dist_names == min(dist_names))
    fuzzy_match <- fuzzy_match[which_closest]

    # Reuse the exact_match function, but look only for fuzzy matches
    pos_genus <-
      as.numeric(perutimber::perutimber_sps_class[pos_genus, "id"][fuzzy_match])
    n_pos_genus <- length(pos_genus)

    res_fuzzy <- matrix(nrow = n_pos_genus, ncol = n_class)

    for (i in 1:n_pos_genus) {
      res_fuzzy[i,] <- .pt_exact_match(splist_class_i,
                                    pos_genus[i],
                                    n_class,
                                    fuzzy = TRUE)
    }
    # keep only the ones with highest number of classes matches
    rights <- apply(res_fuzzy[,-1, drop = FALSE],
                    1,
                    function(x) {
                      sum(x == "TRUE")
                    })
    pos_genus2 <- which(rights == max(rights))

    # If more than one
    if (length(pos_genus2) > 1) {
      sub_tab <- perutimber::tab_perutimber[res_fuzzy[pos_genus2, 1],]
      pos_genus2 <- which(sub_tab$taxonomic_status == "Accepted")

      if (length(pos_genus2) == 0) {
        pos_genus2 <- 1
        warning(
          paste0(
            "More than one name was fuzzy matched for species ",
            name1,
            ". Only the first name was returned."
          ),
          call. = FALSE
        )
      } else {
        warning(
          paste0(
            "More than one name was fuzzy matched for species ",
            name1,
            ". Only the first accepted name was returned."
          ),
          call. = FALSE
        )
      }
    }
    return(res_fuzzy[pos_genus2[1],])
  }
}

