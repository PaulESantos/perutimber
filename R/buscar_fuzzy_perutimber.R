

lcvp_fuzzy_search <- function(splist,
                              max_distance = 0.2,
                              genus_fuzzy = FALSE,
                              status = c( "Accepted",
                                          "Synonym",
                                          "No opinion"),
                              bind_result = TRUE,
                              keep_closest = TRUE,
                              progress_bar = FALSE) {
  #hasData() # Check if LCVP is installed
  # Defensive functions, check for user input errors
  ## Change factors in characters
  if (is.factor(splist)) {
    splist <- as.character(splist)
  }
  .names_check(splist, "splist")
  .check_status(status)

  # Fix species name
  species_std <- .names_standardize(splist)

  # Classify species
  species_class <- .splist_classify(species_std)

  # Check binomial
  .check_binomial(species_class, splist)

  # Run individual algorithm to multiple species
  n_sps <- length(splist)
  result <- list()
  # Progress_bar
  if (progress_bar) {
    pb <- utils::txtProgressBar(min = 0, max = n_sps, style = 3)
  }

  for (i in 1:n_sps) {
    result[[i]] <-
      .lcvp_fuzzy_search_ind(species_class[i, , drop = FALSE],
                             max_distance,
                             status,
                             keep_closest,
                             genus_fuzzy = genus_fuzzy)
    if (progress_bar) {
      utils::setTxtProgressBar(pb, i)
    }
  }
  if (progress_bar) {
    close(pb)
  }
  # If need to bind the results
  if (bind_result) {
    result <- do.call(rbind, result)
    result <- result[!is.na(result[, 1]), , drop = FALSE]
    if (nrow(result) == 0) {
      return(NULL)
    }
  } else {
    names(result) <- splist
  }
  return(result)
}




#----------------------------------------------------

.lcvp_fuzzy_search_ind <- function(species_class,
                                   max_distance,
                                   status,
                                   keep_closest,
                                   genus_fuzzy) {


  if (is.na(species_class[, 3])) {
    warning(paste0("'", species_class[, 1], "' does not include an epithet."),
            call. = FALSE)
    return(NA)
  } else {
    # Now match
    ## Get the genus  first
    max_distance2 <- ifelse(genus_fuzzy, max_distance, 0)
    gen_number <- .lcvp_group_ind(species_class[1, 2],
                                  perutimber::tab_perutimber_position$genus,
                                  max_distance = max_distance2,
                                  FALSE)
    pos_genus <- unlist(.genus_search_multiple(gen_number))
    n_class <- ncol(perutimber::perutimber_sps_class)

    if (!any(is.na(pos_genus))) {
      # Try fuzzy
      pos_res <- .fuzzy_match(species_class[1,],
                              pos_genus,
                              max_distance,
                              n_class,
                              return_all = TRUE,
                              keep_closest = keep_closest,
                              max_distance2 = max_distance2)
    } else {
      # Fuzzy if did not find the genus
      pos_res <- NULL
    }
    if (length(pos_res) > 0 & !all(is.na(pos_res))) {
      # Result
      result <- perutimber::tab_perutimber[pos_res, , drop = FALSE]
      ## names 1 and 2
      name1 <- paste(species_class[1, 2], species_class[1, 3])
      name2 <- paste(perutimber::perutimber_sps_class[as.numeric(pos_res), 2],
                     perutimber::perutimber_sps_class[as.numeric(pos_res), 3])

      # Add a column indicating the distance
      Name.Distance <- t(utils::adist(name1, name2))
      result <- cbind(result, Name.Distance)
      result <- result[order(Name.Distance), , drop = FALSE]
      if (!all(c( "Accepted", "Synonym", "No opinion") %in% status)) {
        result <- result[result$taxonomic_status %in% status, , drop = FALSE]
      }

      rownames(result) <- NULL
      return(result)
    } else {
      warning(paste0("No match found for ", "'", species_class[, 1], "'."),
              call. = FALSE)
      return(NA)
    }
  }
}


# -------------------------------------------------------------------------


.check_status <- function(status) {

  status_valid <- c( "Accepted", "Synonym", "No opinion")
  check <- all(status %in% status_valid)
  if (!check) {
    stop(paste0("status argument should be one of the following: ",
                paste0("'", status_valid, "'", collapse = ", "), ". Not ",
                paste0("'", status, "'", collapse = ", ")),
         call. = FALSE)
  }
}

