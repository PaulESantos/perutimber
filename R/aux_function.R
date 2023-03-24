#' This code has two functions to classify species names according to
#' Genus, epithet, authors and infracategories
#' Function wrap of .classify_algo for multiple species
#' @keywords internal
.pt_splist_classify <- function(x) {
  # Infrataxa identifiers
  infrasp <- c("subsp.", "ssp.", "var.", "subvar.",
               "forma", "f.", "subf.")
  Infrasp_cat <- toupper(infrasp)
  # Regular expression to make sure, infra code is between names
  Infrasp_cat_reg <- paste("[[:alpha:]]",
                           gsub("\\.",
                                "\\\\.",
                                Infrasp_cat),
                           "[[:alpha:]]")
  # Split names
  x_split <- strsplit(x, " ")

  # Aply the algorithm
  result <- lapply(x_split,
                   .classify_algo,
                   Infrasp_cat_reg)

  # Combine result list into a matrix
  result <- do.call(rbind, result)
  result <- cbind(x, result)
  # Combine categories and remove
  result[, 5] <- paste0(result[, 5], result[, 6])
  result[, 9] <- paste0(result[, 9], result[, 10])
  result <- result[, -c(6, 10), drop = FALSE]

  # Give the colnames of the matrix
  colnames(result) <- c(
    "Species",
    "Genus",
    "Epithet",
    "Author",
    "Subspecies",
    "Variety",
    "Subvariety",
    "Forma",
    "Subforma"
  )
  return(result)
}

# -------------------------------------------------------------------------
#' The algorithm for one name
#' @keywords internal
.classify_algo <- function(x_split_i,
                           Infrasp_cat_reg) {

  # Base output
  output <- character(10)

  # Count the number of names
  n <- length(x_split_i)

  # Genus and epithet
  output[1:2] <- x_split_i[1:2]


  # Check for infrataxa
  if (n > 2) {
    # Connect previous and next name to check for infras
    x_split_i_paste <- x_split_i
    x_split_i_paste[2:n] <- paste(substr(x_split_i[1:(n - 1)], 1, 1),
                                  x_split_i[2:n],
                                  substr(x_split_i[3:n],1 , 1))

    infra_check <- sapply(as.list(Infrasp_cat_reg),
                          function(x, y) {
                            regexpr(x, y) == 1
                          },
                          x_split_i_paste)
    infra_id <- rowSums(infra_check) > 0



    # if there is none get only the author name
    if (!any(infra_id)) {
      output[3] <- paste(x_split_i[3:n],
                         collapse = " ")
    } else {
      # If it has infra categories, get them

      n_infra <- sum(infra_id) # Number of infra categories
      pos <- which(infra_id)
      for (i in 1:n_infra) {
        # do it for all infra names
        # Get the position of the infra
        pos_1 <- pos[i] + 1
        pos_out <- which(infra_check[pos[i], ]) + 3
        output[pos_out] <- x_split_i[pos_1]
      }
      if (n > pos_1) {
        # get the author
        output[3] <- paste(x_split_i[(pos_1 + 1):n],
                           collapse = " ")
      }
      if (pos[1] > 3) { # Author names before infras
        output[3] <- paste(x_split_i[3:(pos[1] - 1)],
                           collapse = " ")
      }
    }
  }
  return(output)
}

#' Function to check list of names input
#' @keywords internal
.pt_names_check <- function(splist,
                         argument_name) {

  # Check if it is a character
  if (!is.character(splist) | !is.vector(splist)) {
    stop(paste0(argument_name,
                " should be a character vector, not '",
                paste(class(splist), collapse = " "), "'"),
         call. = FALSE)
  }
  enc_valid <- !validEnc(splist)

  # Check if it has invalid encoding
  if (any(enc_valid)) {
    stop(paste(argument_name,
               "should include only valid characters,",
               "please check the name(s) at position(s):",
               paste(which(enc_valid), collapse = ", ")),
         call. = FALSE)
  }
}


#' Check the search_by
#' @keywords internal
.pt_search_by_check <- function(search_by) {

  cats <- c("Genus", "Family", "Order", "Author")
  check <- search_by %in% cats
  if (!check) {
    stop(paste0("search_by argument should be one of the following: ",
                paste0("'", cats, "'", collapse = ", "), ". Not '", search_by, "'"),
         call. = FALSE)
  }
}


#' Check if names are binomial
#' @keywords internal
.pt_check_binomial <- function(splist_class, splist) {

  missing_bino <- which(apply(splist_class[, 2:3, drop = FALSE],
                              1,
                              function(x) {any(is.na(x))}))
  if (length(missing_bino) > 0) {
    stop(paste0("splist should include only binomial names,",
                " please check the following names: ",
                paste(paste0("'", splist[missing_bino], "'"), collapse = ", ")),
         call. = FALSE)

  }
}


#' Check inputs for
#' @keywords internal

.pt_check_join <- function(x, y, sp_columns, type) {
  # Check classes
  class_x <- class(x)
  if (!"data.frame" %in% class_x) {
    stop(paste0("x should be a data.frame, not '", class_x, "'."),
         call. = FALSE)
  }
  class_y <- class(y)
  if (!"data.frame" %in% class_y) {
    stop(paste0("y should be a data.frame, not '", class_x, "'."),
         call. = FALSE)
  }

  class_sp <- class(sp_columns)
  if (class_sp != "character") {
    stop(paste0("sp_columns should be a character, not '", class_sp, "'."),
         call. = FALSE)
  }

  # Length names
  n_sp_columns <- length(sp_columns)
  if (n_sp_columns != 2) {
    stop(paste0("sp_columns should include 2 characters. Not ",
                n_sp_columns, "."),
         call. = FALSE)
  }

  # Check if names correspond to columns
  if (!sp_columns[1] %in% colnames(x)) {
    stop(paste0("First name in sp_columns '", sp_columns[1], "'",
                " not found in x columns names."),
         call. = FALSE)
  }
  if (!sp_columns[2] %in% colnames(y)) {
    stop(paste0("Second name in sp_columns '", sp_columns[2], "'",
                " not found in y columns names."),
         call. = FALSE)
  }

  type_valid <- c("full", "left", "right", "inner")
  check <- all(type %in% type_valid)
  if (!check) {
    stop(paste0("type argument should be one of the following: ",
                paste0("'", type_valid, "'", collapse = ", "), ". Not ",
                paste0("'", type, "'", collapse = ", ")),
         call. = FALSE)
  }

}

#'
#' Check function inputs
#' @keywords internal
.pt_check_funcs <- function(func_numeric,
                         func_character,
                         func_logical) {

  if (!is.function(func_numeric)) {
    stop("func_numeric should be a function.")
  }
  if (!is.function(func_character)) {
    stop("func_character should be a function.")
  }
  if (!is.function(func_logical)) {
    stop("func_logical should be a function.")
  }
}

#' @keywords internal
#'
.pt_check_x <- function(x) {
  # Check classes
  class_x <- class(x)
  if (!"data.frame" %in% class_x) {
    stop(paste0("x should be a data.frame, not '", class_x, "'."),
         call. = FALSE)
  }
}

#' Check status input
#' @keywords internal

.pt_check_status <- function(status) {

  status_valid <- c("accepted", "synonym", "unresolved", "external")
  check <- all(status %in% status_valid)
  if (!check) {
    stop(paste0("status argument should be one of the following: ",
                paste0("'", status_valid, "'", collapse = ", "), ". Not ",
                paste0("'", status, "'", collapse = ", ")),
         call. = FALSE)
  }
}

#' Make names standard
#' @keywords internal
.pt_names_standardize <- function(splist) {
  fixed1 <- toupper(splist) # all up
  fixed2 <- gsub("CF\\.", "", fixed1)
  fixed3 <- gsub("AFF\\.", "", fixed2)
  fixed4 <- trimws(fixed3) # remove trailing and leading space
  fixed5 <- gsub("_", " ", fixed4) # change names separated by _ to space

  # Hybrids
  fixed6 <- gsub("(^X )|( X$)|( X )", " ", fixed5)
  hybrids <- fixed5 == fixed6
  if (!all(hybrids)) {
    sp_hybrids <- splist[!hybrids]
    warning(paste("The 'x' sign indicating hybrids have been removed in the",
                  "following names before search:",
                  paste(paste0("'", sp_hybrids, "'"), collapse = ", ")),
            immediate. = TRUE, call. = FALSE)
  }
  # Merge multiple spaces
  fixed7 <- gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", fixed6, perl = TRUE)
  return(fixed7)
}

#'
#' @keywords internal
.pt_keep_all <- function(x) {
  return(paste(unique(x), collapse = ", "))
}


#' Function to match the closest fuzzy name
#' @keywords internal
.pt_agrep_whole <- function(x, y, max_distance) {
  if (max_distance < 1 & max_distance > 0) {
    max_distance <- ceiling(nchar(x) * max_distance)
  }
  a <- utils::adist(x, y)
  return(which(a <= max_distance))
}

