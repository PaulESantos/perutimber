# Small functions to support main functions
# Author: Paul Santos
#' Omint morfoespecies
#' @keywords internal
#'
morf_sps_omit <- function(x){
  morfo_species <- grepl("[a-z]{1,}[0-9]{1,}|[a-z]{1,}[0-9]{1,}$|[a-z]{1,}[0-9]{1,}\\.| [0-9]{1,}",
                         x)
  if (length(morfo_species) != 0) {
    warning(paste("The following names are 'morfoespecies' and been removed",
                  "before search:",
                  paste(paste0("'", x[morfo_species], "'"), collapse = ", ")),
            immediate. = TRUE, call. = FALSE)
  }

  x[!morfo_species]
}

#' Standardise infraspecific ranks.
#' @keywords internal
#'
standardize_infrasps <- function(x){
  infra_subsp <- gsub(paste0(c(" subsp ", " ssp ", " ssp. ", " spp ", " spp. "),
                             collapse = "|"),
                      " subsp. ",
                      x)
  infra_var <- gsub(paste0(c(" var ", " variety ", " variedad "),
                           collapse = "|"),
                    " var. ",
                    infra_subsp)
  infra_forma <- gsub(paste0(c(" f ", " forma "),
                             collapse = "|"),
                      " f. ",
                      infra_var)
  return(infra_forma)
}

#' Remove hybrid symbol from names.
#' @keywords internal
standardize_names <- function(x){
  cap_x <- toupper(x)
  hybrids <- grep("(^X)|(X$)|( X )|(\u00d7)",
                  cap_x,
                  value = TRUE)

  if (length(hybrids) != 0) {
    warning(paste("The 'X' sign indicating hybrids have been removed in the",
                  "following names before search:",
                  paste(paste0("'", hybrids, "'"), collapse = ", ")),
            immediate. = TRUE, call. = FALSE)
  }

  gsub("\\s+",
       " ",
       gsub("CF\\.|AFF\\.|(^X)|(X$)|( X )|(\u00d7)|_",
            " ",
            cap_x))

}

# -------------------------------------------------------------------------

#' @keywords internal
check_names <- function(splist,
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


# -------------------------------------------------------------------------
#' @keywords internal
check_binomial <- function(splist_class, splist) {

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
