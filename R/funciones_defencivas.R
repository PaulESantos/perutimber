
#-------------------------------------------------------#
# Function to check list of names input
.names_check <- function(splist,
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


#-------------------------------------------------------#
# Check if names are binomial
.check_binomial <- function(splist_class, splist) {

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
#-------------------------------------------------------#
# Make names standard
.names_standardize <- function(splist) {
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

#-------------------------------------------------------#
# Function to match the closest fuzzy name
.agrep_whole <- function(x, y, max_distance) {
  if (max_distance < 1 & max_distance > 0) {
    max_distance <- ceiling(nchar(x) * max_distance)
  }
  a <- utils::adist(x, y)
  return(which(a <= max_distance))
}

