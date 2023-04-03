# Find a pattern at the end of the character
#' @keywords internal
.find_mat <- function(x, pattern) {
  n_c <- nchar(x)
  n_p <- nchar(pattern) - 1
  return(substr(x, n_c - n_p, n_c) == pattern)
}

# find lati or late
.sub_lat <- function(x) {
  if (all(substr(x, 1, 4) %in% c("LATE", "LATI"))) {
    substring(x, 1, 4) <- "LATE"
    x2 <- x
    substring(x2, 1, 4) <- "LATI"
    return(c(x, x2))
  } else {
    return(x)
  }
}

# Find which pattern matched
#' @keywords internal
.find_common <- function(x) {
  ei <- .find_mat(x, "EI")
  ii <- .find_mat(x, "II")
  i <- .find_mat(x, "I") & !ii & !ei
  iae <- .find_mat(x, "IAE")
  ae <- .find_mat(x, "AE") & !iae
  iifolia <- .find_mat(x, "IIFOLIA")
  iiflora <- .find_mat(x, "IIFLORA")
  ifolia <- .find_mat(x, "IFOLIA") & !iifolia
  iflora <- .find_mat(x, "IFLORA") & !iiflora
  iodes <- .find_mat(x, "IODES")
  oides <- .find_mat(x, "OIDES")
  odes <- .find_mat(x, "ODES") & !iodes
  stats::setNames(
    c(ei, ii, i, iae, ae, iifolia, iiflora, ifolia, iflora,
      iodes, oides, odes),
    c("ei", "ii", "i", "iae", "ae", "iifolia", "iiflora",
      "ifolia", "iflora", "iodes", "oides", "odes")
  )
}

# Substitute
#' @keywords internal
.sub_common <- function(x) {
  x0 <- x
  commons <- which(.find_common(x))
  n_c <- nchar(x)
  n_p <- nchar(names(commons))
  if (length(n_p) != 0) {
    base_str <- substr(x, 1, n_c - n_p)
    sub_str <-
      list(
        "EI" = 1:3,
        "II" = 2:3,
        "I" = 2:3,
        "IAE" = 2:5,
        "AE" = 2:5,
        "IIFOLIA" = c(6, 8),
        "IIFLORA" = c(7, 9),
        "IFOLIA" = c(6, 8),
        "IFLORA" = c(7, 9),
        "IODES" = 10:12,
        "OIDES" = 10:12,
        "ODES" = 10:12
      )
    x <- paste0(base_str, names(sub_str)[sub_str[[commons]]])
  }
  result <- .sub_lat(x)
  return(result[result != x0])
}

