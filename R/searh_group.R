#' Function to search species names,
#' based on group (genus, family, order)
#' @keywords internal
.pt_group_ind <- function(group_name,
                            group_ref,
                            max_distance,
                            only_one = TRUE,
                            closest = FALSE) {
  # Get the position
  group_pos <- which(group_ref == group_name)
  # Fuzzy match if it did not work
  if (length(group_pos) == 0) {
    if (max_distance > 0) {
      group_pos <- .pt_agrep_whole(group_name,
                                group_ref,
                                max_distance = max_distance)
      closest1 <- utils::adist(group_name, group_ref[group_pos])

      if (closest & length(group_pos) > 0) {
        which_closest1 <- which(closest1 == min(closest1))
        group_pos <- group_pos[which_closest1]
      }
      n_temp <- length(group_pos)

      if (n_temp > 1 & only_one) {
        # If more than one, get the closest
        which_closest <- which.min(utils::adist(group_name,
                                                group_ref[group_pos]))
        group_pos <- group_pos[which_closest] # choose more than one
      }
      if (n_temp == 0) {
        # if no match is found, return NA
        group_pos <- NA
      }
    } else {
      group_pos <- NA
    }
  }

  return(group_pos)
}
