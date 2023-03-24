# Algorithm to look for the exact or fuzzy genus names
# Author: Bruno Vilela

#-------------------------------------------------------#
# Function to wrap .genus_search for multiple genus names
.pt_genus_search_multiple <- function(gen_pos) {
  # Length of genus positions
  n_positions <- length(gen_pos)
  # List to result
  gen_pos_mult <- list()
  # Loop to apply the individual functions
  for (i in seq_len(n_positions)) {
    gen_pos_mult[[i]] <- .pt_genus_search(gen_pos[i])
  }
  if(!all(is.na(gen_pos))) {
    # Genus names in the list
    names(gen_pos_mult) <- perutimber::tab_perutimber_position$genus[gen_pos]
  }
  # Return the list with the positions
  return(gen_pos_mult)
}

#-------------------------------------------------------#
# Transform group match into actual genus positions
.pt_genus_search <- function(group_pos) {
  #group_pos = result from group_search
  if (all(is.na(group_pos))) {
    return(NA)
  } else {
    # Identify their actual positions
    gen_pos <- NULL
    for (k in seq_along(group_pos)) {
      # Get the genus start and end position
      genus_sequence <- c(group_pos[k], group_pos[k] + 1)
      tab_gen_pos <- perutimber::tab_perutimber_position[genus_sequence, 1]
      # For the last one sequence to the end of the table
      if (is.na(tab_gen_pos[2])) {
        gen_pos <- c(gen_pos, tab_gen_pos[1]:nrow(perutimber::tab_perutimber))
      } else {
        # Now sequence over it
        gen_pos <- c(gen_pos, tab_gen_pos[1]:(tab_gen_pos[2] - 1))
      }
    }
    # Generate a vector to use for searching
    return(gen_pos)
  }
}

