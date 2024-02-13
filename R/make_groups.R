
#' Make groups
#'
#' Make groups of two randomly from a list of names
#'
#' @param names A list of names
#'
#' @return A two column matrix of names
#' @export
#'
#' @examples
make_groups <- function(names) {
  if(length(names)%%2 > 0) {
    stop("Uneven number of people")
  }
  shuffled <- matrix(sample(names), ncol = 2)
  return(shuffled)
}

