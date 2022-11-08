#' Get diagonally aligned cells for each cell in a matrix
#'
#' @param mat A matrix.
#'
#' @return A list of length(mat) where each element (i) contains the index vectors of each cell diagonally offset from mat[i].
#' @export
#'
#' @examples
#' m = matrix(1,3,3)
#' diagonals <- get.diag(m)
get.diag <- function(mat){
  # indicator for all diagonals
  diag.ind <- row(mat) - col(mat)
  diag.ind.opp <- row(mat) + col(mat)
  # split
  cells.diag <- c(split(1:length(mat), diag.ind), split(1:length(mat), diag.ind.opp))
  ## prune out single cell diagonals
  cells.diag.pruned <- cells.diag[-which(lengths(cells.diag) == 1)]
  # for each cell, find other diagonal cells
  diagonals <- lapply(1:length(mat), function(x) {
    out <- unique(unlist(cells.diag.pruned[which(sapply(1:length(cells.diag.pruned), function(z) (1:length(mat))[x] %in% cells.diag.pruned[[z]]))]))
    # remove cell ID from vector
    out <- out[-which(out == x)]
  })
  return(diagonals)
}
