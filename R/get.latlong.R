#' Get latitudinally and longitudinally aligned cells for each cell in a matrix
#'
#' @param mat A matrix.
#'
#' @return A list of length(mat) where each element (i) contains the index vectors of each cell latitudinal or longitudinally offset from mat[i].
#' @export
#'
#' @keywords internal
#'
#' @examples
#' m = matrix(1,3,3)
#' latlongs <- get.latlong(m)
get.latlong <- function(mat){
  # get cells that can be reached via a latitudinal or longitudinal migration for each cell
  cardinals <- lapply(1:length(mat), function(x){
    out <- c(which(row(mat) == row(mat)[x]), which(col(mat) == col(mat)[x]))
    # remove cell ID from vector
    out <- out[-which(out == x)]
  })
  return(cardinals)
}
