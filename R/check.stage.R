#' Check custom stage object adheres to required structure
#'
#' check.stage() checks that a custom stage object adheres to the basic structure required by simulatR and assigns
#' the "stage" class if it passes all the checks. The function checks that the first three elements of the object,
#' "regions", "dimensions", and "distances", are all matrices. It then checks that "dimensions" and "distances" are comprised
#' entirely of numeric values. It then checks that there are the same number of cells in the "regions" and "dimensions" elements.
#' Finally, it checks that the "distances" object includes values for every possible combination of regions.
#'
#' @param input A list with 3 elements (regions, dimensions, distances).
#' @param name.out A string to be included in the Rds output file name if export = TRUE. Default is "new".
#' @param export If TRUE, stage will be saved as an Rds file. Default is FALSE.
#'
#' @references Arrhenius, O. (1921). Species and area. \emph{Journal of Ecology}, 9, 95-99.
#'
#' @return A basic stage object.
#' @export
#'
#' @examples
#' # create a mock stage object
#' s <- list("regions" = matrix(seq(1,9,1), 3, 3), "dimensions" = matrix(400, 3, 3),
#' "distances" = matrix(20, 9, 9))
#'
#' # check object and assign "stage" class if it conforms.
#' stage <- check.stage(s)
#'
check.stage <- function(input, name.out = "new", export = F){
  # check regions object is formatted as a matrix
  if(!is.matrix(input[[1]])){
    stop("regions (input[[1]]) are not formatted as a matrix")
  }
  # check dimensions object is formatted as a matrix
  if(!is.matrix(input[[2]])){
    stop("dimensions (input[[2]]) are not formatted as a matrix")
  }
  # check distances object is formatted as a matrix
  if(!is.matrix(input[[3]])){
    stop("distances (input[[3]]) are not formatted as a matrix")
  }
  # check dimensions object contains numeric values
  if(!is.numeric(input[[2]])){
    stop("dimensions (input[[2]]) are not entirely numeric")
  }
  # check distances object contains numeric values
  if(!is.numeric(input[[3]])){
    stop("distances (input[[3]]) are not entirely numeric")
  }
  # check dimensions object is same size as regions object
  if(!length(input[[1]] == length(input[[2]]))){
    stop("different number of cells in regions (input[[1]]) and dimensions (input[[2]])")
  }
  # check distances object has the same number of rows/columns as the regions object does cells
  if(!length(input[[1]]) == ncol(input[[3]]) && length(input[[1]]) == nrow(input[[3]])){
    stop("some distances (input[[3]]) between regions (input[[1]]) are missing")
  }
  # if everything checks out, say so
  message("Object conforms is a stage")
  # assign status
  out <- structure(input, class = "stage")
  ## export if set
  if(export){
    saveRDS(out, file = paste0(name.out, "_stage.Rds"))
  }
  return(out)
}
