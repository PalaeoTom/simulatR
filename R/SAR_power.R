#' Calculate carrying capacity for each region using power model with default parameters
#'
#' SAR_power() uses the power model (Arrhenius, 1921) to estimate the species carrying capacity of each region of a stage.
#' For each region, parameters c and z are drawn from Weibull distributions with shape and scale values derived from the datasets of
#' Matthews et al. (in prep).
#'
#' @param area A matrix specifying the size (in hectares) of each region of a stage.
#'
#' @references Arrhenius, O. (1921). Species and area. \emph{Journal of Ecology}, 9, 95-99.
#'
#' @return A matrix specifying the species carrying capacity of each region of a stage.
#' @export
#'
#' @examples
#' # create a mock area object
#' example <- matrix(20,4,4)
#'
#' # estimate species carrying capacity for each region
#' capacities <- SAR_power(example)
SAR_power <- function(area){
  ## check input is a matrix
  if(!is.matrix(area)){
    stop("input is not a matrix")
  }
  ## create empty frame
  cc <- area
  ## apply transformation using power SAR
  cc <- apply(cc, c(1,2), function(x) round(rweibull(1, shape = 0.844303639, scale = 14.94743187)*x^rweibull(1, shape = 1.81851964, scale = 0.231839438), digits = 0))
}


