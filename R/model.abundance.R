#' Default model for changes in abundance over time
#'
#' @param A0
#' @param MVP
#' @param time
#' @param factor
#'
#' @return
#' @export
#'
#' @examples
model.abundance <- function(A0, MVP, time, factor = 1){

  A0 - MVP

  # When above MVP, sample from normal distribution
  rnorm(1, mean = 0, sd = 1000*factor)

  # When below MVP, mean shifts depending on how far below MVP the abundance is
  hist(rnorm(10000, mean = (A0-MVP)*factor, sd = 1000*factor))






}



## Need two
