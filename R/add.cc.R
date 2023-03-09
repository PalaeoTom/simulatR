#' Add carrying capacity to stage using distributions of parameters derived from Matthews et al. data
#'
#' add.cc adds an element to an existing stage object specifying the species carrying capacity of the region. Can estimate carrying capacities
#' using the power model (Arrhenius, 1921) or another model specified by the user.
#'
#' @param stage A stage object or the name of the stage Rds file as a string.
#' @param model Species-area model used to calculate carrying capacity. Default is power model (Arrhenius, 1921).
#' @param power.non0 If TRUE, all carrying capacities estimated by the default power model will be 1 or higher (i.e. no zeros). Default is FALSE.
#' @param name.out If export = TRUE, a string that specifies the name of the output file.
#' @param export If TRUE, updated stage with carrying capacities is exported as an Rds file to working directory.
#'
#' @import stats
#'
#' @references Arrhenius, O. (1921). Species and area. \emph{Journal of Ecology}, 9, 95-99.
#'
#' @return A stage object with element "cc" added.
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
#' # add carrying capacities
#' stage.cc <- add.cc(stage)
add.cc <- function(stage, model = "power", power.non0 = F, name.out = "new", export = F){
  ## if load is TRUE, read in stage from Rds object
  if(is.object(stage)){
    ## check stage
    stage0 <- check.stage(stage)
    ## isolate dimensions
    dimensions <- stage0$"dimensions"
  } else {
    ## read in
    stage <- readRDS(paste0(stage, ".Rds"))
    ## check stage
    stage0 <- check.stage(stage0)
    ## isolate dimensions
    dimensions <- stage0$"dimensions"
  }
  ## transform area into carrying capacity object using specified model. Default is power.
  if(model == "power"){
    carrying.capacity <- SAR_power(dimensions, non0 = power.non0)
  } else {
    carrying.capacity <- model(dimensions)
  }
  ## append carrying capacity element to new version of stage0 object
  stage1 <- stage0
  stage1[[length(stage0)+1]] <- carrying.capacity
  names(stage1)[length(stage0)+1] <- "carrying.capacity"
  ## export if set
  if(export){
    saveRDS(stage1, file = paste0(name.out, "_stage0.Rds"))
  } else {
  return(stage1)
  }
}

