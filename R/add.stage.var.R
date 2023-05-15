#' Add variable to stage
#'
#' add.stage.var adds an element to an existing stage object. The new element should be provided either as a function, which uses the existing stage variables as inputs, or as a matrix
#' with the same dimensions of the 'regions' stage element, where each cell of the matrix specifies the value of the new variable for the corresponding region.
#'
#' @param stage A stage object or the name of the stage Rds file as a string.
#' @param var A function specifying how the variable should be calculated using existing stage elements OR a matrix of the same dimensions as the 'regions' stage element, specifying the variable value for each region.
#' @param var.name A string specifying name of new variable. No punctuation should be included in name. Default = "new.variable".
#' @param name.out If export = TRUE, a string that specifies the name of the output file.
#' @param export If TRUE, updated stage with new variable is exported as an Rds file to working directory.
#'
#' @return A stage object with new variable added.
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
#' # Add pre-defined variable
#' new.variable <- matrix(10,3,3)
#' new.stage1 <- add.var.stage(stage = stage, var = new.variable, var.name = "new.var1")
#'
#' # Define function to define new variable
#' new.variable.function <- function(stage){
#' var0 <- stage[["dimensions"]]
#' var1 <- sqrt(var0)
#' }
#'
#' # Add new variable using function
#' new.stage2 <- add.var.stage(stage = stage, var = new.variable.function, var.name = "new.var2")
#'

add.var.stage <- function(stage, var, var.name = "new.variable", name.out = "new", export = F){
  ## if load is TRUE, read in stage from Rds object
  if(is.object(stage)){
    # check stage
    if(!class(stage)=="stage"){
      stop("stage is not a stage object")
    }
  } else {
    ## read in
    stage <- readRDS(paste0(stage, ".Rds"))
    # check stage
    if(!class(stage)=="stage"){
      stop("stage is not a stage object")
    }
  }
  ## create output object
  out <- stage
  ## generate new variable object
  if(is.function(var)){
    new <- var(stage)
  } else {
    # check new variable is formatted as a matrix
    if(!is.matrix(var)){
      stop("new variable is not formatted as a matrix")
    }
    # check dimensions are correct
    if(!ncol(stage[["regions"]]) == ncol(var) && nrow(stage[["regions"]]) == nrow(var)){
      stop("different number of columns and/or rows in new variable and regions element of stage")
    }
    new <- var
  }
  ## Check if stage has 'stage.variables' element
  if(any(names(out) == "stage.variables")){
    ## Add to existing list
    out$stage.variables[[length(out$"stage.variables")+1]] <- new
    names(out$stage.variables)[length(out$stage.variables)] <- var.name
    out$"variable.names" <- names(out$"stage.variables")
  } else {
    ## create population variables object
    out$"stage.variables" = list(var.name = new)
    out$"variable.names" <- names(out$"stage.variables")
  }
  ## export if set
  if(export){
    saveRDS(out, file = paste0(name.out, "_stage.Rds"))
  } else {
  return(out)
  }
}
