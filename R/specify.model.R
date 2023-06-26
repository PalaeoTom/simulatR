#' Specify model
#'
#' Creates an object that specifies the details of a model.
#'
#' @param type Either "binary" or "continuous".
#' @param variables A character vector containing the names of the population and stage variables included in the model or NA (where there are no variables involved).
#' @param expression A character vector specifying the construction of the model as a single string.
#'
#' @return a model object
#' @export
#'
#' @examples
#' # Run the function
#' model <- specify.model(type = "binary", variables = NA, expression = "runif(1) > 0.5")
specify.model <- function(type, variables, expression, name.out = "new", export = F){
  ## Check type is one of three possibilities
  if(!type=="binary"&&!type=="continuous"){
    stop("argument 'type' is not 'binary' or 'continuous'")
  }
  ## Check variables argument is a character vector
  if(!is.character(variables)&&!is.na(variables)){
    stop("argument 'variables' is not a character vector or NA")
  }
  ## Check expression is a character vector
  if(!is.character(expression)){
    stop("argument 'expression' is not a character vector")
  }
  ## Check expression is only one string
  if(length(expression)>1){
    stop("argument 'expression' is not a single string")
  }
  ## Assemble into model structure
  out <- list("type" = type,
                 "variables" = variables,
                 "expression" = expression)
  ## Assign model class
  out <- structure(out, class = "model")
  ## export if set
  if(export){
    saveRDS(out, file = paste0(name.out, "_stage.Rds"))
  } else {
    return(out)
  }
}
