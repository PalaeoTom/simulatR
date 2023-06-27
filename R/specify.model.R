#' Specify model
#'
#' Creates a model object that specifies the details of a model to be used in simulation. Only produces level-1 models (i.e. no models can be included as variables)
#'
#' @param s A stage object
#' @param p A populations object
#' @param type Either "binary" or "continuous".
#' @param variables A character vector containing the names of the population and stage variables included in the model or NA (where there are no variables involved).
#' @param expression A character vector specifying the construction of the model as a single string.
#' @param ID A character string assigning a unique ID to the model. Default is "new.model'.
#'
#' @return a model object
#' @export
#'
#' @examples
#' # make stage
#' s <- make.stage(n.col = 5, n.row = 5, ar = 400)
#'
#' add a random variable to stage
#' s <- add.var.stage(s, var = matrix(runif(25, min = 10, max = 50), 5, 5), var.name = "SV1")
#'
#' # define population variables
#' PVs <- set.pop.var.seeds(min.via.pop = "off", new.var.name = "PV1", new.var.seed = function() runif(1,10,50))
#'
#' # generate seed populations
#' p0 <- gen.seed.pops(stage = s, pop.var.seeds = PVs, n = 10)
#'
#' # define model
#' m <- specify.model(s = s, p = p0, type = "binary", variables = c("SV1", "PV1"), expression = "PV1 <= SV1", ID = "m")
specify.model <- function(s, p, type, variables, expression, ID = "new.model"){
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
  ## Check s is a stage
  if(!class(s)=="stage"){
    stop("s is not a stage object")
  }
  ## check p is a populations object
  if(!class(p)=="populations"){
    stop("p is not a populations object")
  }
  ## Check variables are present in either s or p
  if(any(is.na(match(variables,c(s$variable.names, p$variable.names))))){
    stop("one or more variables specified are not included in stage or populations objects provided")
  }
  ## Assemble into model structure
  out <- list(
    "ID" = paste0(ID,".1"),
    "type" = type,
    "level" = 1,
    "variables" = variables,
    "expression" = expression)
  ## Assign model class
  out <- structure(out, class = "model")
}
