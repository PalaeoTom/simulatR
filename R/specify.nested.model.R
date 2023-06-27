#' Specify nested model
#'
#' Creates a model object that specifies the details of a model to be used in simulation. Used to build model objects that draw on other models as variables (i.e. nested models).
#' Models should be nested one level at a time. For example, if you want to nest a nested model (i.e. create a level-3 model), first you need to create the level-2 nested model,
#' then submit that as a variable during the creation of the level-3 model. Note that if you submit a list of models as m, these will be referred to in the model by the names provided or
#' in the order they appear in the list. For example, if you don't assign names to your models via names(), the first model in the list will be assigned the identifier "m1", the second "m2", and so forth.
#'
#' @param s A stage object
#' @param p A populations object
#' @param m Either a model object, or a list of model objects.
#' @param type Either "binary" or "continuous".
#' @param variables A character vector containing the names of the population variables, stage variables, or NA (where there are no variables involved).
#' @param expression A character vector specifying the construction of the model as a single string. If m is a model, it should be called in the string via 'm'. If m is a list of models, the models should be specified by their names (names(m), or their position in the list with the prefix 'm' ('m1', 'm2', and so forth).
#' @param ID A character string assigning a unique ID to the model. Default is "new.model'. Note that all IDs provided will have a suffix appended denoting their level (e.g. a level-2 model with ID "m1" will be labelled "m1.2").
#'
#' @return a model object
#' @export
#'
#' @examples
#' #' # make stage
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
#' m1 <- specify.model(s = s, p = p0, type = "binary", variables = c("SV1", "PV1"), expression = "PV1 <= SV1", ID = "m1")
#'
#' # define second model
#' m2 <- specify.model(s = s, p = p0, type = "binary", variables = c("SV1", "PV1"), expression = "PV1 <= SV1", ID = "m2")
#'
#' # get list
#' m.list <- list(m1, m2)
#'
#' # build nested model
#' m3 <- specify.nested.model(s = s, p = p0, m = m.list, type = "binary", variables = c("SV1", "PV1"), expression = "PV1*m1.1 <= SV1*m2.1", ID = "m3")
specify.nested.model <- function(s, p, m, type, variables, expression, ID = "new.model"){
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
  ## check m is a model object
  if(!class(m)=="model"){
    ## if not is it a list?
    if(!is.list(m)){
      stop("m is not a model or a list")
    }
    ## if it is a list, is it a list of models?
    for(i in 1:length(m)){
      if(!class(m[[i]]) == "model"){
        stop("one or more elements of list m are not model objects")
      }
    }
    ## if it is a list of models, check models are named or assign names
    if(is.null(names(m))){
      names(m) <- paste0("m", seq(1,length(m),1))
    }
  }
  ## Check variables are present in either s or p
    if(any(is.na(match(variables,c(s$variable.names, p$variable.names))))){
      stop("one or more variables specified are not included in stage or populations objects provided")
    }
  ## Get model level
  if(class(m) == "model"){
    level <- m$level+1
  } else {
    level <- max(sapply(1:length(m), function(x) m[[x]]$level))+1
  }
  ## Assemble into model structure
  out <- list(
    "ID" = paste0(ID,".",level),
    "type" = type,
    "level" = level,
    "variables" = variables,
    "models" = m,
    "expression" = expression)
  ## Assign model class
  out <- structure(out, class = "model")
}
