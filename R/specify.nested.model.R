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
#' @param variables A character vector containing the names of the population and stage variables included in the model or NA (where there are no variables involved).
#' @param expression A character vector specifying the construction of the model as a single string. Model should be referred to as 'm' or, if multiple models are included, the model names or 'm1', 'm2', and so forth.
#'
#' @return a model object
#' @export
#'
#' @examples
#' # Run the function
#' model <- specify.model(type = "binary", variables = NA, expression = "runif(1) > 0.5")
specify.nested.model <- function(s, p, m, type, variables, expression, name.out = "new", export = F){
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
  ## Check variables are present in either s or p when m is a model
  if(class(m)=="model"){
    if(any(is.na(match(variables,c(s$variable.names, p$variable.names, "m"))))){
      stop("one or more variables specified are not included in stage or populations objects provided")
    }
  } else {
    ## Check variables are present in either s or p when m is a list of model
    if(any(is.na(match(variables,c(s$variable.names, p$variable.names, names(m)))))){
      stop("one or more variables specified are not included in stage, populations objects, or models provided")
    }
  }
  ## Get model level
  if(class(m) == "model"){
    level <- m$level+1
  } else {
    level <- max(sapply(1:length(m), function(x) m[[x]]$level))+1
  }
  ## Assemble into model structure
  out <- list("type" = type,
              "level" = level,
              "variables" = variables,
              "models" = m,
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
