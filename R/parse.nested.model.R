#' Parse and evaluate a nested model
#'
#' Works on models of all levels.
#'
#' @param model A level-2 or greater model created with specify.nested.model.
#' @param stage A stage object created with make.stage or checked with check.stage.
#' @param p0 A populations object created with gen.seed.pops or simulate.
#' @param p A string specifying the name of the population.
#' @param r A numeric value specifying the region the process is occurring in/to.
#'
#' @return A numeric value.
#' @export
#'
#' @keywords internal
#'
#' @examples
#' # make stage
#' s <- make.stage(n.col = 5, n.row = 5, ar = 400)
#'
#' # add a random variable to stage
#' s <- add.var.stage(s, var = matrix(runif(25, min = 10, max = 50), 5, 5), var.name = "SV1")
#'
#' # define population variables
#' PVs <- set.pop.var.seeds(min.via.pop = "off", new.var.name = "PV1", new.var.seed = function() runif(1,10,50))
#'
#' # generate seed populations
#' p0 <- gen.seed.pops(stage = s, pop.var.seeds = PVs, n = 10)
#'
#' # Create a level-1 continuous model
#' m1 <- specify.model(s = s, p = p0, type = "continuous", variables = c("SV1", "PV1"), expression = "abs(PV1 - SV1)", ID = "m1")
#'
#' # And then a level-1 binary model
#' m2 <- specify.model(s = s, p = p0, type = "binary", variables = c("SV1", "PV1"), expression = "PV1 <= SV1", ID = "m2")
#'
#' # Next, create a level-2 binary model
#' m3 <- specify.nested.model(s = s, p = p0, m = list(m1,m2), type = "binary", variables = c("PV1"), expression = "1 <= PV1/(m1.1*m2.1)", ID = "m3")
#'
#' # Try parsing
#' parse.nested.model(m = m3, s = s, p0 = p0, p = sample(1:length(p0$population.variables), 1), r = sample(s$regions, 1))
#'
#' # Create another level-1 model
#' m4 <- specify.model(s = s, p = p0, type = "continuous", variables = c("SV1", "PV1"), expression = "PV1*SV1", ID = "m4")
#'
#' # Create a level-3 continuous model
#' m5 <- specify.nested.model(s = s, p = p0, m = list(m3,m4), type = "continuous", variables = NA, expression = "m3.2*m4.1", ID = "m5")
#'
#' parse and evaluate model
#' parse.nested.model(m = m5, s = s, p0 = p0, p = sample(1:length(p0$population.variables), 1), r = sample(s$regions, 1))
parse.nested.model <- function(m, s, p0, p, r){
  ## check stage is a stage object
  if(!class(s)=="stage"){
    stop("stage is not a stage object")
  }
  ## check p0 is a p0 object
  if(!class(p0)=="populations"){
    stop("p0 is not a populations object")
  }
  ## check model is a model object
  if(!class(m)=="model"){
    stop("model is not a model object")
  }
  ## If model is of a higher level than 1, break
  if(m$level == 1){
    stop("m is a level-1 model. Please parse with parse.model")
  }
  ## Binary models
  if(m$"type" == "binary"){
    ## without variables
    if(all(is.na(m$variables))){
      ## get levels
      levels <- 1:(m$level-1)
      ## convert models into values, start with lv1 models
      for(i in levels){
        ## isolate models of that level
        for(a in grep(paste0(".",i), m$nested.models, fixed = T)){
          assign(m$nested.models[a], parse.model(m = m$models[[a]], s = s, p0 = p0, p = p, r = r), pos = .GlobalEnv)
        }
      }
      if(eval(parse(text = m$"expression"))){
        out <- 1
      } else {
        out <- 0
      }
    } else {
      ## match model variables to stage and population variables
      vars <- lapply(1:length(m$"variables"), function(x){
        ## variable in stage, not populations
        if(any(s$"variable.names" == m$"variables"[x]) && !any(p0$"variable.names" == m$"variables"[x])){
          ## get var
          var <- s$"stage.variables"[[which(names(s$"stage.variables") == m$"variables"[x])]][r]
        }
        ## variable in populations, not stage
        if(!any(s$"variable.names" == m$"variables"[x]) && any(p0$"variable.names" == m$"variables"[x])){
          ## get var
          var <- p0$"population.variables"[[p]][[which(names(p0$"population.variables"[[p]]) == m$variables[x])]]
        }
        ## if present in both, break and request re-label
        if(any(s$"variable.names" == m$"variables"[x]) && any(p0$"variable.names" == m$"variables"[x])){
          stop(paste0("model variable ", x, " is present is both a stage and population variable. Re-label to differentiate and re-try"))
        }
        ## if present in neither, break and request re-label
        if(!any(s$"variable.names" == m$"variables"[x]) && !any(p0$"variable.names" == m$"variables"[x])){
          stop(paste0("model variable ", x, " is not present is not a stage and population variable. Please ensure model correctly specifies a stage or population variables"))
        }
        return(var)
      })
      ## convert isolate variables into objects
      for(i in 1:length(vars)) assign(m$"variables"[i], vars[[i]])
      ## get levels
      levels <- 1:(m$level-1)
      ## convert models into values, start with lv1 models
      for(i in levels){
        ## isolate models of that level
        for(a in grep(paste0(".",i), m$nested.models, fixed = T)){
          assign(m$nested.models[a], parse.model(m = m$models[[a]], s = s, p0 = p0, p = p, r = r), pos = .GlobalEnv)
        }
      }
      ## evaluate model and return 1 if true, 0 if false
      if(eval(parse(text = m$"expression"))){
        out <- 1
      } else {
        out <- 0
      }
    }
  }
  ## continuous models
  if(m$"type" == "continuous"){
    ## no variables
    if(all(is.na(m$variables))){
      ## get levels
      levels <- 1:(m$level-1)
      ## convert models into values, start with lv1 models
      for(i in levels){
        ## isolate models of that level
        for(a in grep(paste0(".",i), m$nested.models, fixed = T)){
          assign(m$nested.models[a], parse.model(m = m$models[[a]], s = s, p0 = p0, p = p, r = r), pos = .GlobalEnv)
        }
      }
      ## evaluate model and return value
      out <- eval(parse(text = m$expression))
    } else {
      ## match model variables to stage and population variables
      vars <- lapply(1:length(m$"variables"), function(x){
        ## variable in stage, not populations
        if(any(s$"variable.names" == m$"variables"[x]) && !any(p0$"variable.names" == m$"variables"[x])){
          ## get var
          var <- s$"stage.variables"[[which(names(s$"stage.variables") == m$"variables"[x])]][r]
        }
        ## variable in populations, not stage
        if(!any(s$"variable.names" == m$"variables"[x]) && any(p0$"variable.names" == m$"variables"[x])){
          ## get var
          var <- p0$"population.variables"[[p]][[which(names(p0$"population.variables"[[p]]) == m$variables[x])]]
        }
        ## if present in both, break and request re-label
        if(any(s$"variable.names" == m$"variables"[x]) && any(p0$"variable.names" == m$"variables"[x])){
          stop(paste0("model variable ", x, " is present is both a stage and population variable. Re-label to differentiate and re-try"))
        }
        ## if present in neither, break and request re-label
        if(!any(s$"variable.names" == m$"variables"[x]) && !any(p0$"variable.names" == m$"variables"[x])){
          stop(paste0("model variable ", x, " is not present is not a stage and population variable. Please ensure model correctly specifies a stage or population variables"))
        }
        return(var)
      })
      ## convert isolate variables into objects
      for(i in 1:length(vars)) assign(m$"variables"[i], vars[[i]])
      ## get levels of model
      levels <- 1:(m$level-1)
      ## convert models into values, start with lv1 models
      for(i in levels){
        ## isolate models of that level
        for(a in grep(paste0(".",i), m$nested.models, fixed = T)){
          assign(m$nested.models[a], parse.model(m = m$models[[a]], s = s, p0 = p0, p = p, r = r), pos = .GlobalEnv)
        }
      }
      ## evaluate model and return value
      out <- eval(parse(text = m$expression))
    }
  }
  return(out)
}


