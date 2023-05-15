#' Parse and evaluate a model
#'
#' @param model A model object created with specify.model.
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
#' PVs <- set.pop.var.seeds(min.via.pop = "off", gen.het = "off", new.var.name = "PV1", new.var.seed = function() runif(1,10,50))
#'
#' # generate seed populations
#' p0 <- gen.seed.pops(stage = s, pop.var.seeds = PVs, n = 10)
#'
#' # define model
#' m <- specify.model(type = "binary", variables = c("SV1", "PV1"), expression = "PV1 <= SV1")
#'
#' parse and evaluate model - population and region chosen randomly
#' parse.model(m = m, s = s, p0 = p0, p = sample(SPs$populations.present, 1), r = sample(s$regions, 1))
parse.model <- function(m, s, p0, p, r){
  ## check stage is a stage object
  if(!class(s)=="stage"){
    stop("stage is not a stage object")
  }
  ## check model is a model object
  if(!class(m)=="model"){
    stop("model is not a model object")
  }
  ## check p0 is a p0 object
  if(!class(p0)=="populations"){
    stop("p0 is not a populations object")
  }
  ## Binary models
  if(type == "binary"){
    ## match model variables to stage and population variables

  }
  ## Bounded models
  if(type == "bounded"){

  }
  ## Unbounded models
  if(type == "unbounded"){

  }
}


