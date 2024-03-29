#' Generate seed populations
#'
#' gen.seed.pops generates n seed populations with variables defined by pop.var.seeds and assigns them to regions within the provided stage using method.
#' If method = "random", regions are randomly selected with replacement for population.
#'
#' @param stage a stage object.
#' @param pop.var.seeds a pop.var.seeds object.
#' @param n a numeric value, specifying the number of populations to be seeded.
#' @param method Either "random" (the default) or a function with a stage object as an input. If "random", seed populations will be randomly assigned to regions specified in stage object.
#' @param export If TRUE, stage will be saved as an Rds file. Default is FALSE.
#' @param name.out A string to be included in the Rds output file name if export = TRUE. Default is "new".
#'
#' @return A populations object.
#' @export
#'
#' @examples
#' # create a mock stage object
#' s <- list("regions" = matrix(seq(1,9,1), 3, 3), "dimensions" = matrix(400, 3, 3), "distances" = matrix(20, 9, 9))
#'
#' # check object and assign "stage" class if it conforms.
#' s <- check.stage(s)
#'
#' # Generate a generic pop.var.seeds object
#' pvs <- set.pop.var.seeds(pop.gen.threshold = 0.1)
#'
#' # Now generate seed populations
#' seed.pops <- gen.seed.pops(stage = s, pop.var.seeds = pvs, n = 10)
#'
gen.seed.pops <- function(stage, pop.var.seeds, n, method = "random", export = F, name.out = "new"){
  ## check stage is a stage object
  if(!class(stage)=="stage"){
    stop("stage is not a stage object")
  }
  ## check pop.var.seeds is a pop.var.seeds object
  if(!class(pop.var.seeds)=="pop.var.seeds"){
    stop("pop.var.seeds is not a pop.var.seeds object")
  }
  ## check n is numeric
  if(!is.numeric(n)){
    stop("n is not numeric")
  }
  ## check method is "random" or a function
  if(!method == "random" && !is.function(method)){
    stop("provided seeding method is not 'random' or a function")
  }
  ## get regions to be populated (vector of numbers - each entry corresponds to a population
  if(method == "random"){
    rtbp <- sample(as.vector(stage[[1]]), n, replace = T)
  } else {
    rtbp <- method(stage)
  }
  ## initialise list of regions (length = number of cells in stage regions object)
  occ.reg <- vector("list", length(stage[[1]]))

  ## initialise list of seed populations (length = n)
  seed.pops <- vector("list", n)

  ## initalise list of species
  pop.species <- as.list(1:n)

  ## for each entry in the vector: 1) add position of entry in vector to list of regions; 2) add list of population variables to seed population list.
  var.names <- pop.var.seeds$variable.names
  variables <- pop.var.seeds
  variables$variable.names <- NULL
  ## populate regions
  for(y in 1:length(rtbp)){
    occ.reg[[rtbp[y]]] <- c(occ.reg[[rtbp[y]]], y)
  }
  ## assign variable values for each seed population
  for(z in 1:n){
    seed.pops[[z]] <- sapply(1:length(variables), function(x){
      if(is.function(variables[[x]])){
        variables[[x]]()
        } else {
        variables[[x]]
        }
    })
    names(seed.pops[[z]]) <- var.names
  }
  ## combine in single output
  t0 <- list("distances" = stage$distances,"variable.names" = var.names,
             "populated.regions" = occ.reg, "species.representation" = pop.species, "population.variables" = seed.pops)
  ## Assign populations class
  t0 <- structure(t0, class = "populations")
  ## export if set
  if(export){
    saveRDS(t0, file = paste0(name.out, "_seed_pops.Rds"))
  } else {
    return(t0)
  }
}
