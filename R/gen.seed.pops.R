
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
  ## get regions to be populated (vector of numbers - each entry corresponds to a population)
  rtbp <- sample(as.vector(stage[[1]]), n, replace = T)

  ## initialise list of regions (length = number of cells in stage regions object)
  occ.reg <- as.list(matrix(nrow = length(stage[[1]])))

  ## initialise list of seed populations (length = n)
  seed.pops <- as.list(matrix(nrow = n))

  ## for each entry in the vector: 1) add position of entry in vector to list of regions; 2) add list of population variables to seed population list.
  unlist(lapply(1:length(pop.var.seeds), function(x) pop.var.seeds[[x]]()))


  ## return pair of lists (1 - occupied regions, 2 - populations) as seeds object

}
