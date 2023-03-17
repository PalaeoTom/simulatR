n = 10

gen.seed.pops <- function(stage, pop.var.seeds, n, method = "random", export = F, name.out = "new"){
  ## check stage is a stage object

  ## check pop.var.seeds is a pop.var.seeds object

  ## check n is numeric

  ## check method is "random" or a function

  ## get regions to be populated (vector of numbers - each entry corresponds to a population)

  ## check regions to be populated is a vector of numbers

  ## initialise list of regions (length = number of cells in stage regions object)

  ## initialise list of seed populations (length = n)

  ## for each entry in the vector: 1) add position of entry in vector to list of regions; 2) add list of population variables to seed population list.

  ## return pair of lists (1 - occupied regions, 2 - populations) as seeds object

}
