dispersal <- function(stage, p0, t0, t1, SF = 1){
  ## get distances between regions
  distances <- stage[[3]]
  ## if no min.via.pop
  for (i in 1:length(p0)){
    ## find occupied regions
    loc <- which(sapply(p0[[1]], function(y) i %in% y))
    ## if one region

    # if more than one region

  }
  ## if min.via.pop

  ## if one region source

  ## if more than one source region



}
