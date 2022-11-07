## Function: getting indices of cells diagonal to all cells in a matrix
get.diag <- function(mat){
  # indicator for all diagonals
  diag.ind <- row(mat) - col(mat)
  diag.ind.opp <- row(mat) + col(mat)
  # split
  cells.diag <- c(split(mat, diag.ind), split(mat, diag.ind.opp))
  ## prune out single cell diagonals
  cells.diag.pruned <- cells.diag[-which(lengths(cells.diag) == 1)]
  # for each cell, find other diagonal cells
  diagonals <- lapply(1:length(mat), function(x) {
    out <- unique(unlist(cells.diag.pruned[which(sapply(1:length(cells.diag.pruned), function(z) mat[x] %in% cells.diag.pruned[[z]]))]))
    # remove cell ID from vector
    out <- out[-which(out == x)]
  })
  return(diagonals)
  }

## Function: getting indices of cells in same row or column as each cell in a matrix
get.card <- function(mat){
  # get cells that can be reached via a cardinal migration for each cell
  cardinals <- lapply(1:length(mat), function(x){
    out <- c(which(row(mat) == row(mat)[x]), which(col(mat) == col(mat)[x]))
    # remove cell ID from vector
    out <- out[-which(out == x)]
  })
  return(cardinals)
}

#### Setting the stage ####

# n.col = number of columns (integer)
# n.row = number of rows (integer)
# a = area of regions (integer or vector of values)
# func = function for transforming area into distance between areas in cardinal directions (NSEW)

make.stage <- function(n.col, n.row, ar, name.out, distance.alt = F, od.path = "average") {
  # get number of cells
  n.cell <- n.col*n.row
  # make stage
  stage <- matrix(seq(1:n.cell), n.row, n.col)
  # set area of each cell
  area <- matrix(ar, n.row, n.col)
  # get distances between regions
  if(isFALSE(distance.alt)){
    # get distance to cross each region in a cardinal direction
    card <- matrix(sqrt(area), n.row, n.col)
    # get distance to cross each region diagonally
    diag <- apply(card, c(1,2), function(x) sqrt(2*(x^2)))
    # get diagonal migratory pathways for each cell
    diagonals <- get.diag(stage)
    # get cardinal migratory pathways for each cell
    cardinals <- get.card(stage)
    # get off-diagonals
    off.diagonals <- lapply(1:length(stage), function(y){
      # get cells that are neither cardinal or diagonal in their position relative to the cell in question
      out <- intersect(which(is.na(match(seq(1:length(stage)), diagonals[[y]]))), which(is.na(match(seq(1:length(stage)), cardinals[[y]]))))
      # remove cell ID from vector
      out <- out[-which(out == y)]
    })
    # compute distances
  distances <- sapply(1:length(stage), function(a){
    by.col <- sapply(1:length(stage), function(b){
      if(a == b) {
        dist <- 0
      } else {
        # if column index (destination) present in diagonals[row]
        if(any(!is.na(match(diagonals[[a]], b)))){
          # if only 1 region separates origin and destination
          if(abs(row(stage)[a]-row(stage)[b]) == 2){
            # get number of regions between diagonal elements
            diff <- (a-b)/(abs(row(stage)[a]-row(stage)[b]))
            # get distance
            dist <- diag[a]/2 + diag[a-diff] + diag[b]/2
          }
          # if more than 1 region separates origin and destination
          if(abs(row(stage)[a]-row(stage)[b]) > 2){
            # get number of regions between diagonal elements
            diff <- (a-b)/(abs(row(stage)[a]-row(stage)[b]))
            # get number of diagonal elements in between original and destination
            n <- abs(row(stage)[a]-row(stage)[b])-1
            # get distance
            dist <- diag[a]/2 + sum(sapply(1:n, function(t) out <- diag[a-(diff*t)])) + diag[b]/2
          }
          # if no regions separate origin and destination
          if(abs(row(stage)[a]-row(stage)[b]) == 1){
            dist <- diag[a]/2 + diag[b]/2
          }
        }
        # if column index (destination) present in cardinals[row]
        if(any(!is.na(match(cardinals[[a]], b)))){
          # if east/west
          if(abs(col(stage)[a]-col(stage)[b]) >= 1 && abs(row(stage)[a]-row(stage)[b]) == 0){
            # if exactly 1 region separates origin and destination
            if(abs(col(stage)[a]-col(stage)[b]) == 2){
              # get number of regions between cardinal elements
              diff <- (a-b)/(abs(col(stage)[a]-col(stage)[b]))
              # get distance
              dist <- card[a]/2 + card[a-diff] + card[b]/2
            }
            # if more than 1 region separates origin and destination
            if(abs(col(stage)[a]-col(stage)[b]) > 2){
              # get number of regions between cardinal elements
              diff <- (a-b)/(abs(col(stage)[a]-col(stage)[b]))
              # get number of cardinal elements in between original and destination
              n <- abs(col(stage)[a]-col(stage)[b])-1
              # get distance
              dist <- card[a]/2 + sum(sapply(1:n, function(t) out <- card[a-(diff*t)])) + card[b]/2
            }
            # if no regions separate origin and destination
            if(abs(col(stage)[a]-col(stage)[b]) == 1){
              dist <- card[a]/2 + card[b]/2
            }
          }
          # if north/south
          if(abs(row(stage)[a]-row(stage)[b]) >= 1 && abs(col(stage)[a]-col(stage)[b]) == 0){
            # if exactly 1 region separates origin and destination
            if(abs(row(stage)[a]-row(stage)[b]) == 2){
              # get number of regions between cardinal elements
              diff <- (a-b)/(abs(row(stage)[a]-row(stage)[b]))
              # get distance
              dist <- card[a]/2 + card[a-diff] + card[b]/2
            }
            # if more than 1 region separates origin and destination
            if(abs(row(stage)[a]-row(stage)[b]) > 2){
              # get number of regions between cardinal elements
              diff <- (a-b)/(abs(row(stage)[a]-row(stage)[b]))
              # get number of cardinal elements in between original and destination
              n <- abs(row(stage)[a]-row(stage)[b])-1
              # get distance
              dist <- card[a]/2 + sum(sapply(1:n, function(t) out <- card[a-(diff*t)])) + card[b]/2
            }
            # if no regions separate origin and destination
            if(abs(row(stage)[a]-row(stage)[b]) == 1){
              dist <- card[a]/2 + card[b]/2
            }
          }
        }
        # if column index (destination) present in off.diagonals[row]
        if(any(!is.na(match(off.diagonals[[a]], b)))){
          # calculate EW distance
          if(length(col(stage)[a]:col(stage)[b]) > 2){
            # get tail indices
            p1.EW.tail.i <- c(head(stage[row(stage)[a],col(stage)[a]:col(stage)[b]], n = 1),
                             tail(stage[row(stage)[a],col(stage)[a]:col(stage)[b]], n = 1))
            p2.EW.tail.i <- c(head(stage[row(stage)[b],col(stage)[a]:col(stage)[b]], n = 1),
                              tail(stage[row(stage)[b],col(stage)[a]:col(stage)[b]], n = 1))
            # get EW distance
            p1.EW.d <- sum(sum(card[p1.EW.tail.i])/2,sum(card[stage[row(stage)[a],col(stage)[a]:col(stage)[b]][which(is.na(match(stage[row(stage)[a],col(stage)[a]:col(stage)[b]], p1.EW.tail.i)))]]))
            p2.EW.d <- sum(sum(card[p2.EW.tail.i])/2,sum(card[stage[row(stage)[b],col(stage)[a]:col(stage)[b]][which(is.na(match(stage[row(stage)[b],col(stage)[a]:col(stage)[b]], p2.EW.tail.i)))]]))
          } else {
            # get EW distance
            p1.EW.d <- sum(card[stage[row(stage)[a],col(stage)[a]:col(stage)[b]]])/2
            p2.EW.d <- sum(card[stage[row(stage)[b],col(stage)[a]:col(stage)[b]]])/2
          }
          # calculate NS distance
          if(length(row(stage)[a]:row(stage)[b]) > 2){
            # get tail indices
            p1.NS.tail.i <- c(head(stage[row(stage)[a]:row(stage)[b],col(stage)[b]], n = 1),
                              tail(stage[row(stage)[a]:row(stage)[b],col(stage)[b]], n = 1))
            p2.NS.tail.i <- c(head(stage[row(stage)[a]:row(stage)[b],col(stage)[a]], n = 1),
                              tail(stage[row(stage)[a]:row(stage)[b],col(stage)[a]], n = 1))
            # get distance
            p1.NS.d <- sum(sum(card[p1.NS.tail.i])/2, sum(card[stage[row(stage)[a]:row(stage)[b],col(stage)[b]][which(is.na(match(stage[row(stage)[a]:row(stage)[b],col(stage)[b]], p1.NS.tail.i)))]]))
            p2.NS.d <- sum(sum(card[p1.NS.tail.i])/2, sum(card[stage[row(stage)[a]:row(stage)[b],col(stage)[a]][which(is.na(match(stage[row(stage)[a]:row(stage)[b],col(stage)[a]], p2.NS.tail.i)))]]))
          } else {
            p1.NS.d <- sum(card[stage[row(stage)[a]:row(stage)[b],col(stage)[b]]])/2
            p2.NS.d <- sum(card[stage[row(stage)[a]:row(stage)[b],col(stage)[a]]])/2
          }
          # get hypotenuses
          p1.d <- sqrt(sum((p1.EW.d^2),(p1.NS.d^2)))
          p2.d <- sqrt(sum((p2.EW.d^2),(p2.NS.d^2)))
          # right angle aligned with destination
          if(od.path == "min"){
            dist <- min(p1.d,p2.d)
          }
          if(od.path == "max"){
            dist <- max(p1.d,p2.d)
          }
          if(od.path == "average"){
            dist <- mean(p1.d,p2.d)
          }
        }
      }
      return(dist)
    })
  })
  } else {
    # if function
    if(is.function(distance.alt)){
      distances <- distance.alt(stage, area)
    } else {
      # otherwise, assume it to be a distance matrix between each region
      distances <- distance.alt
    }
  }
  ## prepare to export
  out <- list("stage" = stage, "area" = area, "distances" = distances)
  saveRDS(out, file = paste0(name.out, "_stage.Rds"))
}
