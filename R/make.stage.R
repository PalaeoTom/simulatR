#' Create basic stage object
#'
#' 'make.stage()' generates an object of class "stage", which is a list with at least three elements (more can be added with other functions). The first is a matrix with each cell (hereafter region) assigned an identifying number (the 'regions').
#' The second is a corresponding matrix presenting the area values assigned to each of these regions (the 'dimensions'). The third is a distance matrix where each column and row represents a region,
#' and the values within the distances separating them (the 'distances').
#'
#' The default distance calculation method assumes each region to be a square and takes the square root of the area
#' as the latitudinal and longitudinal dimensions of the region. Pythagoras' theorem is applied to these latitudinal and longitudinal distances to get the distance to cross these regions diagonally.
#' Distances between regions are calculated assuming that species have to migrate from the center of one region to the center of another. As such, when the distance separating two
#' regions is calculated, half the latitudinal, longitudinal, or diagonal distance (depending on direction of travel) of the origin and destination regions is added to the distance values of any regions
#' the species have to traverse. The distances separating regions that are not longitudinally, latitudinally, or diagonally aligned are also calculated using Pythagoras' theorem, using the summed latitudinal
#' distances of all regions comprising the adjacent side and the summed longitudinal distances of all regions comprising the opposite. Two hypotenuses are calculated; one where the adjacent side is aligned
#' with the origin region, and one where it is aligned with the destination. The minimum, maximum, or average of these distances are then assigned as the distance between the two regions, depending on 'od.path'.
#'
#' A separate distance calculation method can be provided via the argument 'distance.alt'. This should be provided in the form of a function with arguments 'stage' and 'dimensions', which accept matrices formatted
#' as described.
#'
#' Outputted as an object by default. Can be exported as an Rds file.
#'
#' @param n.col The number of columns desired for this stage.
#' @param n.row The number of rows desired for this stage.
#' @param ar The area of each region. Can be a single numeric value, in which case all regions are assigned the same area, or a numeric vector of length n.col*n.row, in which case area values are assigned by index.
#' @param export If TRUE, stage will be saved as an Rds file. Default is FALSE.
#' @param name.out A string to be included in the Rds output file name if export = TRUE. Default is "new".
#' @param distance.alt Either FALSE (default) or an alternative function for calculating the distances between regions.
#' @param od.path How to calculate the distance between regions which are not diagonally, longitudinally, or latitudinally aligned with one another when the default distance calculation method is used.
#' For such distances, the default method calculates two hypotenuses using Pythagoras' theorem; one where the adjacent side is latitudinally aligned with the origin region, and one where
#' it is aligned with the destination region. "min" assigns the smallest of these two hypotenuses as the distance between these regions. "max" the largest".
#' :average" (the default) assigns the mean value.
#'
#' @return A basic stage object.
#' @import utils
#' @export
#'
#' @examples
#' # all regions have the same area
#' stage <- make.stage(3,3,100)
#'
#' # regions have different areas
#' stage <- make.stage(3,3,c(100,200,300,400,500,600,700,800,900))
#'
make.stage <- function(n.col, n.row, ar, export = F, name.out = "new", distance.alt = F, od.path = "average") {
  # get number of cells
  n.cell <- n.col*n.row
  # make regions
  regions <- matrix(seq(1:n.cell), n.row, n.col)
  # set dimensions of each cell
  dimensions <- matrix(ar, n.row, n.col)
  # get distances between regions
  if(isFALSE(distance.alt)){
    # get distance to cross each region in a cardinal direction
    card <- matrix(sqrt(dimensions), n.row, n.col)
    # get distance to cross each region diagonally
    diag <- apply(card, c(1,2), function(x) sqrt(2*(x^2)))
    # get diagonal migratory pathways for each cell
    diagonals <- get.diag(regions)
    # get cardinal migratory pathways for each cell
    latlongs <- get.latlong(regions)
    # get off-diagonals
    off.diagonals <- lapply(1:length(regions), function(y){
      # get cells that are neither cardinal or diagonal in their position relative to the cell in question
      out <- intersect(which(is.na(match(seq(1:length(regions)), diagonals[[y]]))), which(is.na(match(seq(1:length(regions)), latlongs[[y]]))))
      # remove cell ID from vector
      out <- out[-which(out == y)]
    })
    # compute distances
  distances <- sapply(1:length(regions), function(a){
    by.col <- sapply(1:length(regions), function(b){
      if(a == b) {
        dist <- 0
      } else {
        # if column index (destination) present in diagonals[row]
        if(any(!is.na(match(diagonals[[a]], b)))){
          # if only 1 region separates origin and destination
          if(abs(row(regions)[a]-row(regions)[b]) == 2){
            # get number of regions between diagonal elements
            diff <- (a-b)/(abs(row(regions)[a]-row(regions)[b]))
            # get distance
            dist <- diag[a]/2 + diag[a-diff] + diag[b]/2
          }
          # if more than 1 region separates origin and destination
          if(abs(row(regions)[a]-row(regions)[b]) > 2){
            # get number of regions between diagonal elements
            diff <- (a-b)/(abs(row(regions)[a]-row(regions)[b]))
            # get number of diagonal elements in between original and destination
            n <- abs(row(regions)[a]-row(regions)[b])-1
            # get distance
            dist <- diag[a]/2 + sum(sapply(1:n, function(t) out <- diag[a-(diff*t)])) + diag[b]/2
          }
          # if no regions separate origin and destination
          if(abs(row(regions)[a]-row(regions)[b]) == 1){
            dist <- diag[a]/2 + diag[b]/2
          }
        }
        # if column index (destination) present in latlongs[row]
        if(any(!is.na(match(latlongs[[a]], b)))){
          # if east/west
          if(abs(col(regions)[a]-col(regions)[b]) >= 1 && abs(row(regions)[a]-row(regions)[b]) == 0){
            # if exactly 1 region separates origin and destination
            if(abs(col(regions)[a]-col(regions)[b]) == 2){
              # get number of regions between cardinal elements
              diff <- (a-b)/(abs(col(regions)[a]-col(regions)[b]))
              # get distance
              dist <- card[a]/2 + card[a-diff] + card[b]/2
            }
            # if more than 1 region separates origin and destination
            if(abs(col(regions)[a]-col(regions)[b]) > 2){
              # get number of regions between cardinal elements
              diff <- (a-b)/(abs(col(regions)[a]-col(regions)[b]))
              # get number of cardinal elements in between original and destination
              n <- abs(col(regions)[a]-col(regions)[b])-1
              # get distance
              dist <- card[a]/2 + sum(sapply(1:n, function(t) out <- card[a-(diff*t)])) + card[b]/2
            }
            # if no regions separate origin and destination
            if(abs(col(regions)[a]-col(regions)[b]) == 1){
              dist <- card[a]/2 + card[b]/2
            }
          }
          # if north/south
          if(abs(row(regions)[a]-row(regions)[b]) >= 1 && abs(col(regions)[a]-col(regions)[b]) == 0){
            # if exactly 1 region separates origin and destination
            if(abs(row(regions)[a]-row(regions)[b]) == 2){
              # get number of regions between cardinal elements
              diff <- (a-b)/(abs(row(regions)[a]-row(regions)[b]))
              # get distance
              dist <- card[a]/2 + card[a-diff] + card[b]/2
            }
            # if more than 1 region separates origin and destination
            if(abs(row(regions)[a]-row(regions)[b]) > 2){
              # get number of regions between cardinal elements
              diff <- (a-b)/(abs(row(regions)[a]-row(regions)[b]))
              # get number of cardinal elements in between original and destination
              n <- abs(row(regions)[a]-row(regions)[b])-1
              # get distance
              dist <- card[a]/2 + sum(sapply(1:n, function(t) out <- card[a-(diff*t)])) + card[b]/2
            }
            # if no regions separate origin and destination
            if(abs(row(regions)[a]-row(regions)[b]) == 1){
              dist <- card[a]/2 + card[b]/2
            }
          }
        }
        # if column index (destination) present in off.diagonals[row]
        if(any(!is.na(match(off.diagonals[[a]], b)))){
          # calculate EW distance
          if(length(col(regions)[a]:col(regions)[b]) > 2){
            # get tail indices
            p1.EW.tail.i <- c(head(regions[row(regions)[a],col(regions)[a]:col(regions)[b]], n = 1),
                             tail(regions[row(regions)[a],col(regions)[a]:col(regions)[b]], n = 1))
            p2.EW.tail.i <- c(head(regions[row(regions)[b],col(regions)[a]:col(regions)[b]], n = 1),
                              tail(regions[row(regions)[b],col(regions)[a]:col(regions)[b]], n = 1))
            # get EW distance
            p1.EW.d <- sum(sum(card[p1.EW.tail.i])/2,sum(card[regions[row(regions)[a],col(regions)[a]:col(regions)[b]][which(is.na(match(regions[row(regions)[a],col(regions)[a]:col(regions)[b]], p1.EW.tail.i)))]]))
            p2.EW.d <- sum(sum(card[p2.EW.tail.i])/2,sum(card[regions[row(regions)[b],col(regions)[a]:col(regions)[b]][which(is.na(match(regions[row(regions)[b],col(regions)[a]:col(regions)[b]], p2.EW.tail.i)))]]))
          } else {
            # get EW distance
            p1.EW.d <- sum(card[regions[row(regions)[a],col(regions)[a]:col(regions)[b]]])/2
            p2.EW.d <- sum(card[regions[row(regions)[b],col(regions)[a]:col(regions)[b]]])/2
          }
          # calculate NS distance
          if(length(row(regions)[a]:row(regions)[b]) > 2){
            # get tail indices
            p1.NS.tail.i <- c(head(regions[row(regions)[a]:row(regions)[b],col(regions)[b]], n = 1),
                              tail(regions[row(regions)[a]:row(regions)[b],col(regions)[b]], n = 1))
            p2.NS.tail.i <- c(head(regions[row(regions)[a]:row(regions)[b],col(regions)[a]], n = 1),
                              tail(regions[row(regions)[a]:row(regions)[b],col(regions)[a]], n = 1))
            # get distance
            p1.NS.d <- sum(sum(card[p1.NS.tail.i])/2, sum(card[regions[row(regions)[a]:row(regions)[b],col(regions)[b]][which(is.na(match(regions[row(regions)[a]:row(regions)[b],col(regions)[b]], p1.NS.tail.i)))]]))
            p2.NS.d <- sum(sum(card[p1.NS.tail.i])/2, sum(card[regions[row(regions)[a]:row(regions)[b],col(regions)[a]][which(is.na(match(regions[row(regions)[a]:row(regions)[b],col(regions)[a]], p2.NS.tail.i)))]]))
          } else {
            p1.NS.d <- sum(card[regions[row(regions)[a]:row(regions)[b],col(regions)[b]]])/2
            p2.NS.d <- sum(card[regions[row(regions)[a]:row(regions)[b],col(regions)[a]]])/2
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
      distances <- distance.alt(regions, dimensions)
    } else {
      # otherwise, assume it to be a distance matrix between each region
      distances <- distance.alt
    }
  }
  ## prepare output object
  out <- list("regions" = regions, "dimensions" = dimensions, "distances" = distances)
  ## set class to stage
  out <- structure(out, class = "stage")
  ## export if set
  if(export){
    saveRDS(out, file = paste0(name.out, "_stage.Rds"))
  }
  return(out)
}

test <- make.stage(3,3,400)
