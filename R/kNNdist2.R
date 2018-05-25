#' @import FastKNN
NULL

#' Calculate k-Nearest Neighbors Distance
#'
#' @param sdm String Distance Matrix, as returned by \code{calculate_distance} function
#' @param k Number of neighbors to compute distance
#'
#' @keywords internal
#' @export
kNNdist2 <- function(sdm, k){
    n <- nrow(sdm)
    nn <- matrix(0, n, k) # n x k
    nd <- nn
    for (i in 1:n){
        nn[i, ] <- FastKNN::k.nearest.neighbors(i, sdm, k = k)
        nd[i, ] <- sdm[i, nn[i, ]]
    }
    return(nd)
}

