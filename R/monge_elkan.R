#' @import stringdist
NULL

#' Monge-Elkan string similarity
#'
#' @param A String to be compared
#' @param B String to be compared
#' @param split Character to be used to split A and B (Default: " ")
#' @param sim Similatity method for intra-token distance (Default: "jw")
#' @param p Jaro-Winkler penalty (Default: 0.1)
#'
#' @export
monge_elkan <- function(A, B, split=" ", sim="jw", p=0.1){
    a <- unlist(strsplit(A, split=split))
    b <- unlist(strsplit(B, split=split))

    d <- 1 - stringdist::stringdistmatrix(a, b, method=sim, p=p)
    res <- (1/length(a))*sum(apply(d, 1, max))
    return(res)
}

#' Monge-Elkan string similarity matrix (w/ Jaro-Winkler intra-token distance)
#'
#' @param string_vecA Vector of strings to be compared
#' @param string_vecB Vector of strings to be compared
#'
#' @export
monge_elkan_matrix <- function(string_vecA, string_vecB){
    res <- sapply(string_vecA, function(x) sapply(string_vecB, function(y) monge_elkan(x,y)))
    return(res)
}
