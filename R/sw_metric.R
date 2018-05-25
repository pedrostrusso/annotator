#' @import textreuse
NULL

#' Smith-Waterman similarity metric
#'
#' @param A String to be compared
#' @param B String to be compared
#' @export
sw_metric <- function(A, B){
    res <- sqrt(textreuse::align_local(A, B)$score +
                    textreuse::align_local(B, B)$score -
                    2*textreuse::align_local(A, B)$score)
    return(res)
}

#' Smith-Waterman similarity matrix
#'
#' @param string_vecA Vector of strings to be compared
#' @param string_vecB Vector of strings to be compared
#' @export
sw_matrix <- function(string_vecA, string_vecB){
    res <- sapply(string_vecA, function(A) sapply(string_vecB, function(B) sw_metric(A, B)))
    return(res)
}
