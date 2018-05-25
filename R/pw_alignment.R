#' @import Biostrings
NULL

#' Pairwise alignment
#'
#' @param A String to be compared
#' @param B String to be compared
#' @param type Type of alignment
#' @param gapOpening Gap opening penalty
#' @param gapExtension Gap extension penalty
#' @param scoreOnly Return just the score for the similarity (Default: TRUE)
#'
#' @export
pw_alignment_metric <- function(A, B, type="global", gapOpening=10, gapExtension=4, scoreOnly=TRUE){
    res <- sqrt(pairwiseAlignment(A, A,
                                  type=type,
                                  gapOpening=gapOpening,
                                  gapExtension=gapExtension,
                                  scoreOnly=scoreOnly)
                + pairwiseAlignment(B, B,
                                    type=type,
                                    gapOpening=gapOpening,
                                    gapExtension=gapExtension,
                                    scoreOnly=scoreOnly)
                - 2*pairwiseAlignment(A, B,
                                      type=type,
                                      gapOpening=gapOpening,
                                      gapExtension=gapExtension,
                                      scoreOnly=scoreOnly))
    return(res)
}


#' Pairwise alignment matrix
#'
#' @param string_vecA Vector of strings to be compared
#' @param string_vecB Vector of strings to be compared
#' @param type Type of alignment
#' @param gapOpening Gap opening penalty
#' @param gapExtension Gap extension penalty
#' @param scoreOnly Return just the score for the similarity (Default: TRUE)
#'
#' @export
pw_alignment_matrix <- function(string_vecA, string_vecB,
                                type="global", gapOpening=10,
                                gapExtension=4, scoreOnly=TRUE){
    res <- sapply(string_vecA, function(A){
        sapply(string_vecB, function(B){
            pw_alignment_metric(A, B,
                                type=type,
                                gapOpening=gapOpening,
                                gapExtension=gapExtension,
                                scoreOnly=scoreOnly)
        })
    })
    return(res)
}


