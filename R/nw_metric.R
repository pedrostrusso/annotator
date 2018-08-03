#' @import NameNeedle
NULL

#' Needleman-Wunsch string similarity
#'
#' @param A String to be compared
#' @param B String to be compared
#' @param my_params Match, mismatch, gap and gap character values
#'     (Default: NameNeedle::defaultNeedleParams)
#'
#' @export
nw_metric <- function(A, B, nw_match=1, nw_mismatch=-1, gap=-1, gap_char="*"){
    #my_params <- NameNeedle::defaultNeedleParams
    my_params <- list()
    my_params$MATCH <- nw_match
    my_params$MISMATCH <- nw_mismatch
    my_params$GAP <- gap
    my_params$GAPCHAR <- gap_char

    res <- sqrt(NameNeedle::needleScores(A, A, params=my_params) +
                    NameNeedle::needleScores(B, B, params=my_params) -
                    2*NameNeedle::needleScores(A, B, params=my_params))
    return(res)
}

#' Needleman-Wunsch string similarity matrix
#'
#' @param string_vecA Vector of strings to be compared
#' @param string_vecB Vector of strings to be compared
#' @param ... Additional arguments to \code{nw_metrix()} function
#'     (Default: NameNeedle::defaultNeedleParams)
#'
#' @export
nw_matrix <- function(string_vecA, string_vecB, ...){
    res <- sapply(string_vecA, function(A) sapply(string_vecB, function(B) nw_metric(A, B, ...)))
    return(res)
}
