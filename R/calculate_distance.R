#' @import stringdist
#' @import NameNeedle
NULL

#' Calculate distance
#'
#' @param annot A data.frame object containing sample annotation values
#' @param method String similarity measure to be used. One of ("jw", "jaro",
#'     "sw1", "sw2", "nw1", "nw2", "nw3", "me", "lv", "dl", "osa",
#'     "lcs", "qgram", "cosine", "jaccard")
#' @param col The column to be used to infer classes from. Default method "combine"
#'     attempts to combine all columns for class inferral.
#' @param ... Additional arguments to the distance functions
#' @export
calculate_distance <- function(annot, method, col, ...){
    if(method == "sw1"){
        sdm <- sw_matrix(annot[, col], annot[, col])
    }else if(method == "sw2"){
        sdm <- pw_alignment_matrix(annot[, col], annot[, col], type="local")
    }else if(method == "me"){
        sdm <- monge_elkan_matrix(annot[, col], annot[, col])
    }else if(method == "nw1"){
        sdm <- nw_matrix(annot[, col], annot[, col], ...)
    }else if(method == "nw2"){
        sdm <- pw_alignment_matrix(annot[, col], annot[, col], type="global")
    }else if (method == "nw3"){
        my_params <- NameNeedle::defaultNeedleParams
        my_params$MATCH <- 2
        my_params$MISMATCH <- -2
        sdm <- nw_matrix(annot[, col], annot[, col], my_params=my_params)
    }else if(method == "jw"){
        sdm <- stringdist::stringdistmatrix(annot[, col], annot[, col], method=method, p=0.1)
    }else if(method == "jaro"){
        sdm <- stringdist::stringdistmatrix(annot[, col], annot[, col], method="jw", p=0)
    }else{
        sdm <- stringdist::stringdistmatrix(annot[, col], annot[, col], method=method)
    }
    return(sdm)
}
