#' Annotate GEO samples
#'
#' @param annot A data.frame object containing sample annotation values
#' @param method String similarity measure to be used. One of ("jw", "jaro",
#'     "sw", "nw", "me", "lv", "dl", "osa", "lcs", "qgram", "cosine", "jaccard")
#' @param column The column to be used to infer classes from. Default method "combine"
#'     attempts to combine all columns for class inferral.
#' @param ... Additional arguments to the distance functions
#' @export
annotate <- function(annot, method=c("jw", "jaro", "sw", "nw", "me", "lv", "dl", "osa",
                                      "lcs", "qgram", "cosine", "jaccard"), p=0, ...){

    method <- match.arg(method)

    annot0 <- annot
    annot$Sample_geo_accession <- NULL
    a <- sapply(annot, function(col){
        ifelse(length(unique(col)) == 1, FALSE, TRUE)
    })
    annot <- annot[, a, drop=FALSE]

    annot <- as.data.frame(lapply(annot, function(col) {
        as.character(trimws(gsub("(?!\\+)[[:punct:]'[:blank:]]+", " ", tolower(col), perl=TRUE)))
    }), stringsAsFactors = FALSE)

    annot$mega_col <- Reduce(paste, annot)
    annot <- as.data.frame(annot$mega_col, stringsAsFactors = FALSE)
    names(annot) <- "mega_col"
    annot0$mega_col <- annot$mega_col

    column <- "mega_col"
    scores_res <- get_clusters(annot, method, column, ...)

    # Stop if no clusters are found (only one cluster)
    if(length(unique(scores_res[[1]]$cluster)) == 1){
        stop("All strings were assigned to the same cluster. Unable to classify samples")
    }

    # Stop if all labels are in a separate cluster
    if(length(unique(scores_res[[1]]$cluster)) == nrow(scores_res[[1]])){
        stop("Each string has been assigned to its own cluster. Unable to classify samples")
    }

    annot0$Class <- scores_res[[1]]$cluster

    annot0 <- label_annot(annot0)

    return(annot0)
}
