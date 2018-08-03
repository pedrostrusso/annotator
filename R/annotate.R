#' Annotate GEO samples
#'
#' @param annot A data.frame object containing sample annotation values
#' @param method String similarity measure to be used. One of ("jw", "jaro",
#'     "sw1", "sw2", "nw1", "nw2", "nw3", "me", "lv", "dl", "osa",
#'     "lcs", "qgram", "cosine", "jaccard")
#' @param column The column to be used to infer classes from. Default method "combine"
#'     attempts to combine all columns for class inferral.
#' @param ... Additional arguments to the distance functions
#' @export
annotate <- function(annot, method=c("jw", "jaro", "sw1", "sw2", "nw1", "nw2", "nw3", "me", "lv", "dl", "osa",
                                     "lcs", "qgram", "cosine", "jaccard"), column="combine", ...){

    method <- match.arg(method)
    column <- match.arg(column)

    annot0 <- annot

    rownames(annot) <- annot$Sample_geo_accession
    annot$Sample_geo_accession <- NULL
    annot$Sample_series_id <- NULL
    annot$Sample_platform_id <- NULL
    annot$Sample_supplementary_file <- NULL

    a <- sapply(annot, function(col){
        ifelse(length(unique(col)) == 1, FALSE, TRUE)
    })
    annot <- annot[, a, drop=FALSE]
    annot <- as.data.frame(apply(annot, 2, function(col) {
        as.character(trimws(gsub("(?!\\+)[[:punct:]'[:blank:]]+", " ", tolower(col), perl=TRUE)))
    }), stringsAsFactors = FALSE)

    if(column %in% names(annot)){
        annot <- as.data.frame(annot[, column], stringsAsFactors = FALSE)
    }else if(column == "combine"){
        annot$mega_col <- Reduce(paste, annot)
        annot <- as.data.frame(annot$mega_col, stringsAsFactors = FALSE)
        names(annot) <- "mega_col"
        annot0$mega_col <- annot$mega_col
    }

    scores_res <- lapply(names(annot), function(col){

        sdm <- calculate_distance(annot, method, col, ...)

        cl <- hdbscan2(xdist=sdm, minPts=3, gen_simplified_tree = FALSE, gen_hdbscan_tree = TRUE)

        clusts <- cl$cluster

        zeroes <- clusts[clusts == 0]
        if(length(zeroes) > 0){
            clusts[clusts == 0] <- seq(from=max(unique(clusts))+1, length.out=length(zeroes))
        }

        res <- as.data.frame(cbind(annot[, col], clusts, cl$membership_prob, cl$outlier_scores))
        names(res) <- c("name", "cluster", "membership", "outlier_score")
        #res <- res[order(res$cluster),]

        res <- list(res, cl$cluster_scores)
        names(res) <- c(col, "cluster_scores")
        res
    })
    names(scores_res) <- names(annot)

    # Stop if no clusters are found (only one cluster)
    if(do.call(sum, lapply(scores_res, function(x) length(unique(x$cluster)) == 1)) == length(scores_res)){
        stop("All strings were assigned to the same cluster. Unable to classify samples")
    }

    # Stop if all labels are in a separate cluster
    if(length(unique(scores_res[[1]][[1]]$cluster)) == nrow(annot0)){
        stop("Each string has been assigned to its own cluster. Unable to classify samples")
    }

    res <- scores_res[["mega_col"]]

    annot0$Class <- res[[1]]$cluster

    annot0 <- label_annot(annot0)

    return(annot0)
}
