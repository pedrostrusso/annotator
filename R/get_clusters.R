get_clusters <- function(annot, method, col, ...){
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
    return(res)
}
