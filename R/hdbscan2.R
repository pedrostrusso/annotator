#' @import dbscan
NULL

#' Calculate HDBSCAN algorithm
#'
#' This is a modified version of the hdbscan function from the dbscan package
#' in order to be able to calculate the KNN distance starting from a distance
#' matrix.
#'
#' @param xdist A distance matrix
#' @param minPts Minimum number of points per cluster
#'
#' @keywords internal
#' @export
#'
hdbscan2 <- function(xdist, minPts=5) {

    ## Calculate Core distance using kNN
    euc_dist <- as.dist(xdist)
    #euc_dist <- dist(as.matrix(xdist), method = "euclidean")

    #euc_dist <- stringdistmatrix(annot[, col], annot[, col], method="", p=p)

    core_dist <- kNNdist2(xdist, k = minPts - 1)[, minPts - 1]

    ## Mutual Reachability matrix
    mrd <- dbscan:::mrd(euc_dist, core_dist)

    ## Get MST, convert to RSL representation
    n <- nrow(xdist)
    mst <- dbscan:::prims(mrd, n)
    hc <- dbscan:::hclustMergeOrder(mst, order(mst[, 3]))
    hc$call <- match.call()

    ## Process the hierarchy to retrieve all the necessary info needed by HDBSCAN
    res <- dbscan:::computeStability(hc, minPts, compute_glosh = TRUE)
    res <- dbscan:::extractUnsupervised(res)
    cl <- attr(res, "cluster")
    sl <- attr(res, "salient_clusters")

    ## Generate membership 'probabilities' using core distance as the measure of density
    prob <- rep(0, length(cl))
    for (cid in sl){
        ccl <- res[[as.character(cid)]]
        max_f <- max(core_dist[which(cl == cid)])
        pr <- (max_f - core_dist[which(cl == cid)])/max_f
        prob[cl == cid] <- pr
    }

    ## Match cluster assignments to be incremental, with 0 representing noise
    if (any(cl == 0)) {
        cluster <- match(cl, c(0, sl))-1
    } else { cluster <- match(cl, sl) }
    cl_map <- structure(sl, names=unique(cluster[hc$order][cluster[hc$order] != 0]))

    ## Stability scores
    ## NOTE: These scores represent the stability scores -before- the hierarchy traversal
    cluster_scores <- sapply(sl, function(sl_cid) res[[as.character(sl_cid)]]$stability)
    names(cluster_scores) <- names(cl_map)

    ## Return everything HDBSCAN does
    attr(res, "cl_map") <- cl_map # Mapping of hierarchical IDS to 'normalized' incremental ids
    out <- structure(list(cluster=cluster, minPts=minPts,
                          cluster_scores=cluster_scores, # (Cluster-wide cumulative) Stability Scores
                          membership_prob=prob, # Individual point membership probabilities
                          outlier_scores=attr(res, "glosh"), # Outlier Scores
                          hc=hc # Hclust object of MST (can be cut for quick assignments)
    ), class="hdbscan", hdbscan=res) # hdbscan attributes contains actual HDBSCAN hierarchy

    return(out)
}
