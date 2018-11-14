#' Calculate false positive rate between two clustering methods
#'
#' @param group1 The first clustering method
#' @param group2 The reference ("true") method
#'
#' @return The false positive rate
#' @export
#'
#' @examples
#' g1 <- sample(1:2, size=10, replace=TRUE)
#' g2 <- sample(1:3, size=10, replace=TRUE)
#' get_fpr(g1, g2)
get_fpr <- function(group1, group2){
    x <- abs(sapply(group1, function(x) x - group1))
    x[x > 1] <- 1
    y <- abs(sapply(group2, function(x) x - group2))
    y[y > 1] <- 1

    diffs <- x - y
    sums <- x + y
    diffs <- diffs[lower.tri(diffs)]
    sums <- sums[lower.tri(sums)]

    fp <- sum(abs(diffs[diffs < 0]))
    tn <- length(sums[sums == 2])
    fpr <- fp/(fp+tn)
    return(fpr)
}

#' Calculate true positive rate between two clustering methods
#'
#' @param group1 The first clustering method
#' @param group2 The reference ("true") method
#'
#' @return The true positive rate
#' @export
#'
#' @examples
#' g1 <- sample(1:2, size=10, replace=TRUE)
#' g2 <- sample(1:3, size=10, replace=TRUE)
#' get_tpr(g1, g2)
get_tpr <- function(group1, group2){
    x <- abs(sapply(group1, function(x) x - group1))
    x[x > 1] <- 1
    y <- abs(sapply(group2, function(x) x - group2))
    y[y > 1] <- 1

    diffs <- x - y
    sums <- x + y
    diffs <- diffs[lower.tri(diffs)]
    sums <- sums[lower.tri(sums)]

    tp <- length(sums[sums == 0])
    fn <- length(diffs[diffs == 1])
    tpr <- tp/(tp+fn)
    return(tpr)
}

#' Calculate false negative rate between two clustering methods
#'
#' @param group1 The first clustering method
#' @param group2 The reference ("true") method
#'
#' @return The false negative rate
#' @export
#'
#' @examples
#' g1 <- sample(1:2, size=10, replace=TRUE)
#' g2 <- sample(1:3, size=10, replace=TRUE)
#' get_fnr(g1, g2)
get_fnr <- function(group1, group2){
    x <- abs(sapply(group1, function(x) x - group1))
    x[x > 1] <- 1
    y <- abs(sapply(group2, function(x) x - group2))
    y[y > 1] <- 1

    diffs <- x - y
    sums <- x + y
    diffs <- diffs[lower.tri(diffs)]
    sums <- sums[lower.tri(sums)]

    fn <- length(diffs[diffs == 1])
    tp <- length(sums[sums == 0])
    fnr <- fn/(tp + fn)
    return(fnr)
}
