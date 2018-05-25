#' @import PTXQC
NULL

#' Automatic label creation
#'
#' @param annot0 Sample annotation data.frame with a column called "mega_col"
#'     to infer labels from.
#'
#' @export
label_annot <- function(annot0){

    x <- annot0[, c("mega_col", "Class")]

    all_strings <- annot0$mega_col
    all_keywords <- unlist(strsplit(all_strings, " "))
    all_label <- unique(all_keywords[all_keywords %in% names(which(table(all_keywords) >= length(all_strings)))])

    for(cluster in as.character(unique(x$Class))){
        # 1st label: unique cluster keywords (words repeated in all instances of the class)
        clust_data <- x[x$Class == cluster, ]
        strings <- clust_data$mega_col
        keywords <- unlist(strsplit(strings, " "))
        label <- paste(unique(keywords[keywords %in% names(which(table(keywords) >= length(strings)))]), collapse=" ")
        annot0[annot0$Class == cluster, "label"] <- label

        # 2nd label: unique cluster keywords not present in keywords across clusters
        label <- unique(keywords[keywords %in% names(which(table(keywords) >= length(strings)))])
        label <- paste(label[!label %in% all_label], collapse = " ")
        annot0[annot0$Class == cluster, "label2"] <- label

        # 3rd label: longest common substring in the cluster
        label <- trimws(PTXQC::LCSn(clust_data$mega_col))
        annot0[annot0$Class == cluster, "label3"] <- label
    }
    return(annot0)
}
