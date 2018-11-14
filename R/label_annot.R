#' @importFrom PTXQC LCSn
#' @importFrom qualV LCS
NULL

#' Automatic label creation
#'
#' @param annot0 Sample annotation data.frame with a column called "mega_col"
#'     to infer labels from.
#' @param method Character string corresponding to a method to create the labels by.
#' One of "all", "common", "unique", "lcsubstr", "lcsubseq". See Details.
#'
#' @details The methods available for defining class labels are as follows:
#' \itemize{
#'    \item{all }{Return all labelling methods.}
#'    \item{common }{Strings shared by all instances of a class}
#'    \item{unique }{Strings unique to the instances of a particular class}
#'    \item{lcsubstr }{Longest common substring in a class}
#'    \item{lcsubseq }{Longest common subsequence in a class}
#' }
#' @export
label_annot2 <- function(annot0, method="all"){

    x <- annot0[, c("mega_col", "Class")]

    all_strings <- annot0$mega_col
    all_keywords <- unlist(strsplit(all_strings, " "))
    all_label <- unique(all_keywords[all_keywords %in% names(which(table(all_keywords) >= length(all_strings)))])

    if(method == "all"){
        label_names <- c("label1", "label2", "label3", "label4")
    }else{
        label_names <- c("label", "label", "label", "label")
    }

    for(cluster in as.character(unique(x$Class))){
        if(method == "all" | method == "common"){
            # 1st label: unique cluster keywords (words repeated in all instances of the class)
            clust_data <- x[x$Class == cluster, ]
            strings <- clust_data$mega_col
            keywords <- unlist(strsplit(strings, " "))
            label <- paste(unique(keywords[keywords %in% names(which(table(keywords) >= length(strings)))]), collapse=" ")
            annot0[annot0$Class == cluster, label_names[1]] <- label
        }

        if(method == "all" | method == "unique"){
            # 2nd label: unique cluster keywords not present in keywords across clusters
            label <- unique(keywords[keywords %in% names(which(table(keywords) >= length(strings)))])
            label <- paste(label[!label %in% all_label], collapse = " ")
            annot0[annot0$Class == cluster, label_names[2]] <- label
        }

        if(method == "all" | method == "lcsubstr"){
            # 3rd label: longest common substring in the cluster
            label <- trimws(PTXQC::LCSn(clust_data$mega_col))
            annot0[annot0$Class == cluster, label_names[3]] <- label
        }

        if(method == "all" | method == "lcsubseq"){
            # 4th label: longest common subsequence in the cluster
            res <- character(0)
            for(string in clust_data$mega_col){
                a <- unlist(strsplit(string, " "))
                if(length(res) == 0){
                    res <- a
                }else{
                    z <- qualV::LCS(res, a)
                    res <- z$LCS
                }
            }
            label <- paste(res, collapse= " ")
            annot0[annot0$Class == cluster, label_names[4]] <- label
        }
    }
    return(annot0)
}
