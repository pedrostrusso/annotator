#' @importFrom fossil rand.index
NULL

#' Get Rand index of annotated samples
#'
#' @param check_dir Directory which to check the annotations
#' @param methods Similarity methods to get cluster values
#'
#' @export
get_rand <- function(check_dir, methods = c("jw", "sw1", "nw1", "me", "lv",
                                             "dl", "osa", "lcs", "qgram",
                                             "cosine", "jaccard")){
    #check_dir <- "/home/prusso/Documents/CSBL/annotator/intermediate/annot_test2"

    studies <- grep("fail_", dir(check_dir), value=TRUE, invert=TRUE)

    cols <- c("studies", methods)

    cols <- lapply(cols, function(x) character(0))
    names(cols) <- c("studies", paste0(methods, "_cluster"))
    cluster_res <- as.data.frame(cols)

    cluster_list <- list()

    for(folder in studies){
        message("Study: ", folder)

        study <- file.path(check_dir, folder, paste0(folder, "_auto_annotation.tsv"))
        annot0 <- data.table::fread(study, data.table=F)

        cluster_cols <- grep("cluster", names(annot0), value=T)

        true_col <- factor(annot0$TRUE_col)
        levels(true_col) <- seq(1, length(unique(true_col)))
        true_col <- as.numeric(true_col)

        row <- folder
        for(column in names(cluster_res)[-1]){
            if(!column %in% cluster_cols){
                row <- c(row, NA)
            }else{
                compare_col <- annot0[, column]
                row <- c(row, rand.index(compare_col, true_col))
            }
        }
        cluster_list[[length(cluster_list)+1]] <- row
        #cluster_res[nrow(cluster_res)+1,] <- row
    }
    cluster_res <- as.data.frame(do.call("rbind", cluster_list))
    names(cluster_res) <- c("studies", paste0(methods, "_cluster"))

    write.table(cluster_res, "cluster_res2.tsv", row.names=FALSE, quote=FALSE, sep="\t")
}
