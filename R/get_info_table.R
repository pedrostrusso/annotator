#' Get information table
#'
#' @param check_dir folder containing studies 
#' @param func a function for calculations. One of get_fnr, get_fpr or get_tpr.
#' @param methods the methods to evaluate. Default is all of ("jaro", "jw", "sw1", "nw1", 
#' "nw3", "me", "lv","dl", "osa", "lcs", "qgram", "cosine", "jaccard")
#'
#' @return a table containing the specified calculation for each study using each method
#' @export
#'
#' @examples
get_info_table <- function(check_dir, func, methods = c("jaro", "jw", "sw1", "nw1", "nw3", "me", "lv",
                                            "dl", "osa", "lcs", "qgram",
                                            "cosine", "jaccard")){
    #check_dir <- "/home/prusso/Documents/CSBL/annotator/intermediate/annot_test4"
    
    #studies <- grep("fail_", dir(check_dir), value=TRUE, invert=TRUE)
    studies <- dir(check_dir)
    
    cols <- c("studies", methods)
    
    cols <- lapply(cols, function(x) character(0))
    names(cols) <- c("studies", paste0(methods, "_cluster"))
    cluster_res <- as.data.frame(cols)
    
    cluster_list <- list()
    
    for(folder in studies){
        message("Study: ", folder)
        
        gse <- gsub("fail_", "", folder)
        
        study <- file.path(check_dir, folder, paste0(gse, "_auto_annotation.tsv"))
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
                row <- c(row, func(compare_col, true_col))
            }
        }
        cluster_list[[length(cluster_list)+1]] <- row
        #cluster_res[nrow(cluster_res)+1,] <- row
    }
    cluster_res <- as.data.frame(do.call("rbind", cluster_list))
    names(cluster_res) <- c("studies", paste0(methods, "_cluster"))
    
    write.table(cluster_res, "results/cluster_fnr.tsv", row.names=FALSE, quote=FALSE, sep="\t")
}