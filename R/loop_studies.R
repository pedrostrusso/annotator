#' @import data.table
NULL

#' Loop over studies and annotate sample classes
#'
#' @param studies_folder Directory from which to get study annotation files
#' @param out_folder Directory to write the annotated file to
#' @param methods Similarity measures to be applied to each sample annotation file
#'
#' @import data.table

loop_studies <- function(studies_folder, out_folder,
                         methods=c("jw", "sw", "nw", "me", "lv", "dl",
                                   "osa", "lcs", "qgram", "cosine", "jaccard")){
    studies <- dir(studies_folder)
    sapply(studies, function(folder){
        message("Study: ", folder)

        study <- file.path(studies_folder, folder, paste0(folder, "_annotation.tsv"))
        annot0 <- data.table::fread(study, data.table=F)
        res <- annot0

        for(method in methods){
            message("Method: ", method)
            annot <- annot0

            possible_error <- tryCatch(w <- annotate(annot, method = method, p=0.1),
                                       error=function(e) e)

            if(!inherits(possible_error, "error")){
                w <- possible_error
                w <- w[, c("mega_col", "Class", "label", "label2", "label3", "label4")]

                res[, "mega_col"] <- w$mega_col
                res[, paste0(method, "_cluster")] <- w$Class
                res[, paste0(method, "_label1")] <- w$label
                res[, paste0(method, "_label2")] <- w$label2
                res[, paste0(method, "_label3")] <- w$label3
                res[, paste0(method, "_label4")] <- w$label4
            }
        }
        out <- file.path(out_folder, folder, paste0(folder, "_auto_annotation.tsv"))
        res <- subset(res, select = -c(Sample_series_id,
                                       Sample_platform_id,
                                       Sample_supplementary_file))
        dir.create(file.path(out_folder, folder))
        write.table(res, out, sep="\t", row.names=FALSE, quote=FALSE)
    })
}
