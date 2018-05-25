#' Get sample annotation data
#'
#' @param out_file File to write data to
#' @param geo_id GSE ID to get data
#'
#' @export
#'
get_sample_annot <- function(out_file, geo_id){
    path <- system.file(package="annotator", "get_sample_annot.py")
    command <- paste("python", path, out_file, geo_id, ">", out_file)
    system(command, intern=FALSE)
}
