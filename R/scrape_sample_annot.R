#' @importFrom rentrez entrez_search entrez_summary
#' @importFrom plyr ldply
#' @importFrom rvest html_nodes html_text
#' @importFrom xml2 as_xml_document read_html
NULL

scrape_sample_annot <- function(gse_id){
    gds_search <- rentrez::entrez_search(db="gds", term=paste0(gse_id, "[ACCN] AND gsm[ETYP]"))
    search_res <- rentrez::entrez_summary(db="gds", id=gds_search$ids)
    res <- lapply(search_res, unlist)
    res <- plyr::ldply(res)
    gsm_ids <- res$accession
    sample_chars <- sapply(gsm_ids, scrape_sample_char)
    res$Sample_characteristics_ch1 <- sample_chars
    res$gse <- paste0("GSE", res$gse)
    res$gpl <- paste0("GPL", res$gpl)
    res$gds <- paste0("GDS", res$gds)
    res <- subset(res, select=c("gse", "accession", "gpl", "title", "summary", "Sample_characteristics_ch1"))
    names(res) <- c("Sample_series_id", "Sample_geo_accession", "Sample_platform_id",
                    "Sample_title", "Sample_source_name_ch1", "Sample_characteristics_ch1")
    return(res)
}

scrape_sample_char <- function(gsm_id){
    url <- paste0("https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=", gsm_id)
    webpage <- read_html(url)
    chars <- html_nodes(webpage, xpath="//td[.='Characteristics']/following-sibling::td[1]")
    chars <- gsub("<br>|\n", " ", chars)
    chars <- html_text(xml2::as_xml_document(chars))
    chars <- trimws(chars)
    return(chars)
}
