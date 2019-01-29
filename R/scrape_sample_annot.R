#' @importFrom rentrez entrez_search entrez_summary
#' @importFrom plyr ldply
#' @importFrom dplyr bind_rows
#' @importFrom rvest html_nodes html_text
#' @importFrom xml2 as_xml_document read_html
NULL

scrape_sample_char <- function(gsm_id){
    url <- paste0("https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=", gsm_id)
    webpage <- read_html(url)
    chars <- html_nodes(webpage, xpath="//td[.='Characteristics']/following-sibling::td[1]")
    chars <- gsub("<br>|\n", " ", chars)
    chars <- html_text(xml2::as_xml_document(chars))
    chars <- trimws(chars)
    return(chars)
}

scrape_sample_annot <- function(gse_id){
    gds_search <- rentrez::entrez_search(db="gds", term=paste0(gse_id, "[ACCN] AND gsm[ETYP]"))
    sample_count <- gds_search$count

    results <- data.frame()
        
    for(seq_start in seq(0, sample_count, 40)){
        if(seq_start == sample_count) break
        
        gds_search <- rentrez::entrez_search(db="gds", term=paste0(gse_id, "[ACCN] AND gsm[ETYP]"),
                                             use_history=TRUE, retmax=40, retstart=seq_start)
        search_res <- rentrez::entrez_summary(db="gds", id=gds_search$ids)
        if(!inherits(search_res, "esummary_list")){
            temp <- list()
            uid <- search_res$uid
            temp[[uid]] <- search_res
            search_res <- temp
        }
        res <- lapply(search_res, unlist)
        res <- plyr::ldply(res)
        results <- bind_rows(results, res)
    }
            
    gsm_ids <- results$accession
    results$Sample_characteristics_ch1 <- sapply(gsm_ids, scrape_sample_char)
    results$gpl <- paste0("GPL", results$gpl)
    results$gds <- paste0("GDS", results$gds)
    results <- subset(results, select=c("gse", "accession", "gpl", "title", "summary", "Sample_characteristics_ch1"))
    names(results) <- c("Sample_series_id", "Sample_geo_accession", "Sample_platform_id",
                                                                "Sample_title", "Sample_source_name_ch1", "Sample_characteristics_ch1")
    return(results)
}

