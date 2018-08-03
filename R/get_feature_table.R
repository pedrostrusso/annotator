# tagPOS <-  function(x, ...) {
#     s <- as.String(x)
#     word_token_annotator <- Maxent_Word_Token_Annotator()
#     a2 <- Annotation(1L, "sentence", 1L, nchar(s))
#     a2 <- NLP::annotate(s, word_token_annotator, a2)
#     a3 <- NLP::annotate(s, Maxent_POS_Tag_Annotator(), a2)
#     a3w <- a3[a3$type == "word"]
#     POStags <- unlist(lapply(a3w$features, `[[`, "POS"))
#     POStagged <- paste(sprintf("%s/%s", s[a3w], POStags), collapse = " ")
#     list(POStagged = POStagged, POStags = POStags)
# }
#
# get_feature_table <- function(annot_folder){
#     options(java.parameters = "- Xmx1024m")
#
#     annot_folder <- "/home/prusso/Documents/CSBL/annotator/intermediate/annot_test4"
#     annotations <- dir(annot_folder)
#
#     feature_list <- list()
#
#     for(folder in annotations){
#         gse <- gsub("fail_", "", folder)
#         print(gse)
#
#         annot_file <- file.path(annot_folder, folder, paste0(gse, "_auto_annotation.tsv"))
#         annot0 <- data.table::fread(annot_file, data.table=FALSE)
#         annot0$TRUE_col <- NULL
#         annot0$Sample_geo_accession <- NULL
#
#         if(startsWith(folder, "fail_")){
#             a <- sapply(annot0, function(col){
#                 ifelse(length(unique(col)) == 1, FALSE, TRUE)
#             })
#             annot <- annot0[, a, drop=FALSE]
#             annot <- as.data.frame(apply(annot, 2, function(col) {
#                 as.character(trimws(gsub("(?!\\+)[[:punct:]'[:blank:]]+", " ", tolower(col), perl=TRUE)))
#             }), stringsAsFactors = FALSE)
#             annot$mega_col <- Reduce(paste, annot)
#             annot <- as.data.frame(annot$mega_col, stringsAsFactors = FALSE)
#             names(annot) <- "mega_col"
#             annot0$mega_col <- annot$mega_col
#         }
#
#         row <- folder
#
#         mega_col <- annot0$mega_col
#
#         mean_len <- mean(sapply(mega_col, nchar))
#         max_len <- max(sapply(mega_col, nchar))
#         min_len <- min(sapply(mega_col, nchar))
#         word_num_mean <- mean(sapply(mega_col, textreuse::wordcount))
#
#         mega_vec <- paste(mega_col, collapse = " ")
#         words <- strsplit(mega_vec, " ")[[1]]
#         mean_word_len <- mean(sapply(words, nchar))
#
#         pos_tags <- sapply(mega_col, function(x){
#             tagged <- tagPOS(x)
#             tagged$POStags
#         })
#
#         noun_num_mean <- mean(unlist(lapply(pos_tags, function(x){
#             sum(x == "NN" | x == "NNS" | x == "NNP" | x == "NNPS" )
#         })))
#
#         adj_num_mean <- mean(unlist(lapply(pos_tags, function(x){
#             sum(x == "JJ" | x == "JJR" | x == "JJS")
#         })))
#
#         card_num_mean <- mean(unlist(lapply(pos_tags, function(x){
#             sum(x == "CD")
#         })))
#
#         verb_num_mean <- mean(unlist(lapply(pos_tags, function(x){
#             sum(x == "VB"| x == "VBD" | x == "VBG" | x == "VBN" | x == "VBP" | x == "VBZ")
#         })))
#
#         prep_num_mean <- mean(unlist(lapply(pos_tags, function(x){
#             sum(x == "IN")
#         })))
#
#         test_annot <- annot0[, c("Sample_title", "Sample_source_name_ch1", "Sample_characteristics_ch1")]
#         a <- lapply(test_annot, function(col){
#             ifelse(length(unique(col)) == 1, FALSE, TRUE)
#         })
#         used_title <- ifelse(a$Sample_title, TRUE, FALSE)
#         used_source <- ifelse(a$Sample_source_name_ch1, TRUE, FALSE)
#         used_characteristics <- ifelse(a$Sample_title, TRUE, FALSE)
#
#         row <- c(row, mean_len, max_len, min_len, word_num_mean,
#                  noun_num_mean, adj_num_mean, card_num_mean, verb_num_mean,
#                  mean_word_len, prep_num_mean, used_title, used_source,
#                  used_characteristics)
#
#         feature_list[[length(feature_list)+1]] <- row
#     }
#     feature_table <- as.data.frame(do.call("rbind", feature_list))
#
#     feature_table[, 1] <- as.character(feature_table[, 1])
#     feature_table[, c(2:11)] <- apply(feature_table[, c(2:11)], 2, as.numeric)
#     # feature_table[, 12] <- as.factor(feature_table[, 12])
#     # feature_table[, 13] <- as.factor(feature_table[, 13])
#     # feature_table[, 14] <- as.factor(feature_table[, 14])
#     names(feature_table) <- c("row", "mean_len", "max_len", "min_len", "word_num_mean",
#                               "noun_num_mean", "adj_num_mean", "card_num_mean", "verb_num_mean",
#                               "mean_word_len", "prep_num_mean", "used_title", "used_source",
#                               "used_characteristics")
# }
#
# res_table <- data.table::fread("/home/prusso/Documents/CSBL/annotator/results/annot_test4/cluster_res4_edited.tsv",
#                                data.table=FALSE, stringsAsFactors = FALSE)
#
# feature_table$rand <- res_table$nw1_cluster[1:100]
#
# feature_table$Class <- NA
# #feature_table[feature_table$rand == 0, "Class"] <- "Terrible"
# #feature_table[feature_table$rand > 0 & feature_table$rand < 0.33, "Class"] <- "Poor"
# feature_table[feature_table$rand >= 0 & feature_table$rand < 0.33, "Class"] <- "Terrible"
# feature_table[feature_table$rand >= 0.33 & feature_table$rand < 0.66, "Class"] <- "Good"
# feature_table[feature_table$rand >= 0.66 & feature_table$rand < 1, "Class"] <- "Great"
# feature_table[feature_table$rand == 1, "Class"] <- "Perfect"
#
# feature_table$rand <- NULL

