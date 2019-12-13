#' Add a dummy columm
#'
#' @import dplyr
#' @import tidyr
#' @param pos_tibble A tibble.

#' @return  \code{tibble} with a column of 1
#' @export

add_sent_list <- function(pos_tibble) {

  tmp_out <- pos_tibble %>%
    select(doc_id,
           paragraph_id,
           sentence_id,
           token_id,
           token,
           lemma,
           xpos,
           head_token_id,
           dep_rel) %>%
    gather(key = col_name, value = value, -doc_id, -paragraph_id, -sentence_id, -token_id) %>%
    mutate(united_column = paste0(col_name, "<tag>", value )) %>%
    group_by(doc_id, paragraph_id, sentence_id, token_id) %>%
    mutate(token_col = paste0(united_column, collapse = "<col>")) %>%
    ungroup() %>%
    select(-united_column, -col_name, -value) %>%
    mutate(token_col= paste0("token_id", "<tag>", token_id, "<col>", token_col)) %>%
    select(-token_id) %>%
    group_by(doc_id, paragraph_id, sentence_id) %>%
    summarise(sent_col = paste0(token_col, collapse = "<spc>"))


  output_tibble <- pos_tibble %>%
    left_join(tmp_out)

  return(output_tibble)
}



# Hold code to merge togheter the document

# tmp_out_1 %>%
#   mutate(sentence_id = paste0(paragraph_id, "_", sentence_id)) %>%
#   select(-paragraph_id) %>%
#   group_by(doc_id) %>%
#   nest(sentence_id, sent_col) %>%
#   rename(sent_list = data)

# tester_pos %>%
#   slice(1:10) %>%
#   add_sent_list() %>%
#   pull(sent_col)
