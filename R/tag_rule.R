#' Add a dummy columm
#'
#' @import dplyr
#' @import tidyr
#' @param pos_tibble A tibble.

#' @return  \code{tibble} with a column of 1
#' @export

add_sent_list <- function(pos_tibble) {

  tmp_out <- pos_tibble %>%
    # Select columns in pos tagging table
    select(doc_id,
           paragraph_id,
           sentence_id,
           token_id,
           token,
           lemma,
           xpos,
           head_token_id,
           dep_rel) %>%
    # Gather columns in pos tagging table
    gather(key = col_name, value = value,
           -doc_id, -paragraph_id, -sentence_id, -token_id) %>%
    # Unite each value of charateristc of one token with charateristic
    # using <tag>
    mutate(united_column = paste0(col_name, "<tag>", value )) %>%
    group_by(doc_id, paragraph_id, sentence_id, token_id) %>%
    # Unite all charateristics togheter
    mutate(token_col = paste0(united_column, collapse = "<col>")) %>%
    ungroup() %>%
    # Remove unsefull columns
    select(-united_column, -col_name, -value) %>%
    # Add token_id to token_col string
    mutate(token_col = paste0("token_id", "<tag>",
                              token_id, "<col>", token_col)) %>%
    # Remove token_id column
    select(-token_id) %>%
    group_by(doc_id, paragraph_id, sentence_id) %>%
    # Unite all token_col objects in the same sentece
    summarise(sent_col = paste0(token_col, collapse = "<spc>")) %>%
    ungroup()

# Add to input pos_tibble the sent_col
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
