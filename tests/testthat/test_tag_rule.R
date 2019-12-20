context("Check the output of the tag_rule functions")

library(stringr)

test_that("test that the output has right number of column and of row ", {
  expect_equal(ncol(add_sent_list(tester_pos_short)),
               ncol(tester_pos_short) + 1)

  expect_equal(nrow(add_sent_list(tester_pos_short)),
               nrow(tester_pos_short))
})


 test_that("test that <tag> count is equal to 6*nword in the sentence", {

   table_test <- tester_pos %>%
     filter(doc_id %in% sample(doc_id, 1)) %>%
     filter(paragraph_id %in% sample(paragraph_id, 1)) %>%
     filter(sentence_id %in% sample(sentence_id, 1)) %>%
     add_sent_list() %>%
     mutate(number_of_label = str_count(sent_col, "<tag>")) %>%
     mutate(number_of_word = nrow(.))

   n_tag <- table_test[[1,"number_of_label"]]
   n_word <- table_test[[1,"number_of_word"]]

   expect_equal(n_tag, n_word*6)
 })

 test_that("test that <col> count is equal to 5*nword in the sentence", {

   table_test <- tester_pos %>%
     filter(doc_id %in% sample(doc_id, 1)) %>%
     filter(paragraph_id %in% sample(paragraph_id, 1)) %>%
     filter(sentence_id %in% sample(sentence_id, 1)) %>%
     add_sent_list() %>%
     mutate(number_of_label = str_count(sent_col, "<col>")) %>%
     mutate(number_of_word = nrow(.))

   n_col <- table_test[[1,"number_of_label"]]
   n_word <- table_test[[1,"number_of_word"]]

   expect_equal(n_col, n_word*5)
 })


test_that("test that <spc> count is equal to nword in the sentence minus 1", {

  table_test <- tester_pos %>%
    filter(doc_id %in% sample(doc_id, 1)) %>%
    filter(paragraph_id %in% sample(paragraph_id, 1)) %>%
    filter(sentence_id %in% sample(sentence_id, 1)) %>%
    add_sent_list() %>%
    mutate(number_of_label = str_count(sent_col, "<spc>")) %>%
    mutate(number_of_word = nrow(.))

  n_spc <- table_test[[1,"number_of_label"]]
  n_word <- table_test[[1,"number_of_word"]]

  expect_equal(n_spc, n_word - 1)
})


