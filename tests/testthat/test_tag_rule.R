context("Check the output of the tag_rule functions")

library(stringr)

test_that("test that the output has right number of column and of row ", {
  expect_equal(ncol(add_sent_list(tester_pos_short)),
               ncol(tester_pos_short) + 1)
  expect_equal(nrow(add_sent_list(tester_pos_short)),
               nrow(tester_pos_short))
})


# test_that("test that <tag> count is equal to 6*nword in the sentence", {
#   expect_equal(ncol(add_sent_list(tester_pos_short)),
#                ncol(tester_pos_short) + 1)
# })
#
# test_that("test that <col> count is equal to 5*nword in the sentence", {
#   expect_equal(ncol(add_sent_list(tester_pos_short)),
#                ncol(tester_pos_short) + 1)
# })


test_that("test that <spc> count is equal to nword in the sentence minus 1", {

  table_test <- tester_pos_short %>%
    add_sent_list() %>%
    mutate(number_of_label = str_count(sent_col, "<spc>")) %>%
    group_by(sentence_id) %>%
    mutate(number_of_word = nrow(.))



    test_sentence <- tester_pos_short %>%
      add_sent_list() %>%
      .[[1, 15]]

  expect_equal(ncol(add_sent_list(tester_pos_short)),
               ncol(tester_pos_short) + 1)
})


# test_that("str_length of factor is length of level", {
#   expect_equal(str_length(factor("a")), 1)
#   expect_equal(str_length(factor("ab")), 2)
#   expect_equal(str_length(factor("abc")), 3)
# })
#
# test_that("str_length of missing is missing", {
#   expect_equal(str_length(NA), NA_integer_)
#   expect_equal(str_length(c(NA, 1)), c(NA, 1))
#   expect_equal(str_length("NA"), 2)
# })
