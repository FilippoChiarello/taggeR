#' Add a dummy columm
#'
#' @import dplyr
#' @param tibble A tibble.

#' @return  \code{tibble} with a column of 1
#' @export

add <- function(tibble) {
  output_tib <- tibble %>%
    mutate(new_column = 1)

  return(output_tib)
}


