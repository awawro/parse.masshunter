#' Read a .csv file exported from MassHunter and assigns data blocks
#'
#' @param filename A string with the path of the relevant .csv file.
#' @return A data frame with three columns.
#' @import dplyr readr
read_masshunter_csv <- function(filename){
  raw.df <- readr::read_csv(filename, col_names = c("V1", "V2", "V3"), col_types = "ccc")

  # find data block start and length
  block.start <- which(str_sub(raw.df$V1, 1L, 2L) == "#\"")
  block.length <- c(diff(block.start), (nrow(raw.df) - block.start[length(block.start)] + 1))

  # assign block id to each row
  raw.df <- dplyr::mutate(raw.df, block.id = rep(1:length(block.start), block.length))
}
