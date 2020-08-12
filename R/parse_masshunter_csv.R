#' Read .csv files exported by Agilent's MassHunter software into an R data frame.
#'
#' @param filename A string with the path of the relevant .csv file.
#' @return A data frame with three columns.
#' @import dplyr tidyr stringr
#' @export
parse_masshunter_csv <- function(filename){
  raw.df<- read_masshunter_csv(filename)
  # split into data and metadata into separate data frames
  data.df <- dplyr::filter(raw.df, str_sub(V1, 1L, 1L) != "#")
  meta.df <- dplyr::filter(raw.df, str_sub(V1, 1L, 2L) == "#\"") %>%
    dplyr::rename(metadata = V1) %>%
    dplyr::select(c(metadata, block.id)) %>%
    # extract metadata
    dplyr::mutate(file = str_extract(metadata, "[:graph:]+\\.d"),
           polarity = str_sub(metadata, 3L ,3L),
           type = str_extract(metadata, "(MRM|EIC|SIM)"),
           EIC = str_extract(metadata, "(?<=EIC\\()[:graph:]+(?=\\))"),
           SIM = str_extract(metadata, "(?<=SIM\\()[:graph:]+(?=\\))"),
           Frag = str_extract(metadata, "(?<=Frag=)[:graph:]+(?=V)"),
           CID = str_extract(metadata, "(?<=CID@)[:graph:]+"),
           precursor.ion = str_extract(metadata, "(?<=\\()[:graph:]+(?= -)"),
           product.ion = str_extract(metadata, "(?<=> )[:graph:]+(?=\\))")
    )

  # left_join metadata to data rows and remove temporarily added key column
  joined.df <- data.df %>%
    dplyr::left_join(meta.df, by = "block.id") %>%
    dplyr::rename(point = V1, time = V2, intensity = V3) %>%
    dplyr::select(-block.id)

  # remove empty cols, e.g. MRMs if all chromatograms are EIC
  out.df <- Filter(function(x) !all(is.na(x)), joined.df) %>%
    mutate(time = as.numeric(time),
           intensity = as.numeric(intensity))
  return(out.df)
}
