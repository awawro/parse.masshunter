#' Read .csv files exported by Agilent's MassHunter software into an R data frame.
#'
#' @param filename A string with the path of the relevant .csv file.
#' @return A data frame with three columns.
#' @export
parse_masshunter_csv <- function(filename){
  raw.df<- read_masshunter_csv(filename)
  # split into data and metadata into separate data frames
  data.df <- dplyr::filter(raw.df, str_sub(V1, 1L, 1L) != "#")
  meta.df <- dplyr::filter(raw.df, str_sub(V1, 1L, 2L) == "#\"") %>%
    dplyr::rename(metadata = V1) %>%
    dplyr::select(c(metadata, block.id)) %>%
    # extract metadata
    dplyr::mutate(file = stringr::str_extract(metadata, "[:graph:]+\\.d"),
           polarity = stringr::str_sub(metadata, 3L ,3L),
           type = stringr::str_extract(metadata, "(MRM|EIC|SIM)"),
           EIC = stringr::str_extract(metadata, "(?<=EIC\\()[:graph:]+(?=\\))"),
           SIM = stringr::str_extract(metadata, "(?<=SIM\\()[:graph:]+(?=\\))"),
           Frag = stringr::str_extract(metadata, "(?<=Frag=)[:graph:]+(?=V)"),
           CID = stringr::str_extract(metadata, "(?<=CID@)[:graph:]+"),
           precursor.ion = stringr::str_extract(metadata, "(?<=\\()[:graph:]+(?= -)"),
           product.ion = stringr::str_extract(metadata, "(?<=> )[:graph:]+(?=\\))")
    )

  # left_join metadata to data rows and remove temporarily added key column
  joined.df <- data.df %>%
    dplyr::left_join(meta.df, by = "block.id") %>%
    dplyr::rename(point = V1, time = V2, intensity = V3) %>%
    dplyr::select(-block.id)

  # remove empty cols, e.g. MRMs if all chromatograms are EIC
  out.df <- Filter(function(x) !all(is.na(x)), joined.df) %>%
    dplyr::mutate(time = as.numeric(time),
           intensity = as.numeric(intensity))
  return(out.df)
}
