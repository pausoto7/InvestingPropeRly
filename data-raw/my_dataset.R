


scotia_morgage_doc_raw <- utils::read.csv("data-raw/pcbanking_woodward.csv", header = FALSE)

colnames(scotia_morgage_doc_raw) <- c("Date", "Amount", "Dash", "Type", "OtherInfo")
scotia_morgage_doc_raw <- dplyr::select(scotia_morgage_doc_raw, -Dash)

scotia_morgage_doc_raw$Date <- lubridate::mdy(scotia_morgage_doc_raw$Date)


scotia_morgage_doc <- scotia_morgage_doc_raw %>%
  dplyr::mutate(OtherInfo = stringr::str_trim(OtherInfo)) %>%
  filter(OtherInfo != "MANULIFE",
         OtherInfo != "ICBC")


usethis::use_data(scotia_morgage_doc, overwrite = TRUE)




property_md_lookup <- utils::read.csv("data-raw/property_metadata_lookup.csv")


usethis::use_data(property_md_lookup, overwrite = TRUE)
