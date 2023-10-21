


scotia_mortgage_doc_raw <- utils::read.csv("data-raw/pcbanking_woodward.csv", header = FALSE)

colnames(scotia_mortgage_doc_raw) <- c("Date", "Amount", "Dash", "Type", "OtherInfo")
scotia_mortgage_doc_raw <- dplyr::select(scotia_mortgage_doc_raw, c("Date", "Amount", "Dash", "Type", "OtherInfo"))

scotia_mortgage_doc_raw$Date <- lubridate::mdy(scotia_mortgage_doc_raw$Date)

scotia_mortgage_doc_raw$OtherInfo <- iconv(scotia_mortgage_doc_raw$OtherInfo, from = "UTF-8", to = "UTF-8", sub = "")

scotia_mortgage_doc <- scotia_mortgage_doc_raw %>%
  mutate(Type = trimws(Type),
         OtherInfo = trimws(OtherInfo)) %>%
  dplyr::mutate(OtherInfo = stringr::str_trim(OtherInfo)) %>%
  filter(OtherInfo != "MANULIFE",
         OtherInfo != "ICBC",
         OtherInfo != "MB-CREDIT CARD/LOC PAY",
         Type != "WITHDRAWAL") %>%
  mutate(TypeAndInfo = paste(Type, OtherInfo))


usethis::use_data(scotia_mortgage_doc, overwrite = TRUE)




property_md_lookup <- utils::read.csv("data-raw/property_metadata_lookup_20231020.csv")


property_md_lookup <- pivot_longer(property_md_lookup,
                                        cols = January:December,
                                        names_to = "month",
                                        values_to = "Rent")

usethis::use_data(property_md_lookup, overwrite = TRUE)

