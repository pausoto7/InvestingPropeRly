


scotia_mortgage_doc_raw <- utils::read.csv("data-raw/pcbanking_woodward.csv", header = FALSE)

colnames(scotia_mortgage_doc_raw) <- c("Date", "Amount", "Dash", "Type", "OtherInfo")
scotia_mortgage_doc_raw <- dplyr::select(scotia_mortgage_doc_raw, c("Date", "Amount", "Dash", "Type", "OtherInfo"))

scotia_mortgage_doc_raw$Date <- lubridate::mdy(scotia_mortgage_doc_raw$Date)

scotia_mortgage_doc_raw$OtherInfo <- iconv(scotia_mortgage_doc_raw$OtherInfo, from = "UTF-8", to = "UTF-8", sub = "")

scotia_mortgage_doc <- scotia_mortgage_doc_raw %>%
  mutate(Type = trimws(Type),
         OtherInfo = trimws(OtherInfo)) %>%
  dplyr::mutate(OtherInfo = stringr::str_trim(OtherInfo)) %>%
  dplyr::mutate(OtherInfo = ifelse(OtherInfo == "Optimum Socit d'Assurance In",
                                   "Optimum Sociiti d'Assurance In", OtherInfo )) %>%
  dplyr::mutate(OtherInfo = ifelse(OtherInfo == "RELIANCECOMFORT",
                                   "RELIANCE COMFORT", OtherInfo )) %>%

  dplyr::filter(OtherInfo != "MANULIFE",
         OtherInfo != "ICBC",
         OtherInfo != "MB-CREDIT CARD/LOC PAY.",
         Type != "WITHDRAWAL",
         OtherInfo != "PAYPROP CANADA LTD",
         Type != "Third Party Credit",
         OtherInfo != "WIRE PAYMENT",
         OtherInfo != "MB-TRANSFER",
         Type != "Customer Transfer Dr.",
         OtherInfo != "Customer Transfer Cr.",
         Type != "Deposit",
         Type != "DEPOSIT",
         Type != "ABM Deposit",
         OtherInfo != "QUESTRADE INC",
         OtherInfo != "PC FROM 705570129321") %>%
  mutate(TypeAndInfo = paste(Type, OtherInfo)) %>%
  dplyr::select(-c("Dash"))



usethis::use_data(scotia_mortgage_doc, overwrite = TRUE)




property_md_lookup <- utils::read.csv("data-raw/property_metadata_lookup_20231020.csv")


property_md_lookup <- tidyr::pivot_longer(property_md_lookup,
                                        cols = January:December,
                                        names_to = "month",
                                        values_to = "Rent")

usethis::use_data(property_md_lookup, overwrite = TRUE)

