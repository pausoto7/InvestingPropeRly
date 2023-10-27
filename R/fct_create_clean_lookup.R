






create_clean_lookup <- function(property_md_lookup){
  print("Starting create_clean_lookup function")

  sum_property_md_lookup <- property_md_lookup %>%
    group_by(month, PropertyNickname, Year) %>%
    mutate(TotalPropertyRent = sum(Rent))

  sum_property_md_lookup_clean <- sum_property_md_lookup %>%
    select( -UnitNum, -Rent) %>%
    unique() %>%
    rename(year = Year) %>%
    mutate(month = full_month_to_abbrev(month))

  return(sum_property_md_lookup_clean)
}














