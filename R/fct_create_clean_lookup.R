
#' create_clean_lookup
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd



create_clean_lookup <- function(property_md_lookup){
  print("Starting create_clean_lookup function")


  sum_property_md_lookup_long <- property_md_lookup %>%
    group_by(month, PropertyNickname, Year) %>%
    summarise(TotalPropertyRent = sum(Rent))

  sum_property_md_lookup_clean <- sum_property_md_lookup_long %>%
    rename(year = Year) %>%
    mutate(month = full_month_to_abbrev(month))

  return(sum_property_md_lookup_clean)
}














