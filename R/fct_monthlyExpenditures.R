#' monthlyExpenditures
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd


property_raw_file <-scotia_morgage_doc
nickname <- "Woodward"


monthly_cost_summary <- function(property_raw_file, nickname){

  monthly_cost_df <- property_raw_file %>%
    filter(Date > as.Date("2021-07-01")) %>%
    filter(Amount <= 0) %>% # only include costs as this is monthly cost summary
    group_by(year = lubridate::year(Date), month = lubridate::month(Date, label = TRUE, abbr = TRUE)) %>%
    summarize(costs_total = sum(Amount)) %>%
    ungroup() %>%
    mutate(Date = paste(as.character(year), month, "01", sep = "-"),
           fake_date_for_plotting = paste("2000", month, "01", sep = "-")) %>%
    mutate(PropertyNickname = nickname, .before = year)

return(monthly_cost_df)


}


sum_property_md_lookup <- property_md_lookup %>%
  group_by(PropertyNickname, RentYear) %>%
  mutate(TotalPropertyRent = sum(RentAmount))



net_income_summary <- function(monthly_cost_df, sum_property_md_lookup){

  sum_property_md_lookup_clean <- sum_property_md_lookup %>%
    select(-RentalPropertyAddress, -UnitNum, -RentAmount) %>%
    unique() %>%
    rename(year = RentYear)

  all_monthly_summary <- monthly_cost_df %>%
    left_join(sum_property_md_lookup_clean) %>%
    mutate(net_monthly_income = TotalPropertyRent*(1-PropertyManagerPercFee) + costs_total) #(costs are in negative's which is why we're adding)


}





# monthly cost
ggplot() +
  geom_line(data = monthly_cost_df, aes(x = lubridate::ymd(Date), y = total )) +
  scale_x_date(name = "Date") +
  theme_bw()

#monthly income


ggplot() +
  geom_line(data = all_monthly_summary, aes(x = lubridate::ymd(Date), y = net_monthly_income )) +
  scale_x_date(name = "Date") +
  theme_bw()

