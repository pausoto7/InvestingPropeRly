#' monthlyExpenditures
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

data('scotia_mortgage_doc')
data('property_md_lookup')

# property_raw_file <-scotia_mortgage_doc
# nickname <- "Woodward"


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

  return(all_monthly_summary)
}

property_colours <- data.frame(PropertyNickname = c("Woodward", "Murphy", "Home"),
                               colour = c("deeppink4", "sienna3", "cyan4"))



# monthly cost

cost_bar_plot <- function(property_raw_file, nickname){


  monthly_cost_df <- monthly_cost_summary(property_raw_file, nickname)

  ggplot() +
    geom_col(data = monthly_cost_df, aes(x = month, y = costs_total, group= as.character(year),
                                         fill = as.character(year)),
             position = position_dodge2(width = 1, preserve = "single"),
             color = "black") +
    scale_fill_brewer(palette = "BrBG") +
    scale_y_reverse()+
    theme_bw()

}



#monthly income


net_income_plot <- function(property_raw_file, nickname, startdate, enddate, sum_property_md_lookup){

  all_monthly_summary <- net_income_summary(monthly_cost_summary(property_raw_file, nickname), sum_property_md_lookup)

  appropriate_color <- property_colours$colour[which(property_colours$PropertyNickname == nickname)]

  plot <- ggplot() +
    geom_line(data = all_monthly_summary, aes(x = lubridate::ymd(Date), y = net_monthly_income),
              size = 1, color = appropriate_color) +
    geom_point(data = all_monthly_summary, aes(x = lubridate::ymd(Date), y = net_monthly_income),
               size = 2, color = appropriate_color )+
    geom_hline(yintercept=0)+
    geom_vline(xintercept = as.Date("2022-01-01")) +
    annotate("text",x = as.Date("2021-12-27"), y =-500, label = "2022", angle = 90) +
    geom_vline(xintercept = as.Date("2023-01-01")) +
    annotate("text",x = as.Date("2022-12-27"), y =-500, label = "2023", angle = 90) +
    scale_x_date(name = "Date",
                 date_breaks = "2 months",
                 date_minor_breaks = "1 month",
                 date_labels = "%b\n%Y",
                 limits = c(startdate, enddate)) +
    scale_y_continuous(name = "Cost ($)") +
    theme_bw() +
    theme(panel.grid.minor.y = element_blank())

  plot <- plot %>%
    plotly::style(hoverinfo = "y")

  return(plot)
}





# ggplot() +
#   geom_line(data = all_monthly_summary, aes(x = lubridate::ymd(Date), y = net_monthly_income ), size = 1, color = "red") +
#   geom_point(data = all_monthly_summary, aes(x = lubridate::ymd(Date), y = net_monthly_income),  size = 2, color = "red" )+
#   geom_hline(yintercept=0)+
#   scale_x_date(name = "Date", date_breaks = "1 year", date_minor_breaks = "2 month", date_labels = "%Y %b",
#                limits = c(as.Date("2022-03-01"), as.Date("2022-12-01"))) +
#   theme_bw() +
#   theme(panel.grid.minor.y = element_blank())







#
# hc_all_monthly_summary <- all_monthly_summary %>%
#   select(Date, net_monthly_income) %>%
#   mutate(Date = lubridate::ymd(Date))
#
# highcharter::highchart(type = "stock") %>%
#   highcharter::hc_add_series(highcharter::hcaes(x = as.Date(all_monthly_summary$Date),
#                                                 y = all_monthly_summary$net_monthly_income))
#
#
#
# all_monthly_summary$Date <- lubridate::ymd(all_monthly_summary$Date)
#
# all_monthly_summary %>%
#   highcharter::hchart(.,
#                       type = "line",
#                       hcaes(x = Date,
#                             y = net_monthly_income))
#


