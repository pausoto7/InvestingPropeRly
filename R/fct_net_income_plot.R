
net_income_plot <- function(filtered_property_raw_df, startdate, enddate, sum_property_md_lookup_clean){
  print("Starting net_income_plot function")

  all_monthly_summary <- monthly_income(monthly_cost_summary(filtered_property_raw_df), sum_property_md_lookup_clean)


  #startdate <- as.Date("2021-01-01")
  #enddate <- as.Date("2023-12-31")
  appropriate_color <- "darkblue"

  plot <- ggplot() +
    geom_line(data = all_monthly_summary, aes(x = lubridate::ymd(Date), y = net_monthly_income),
              linewidth = 1, color = appropriate_color) +
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

