# monthly cost

cost_bar_plot <- function(filtered_property_raw_df, year, costType){
  print("Starting cost_bar_plot function")


  monthly_cost_df <- monthly_cost_summary(filtered_property_raw_df)

  ggplot() +
    geom_col(data = monthly_cost_df, aes(x = month, y = costs_total, group= as.character(year),
                                         fill = as.character(year)),
             position = position_dodge2(width = 1, preserve = "single"),
             color = "black") +
    scale_fill_brewer(palette = "Spectral") +
    scale_y_reverse()+
    theme_bw()

}


cost_line_graph <- function(filtered_property_raw_df){ # make costType a list?


  monthly_cost_df <- monthly_cost_summary(filtered_property_raw_df)

  plot <- ggplot() +
    geom_line(data = monthly_cost_df, aes(x = month, y = costs_total, group= as.character(year),
                                         color = as.character(year)),
              linewidth = 1 ) +
    geom_point(data = monthly_cost_df, aes(x = month, y = costs_total, group= as.character(year),
                                          color = as.character(year)),
              size = 2 ) +
    scale_color_brewer(palette = "Set2", name = "Year") +
    scale_y_reverse(name= "Totalcost ($)")+

    theme_bw()

  return(plot)
}



