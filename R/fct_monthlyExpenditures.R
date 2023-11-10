#' monthlyExpenditures
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd



# property_raw_file <-scotia_mortgage_doc
# nickname <- "Woodward"


# monthly_cost_summary
# output is dataframe which adds all our costs together and gives a monthly total
monthly_cost_summary <- function(property_raw_file){
  print("Starting monthly_cost_summary function")

  monthly_cost_df <- property_raw_file %>%
    filter(Date > as.Date("2021-07-01")) %>%
    filter(Amount <= 0) %>% # only include costs as this is monthly cost summary
    group_by(year = lubridate::year(Date), month = lubridate::month(Date, label = TRUE, abbr = TRUE)) %>%
    summarize(costs_total = sum(Amount)) %>%
    ungroup() %>%
    mutate(Date = paste(as.character(year), month, "01", sep = "-"),
           fake_date_for_plotting = paste("2000", month, "01", sep = "-"))
return(monthly_cost_df)


}


# monthly_income
# summarized rents for each property
monthly_income <- function(monthly_cost_df, sum_property_md_lookup_clean){
  print("Starting monthly_income function")

  sum_property_md_lookup_clean <- create_clean_lookup(property_md_lookup)

  sum_property_rents <- sum_property_md_lookup_clean %>%
    group_by(year, month) %>%
    summarise(TotalMonthRent = sum(TotalPropertyRent))

  PropertyManagerPercFee <- 0.093

  all_monthly_summary <- monthly_cost_df %>%
    left_join(sum_property_rents) %>%
    mutate(net_monthly_income = TotalMonthRent*(1-PropertyManagerPercFee) + costs_total) #(costs are in negative's which is why we're adding)

  return(all_monthly_summary)
}

get_cost_msg_txt <- function(costType){
  if (length(costType)==1){
    cost_message_text <- "testing = 1"
  }else if(length(costType) == 0){
    cost_message_text <- "You have not picked any property costs. Please pick at least one to see information."
  }else if(length(costType) > 1){
    cost_message_text <- "You have picked more than one property cost type. At this time this is not possible. Please pick one for the time being"
  }else{
    cost_message_text <- "testing"
  }

  return(cost_message_text)

}


filtered_property_raw <- function(property_raw_file, year, costType){
  print("Starting filtered_property_raw function")


  if (year != "all"){

    property_raw_file <- property_raw_file %>%
      mutate(rentYear = lubridate::year(Date)) %>%
      filter(rentYear == year)

    print("Only getting data for one year")
  }else{
    print("Property data all years")
  }

  if (costType != "all"){

    property_raw_file <- property_raw_file %>%
      filter(TypeAndInfo == costType)

    print("Filtering for single cost type")
  }else{
    print("All cost types")

  }

  return(property_raw_file)

}



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

