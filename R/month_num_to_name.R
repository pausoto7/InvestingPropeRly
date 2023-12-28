




month_number_to_name <- function(month_number) {
  months <- c("January", "February", "March", "April", "May", "June",
              "July", "August", "September", "October", "November", "December")

  return(dplyr::case_when(
    month_number %in% 1:12 ~ months[month_number],
    TRUE ~ as.character(NA)
  ))
}


