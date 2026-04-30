create_bar_plot <- function(data, col, title_name, head_n = 15) {
  if (is.null(data) || nrow(data) == 0 || !(col %in% colnames(data))) return(NULL)
  
  plot_data <- data %>%
    count(!!sym(col)) %>%
    arrange(desc(n))
  
  if(head_n > 0) plot_data <- head(plot_data, head_n)
  if(nrow(plot_data) == 0) return(NULL)
  
  plot_data$category <- as.character(plot_data[[1]])
  
  plot_ly(
    data = plot_data, 
    y = ~reorder(category, n), 
    x = ~n, 
    type = "bar",
    orientation = 'h',
    marker = list(color = "#004a80"),
    text = ~n, textposition = 'auto') %>%
    layout(
      xaxis = list(title = "Chamados"), 
      yaxis = list(title = ""),
      margin = list(l = 150)
    ) %>%
    config(displayModeBar = FALSE)
}
