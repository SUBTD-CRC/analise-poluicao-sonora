create_bar_plot <- function(data, col, title_name, head_n = 15, dark = FALSE) {
  if (is.null(data) || nrow(data) == 0 || !(col %in% colnames(data))) return(NULL)
  
  plot_data <- data %>%
    count(!!sym(col)) %>%
    arrange(desc(n))
  
  if(head_n > 0) plot_data <- head(plot_data, head_n)
  if(nrow(plot_data) == 0) return(NULL)
  
  plot_data$category <- as.character(plot_data[[1]])
  
  text_color <- if(dark) "#ffffff" else "#212529"
  grid_color <- if(dark) "#444444" else "#eeeeee"
  
  plot_ly(
    data = plot_data, 
    y = ~reorder(category, n), 
    x = ~n, 
    type = "bar",
    orientation = 'h',
    marker = list(color = "#004a80"),
    text = ~n, textposition = 'auto',
    textfont = list(color = "#ffffff")
  ) %>%
    layout(
      xaxis = list(
        title = list(text = "Chamados", font = list(color = text_color)),
        tickfont = list(color = text_color),
        gridcolor = grid_color
      ), 
      yaxis = list(
        title = "",
        tickfont = list(color = text_color)
      ),
      paper_bgcolor = 'rgba(0,0,0,0)',
      plot_bgcolor = 'rgba(0,0,0,0)',
      margin = list(l = 150)
    ) %>%
    config(displayModeBar = FALSE)
}
