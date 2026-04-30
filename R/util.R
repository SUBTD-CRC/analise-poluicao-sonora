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

create_pie_chart <- function(data, col, dark = FALSE) {
  if (is.null(data) || nrow(data) == 0 || !(col %in% colnames(data))) return(NULL)
  
  plot_data <- data %>%
    count(!!sym(col)) %>%
    arrange(desc(n))
  
  text_color <- if(dark) "#ffffff" else "#212529"
  
  plot_ly(
    data = plot_data,
    labels = ~get(col),
    values = ~n,
    type = 'pie',
    textinfo = 'label+percent',
    textposition = 'inside',
    insidetextorientation = 'radial',
    marker = list(colors = c("#004a80", "#00a2da", "#66b2ff", "#99ccff", "#cce5ff"))
  ) %>%
    layout(
      paper_bgcolor = 'rgba(0,0,0,0)',
      plot_bgcolor = 'rgba(0,0,0,0)',
      font = list(color = text_color),
      showlegend = TRUE,
      legend = list(orientation = 'v', x = 1, y = 0.5),
      margin = list(l = 10, r = 10, t = 10, b = 10)
    ) %>%
    config(displayModeBar = FALSE)
}

create_single_stacked_bar <- function(data, col, dark = FALSE) {
  if (is.null(data) || nrow(data) == 0 || !(col %in% colnames(data))) return(NULL)
  
  plot_data <- data %>%
    count(!!sym(col)) %>%
    arrange(desc(n))
  
  text_color <- if(dark) "#ffffff" else "#212529"
  
  plot_ly(
    data = plot_data,
    y = " ",
    x = ~n,
    color = ~reorder(get(col), n),
    type = 'bar',
    orientation = 'h',
    colors = "Blues",
    text = ~paste0(get(col), ": ", n),
    hoverinfo = "text"
  ) %>%
    layout(
      barmode = 'stack',
      paper_bgcolor = 'rgba(0,0,0,0)',
      plot_bgcolor = 'rgba(0,0,0,0)',
      font = list(color = text_color),
      xaxis = list(
        title = "Chamados",
        tickfont = list(color = text_color)
      ),
      yaxis = list(
        title = "",
        visible = FALSE
      ),
      legend = list(orientation = 'h', x = 0, y = -0.5),
      margin = list(l = 20, r = 20, t = 20, b = 100)
    ) %>%
    config(displayModeBar = FALSE)
}
