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
      separators = ",.",
      xaxis = list(
        title = list(text = "Chamados", font = list(color = text_color)),
        tickfont = list(color = text_color),
        gridcolor = grid_color,
        separatethousands = TRUE,
        exponentformat = "none"
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
    marker = list(colors = c("#004a80", "#00a2da", "#66b2ff", "#99ccff", "#cce5ff")),
    texttemplate = "%{label}<br>%{percent}<br>(%{value:,.0f})"
  ) %>%
    layout(
      separators = ",.",
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
      separators = ",.",
      paper_bgcolor = 'rgba(0,0,0,0)',
      plot_bgcolor = 'rgba(0,0,0,0)',
      font = list(color = text_color),
      xaxis = list(
        title = "Chamados",
        tickfont = list(color = text_color),
        separatethousands = TRUE,
        exponentformat = "none"
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

create_time_series_plot <- function(data, date_col, granularity = "Automático", dark = FALSE) {
  if (is.null(data) || nrow(data) == 0 || !(date_col %in% colnames(data))) return(NULL)
  
  data[[date_col]] <- as.Date(data[[date_col]])
  
  if (granularity == "Automático") {
    date_range <- range(data[[date_col]], na.rm = TRUE)
    diff_days <- as.numeric(difftime(date_range[2], date_range[1], units = "days"))
    
    if (diff_days <= 45) {
      unit <- "day"
      tick_format <- "%d/%m/%y"
    } else if (diff_days <= 180) {
      unit <- "week"
      tick_format <- "%d/%m/%y"
    } else if (diff_days <= 730) {
      unit <- "month"
      tick_format <- "%m/%y"
    } else {
      unit <- "year"
      tick_format <- "%Y"
    }
  } else {
    unit <- switch(
      granularity,
      "Dia" = "day",
      "Semana" = "week",
      "Mês" = "month",
      "Ano" = "year"
    )
    
    tick_format <- switch(
      granularity,
      "Dia" = "%d/%m/%y",
      "Semana" = "%d/%m/%y",
      "Mês" = "%m/%y",
      "Ano" = "%Y"
    )
  }
  
  plot_data <- data %>%
    mutate(period = lubridate::floor_date(!!sym(date_col), unit = unit)) %>%
    count(period) %>%
    arrange(period)
  
  text_color <- if(dark) "#ffffff" else "#212529"
  grid_color <- if(dark) "#444444" else "#eeeeee"
  
  plot_ly(
    data = plot_data,
    x = ~period,
    y = ~n,
    type = 'scatter',
    mode = 'lines+markers',
    line = list(color = "#004a80", width = 3, shape = "spline"),
    marker = list(color = "#00a2da", size = 6, line = list(color = "#ffffff", width = 1)),
    fill = 'tozeroy',
    fillcolor = if(dark) 'rgba(0, 162, 218, 0.1)' else 'rgba(0, 74, 128, 0.1)',
    hovertemplate = paste0("<b>Data:</b> %{x|", tick_format, "}<br><b>Chamados:</b> %{y:,.0f}<extra></extra>")
  ) %>%
    layout(
      separators = ",.",
      xaxis = list(
        title = "",
        tickfont = list(color = text_color),
        gridcolor = grid_color,
        zeroline = FALSE,
        tickformat = tick_format
      ),
      yaxis = list(
        title = list(text = "Qtd. Chamados", font = list(color = text_color)),
        tickfont = list(color = text_color),
        gridcolor = grid_color,
        zeroline = FALSE,
        separatethousands = TRUE,
        exponentformat = "none"
      ),

      paper_bgcolor = 'rgba(0,0,0,0)',
      plot_bgcolor = 'rgba(0,0,0,0)',
      margin = list(l = 50, r = 20, t = 20, b = 50),
      hovermode = "x unified"
    ) %>%
    config(displayModeBar = FALSE)
}
