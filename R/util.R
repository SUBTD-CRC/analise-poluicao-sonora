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
    texttemplate = "%{x:,.0f}", 
    textposition = 'auto',
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
    texttemplate = "%{x:,.0f}",
    text = ~get(col),
    hoverinfo = "text+x"
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

create_heatmap_plot <- function(data, date_col, dark = FALSE) {
  if (is.null(data) || nrow(data) == 0 || !(date_col %in% colnames(data))) return(NULL)
  
  plot_data <- data %>%
    mutate(
      hora = lubridate::hour(!!sym(date_col)),
      dia_semana = lubridate::wday(!!sym(date_col), label = TRUE, abbr = FALSE, week_start = 1)
    ) %>%
    group_by(dia_semana, hora) %>%
    summarise(n = n(), .groups = "drop")
  
  all_hours <- 0:23
  all_days <- levels(plot_data$dia_semana)
  
  grid <- expand.grid(
    dia_semana = factor(all_days, levels = all_days, ordered = TRUE), 
    hora = all_hours
  )
  
  plot_data <- grid %>%
    left_join(plot_data, by = c("dia_semana", "hora")) %>%
    mutate(n = ifelse(is.na(n), 0, n))
  
  text_color <- if(dark) "#ffffff" else "#212529"
  
  matrix_data <- plot_data %>%
    tidyr::pivot_wider(names_from = hora, values_from = n) %>%
    as.data.frame()
  
  row.names(matrix_data) <- matrix_data$dia_semana
  matrix_data <- matrix_data[, -1]
  
  plot_ly(
    x = colnames(matrix_data),
    y = rownames(matrix_data),
    z = as.matrix(matrix_data),
    type = "heatmap",
    colorscale = "Blues",
    reversescale = TRUE,
    xgap = 1,
    ygap = 1,
    hovertemplate = "<b>Dia:</b> %{y}<br><b>Hora:</b> %{x}h<br><b>Chamados:</b> %{z}<extra></extra>"
  ) %>%
    layout(
      xaxis = list(
        title = "Hora do Dia",
        tickfont = list(color = text_color),
        dtick = 2
      ),
      yaxis = list(
        title = "",
        tickfont = list(color = text_color),
        autorange = "reversed"
      ),
      paper_bgcolor = 'rgba(0,0,0,0)',
      plot_bgcolor = 'rgba(0,0,0,0)',
      font = list(color = text_color),
      margin = list(l = 100, r = 20, t = 20, b = 50)
    ) %>%
    config(displayModeBar = FALSE)
}

create_sla_plot <- function(data, group_col, dark = FALSE) {
  if (is.null(data) || nrow(data) == 0 || !(group_col %in% colnames(data))) return(NULL)
  
  df_base <- data %>%
    mutate(
      status_sla = case_when(
        str_detect(prazo, "no prazo") ~ "No Prazo",
        str_detect(prazo, "fora do prazo") ~ "Fora do Prazo",
        TRUE ~ "Sem Alvo"
      )
    ) %>%
    filter(str_detect(prazo, "no prazo|fora do prazo"))
  
  plot_data <- df_base %>%
    group_by(!!sym(group_col), status_sla) %>%
    summarise(n = n(), .groups = "drop_last") %>%
    mutate(perc = n / sum(n)) %>%
    ungroup() %>%
    group_by(!!sym(group_col)) %>%
    mutate(ordem_sla = sum(perc[status_sla == "No Prazo"], na.rm = TRUE)) %>%
    ungroup()
  
  total_cidade <- df_base %>%
    group_by(status_sla) %>%
    summarise(n = n(), .groups = "drop") %>%
    mutate(
      !!sym(group_col) := "<b>TOTAL</b>",
      perc = n / sum(n),
      ordem_sla = 2
    )
  
  plot_data <- bind_rows(plot_data, total_cidade)
  
  plot_data[[group_col]] <- reorder(as.factor(plot_data[[group_col]]), plot_data$ordem_sla)
  
  sla_colors <- c("No Prazo" = "#004a80", "Fora do Prazo" = "#dc3545")
  text_color <- if(dark) "#ffffff" else "#212529"
  
  plot_ly(
    data = plot_data,
    x = ~perc,
    y = as.formula(paste0("~", group_col)),
    color = ~status_sla,
    type = 'bar',
    orientation = 'h',
    colors = sla_colors,
    text = ~scales::percent(perc, accuracy = 0.1),
    textposition = 'auto',
    hovertemplate = paste0("<b>%{y}</b><br>Status: %{fullData.name}<br>Qtd: %{customdata:,.0f}<br>Perc: %{x:.1%}<extra></extra>"),
    customdata = ~n
  ) %>%
    layout(
      barmode = 'stack',
      separators = ",.",
      xaxis = list(
        title = "Percentual",
        tickformat = ".0%",
        tickfont = list(color = text_color),
        gridcolor = if(dark) "#444444" else "#eeeeee"
      ),
      yaxis = list(
        title = "",
        tickfont = list(color = text_color)
      ),
      paper_bgcolor = 'rgba(0,0,0,0)',
      plot_bgcolor = 'rgba(0,0,0,0)',
      font = list(color = text_color),
      legend = list(orientation = 'h', x = 0, y = -0.1),
      margin = list(l = 180, r = 20, t = 20, b = 50)
    ) %>%
    config(displayModeBar = FALSE)
}
