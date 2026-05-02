# Config -----------------------------------------------------------------------

shinyOptions(cache = cachem::cache_disk("./cache"))

library(shiny)
library(bslib)
library(leaflet)
library(plotly)
library(dplyr)
library(readxl)
library(lubridate)
library(stringr)
library(bsicons)
library(shinyWidgets)
library(sf)

# Data -------------------------------------------------------------------------

tictoc::tic("Carregamento de Dados")
data_files <- list.files("data", pattern = "Extração.*\\.xlsx", full.names = TRUE)
if (length(data_files) == 0) {stop("Nenhum arquivo de dados encontrado.")}
latest_file <- data_files[order(file.info(data_files)$mtime, decreasing = TRUE)][1]

rds_file <- file.path("data", paste0(tools::file_path_sans_ext(basename(latest_file)), ".rds"))
if (file.exists(rds_file) && file.info(rds_file)$mtime >= file.info(latest_file)$mtime) {
  df <- readRDS(rds_file)
} else {
  df <- read_excel(latest_file)
  saveRDS(df, rds_file)
}
tictoc::toc()

# Choices/Selected -------------------------------------------------------------

lista_de_opcoes_bairro <- shinyWidgets::prepare_choices(
  .data = df %>% select(no_subprefeitura, no_bairro, id_bairro) %>% distinct(id_bairro, .keep_all = TRUE),
  label = no_bairro,
  value = id_bairro,
  group_by = no_subprefeitura,
  alias = no_subprefeitura
)

select_bairro <- df %>% 
  group_by(id_bairro) %>% 
  summarise() %>% 
  ungroup()

# GEO --------------------------------------------------------------------------

# Carregar e limpar GeoJSON
geojson_path <- "data/Limite_de_Bairros.geojson"
bairros_sf <- st_read(geojson_path, quiet = TRUE) %>%
  mutate(nome = str_trim(nome))

# Mapeamento Territorial do DF para o GeoJSON
territorial_map <- df %>%
  select(no_bairro, no_subprefeitura, no_regiao_administrativa, no_area_planejamento) %>%
  distinct()

bairros_sf <- bairros_sf %>%
  left_join(territorial_map, by = c("nome" = "no_bairro"))

# Pré-agregar geometrias para performance (Dissolve)
message("Pré-agregando geometrias territoriais...")
agg_geos <- list(
  "Bairro" = bairros_sf %>% select(unit = nome),
  "Subprefeitura" = bairros_sf %>% filter(!is.na(no_subprefeitura)) %>% group_by(unit = no_subprefeitura) %>% summarise(.groups = "drop"),
  "Região Administrativa" = bairros_sf %>% filter(!is.na(no_regiao_administrativa)) %>% group_by(unit = no_regiao_administrativa) %>% summarise(.groups = "drop"),
  "AP" = bairros_sf %>% filter(!is.na(no_area_planejamento)) %>% group_by(unit = no_area_planejamento) %>% summarise(.groups = "drop")
)
message("Pronto!")

# APP --------------------------------------------------------------------------

ui <- page_navbar(
  title = uiOutput("navbar_title"),
  window_title = "Monitoramento de Poluição Sonora",
  fillable = FALSE,
  theme = bs_theme(
    version = 5,
    primary = "#004a80",
    secondary = "#00a2da"
  ),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  ),
  tags$head(
    tags$link(rel = "shortcut icon", href = "Logo.png")
  ),
  sidebar = sidebar(
    width = 320,
    title = "Filtros de Pesquisa",
    dateRangeInput(
      inputId = "date_range", 
      label = "Período:",
      start = as.Date("2019-01-01"),
      end = max(df$dt_inicio, na.rm = TRUE),
      language = "pt-BR", 
      separator = " até "
    ),
    virtualSelectInput(
      inputId = "subtipo_filter", 
      label = "Subtipos:", 
      choices = sort(unique(df$no_subtipo)),
      selected = sort(unique(df$no_subtipo)),
      showValueAsTags = FALSE,
      search = TRUE,
      multiple = TRUE,
      searchPlaceholderText = "Buscar",
      placeholder = "Nenhuma opção selecionada",
      optionsSelectedText = "opções escolhidas",
      allOptionsSelectedText = "Todas as opções",
      selectAllText = "Selecionar Tudo"
    ),
    virtualSelectInput(
      inputId = "bairro_filter", 
      label = "Subprefeitura/Bairro:", 
      choices = lista_de_opcoes_bairro,
      selected = select_bairro$id_bairro,
      showValueAsTags = FALSE,
      search = TRUE,
      multiple = TRUE,
      searchPlaceholderText = "Buscar",
      placeholder = "Nenhuma opção selecionada",
      optionsSelectedText = "opções escolhidas",
      allOptionsSelectedText = "Todas as opções",
      selectAllText = "Selecionar Tudo"
    ),
    virtualSelectInput(
      inputId = "status_filter", 
      label = "Status:", 
      choices = sort(unique(df$no_status)),
      selected = sort(unique(df$no_status)),
      showValueAsTags = FALSE,
      search = TRUE,
      multiple = TRUE,
      searchPlaceholderText = "Buscar",
      placeholder = "Nenhuma opção selecionada",
      optionsSelectedText = "opções escolhidas",
      allOptionsSelectedText = "Todas as opções",
      selectAllText = "Selecionar Tudo"
    ),
    virtualSelectInput(
      inputId = "categoria_filter", 
      label = "Categoria:", 
      choices = sort(unique(df$no_categoria)),
      selected = sort(unique(df$no_categoria)),
      showValueAsTags = FALSE,
      search = TRUE,
      multiple = TRUE,
      searchPlaceholderText = "Buscar",
      placeholder = "Nenhuma opção selecionada",
      optionsSelectedText = "opções escolhidas",
      allOptionsSelectedText = "Todas as opções",
      selectAllText = "Selecionar Tudo"
    ),
  ),
  
  nav_panel(
    title = "Visão Geral",
    icon = bs_icon("graph-up"),
    fillable = FALSE,
    
    layout_column_wrap(
      width = 1/3, 
      fill = FALSE,
      value_box(
        title = "Total Geral", 
        value = textOutput("total_geral"), 
        showcase = bs_icon("megaphone"), 
        theme = "primary"
      ),
      value_box(
        title = "Qtd. de Chamados", 
        value = textOutput("qtd_chamado"), 
        showcase = bs_icon("tag"), 
        theme = "primary"
      ),
      value_box(
        title = "Percentual", 
        value = textOutput("percentual"), 
        showcase = bs_icon("check2-circle"), 
        theme = "primary"
      )
    ),
    
    layout_columns(
      col_widths = 12,
      fill = FALSE,
      card(
        full_screen = TRUE,
        card_header(
          class = "d-flex justify-content-between align-items-center",
          "Evolução Temporal",
          radioGroupButtons(
            inputId = "time_granularity",
            label = NULL,
            choices = c("Automático", "Dia", "Semana", "Mês", "Ano"),
            selected = "Automático",
            status = "primary",
            size = "sm",
            individual = TRUE
          )
        ),
        plotlyOutput("time_series_plot", height = "350px")
      )
    ),
    
    layout_columns(
      col_widths = 12,
      fill = FALSE,
      card(
        card_header("Volume por Subtipo"),
        plotlyOutput("subtipo_plot", height = "550px"),
        full_screen = TRUE
      )
    ),
    
    layout_columns(
      col_widths = c(6, 6),
      fill = FALSE,
      card(
        card_header("Distribuição por Status"),
        plotlyOutput("status_pie", height = "450px"),
        full_screen = TRUE
      ),
      card(
        card_header("Volume por Categoria"),
        plotlyOutput("categoria_treemap", height = "450px"),
        full_screen = TRUE
      )
    )
  ),
  
  nav_panel(
    title = "Análise Territorial",
    icon = bs_icon("geo-alt"),
    fillable = FALSE,
    
    layout_columns(
      col_widths = 12,
      card(
        card_header(
          class = "d-flex justify-content-between align-items-center",
          "Localização dos Chamados",
          div(
            class = "d-flex gap-2",
            radioGroupButtons(
              inputId = "map_mode",
              label = NULL,
              choices = c("Pontos", "Calor"),
              selected = "Pontos",
              status = "primary",
              size = "sm"
            ),
            conditionalPanel(
              condition = "input.map_mode == 'Calor'",
              selectInput(
                inputId = "map_agg",
                label = NULL,
                choices = c("Bairro", "Subprefeitura", "Região Administrativa", "AP"),
                selected = "Bairro",
                width = "180px"
              )
            )
          )
        ),
        leafletOutput("mapa_chamados", height = "600px"),
        full_screen = TRUE
      )
    ),
    
    layout_columns(
      col_widths = c(6, 6),
      card(card_header("Volume por Bairro (Top 15)"), plotlyOutput("bairro_plot"), full_screen = TRUE),
      card(card_header("Volume por Região Administrativa"), plotlyOutput("ra_plot"), full_screen = TRUE)
    ),
    
    layout_columns(
      col_widths = c(6, 6),
      card(card_header("Volume por Subprefeitura"), plotlyOutput("sub_plot"), full_screen = TRUE),
      card(card_header("Volume por Área de Planejamento (AP)"), plotlyOutput("ap_plot"), full_screen = TRUE)
    )
  ),
  
  nav_spacer(),
  nav_item(
    input_dark_mode(id = "dark_mode", mode = "light")
  )
)

server <- function(input, output, session) {
  
  common_cache_keys <- reactive({
    list(
      input$date_range,
      input$bairro_filter,
      input$status_filter,
      input$subtipo_filter,
      input$categoria_filter,
      input$map_mode,
      input$map_agg,
      is_dark()
    )
  })
  output$navbar_title <- renderUI({
    logo_src <- if (isTruthy(input$dark_mode) && input$dark_mode == "dark") {
      "Logo Prefeitura horizontal branco.png"
    } else {
      "Logo Prefeitura horizontal azul.png"
    }
    
    span(
      img(
        src = logo_src, 
        height = "30px", 
        style = "margin-right: 15px; vertical-align: middle;"
      ),
      "Monitoramento de Poluição Sonora"
    )
  })
  
  filtered_df <- reactive({
    df %>% 
      filter(
        dt_inicio >= input$date_range[1] & 
        dt_inicio <= input$date_range[2] &
        id_bairro %in% input$bairro_filter & 
        no_status %in% input$status_filter &
        no_subtipo %in% input$subtipo_filter &
        no_categoria %in% input$categoria_filter
      )
  })
  
  output$total_geral <- renderText({
    format(nrow(df), big.mark = ".")
  })
  
  output$qtd_chamado <- renderText({
    format(nrow(filtered_df()), big.mark = ".")
  })
  
  output$percentual <- renderText({
    scales::percent(nrow(filtered_df()) / nrow(df), accuracy = 0.2, decimal.mark = ",")
  })
  
  output$time_series_plot <- renderPlotly({
    create_time_series_plot(filtered_df(), "dt_inicio", input$time_granularity, dark = is_dark())
  }) %>% 
    bindCache(common_cache_keys(), input$time_granularity)
  
  is_dark <- reactive({

    isTruthy(input$dark_mode) && input$dark_mode == "dark"
  })
  
  output$subtipo_plot <- renderPlotly({
    create_bar_plot(filtered_df(), "no_subtipo", "Subtipo", 10, dark = is_dark()) 
  }) %>% 
    bindCache(common_cache_keys())
  
  output$bairro_plot <- renderPlotly({
    create_bar_plot(filtered_df(), "no_bairro", "Bairro", 15, dark = is_dark())
  }) %>% 
    bindCache(common_cache_keys())
  
  output$ra_plot <- renderPlotly({
    create_bar_plot(filtered_df(), "no_regiao_administrativa", "RA", 15, dark = is_dark())
  }) %>% 
    bindCache(common_cache_keys())
  
  output$sub_plot <- renderPlotly({
    create_bar_plot(filtered_df(), "no_subprefeitura", "Subprefeitura", 0, dark = is_dark())
  }) %>% 
    bindCache(common_cache_keys())
  
  output$ap_plot <- renderPlotly({
    create_bar_plot(filtered_df(), "no_area_planejamento", "AP", 0, dark = is_dark())
  }) %>% 
    bindCache(common_cache_keys())
  
  output$status_pie <- renderPlotly({
    create_pie_chart(filtered_df(), "no_status", dark = is_dark())
  }) %>% 
    bindCache(common_cache_keys())
  
  output$categoria_treemap <- renderPlotly({
    create_single_stacked_bar(filtered_df(), "no_categoria", dark = is_dark())
  }) %>% 
    bindCache(common_cache_keys())
  
  output$mapa_chamados <- renderLeaflet({
    req(filtered_df())
    
    df_filtered <- filtered_df()
    total_calls <- nrow(df_filtered)
    map_data <- df_filtered %>% 
      filter(!is.na(lat) & !is.na(lng))
    
    calls_with_coords <- nrow(map_data)
    percent_coords <- if(total_calls > 0) {
      scales::percent(calls_with_coords / total_calls, accuracy = 0.1, decimal.mark = ",")
    } else {
      "0%"
    }
    
    map_tile <- if(is_dark()) providers$CartoDB.DarkMatter else providers$CartoDB.Positron
    
    l <- leaflet(options = leafletOptions(zoomControl = FALSE, preferCanvas = TRUE, attributionControl = FALSE)) %>%
      addProviderTiles(map_tile) %>%
      addControl(
        html = HTML(paste0(
            "<span>📍</span> ",
            "<span>", percent_coords, " mapeados</span>"
          )),
        position = "bottomleft",
        className = "leaflet-control map-stats-control"
      )
    
    if(nrow(map_data) == 0) {
      return(l %>% setView(lng = -43.1729, lat = -22.9068, zoom = 11))
    }
    
    if (input$map_mode == "Pontos") {
      l %>%
        addCircleMarkers(
          data = map_data,
          lng = ~lng, 
          lat = ~lat, 
          radius = 5, 
          color = "#004a80", 
          fillColor = "#00a2da",
          fillOpacity = 0.6, weight = 1,
          clusterOptions = markerClusterOptions(),
          popup = ~paste0(
            "<b>ID: </b>", if("id_chamado" %in% colnames(map_data)) id_chamado else "N/A", 
            "<br><b>Bairro: </b>", if("no_bairro" %in% colnames(map_data)) no_bairro else "N/A", 
            "<br><b>Subtipo: </b>", if("no_subtipo" %in% colnames(map_data)) no_subtipo else "N/A")
          )
    } else {
      agg_col <- switch(input$map_agg,
        "Bairro" = "no_bairro",
        "Subprefeitura" = "no_subprefeitura",
        "Região Administrativa" = "no_regiao_administrativa",
        "AP" = "no_area_planejamento"
      )
      
      agg_counts <- df_filtered %>%
        group_by(!!sym(agg_col)) %>%
        summarise(n = n(), .groups = "drop")
      
      map_sf <- agg_geos[[input$map_agg]] %>%
        left_join(agg_counts, by = c("unit" = agg_col)) %>%
        mutate(n = ifelse(is.na(n), 0, n))
      
      pal <- colorNumeric(
        palette = "Blues",
        domain = map_sf$n
      )
      
      l %>%
        addPolygons(
          data = map_sf,
          fillColor = ~pal(n),
          fillOpacity = 0.7,
          color = "white",
          weight = 1,
          label = ~paste0(unit, ": ", format(n, big.mark = ".", decimal.mark = ",")),
          highlightOptions = highlightOptions(weight = 3, color = "#666", fillOpacity = 0.9)
        )
    }
  }) %>% 
    bindCache(common_cache_keys())
}

shinyApp(ui, server)
