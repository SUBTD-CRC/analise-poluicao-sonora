# Config -----------------------------------------------------------------------

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

# DF ---------------------------------------------------------------------------

data_files <- list.files("data", pattern = "Extração.*\\.xlsx", full.names = TRUE)
if (length(data_files) == 0) {stop("Nenhum arquivo de dados encontrado.")}
latest_file <- data_files[order(file.info(data_files)$mtime, decreasing = TRUE)][1]
df <- read_excel(latest_file)

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

# APP --------------------------------------------------------------------------

ui <- page_navbar(
  title = uiOutput("navbar_title"),
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
      start = min(df$dt_inicio, na.rm = TRUE),
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
      col_widths = c(7, 5),
      card(
        card_header("Localização dos Chamados"),
        leafletOutput("mapa_chamados", height = "550px"),
        full_screen = TRUE
      ),
      card(
        card_header("Volume por Subtipo"),
        plotlyOutput("subtipo_plot", height = "550px"),
        full_screen = TRUE
      )
    )
  ),
  
  nav_panel(
    title = "Análise Territorial",
    icon = bs_icon("geo-alt"),
    
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
  
  is_dark <- reactive({
    isTruthy(input$dark_mode) && input$dark_mode == "dark"
  })
  
  output$subtipo_plot <- renderPlotly({
    create_bar_plot(filtered_df(), "no_subtipo", "Subtipo", 10, dark = is_dark()) 
  })
  
  output$bairro_plot <- renderPlotly({
    create_bar_plot(filtered_df(), "no_bairro", "Bairro", 15, dark = is_dark())
  })
  
  output$ra_plot <- renderPlotly({
    create_bar_plot(filtered_df(), "no_regiao_administrativa", "RA", 15, dark = is_dark())
  })
  
  output$sub_plot <- renderPlotly({
    create_bar_plot(filtered_df(), "no_subprefeitura", "Subprefeitura", 0, dark = is_dark())
  })
  
  output$ap_plot <- renderPlotly({
    create_bar_plot(filtered_df(), "no_area_planejamento", "AP", 0, dark = is_dark())
  })
  
  output$mapa_chamados <- renderLeaflet({
    req(filtered_df())
    
    map_data <- filtered_df() %>% 
      filter(!is.na(lat) & !is.na(lng))
    
    map_tile <- if(is_dark()) providers$CartoDB.DarkMatter else providers$CartoDB.Positron
    
    if(nrow(map_data) == 0) {
      return(leaflet() %>% addProviderTiles(map_tile) %>% setView(lng = -43.1729, lat = -22.9068, zoom = 11))
    }
    
    leaflet(map_data) %>%
      addProviderTiles(map_tile) %>%
      addCircleMarkers(
        ~lng, 
        ~lat, 
        radius = 5, 
        color = "#004a80", 
        fillColor = "#00a2da",
        fillOpacity = 0.6, weight = 1,
        popup = ~paste0(
          "<b>ID: </b>", if("id_chamado" %in% colnames(map_data)) id_chamado else "N/A", 
          "<br><b>Bairro: </b>", if("no_bairro" %in% colnames(map_data)) no_bairro else "N/A", 
          "<br><b>Subtipo: </b>", if("no_subtipo" %in% colnames(map_data)) no_subtipo else "N/A")
        )
  })
}

shinyApp(ui, server)
