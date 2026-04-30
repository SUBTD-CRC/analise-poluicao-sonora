# Config -----------------------------------------------------------------------

# Library
if(!require("pacman")){
  install.packages("pacman")
} 

pacman::p_load(
  tidyverse, 
  openxlsx, 
  DBI,
  tictoc,
  sf
)

id_subtipo <- "'1135', '5071', '5232'"

# Query ------------------------------------------------------------------------

con <- DBI::dbConnect(
  odbc::odbc(),
  Driver = "ODBC Driver 17 for SQL Server",
  Server = Sys.getenv("DB_SERVER"),
  Database = Sys.getenv("DB_DATABASE"),
  UID = Sys.getenv("DB_USER"),
  PWD = Sys.getenv("DB_PWD"),
  Port = 1433
)

sql <- paste0("
WITH tb_consulta AS(
  SELECT
  tb_chamado.id_chamado,
  tb_chamado.dt_inicio,
  tb_chamado.dt_fim,
  tb_chamado_sla.dt_alvo_finalizacao,
  tb_origem_ocorrencia.no_origem_ocorrencia,
  tb_tipo.id_tipo,
  tb_tipo.no_tipo,
  tb_subtipo.id_subtipo,
  tb_subtipo.no_subtipo,
  tb_unidade_organizacional.no_unidade_organizacional AS gerencia,
  tb_unidade_organizacional.fl_ouvidoria,
  tb_status.no_status,
  tb_status.fl_encerramento,
  tb_categoria.no_categoria,
  tb_chamado.ds_chamado,
  tb_bairro.id_bairro,
  tb_bairro.no_bairro,
  tb_logradouro.no_logradouro,
  tb_territorialidade.no_area_planejamento,
  tb_territorialidade_regiao_administrativa.no_regiao_administrativa,
  st_coordenada.nu_coord_x,
  st_coordenada.nu_coord_Y,
  tb_classificacao_chamado.id_classificacao_chamado,
  tb_andamento.id_andamento
  FROM tb_chamado
  LEFT JOIN tb_origem_ocorrencia ON tb_chamado.id_origem_ocorrencia_fk = tb_origem_ocorrencia.id_origem_ocorrencia
  LEFT JOIN tb_classificacao_chamado ON tb_chamado.id_chamado = tb_classificacao_chamado.id_chamado_fk
  LEFT JOIN tb_classificacao ON tb_classificacao_chamado.id_classificacao_fk = tb_classificacao.id_classificacao
  LEFT JOIN tb_subtipo ON tb_classificacao.id_subtipo_fk = tb_subtipo.id_subtipo
  LEFT JOIN tb_tipo ON tb_subtipo.id_tipo_fk = tb_tipo.id_tipo
  LEFT JOIN tb_categoria ON tb_classificacao.id_categoria_fk = tb_categoria.id_categoria
  LEFT JOIN tb_status ON tb_chamado.id_status_fk = tb_status.id_status
  LEFT JOIN tb_bairro_logradouro ON tb_chamado.id_bairro_logradouro_fk = tb_bairro_logradouro.id_bairro_logradouro
  LEFT JOIN tb_bairro ON tb_bairro_logradouro.id_bairro_fk = tb_bairro.id_bairro
  LEFT JOIN tb_logradouro ON tb_bairro_logradouro.id_logradouro_fk = tb_logradouro.id_logradouro
  LEFT JOIN tb_andamento ON tb_chamado.id_chamado = tb_andamento.id_chamado_fk
  LEFT JOIN tb_territorialidade_regiao_administrativa_bairro ON tb_bairro.id_bairro = tb_territorialidade_regiao_administrativa_bairro.id_bairro_fk
  LEFT JOIN tb_territorialidade_regiao_administrativa ON tb_territorialidade_regiao_administrativa_bairro.id_territorialidade_regiao_administrativa_fk = tb_territorialidade_regiao_administrativa.id_territorialidade_regiao_administrativa
  LEFT JOIN tb_territorialidade ON tb_territorialidade_regiao_administrativa.id_territorialidade_fk = tb_territorialidade.id_territorialidade
  LEFT JOIN tb_chamado_sla ON tb_chamado.id_chamado = tb_chamado_sla.id_chamado_fk
  LEFT JOIN tb_responsavel_chamado ON tb_chamado.id_responsavel_chamado_fk = tb_responsavel_chamado.id_responsavel_chamado
  LEFT JOIN tb_unidade_organizacional ON tb_responsavel_chamado.id_unidade_organizacional_fk = tb_unidade_organizacional.id_unidade_organizacional
  LEFT JOIN st_coordenada ON tb_logradouro.id_logradouro = st_coordenada.id_logradouro_fk AND tb_chamado.ds_endereco_numero = st_coordenada.ds_endereco_numero
),
tb_max_classificacao AS (
  SELECT id_chamado,
  MAX(id_classificacao_chamado) AS max_classificacao,
  MAX(id_andamento) AS max_andamento
  FROM tb_consulta
  GROUP BY id_chamado
),
tb_consulta_recente AS (
  SELECT tb_consulta.*
    FROM tb_consulta
  LEFT JOIN tb_max_classificacao ON tb_consulta.id_chamado = tb_max_classificacao.id_chamado
  WHERE COALESCE(tb_consulta.id_classificacao_chamado, 0) = COALESCE(tb_max_classificacao.max_classificacao, 0)
  AND COALESCE(tb_consulta.id_andamento, 0) = COALESCE(tb_max_classificacao.max_andamento, 0)
)

SELECT * 
  FROM tb_consulta_recente
WHERE 1=1
  AND id_subtipo IN (", id_subtipo, ")
  AND (no_categoria = 'Serviço' OR fl_ouvidoria = 1)
  AND dt_inicio BETWEEN '2025-12-01 00:00:00.000' AND '2025-12-31 23:59:59.000'
")

tic()
consulta <- dbGetQuery(con, sql) 
toc()

# ETL --------------------------------------------------------------------------

consulta_tratamento <- consulta %>%
  mutate(
    across(starts_with("id_"), as.numeric),
    prazo = if_else(
      is.na(dt_fim),
      if_else(
        is.na(dt_alvo_finalizacao),
        "Aberto sem data alvo",
        if_else(
          Sys.time() < dt_alvo_finalizacao,
          "Aberto no prazo",
          "Aberto fora do prazo"
        ),
      ),
      if_else(
        is.na(dt_alvo_finalizacao),
        "Encerrado sem data alvo",
        if_else(
          dt_fim <= dt_alvo_finalizacao,
          "Encerrado no prazo",
          "Encerrado fora do prazo"
        )
      )
    ),
    situacao = if_else(fl_encerramento == TRUE, "Fechado", "Aberto")
  ) %>% 
  arrange(id_chamado) %>% 
  left_join(readxl::read_xlsx("data/arvore_uo.xlsx"), by = "gerencia") %>%
  relocate(no_unidade_organizacional, .after = gerencia) %>%
  relocate(prazo, .after = dt_alvo_finalizacao) %>% 
  relocate(situacao, .after = no_status) %>% 
  select(-c(fl_encerramento, id_classificacao_chamado, id_andamento))  %>%
  mutate(across(where(is.character), ~ str_sub(.x, 1, 32766))) %>% 
  left_join(readxl::read_xlsx("data/subprefeituras.xlsx") %>% select(id_bairro, no_subprefeitura), by = "id_bairro") %>% 
  mutate(
    across(c(no_subprefeitura, no_bairro), ~tidyr::replace_na(., "Ausente")),
    no_bairro = if_else(no_bairro == "NULL", "Ausente", no_bairro),
    id_bairro = if_else(is.na(id_bairro), 0, id_bairro)
  )

print(paste0("Linhas únicas: ", nrow(consulta_tratamento %>% distinct(id_chamado))))

# Geometry ---------------------------------------------------------------------

validos <- consulta_tratamento %>% 
  filter((!is.na(nu_coord_x) & !is.na(nu_coord_Y))) %>%
  st_as_sf(coords = c("nu_coord_x", "nu_coord_Y"), crs = 31983) %>% 
  st_transform(crs = 4326) %>%
  mutate(
    lat = st_coordinates(geometry)[, 2],
    lng = st_coordinates(geometry)[, 1],
    shape = TRUE
  ) %>% 
  st_drop_geometry()

invalidos <- consulta_tratamento %>%
  filter((is.na(nu_coord_x) & is.na(nu_coord_Y))) %>%
  select(-c(nu_coord_x, nu_coord_Y)) %>%
  mutate(
    lat = NA, 
    lng = NA,
    shape = FALSE
  )

df <- bind_rows(validos, invalidos) %>%
  arrange(id_chamado)

# Salva Arquivo ----------------------------------------------------------------

arquivo <- paste0("data/Extração do dia ", Sys.Date(), " Poluição Sonora.xlsx")

wb <- createWorkbook()
options("openxlsx.datetimeFormat" = "dd-mm-yyyy hh:mm:ss")

sheet <- "Base"
addWorksheet(wb, sheet)
writeDataTable(wb, sheet, df, startRow = 1, startCol = 1, tableStyle = "TableStyleMedium2")
setColWidths(wb, sheet, 1:ncol(df), rep(20, length(1:ncol(df))))
showGridLines(wb, sheet, showGridLines = FALSE)

saveWorkbook(wb, arquivo, overwrite = TRUE)
