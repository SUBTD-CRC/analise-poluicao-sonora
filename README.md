# Monitoramento de Poluição Sonora 📢

Este projeto consiste em uma ferramenta de **Business Intelligence (BI)** desenvolvida em R/Shiny para o monitoramento e análise de chamados relacionados à poluição sonora na cidade do Rio de Janeiro. A aplicação permite visualizar dados históricos, identificar focos de reclamações através de mapas e analisar o volume de chamados por diferentes níveis territoriais.

## 🚀 Funcionalidades

- **Dashboard Interativo**: Filtros dinâmicos.
- **Análise Espacial**: Mapa interativo utilizando Leaflet para localização exata dos chamados.
- **Gráficos de Performance**: Visualizações de barras e tendências utilizando Plotly.
- **Métricas de Resumo**: Total de chamados, filtrados e percentual de representatividade.
- **Automação de Dados (ETL)**: Script automatizado para extração de dados diretamente do banco de dados SQL Server da PCRJ e processamento geoespacial.

## 📁 Estrutura do Repositório

```bash
├── app.R                      # Script principal da aplicação Shiny (UI/Server)
├── etl.R                      # Script de Extração, Transformação e Carga (ETL)
├── R/
│   └── util.R                 # Funções auxiliares (ex: criação de gráficos)
├── data/                      # Armazenamento de planilhas e extrações temporárias
├── www/                       # Ativos estáticos (imagens, CSS, logos)
└── analise-poluicao-sonora.Rproj # Arquivo de projeto do RStudio
```

## ⚙️ Como Executar

### Pré-requisitos

Certifique-se de ter o R e o RStudio instalados, além das dependências listadas no `etl.R` e `app.R`. Você pode instalar as principais bibliotecas com o comando:

```r
install.packages(c("shiny", "bslib", "leaflet", "plotly", "tidyverse", "readxl", "openxlsx", "sf", "DBI", "odbc", "shinyWidgets", "bsicons"))
```

### Passo a Passo

  - O script `etl.R` requer conexão com o banco de dados SQL Server. Certifique-se de configurar as seguintes variáveis de ambiente no seu sistema ou no arquivo `.Renviron`:
    - `DB_SERVER`: Endereço IP ou hostname do servidor.
    - `DB_DATABASE`: Nome do banco de dados.
    - `DB_USER`: Usuário do banco.
    - `DB_PWD`: Senha do usuário.
  - Execute o script `etl.R` para gerar a extração mais recente na pasta `data/`.

2. **Execução do App**:
   - Abra o arquivo `app.R` no RStudio.
   - Clique em **"Run App"** ou execute `shiny::runApp()` no console.

## 📊 Dados

Os dados são extraídos do banco da PCRJ, focando nos subtipos de chamados de poluição sonora. O processo de ETL realiza a conversão de coordenadas (SIRGAS 2000 para WGS84) para garantir a compatibilidade com o mapa do Leaflet.
