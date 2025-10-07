#GRÁFICO TERMO DE CONSUMO DE MODA - NGRAM

# 1. CARREGAMENTO DE PACOTES 
options(scipen = 999) 
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("dplyr")) install.packages("dplyr")
if (!require("scales")) install.packages("scales")

library(ggplot2)
library(dplyr)
library(scales)
library(ngramr)
dados_reais <- ngramr::ngram(c("consumo de moda"), year_start = 1980, year_end = 2020, corpus = "portuguese")


# 3. CONSTRUÇÃO DO GRÁFICO COM LEGENDA ATUALIZADA -----------------------------
ggplot(dados_moda, aes(x = Ano, y = Frequencia)) +
  geom_line(color = "#e377c2", linewidth = 1.5) +
  geom_point(color = "#e377c2", size = 3) +
  
  # Configurações dos eixos
  scale_y_continuous(
    limits = c(0, 0.000012),
    breaks = seq(0, 0.000012, by = 0.000002),
    labels = label_number(scale_cut = cut_short_scale()),
    expand = c(0, 0)
  ) +
  scale_x_continuous(
    breaks = seq(1980, 2020, by = 5),
    expand = c(0.02, 0.02)
  ) +
  
  # LEGENDA 
  labs(
    title = "PREVALÊNCIA DO TERMO 'CONSUMO DE MODA'",
    subtitle = "Estimativas do NGram do Google (1980-2020)",
    x = NULL,
    y = "Frequência Relativa\n",
    caption = "Fonte: Elaboração própria com base no Google NGram"
  ) +
  
  # Formatação do tema
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
    plot.subtitle = element_text(hjust = 0.5, color = "gray40", size = 12),
    panel.grid.major = element_line(color = "gray90", linewidth = 0.2),
    panel.grid.minor = element_blank(),
    axis.text = element_text(color = "black"),
    axis.title.y = element_text(margin = margin(r = 10)),
    plot.margin = margin(20, 20, 20, 20)
  )

# 4. SALVANDO O GRÁFICO --------------------------------------------------------
ggsave("prevalencia_consumo_moda.png", width = 10, height = 6, dpi = 300)

# 5. ANÁLISE COMPLEMENTAR ------------------------------------------------------
cat("\nDADOS ESTATÍSTICOS:\n")
dados_moda %>%
  summarise(
    `Período` = paste(range(Ano), collapse = "-"),
    `Média` = format(mean(Frequencia), scientific = TRUE, digits = 3),
    `Máximo` = format(max(Frequencia), scientific = TRUE, digits = 3),
    `Ano do pico` = Ano[which.max(Frequencia)]
  ) %>%
  print()







#GRÁFICO MODA X SUSTENTABILIDADE – GOOGLE TRENDS
# Instalação e carregamento de pacotes
if (!require("gtrendsR")) install.packages("gtrendsR")
if (!require("dplyr")) install.packages("dplyr")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("lubridate")) install.packages("lubridate")

library(gtrendsR)
library(dplyr)
library(ggplot2)
library(lubridate)

# 1. Coleta de dados no Google Trends
tryCatch({
  # Busca nacional (Brasil)
  dados_br <- gtrends(
    keyword = c("Moda", "Sustentabilidade"),
    geo = "BR",
    time = "today 12-m",  # Últimos 12 meses
    gprop = "web",        # Buscas na web
    onlyInterest = TRUE   # Apenas dados de interesse
  )
  
  # Busca em Mossoró (RN)
  dados_mossoro <- gtrends(
    keyword = c("Moda", "Sustentabilidade"),
    geo = "BR-RN",        # Rio Grande do Norte
    time = "today 12-m",
    gprop = "web",
    onlyInterest = TRUE
  )
  
  # 2. Processamento dos dados
  processar_dados <- function(dados, regiao) {
    dados$interest_over_time %>%
      select(date, hits, keyword) %>%
      mutate(
        date = as.Date(date),
        hits = as.numeric(ifelse(hits == "<1", "0.5", hits)),
        regiao = regiao
      )
  }
  
  dados_br <- processar_dados(dados_br, "Brasil")
  dados_mossoro <- processar_dados(dados_mossoro, "Mossoró (RN)")
  
  # Combinar os dados
  dados_completos <- bind_rows(dados_br, dados_mossoro) %>%
    mutate(
      keyword = factor(keyword),
      regiao = factor(regiao)
    )
  
  # 3. Visualização dos dados
  
  # Gráfico de linhas temporal
  grafico_linhas <- ggplot(dados_completos, 
                           aes(x = date, y = hits, color = keyword, linetype = regiao)) +
    geom_line(linewidth = 1) +
    scale_color_manual(values = c("#FF6B6B", "#4ECDC4")) +
    labs(
      title = "Interesse por 'Moda' vs. 'Sustentabilidade'",
      subtitle = "Comparação entre Brasil e Mossoró (RN) - Últimos 12 meses",
      x = "Data",
      y = "Interesse Relativo (Google Trends)",
      color = "Termo de Busca",
      linetype = "Região"
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      plot.title = element_text(face = "bold", size = 14),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
  
  # Gráfico de barras com médias
  dados_media <- dados_completos %>%
    group_by(keyword, regiao) %>%
    summarise(media_hits = mean(hits, na.rm = TRUE))
  
  grafico_barras <- ggplot(dados_media, 
                           aes(x = keyword, y = media_hits, fill = regiao)) +
    geom_col(position = position_dodge(width = 0.8), width = 0.7) +
    geom_text(aes(label = round(media_hits, 1)), 
              position = position_dodge(width = 0.8), 
              vjust = -0.5) +
    scale_fill_manual(values = c("#66C2A5", "#FC8D62")) +
    labs(
      title = "Média de Interesse por Termo de Busca",
      subtitle = "Comparação entre regiões",
      x = "Termo de Busca",
      y = "Média de Interesse",
      fill = "Região"
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      plot.title = element_text(face = "bold", size = 14)
    )
  
  # 4. Exibir os resultados
  print(grafico_linhas)
  print(grafico_barras)
  
  # 5. Salvar os gráficos
  ggsave("interesse_temporal.png", grafico_linhas, width = 10, height = 6)
  ggsave("media_interesse.png", grafico_barras, width = 8, height = 6)
  
  # Retornar os dados para análise adicional
  list(
    dados_completos = dados_completos,
    dados_media = dados_media
  )
  
}, error = function(e) {
  message("Ocorreu um erro na coleta de dados: ", e$message)
  message("Verifique sua conexão com a internet ou tente termos de busca diferentes.")
  NULL
})

 #Gráfico de Interesse Fast fashion x Moda Sustentável no Brasil

# ANÁLISE DE TENDÊNCIAS DE MODA E SUSTENTABILIDADE
# Integração entre Google Trends e Google News API
# Versão consolidada e corrigida

# 1. Instalação e carregamento de pacotes -------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  gtrendsR,       # Dados do Google Trends
  httr,           # Conexão com APIs
  jsonlite,       # Manipulação de JSON
  dplyr,          # Manipulação de dados
  ggplot2,        # Visualização gráfica
  lubridate,      # Manipulação de datas
  ggrepel,        # Melhor disposição de textos
  purrr,          # Programação funcional
  stringr         # Manipulação de strings
)

# 2. Configurações iniciais ---------------------------------------------------
api_key <- "8b09fe5c68e6473e94e1dac09b03a4c6"  # Substitua pela sua chave do NewsAPI

# Termos de busca e período de análise
termos <- c("Moda sustentável","Fast fashion")
regiao <- "BR"
periodo <- "today 12-m"  # Últimos 12 meses

# 3. Funções auxiliares corrigidas --------------------------------------------

# Função para buscar notícias (versão robusta)
buscar_noticias <- function(termo, data_inicio, data_fim, api_key) {
  url <- "https://newsapi.org/v2/everything"
  
  resposta <- tryCatch({
    GET(
      url,
      query = list(
        q = termo,
        from = format(data_inicio, "%Y-%m-%d"),
        to = format(data_fim, "%Y-%m-%d"),
        language = "pt",
        sortBy = "relevancy",
        pageSize = 20,
        apiKey = api_key
      ),
      timeout(10)  # Timeout de 10 segundos
    )
  }, error = function(e) {
    message("Erro na conexão: ", e$message)
    return(NULL)
  })
  
  if (is.null(resposta)) {
    return(data.frame(
      title = character(),
      publishedAt = as.Date(character()),
      url = character(),
      source.name = character(),
      stringsAsFactors = FALSE
    ))
  }
  
  if (status_code(resposta) != 200) {
    message("Erro na API: ", status_code(resposta))
    return(data.frame(
      title = character(),
      publishedAt = as.Date(character()),
      url = character(),
      source.name = character(),
      stringsAsFactors = FALSE
    ))
  }
  
  conteudo <- content(resposta, "text")
  dados <- fromJSON(conteudo)
  
  if (dados$status != "ok" || dados$totalResults == 0) {
    return(data.frame(
      title = character(),
      publishedAt = as.Date(character()),
      url = character(),
      source.name = character(),
      stringsAsFactors = FALSE
    ))
  }
  
  dados$articles %>%
    select(title, publishedAt, url, source.name) %>%
    mutate(publishedAt = as.Date(publishedAt))
}

# Função para identificar picos (versão segura)
identificar_picos <- function(dados, limiar_desvio = 1.5) {
  if (nrow(dados) == 0) {
    stop("Dados vazios fornecidos para identificar_picos()")
  }
  
  dados %>%
    group_by(keyword) %>%
    mutate(
      media = mean(hits, na.rm = TRUE),
      desvio = sd(hits, na.rm = TRUE),
      limiar = media + limiar_desvio * desvio,
      pico = hits > limiar & hits > 20  # Mínimo absoluto de 20
    ) %>%
    filter(pico) %>%
    select(date, keyword, hits) %>%
    ungroup()
}

# 4. Coleta e processamento de dados ------------------------------------------

# 4.1 Obter dados do Google Trends (com tratamento de erro)
message("\nColetando dados do Google Trends...")
dados_trends <- tryCatch({
  g <- gtrends(
    keyword = termos,
    geo = regiao,
    time = periodo,
    gprop = "web",
    onlyInterest = TRUE
  )
  
  if (!"interest_over_time" %in% names(g)) {
    stop("Estrutura de dados inesperada do Google Trends")
  }
  
  g$interest_over_time %>%
    mutate(
      date = as.Date(date),
      hits = as.numeric(ifelse(hits == "<1", "0.5", hits)),
      keyword = factor(keyword)
    )
}, error = function(e) {
  stop("Erro ao coletar dados do Google Trends: ", e$message)
})

# 4.2 Identificar picos de busca
message("Identificando picos de busca...")
picos <- tryCatch({
  identificar_picos(dados_trends)
}, error = function(e) {
  message("Erro ao identificar picos: ", e$message)
  data.frame(date = as.Date(character()), 
             keyword = character(), 
             hits = numeric())
})

# 4.3 Buscar notícias relacionadas aos picos
message("Buscando notícias relacionadas aos picos...")
if (nrow(picos) > 0) {
  contexto_noticias <- picos %>%
    group_split(keyword, date) %>%
    map_dfr(~ {
      termo <- as.character(.x$keyword[1])
      data_pico <- .x$date[1]
      
      message(sprintf("Buscando notícias para '%s' em %s...", termo, data_pico))
      
      noticias <- buscar_noticias(
        termo = termo,
        data_inicio = data_pico - days(14),  # Janela de 2 semanas
        data_fim = data_pico + days(14),
        api_key = api_key
      )
      
      if (nrow(noticias) > 0) {
        noticias %>%
          mutate(
            keyword = termo,
            pico_date = data_pico,
            dias_relativo = as.numeric(publishedAt - data_pico)
          )
      } else {
        NULL  # Será ignorado pelo bind_rows
      }
    })
  
  # Garante um data frame vazio se nenhuma notícia for encontrada
  if (!exists("contexto_noticias") || nrow(contexto_noticias) == 0) {
    contexto_noticias <- data.frame(
      title = character(),
      publishedAt = as.Date(character()),
      url = character(),
      source.name = character(),
      keyword = character(),
      pico_date = as.Date(character()),
      dias_relativo = numeric(),
      stringsAsFactors = FALSE
    )
    message("Nenhuma notícia encontrada para os picos identificados.")
  }
} else {
  contexto_noticias <- data.frame(
    title = character(),
    publishedAt = as.Date(character()),
    url = character(),
    source.name = character(),
    keyword = character(),
    pico_date = as.Date(character()),
    dias_relativo = numeric(),
    stringsAsFactors = FALSE
  )
  message("Nenhum pico significativo identificado nos dados.")
}

# 5. Visualização dos dados 

# 5.1 Gráfico de tendências com picos
if (nrow(dados_trends) > 0) {
  grafico_tendencias <- ggplot(dados_trends, aes(x = date, y = hits, color = keyword)) +
    geom_line(size = 1) +
    {if (nrow(picos) > 0) geom_point(data = picos, aes(x = date, y = hits), 
                                     size = 3, shape = 21, fill = "white", stroke = 1.5)} +
    scale_color_brewer(palette = "Set1") +
    labs(
      title = "Interesse por Moda Sustentável e Fast Fashion no Brasil",
      subtitle = "Picos de busca destacados (últimos 12 meses)",
      x = "Data",
      y = "Interesse Relativo",
      color = "Termo de Busca"
    ) +
    theme_minimal() +
    theme(
      legend.position = "top",
      plot.title = element_text(face = "bold", size = 14),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
  
  print(grafico_tendencias)
}

# 5.2 Gráfico de contexto de notícias (se houver dados)
if (exists("contexto_noticias") && nrow(contexto_noticias) > 0) {
  grafico_contexto <- ggplot(dados_trends, aes(x = date, y = hits, color = keyword)) +
    geom_line(size = 1, alpha = 0.7) +
    {if (nrow(picos) > 0) geom_point(data = picos, aes(x = date, y = hits), 
                                     size = 3, shape = 21, fill = "white", stroke = 1.5)} +
    geom_text_repel(
      data = contexto_noticias %>%
        group_by(keyword, pico_date) %>%
        arrange(desc(abs(dias_relativo))) %>%
        slice(1),
      aes(x = pico_date, y = 100, label = str_wrap(title, 30)),
      nudge_y = 20,
      size = 3,
      color = "black",
      box.padding = 0.5,
      max.overlaps = Inf
    ) +
    scale_color_brewer(palette = "Set1") +
    labs(
      title = "Picos de Busca e Notícias Relacionadas",
      subtitle = "Contexto midiático dos picos de interesse",
      x = "Data",
      y = "Interesse Relativo",
      color = "Termo de Busca"
    ) +
    theme_minimal()
  
  print(grafico_contexto)
}

# 6. Salvar resultados --------------------------------------------------------

# 6.1 Salvar dados processados
write.csv(dados_trends, "dados_trends.csv", row.names = FALSE)
write.csv(picos, "picos_busca.csv", row.names = FALSE)
if (exists("contexto_noticias")) {
  write.csv(contexto_noticias, "noticias_relacionadas.csv", row.names = FALSE)
}

# 6.2 Salvar gráficos
if (exists("grafico_tendencias")) {
  ggsave("tendencias.png", grafico_tendencias, width = 10, height = 6, dpi = 300)
}
if (exists("grafico_contexto")) {
  ggsave("contexto_noticias.png", grafico_contexto, width = 12, height = 8, dpi = 300)
}

message("\nAnálise concluída com sucesso! Resultados salvos em:")
message("- dados_trends.csv: Dados completos do Google Trends")
message("- picos_busca.csv: Picos identificados")
if (exists("contexto_noticias") && nrow(contexto_noticias) > 0) {
  message("- noticias_relacionadas.csv: Notícias encontradas relacionadas aos picos")
}
message("- tendencias.png: Gráfico de tendências")
if (exists("grafico_contexto")) {
  message("- contexto_noticias.png: Gráfico de contexto com notícias")
}
# Instalar pacote se necessário
# install.packages("gtrendsR")

library(gtrendsR)
library(dplyr)

# Coleta de dados
busca <- gtrends(c("fast fashion", "moda sustentável"), geo = "BR", time = "today 12-m")

# Acesso à série temporal
dados <- busca$interest_over_time

# Visualizar estrutura
head(dados)

# Calcular média de interesse por termo
medias <- dados %>%
  group_by(keyword) %>%
  summarise(media_interesse = mean(hits))

print(medias)

#Média de Interesse por termo de busca

# Instalação e carregamento de pacotes
if (!require("gtrendsR")) install.packages("gtrendsR")
if (!require("dplyr")) install.packages("dplyr")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("lubridate")) install.packages("lubridate")

library(gtrendsR)
library(dplyr)
library(ggplot2)
library(lubridate)

# 1. Coleta de dados no Google Trends
tryCatch({
  # Busca nacional (Brasil)
  dados_br <- gtrends(
    keyword = c("Moda", "Sustentabilidade"),
    geo = "BR",
    time = "today 12-m",  # Últimos 12 meses
    gprop = "web",        # Buscas na web
    onlyInterest = TRUE   # Apenas dados de interesse
  )
  
  # Busca em Mossoró (RN)
  dados_mossoro <- gtrends(
    keyword = c("Moda", "Sustentabilidade"),
    geo = "BR-RN",        # Rio Grande do Norte
    time = "today 12-m",
    gprop = "web",
    onlyInterest = TRUE
  )
  
  # 2. Processamento dos dados
  processar_dados <- function(dados, regiao) {
    dados$interest_over_time %>%
      select(date, hits, keyword) %>%
      mutate(
        date = as.Date(date),
        hits = as.numeric(ifelse(hits == "<1", "0.5", hits)),
        regiao = regiao
      )
  }
  
  dados_br <- processar_dados(dados_br, "Brasil")
  dados_mossoro <- processar_dados(dados_mossoro, "Mossoró (RN)")
  
  # Combinar os dados
  dados_completos <- bind_rows(dados_br, dados_mossoro) %>%
    mutate(
      keyword = factor(keyword),
      regiao = factor(regiao)
    )
  
  # 3. Visualização dos dados
  
  # Gráfico de linhas temporal
  grafico_linhas <- ggplot(dados_completos, 
                           aes(x = date, y = hits, color = keyword, linetype = regiao)) +
    geom_line(linewidth = 1) +
    scale_color_manual(values = c("#FF6B6B", "#4ECDC4")) +
    labs(
      title = "Interesse por 'Moda' vs. 'Sustentabilidade'",
      subtitle = "Comparação entre Brasil e Mossoró (RN) - Últimos 12 meses",
      x = "Data",
      y = "Interesse Relativo (Google Trends)",
      color = "Termo de Busca",
      linetype = "Região"
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      plot.title = element_text(face = "bold", size = 14),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
  
  # Gráfico de barras com médias
  dados_media <- dados_completos %>%
    group_by(keyword, regiao) %>%
    summarise(media_hits = mean(hits, na.rm = TRUE))
  
  grafico_barras <- ggplot(dados_media, 
                           aes(x = keyword, y = media_hits, fill = regiao)) +
    geom_col(position = position_dodge(width = 0.8), width = 0.7) +
    geom_text(aes(label = round(media_hits, 1)), 
              position = position_dodge(width = 0.8), 
              vjust = -0.5) +
    scale_fill_manual(values = c("#66C2A5", "#FC8D62")) +
    labs(
      title = "Média de Interesse por Termo de Busca",
      subtitle = "Comparação entre regiões",
      x = "Termo de Busca",
      y = "Média de Interesse",
      fill = "Região"
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      plot.title = element_text(face = "bold", size = 14)
    )
  
  # 4. Exibir os resultados
  print(grafico_linhas)
  print(grafico_barras)
  
  # 5. Salvar os gráficos
  ggsave("interesse_temporal.png", grafico_linhas, width = 10, height = 6)
  ggsave("media_interesse.png", grafico_barras, width = 8, height = 6)
  
  # Retornar os dados para análise adicional
  list(
    dados_completos = dados_completos,
    dados_media = dados_media
  )
  
}, error = function(e) {
  message("Ocorreu um erro na coleta de dados: ", e$message)
  message("Verifique sua conexão com a internet ou tente termos de busca diferentes.")
  NULL
})
5. Gráficos Demográficos
# Carregar pacotes necessários
library(tidyverse)
library(ggplot2)
library(scales)
library(readxl)
library(plotly)
library(shiny)



# 1. Importar os dados
dados <- read_excel(caminho, sheet = "Respostas ao formulário 1")

# 2. Limpeza e preparação dos dados
dados_limpos <- dados %>%
  rename(
    genero = "Qual sua identidade de gênero?",
    etnia = "Qual  sua cor ou raça/etnia ?",
    idade = "Qual a sua idade?"
  ) %>%
  select(genero, etnia, idade) %>%
  mutate(
    genero = str_trim(genero), # Remove espaços extras
    genero = ifelse(genero == "", NA, genero), # Trata valores vazios
    genero = fct_infreq(genero), # Fator ordenado por frequência
    etnia = str_trim(etnia),
    etnia = ifelse(etnia == "", NA, etnia),
    etnia = fct_infreq(etnia),
    idade = str_trim(idade),
    idade = ifelse(idade == "", NA, idade),
    idade = factor(idade, 
                   levels = c("Menos de 18 anos", "De 18 a 29 anos", 
                              "De 30 a 49 anos", "50 anos ou mais"),
                   ordered = TRUE)
  ) %>%
  filter(!is.na(genero), !is.na(etnia), !is.na(idade)) # Remove NAs

# 3. Análise descritiva
cat("\nResumo das variáveis demográficas:\n")
summary(dados_limpos)

# 4. Visualizações

# Configuração do tema para os gráficos
tema_personalizado <- theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.position = "bottom",
    legend.title = element_text(face = "bold")
  )

# Gráfico 1: Distribuição de Gênero
p1 <- ggplot(dados_limpos, aes(x = genero, fill = genero)) +
  geom_bar(stat = "count") +
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5) +
  labs(title = "Distribuição por Identidade de Gênero",
       x = "Identidade de Gênero",
       y = "Número de Respondentes",
       fill = "Gênero") +
  tema_personalizado +
  scale_fill_brewer(palette = "Set2")

# Gráfico 2: Distribuição por Faixa Etária
p2 <- ggplot(dados_limpos, aes(x = idade, fill = idade)) +
  geom_bar(stat = "count") +
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5) +
  labs(title = "Distribuição por Faixa Etária",
       x = "Faixa Etária",
       y = "Número de Respondentes",
       fill = "Idade") +
  tema_personalizado +
  scale_fill_brewer(palette = "Pastel1") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Gráfico 3: Distribuição por Etnia
p3 <- ggplot(dados_limpos, aes(x = etnia, fill = etnia)) +
  geom_bar(stat = "count") +
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5) +
  labs(title = "Distribuição por Cor/Raça/Etnia",
       x = "Cor/Raça/Etnia",
       y = "Número de Respondentes",
       fill = "Etnia") +
  tema_personalizado +
  scale_fill_brewer(palette = "Set3") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Gráfico 4: Gênero por Faixa Etária (gráfico de barras empilhadas)
p4 <- ggplot(dados_limpos, aes(x = idade, fill = genero)) +
  geom_bar(position = "fill") +
  labs(title = "Proporção de Gênero por Faixa Etária",
       x = "Faixa Etária",
       y = "Proporção",
       fill = "Gênero") +
  tema_personalizado +
  scale_fill_brewer(palette = "Set2") +
  scale_y_continuous(labels = percent) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Gráfico 5: Etnia por Gênero (gráfico de barras agrupadas)
p5 <- ggplot(dados_limpos, aes(x = genero, fill = etnia)) +
  geom_bar(position = "dodge") +
  labs(title = "Distribuição de Etnia por Identidade de Gênero",
       x = "Identidade de Gênero",
       y = "Número de Respondentes",
       fill = "Etnia") +
  tema_personalizado +
  scale_fill_brewer(palette = "Set3")

# 5. Exportar os gráficos
# Criar diretório para salvar os gráficos
if (!dir.exists("graficos")) {
  dir.create("graficos")
}

# Salvar gráficos individualmente
ggsave("graficos/genero.png", plot = p1, width = 8, height = 6, dpi = 300)
ggsave("graficos/idade.png", plot = p2, width = 8, height = 6, dpi = 300)
ggsave("graficos/etnia.png", plot = p3, width = 8, height = 6, dpi = 300)
ggsave("graficos/genero_idade.png", plot = p4, width = 9, height = 6, dpi = 300)
ggsave("graficos/etnia_genero.png", plot = p5, width = 9, height = 6, dpi = 300)

# 6. Exibir os gráficos no R
print(p1)
print(p2)
print(p3)
print(p4)
print(p5)

cat("\nAnálise concluída! Os gráficos foram salvos na pasta 'graficos'.\n")


6. Perfil Hierarquico dos Respondentes
library(tidyverse)
library(readxl)
library(ggplot2)
library(forcats)
library(kableExtra)  # Para tabelas estilizadas
library(viridis)     # Para paletas de cores modernas


dados <- read_excel(caminho, sheet = "Respostas ao formulário 1") %>%
  rename(
    idade = "Qual a sua idade?",
    genero = "Qual sua identidade de gênero?",
    raca = "Qual  sua cor ou raça/etnia ?"
  )

Análise hierárquica 
hierarquia <- dados %>%
  count(idade, genero, raca, name = "total") %>%
  arrange(desc(total)) %>%
  mutate(grupo = paste(genero, raca, idade, sep = " | "))

Tabela estilizada (Top 10) 
hierarquia %>%
  head(10) %>%
  select(grupo, total) %>%
  kable(
    caption = "<b>Top 10 Grupos com Mais Respostas</b>",
    col.names = c("Grupo (Gênero | Raça | Idade)", "Total"),
    align = c("l", "c"),
    format = "html"
  ) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover"),
    full_width = FALSE,
    font_size = 14
  ) %>%
  row_spec(1, bold = TRUE, color = "white", background = "#4F81BD")  # Destaca o 1º lugar

Gráfico estilizado (Top 10) -
  ggplot(head(hierarquia, 10), aes(
    x = total,
    y = fct_reorder(grupo, total),
    fill = total  # Preenchimento gradiente
  )) +
  geom_col(width = 0.8) +
  geom_text(
    aes(label = total),
    hjust = 1.2,
    color = "white",
    fontface = "bold",
    size = 4
  ) +
  scale_fill_viridis(option = "D", direction = -1) +  # Paleta 'viridis' (roxo-dourado)
  labs(
    title = "PERFIL DOS RESPONDENTES",
    subtitle = "Hierarquia por Gênero, Raça e Idade",
    x = "Número de Respostas",
    y = NULL,
    caption = "Fonte: Seu Formulário (2024)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 18),
    plot.subtitle = element_text(hjust = 0.5, color = "gray40"),
    axis.text.y = element_text(face = "bold", color = "black"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none"
  )
library(tidyverse)
library(ggplot2)
library(viridis)

# [...] (Seu código anterior de leitura dos dados e análise hierárquica aqui)

ggplot(head(hierarquia, 10), aes(
  x = total,
  y = fct_reorder(grupo, total),
  fill = total
)) +
  geom_col(width = 0.8, show.legend = FALSE) +
  geom_text(
    aes(label = total),
    hjust = 1.2,
    color = "white",
    fontface = "bold",
    size = 4
  ) +
  scale_fill_viridis(option = "D", direction = -1) +
  labs(
    title = "PERFIL DOS RESPONDENTES",
    subtitle = "Hierarquia por Gênero, Raça e Idade",
    x = "Número de Respostas",
    y = NULL,
    caption = "Elaboração própria, 2025"  
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 18, margin = margin(b = 10)),
    plot.subtitle = element_text(hjust = 0.5, color = "grey40", margin = margin(b = 20)),
    plot.caption = element_text(hjust = 1, color = "grey30", face = "italic", size = 10),
    axis.text.y = element_text(face = "bold", color = "black"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  )	
#Frequência Fast Fashion x Impacto ambiental
# Pacotes necessários
library(readxl)
library(dplyr)
library(ggplot2)

# 1. Carregar os dados
caminho <- "C:/Users/FUERN/Desktop/SYANG/pendrive/MONOGRAFIA 2025/Dados Monografia.xlsx"
dados <- read_excel(caminho, sheet = "Respostas ao formulário 1")

# 2. Renomear e padronizar as colunas de interesse
dados <- dados %>%
  rename(
    compra_fast_fashion_raw = `Você compra roupas de Fast-fashion?`,
    impacto_ambiental = `Em que grau você acha que seu consumo de fast-fashion impacta o meio ambiente?`
  )

# 3. Criar variável padronizada para frequência de compra
dados <- dados %>%
  mutate(frequencia_consumo = case_when(
    compra_fast_fashion_raw == "Sim, frequentemente" ~ "Alta",
    compra_fast_fashion_raw == "Sim, às vezes" ~ "Média",
    compra_fast_fashion_raw == "Não, evito" ~ "Baixa",
    compra_fast_fashion_raw == "Não, nunca" ~ "Nenhuma",
    TRUE ~ NA_character_
  )) %>%
  mutate(frequencia_consumo = factor(frequencia_consumo, levels = c("Nenhuma", "Baixa", "Média", "Alta")))

# 4. Verificar frequência por categoria
cat("\nFrequência de respostas por nível de consumo:\n")
print(table(dados$frequencia_consumo))

# 5. Média de percepção de impacto ambiental por grupo
cat("\nMédia de percepção de impacto ambiental por grupo de consumo:\n")
print(aggregate(impacto_ambiental ~ frequencia_consumo, data = dados, mean, na.rm = TRUE))

# 6. Boxplot da relação entre consumo e percepção de impacto
ggplot(dados, aes(x = frequencia_consumo, y = impacto_ambiental, fill = frequencia_consumo)) +
  geom_boxplot() +
  scale_fill_manual(values = c("gray80", "skyblue", "khaki", "tomato")) +
  labs(
    title = "Percepção de impacto ambiental por frequência de consumo de fast-fashion",
    x = "Frequência de consumo de fast-fashion",
    y = "Percepção de impacto ambiental (1-5)"
  ) +
  theme_minimal()

# 7. Teste ANOVA para verificar diferença estatística
anova_result <- aov(impacto_ambiental ~ frequencia_consumo, data = dados)
cat("\nResultado do teste ANOVA:\n")
print(summary(anova_result))

# 8. Teste post-hoc de Tukey (se aplicável)
cat("\nTeste post-hoc de Tukey (se aplicável):\n")
print(TukeyHSD(anova_result))
ggplot(dados, aes(x = frequencia_consumo, y = impacto_ambiental, fill = frequencia_consumo)) +
  stat_summary(fun = "mean", geom = "bar", position = "dodge", width = 0.7) +
  scale_fill_manual(values = c("gray80", "skyblue", "khaki", "tomato")) +
  labs(
    title = "Média de Percepção de Impacto Ambiental por Frequência de Consumo",
    x = "Frequência de Consumo",
    y = "Média de Percepção de Impacto Ambiental"
  ) +
  theme_minimal()
#LDA impacto meio ambiente
# CARREGAR PACOTES E DADOS ----
library(tidyverse)
library(readxl)
library(tm)
library(topicmodels)
library(tidytext)
library(wordcloud)
library(ggthemes)

# Definir cores das categorias
cores_categorias <- c(
  "Uso de Recursos Naturais" = "#1f77b4",
  "Cadeia Produtiva" = "#aec7e8",
  "Impacto Poluidor" = "#17becf",
  "Geração de Resíduos" = "#2ca02c",
  "Impacto na Biodiversidade" = "#98df8a",
  "Roupas Usadas" = "#ff7f0e",
  "Reutilização Social" = "#ffbb78",
  "Práticas de Descarte" = "#d62728",
  "Cuidados com Conservação" = "#ff9896",
  "Impacto Irrelevante" = "#9467bd",
  "Compensação Ambiental" = "#c5b0d5",
  "Consumo Consciente" = "#8c564b",
  "Minimalismo" = "#c49c94"
)

# Carregar dados
dados <- read_excel("C:/Users/FUERN/Desktop/SYANG/pendrive/MONOGRAFIA 2025/Dados.xlsx")

processar_texto <- function(texto) {
  texto %>%
    tolower() %>%
    removePunctuation() %>%
    removeNumbers() %>%
    removeWords(c(stopwords("portuguese"), "roupa", "roupas", "moda", "fashion")) %>%
    stripWhitespace() %>%
    iconv(to = "ASCII//TRANSLIT")
}

corpus <- dados$`impacto no meio ambiente` %>%
  VectorSource() %>%
  VCorpus() %>%
  tm_map(content_transformer(processar_texto))

# Criar DTM com filtro de termos raros
dtm <- DocumentTermMatrix(corpus,
                          control = list(bounds = list(global = c(2, Inf))))

# Remover documentos vazios
dtm <- dtm[rowSums(as.matrix(dtm)) > 0, ]

# MODELO LDA ----
set.seed(123)
k <- 6  # Número de tópicos baseado nas suas categorias
modelo_lda <- LDA(dtm, k = k, control = list(seed = 1234, alpha = 0.1))

# ANÁLISE DOS TÓPICOS ----
# Termos por tópico
termos_por_topico <- tidy(modelo_lda, matrix = "beta")

# Visualização dos principais termos
top_terms <- termos_por_topico %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

# Mapeamento 
mapeamento_categorias <- c(
  "1" = "Geração de Resíduos",
  "2" = "Consumo Consciente",
  "3" = "Cadeia Produtiva",
  "4" = "Reutilização Social",
  "5" = "Impacto Poluidor",
  "6" = "Uso de Recursos Naturais"
)

# Gráfico dos tópicos com cores temáticas
ggplot(top_terms, aes(reorder_within(term, beta, topic), beta, 
                      fill = factor(mapeamento_categorias[as.character(topic)],
                                    levels = names(cores_categorias)))) +
  geom_col(show.legend = TRUE) +
  facet_wrap(~ paste("Tópico", topic, ":", mapeamento_categorias[as.character(topic)]), 
             scales = "free") +
  coord_flip() +
  scale_x_reordered() +
  scale_fill_manual(values = cores_categorias, name = "Categorias") +
  labs(x = NULL, y = "Probabilidade do termo no tópico",
       title = "Análise de Tópicos - Impacto da Moda no Meio Ambiente",
       subtitle = "Principais termos por tópico identificado") +
  theme_minimal() +
  theme(legend.position = "bottom")

# NUVEM DE PALAVRAS POR TÓPICO ----
par(mfrow = c(2, 3), mar = rep(0, 4))
for (i in 1:k) {
  termos_topico <- termos_por_topico %>%
    filter(topic == i) %>%
    arrange(desc(beta))
  
  wordcloud(termos_topico$term, termos_topico$beta * 1000,
            max.words = 30,
            colors = brewer.pal(8, "Dark2"),
            main = paste("Tópico", i, "-", mapeamento_categorias[as.character(i)]))
}

# ATRIBUIÇÃO DE TÓPICOS AOS DOCUMENTOS ----
documentos_com_topicos <- tidy(modelo_lda, matrix = "gamma") %>%
  group_by(document) %>%
  top_n(1, gamma) %>%
  ungroup() %>%
  mutate(topic = as.numeric(topic),
         categoria = mapeamento_categorias[as.character(topic)])
documentos_com_topicos <- documentos_com_topicos %>%
  mutate(document = as.integer(document))

dados_analisados <- dados %>%
  mutate(document = row_number()) %>%
  left_join(documentos_com_topicos, by = "document")

# SALVAR RESULTADOS ----
write_csv(dados_analisados, "dados_com_topicos.csv")

# ANÁLISE DE DISTRIBUIÇÃO ----
dados_analisados %>%
  count(categoria) %>%
  ggplot(aes(x = reorder(categoria, n), y = n, fill = categoria)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values = cores_categorias) +
  labs(title = "Distribuição das Categorias Identificadas",
       x = "Categoria", y = "Número de Respostas") +
  theme_minimal()

#Perfil Socioeconômico 
# Carregar bibliotecas necessárias
library(ggplot2)
library(dplyr)
library(forcats) # Para manipulação de fatores

# Criar um dataframe com os dados
dados <- data.frame(
  Genero = c("Mulheres", "Mulheres", "Homens", "Homens", "NB", "Mulheres", "NB", "Homem", "Homem"),
  Raca_Cor = c("Negras", "Brancas", "Negros", "Brancos", "Brancas", "Amarelas", "Negra", "Amarelo", "Indígena"),
  Quantidade = c(112, 99, 87, 70, 3, 2, 1, 1, 1)
)

# Reordenar fatores para melhor visualização
dados <- dados %>%
  mutate(
    Raca_Cor = fct_relevel(Raca_Cor, 
                           "Brancas", "Brancos", "Negras", "Negros", 
                           "Amarelas", "Amarelo", "Indígena", "Negra"),
    Genero = fct_relevel(Genero, "Mulheres", "Homens", "NB")
  )

# Criar gráfico de barras
ggplot(dados, aes(x = Genero, y = Quantidade, fill = Raca_Cor)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  geom_text(aes(label = Quantidade), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5, size = 3.5) +
  scale_fill_brewer(palette = "Set2", name = "Raça/Cor") +
  labs(
    title = "Perfil Sociodemográfico dos Respondentes",
    subtitle = "Distribuição por Gênero e Raça/Cor",
    x = "Gênero",
    y = "Número de Participantes"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "bottom"
  )
#Nuvem de palavras 

install.packages(c("tidyverse", "readxl", "tidytext", "textdata", "wordcloud", "tm"))

library(tidyverse)
library(readxl)
library(tidytext)
library(textdata)
library(wordcloud)
library(tm) # para stopwords e limpeza extra

# Leitura dos dados
dados <- read_excel("C:/Users/FUERN/Desktop/SYANG/pendrive/MONOGRAFIA 2025/Dados.xlsx") %>%
  rename(resposta = "A redes sociais influenciam sua percepção sobre seu estilo e identidade? Se sim, de que forma? Se não, porque não influenciam? (Se possível, dê exemplos).")

# Limpeza básica do texto
dados <- dados %>%
  mutate(resposta_limpa = resposta %>%
           str_to_lower() %>%
           str_replace_all("[[:punct:]]", "") %>%
           str_replace_all("[[:digit:]]", "") %>%
           str_squish())

# Tokenização
respostas_tokenizadas <- dados %>%
  unnest_tokens(palavra, resposta_limpa)

# Stopwords padrão em português + customizadas
stopwords_pt <- get_stopwords(language = "pt")

stop_custom_expandida <- tibble(word = c(
  # Verbos genéricos
  "ser", "estar", "ter", "fazer", "ver", "usar", "poder", "querer", "achar", "ficar", "parecer", "sentir", "seguir",
  "acabar", "mudar", "influenciar", "falar", "ver", "usar", "comprar", "vestir",
  
  # Conjunções, preposições e partículas
  "que", "como", "com", "sem", "para", "por", "porque", "pois", "sobre", "de", "em", "a", "e", "mas", "ou", "se",
  "no", "na", "ao", "do", "da", "nos", "nas", "num", "numa", "dum", "duma",
  
  # Pronomes, advérbios e outros
  "isso", "isto", "aquele", "aquela", "ele", "ela", "eles", "elas", "muito", "pouco", "ainda", "sempre", "também", 
  "tudo", "algo", "alguém", "nada", "ninguém", "tão", "bem", "mal", "lá", "aqui", "onde",
  
  # Palavras comuns no seu contexto sem valor analítico
  "sim", "não", "forma", "vezes", "tipo", "pessoas", "moda", "roupa", "roupas", "estilo", "estilos", "social","muitas", 
  "sociais", "redes", "influência", "influenciam", "é","é", "pq","sigo","cada","acabo","faz", "então", "sigo", "acabo", "exemplo", "exemplos", "coisa", "coisas", "gente", "algo"
))

# Juntando todas as stopwords
stop_total <- bind_rows(stopwords_pt, stop_custom_expandida)

# Filtrando as palavras úteis
respostas_filtradas <- respostas_tokenizadas %>%
  anti_join(stop_total, by = c("palavra" = "word"))

# Nuvem de palavras refinada
respostas_filtradas %>%
  count(palavra, sort = TRUE) %>%
  with(wordcloud(words = palavra, freq = n, max.words = 100, min.freq = 2,
                 colors = brewer.pal(8, "Dark2"), random.order = FALSE))

