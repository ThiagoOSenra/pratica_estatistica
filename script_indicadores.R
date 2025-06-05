## Pacotes necessários
if(!require(pacman)) install.packages("pacman")
pacman::p_load(readr, dplyr, naniar, tidyr, writexl)

## Manipulado a base de dados

# Importando a Base de dados inicial
dados = read_csv2(file = "microdados_ed_basica_2024.csv")

# Visualizando a Base de dados
head(dados)
guess_encoding(dados)
read.csv2()

# Reimportando a Base de dados corretamente
dados = read_csv2(file = "microdados_ed_basica_2024.csv",
                  locale = locale(encoding = "ISO-8859-1"),
                  na = c("88888"))

# Filtrando os dados: Escolas do Rio de Janeiro
dados_rj = dados |>
  dplyr::filter(SG_UF == "RJ")

View(dados_rj)
glimpse(dados_rj)


# Selecionando variáveis de interesse
dados_rj = dados_rj |>
  dplyr::select(CO_MUNICIPIO, NO_MUNICIPIO, CO_ENTIDADE,TP_DEPENDENCIA,
                IN_INTERNET, IN_BIBLIOTECA, IN_LABORATORIO_CIENCIAS, IN_BANHEIRO,
                IN_AGUA_POTAVEL, IN_LABORATORIO_INFORMATICA, IN_ACESSIBILIDADE_INEXISTENTE,
                IN_ALIMENTACAO, IN_QUADRA_ESPORTES, IN_ENERGIA_REDE_PUBLICA,
                IN_SALA_LEITURA, IN_BANHEIRO_PNE, IN_REFEITORIO, IN_SALA_ATENDIMENTO_ESPECIAL)

View(dados_rj)
glimpse(dados_rj)


# Tratamento das variáveis qualitativas
dados_rj <- dados_rj |>
  # Convertendo o código do município em fator
  mutate(CO_MUNICIPIO = factor(CO_MUNICIPIO)) |>
  
  # Convertendo o código da entidade em fator
  mutate(CO_ENTIDADE = factor(CO_ENTIDADE))   |>
  
  
  # Recodificando o tipo de dependência administrativa da escola
  mutate(TP_DEPENDENCIA = case_when(
    TP_DEPENDENCIA == 1 ~ "Federal",
    TP_DEPENDENCIA == 2 ~ "Estadual",
    TP_DEPENDENCIA == 3 ~ "Municipal",
    TP_DEPENDENCIA == 4 ~ "Privada"
  ))
  
  
glimpse(dados_rj)


# Visualização geral dos dados faltantes na base
vis_miss(x = dados_rj)

# Visualização da quantidade de dados faltantes por variável
gg_miss_var(x = dados_rj)

# Resumo com o número e porcentagem de valores faltantes por variável
miss_var_summary(data = dados_rj)



## Contagem de escolas por dependência administrativa em cada município

# número de escolas por município e dependência
contagem_muni = dados_rj |> 
  group_by(CO_MUNICIPIO, NO_MUNICIPIO, TP_DEPENDENCIA) |>
  summarise(
    num_escolas = n(),
    .groups = "drop"
  )
View(contagem_muni)

# Transformando para o formato wide
tabela_tp_dependencia = contagem_muni |>
  pivot_wider(
    names_from = TP_DEPENDENCIA,
    values_from = num_escolas,
    values_fill = 0
  )
tabela_tp_dependencia <- tabela_tp_dependencia |>
  mutate(num_escolas = Federal + Estadual + Municipal + Privada)

View(tabela_tp_dependencia)

## Contagem das variáveis por  município
tabela_variaveis <- dados_rj |>
  group_by(CO_MUNICIPIO) |>
  summarise(
    IN_INTERNET = sum(IN_INTERNET, na.rm = TRUE),
    IN_BIBLIOTECA = sum(IN_BIBLIOTECA, na.rm = TRUE),
    IN_LABORATORIO_CIENCIAS = sum(IN_LABORATORIO_CIENCIAS, na.rm = TRUE),
    IN_BANHEIRO = sum(IN_BANHEIRO, na.rm = TRUE),
    IN_AGUA_POTAVEL = sum(IN_AGUA_POTAVEL, na.rm = TRUE),
    IN_LABORATORIO_INFORMATICA = sum(IN_LABORATORIO_INFORMATICA, na.rm = TRUE),
    IN_ACESSIBILIDADE_INEXISTENTE = sum(IN_ACESSIBILIDADE_INEXISTENTE, na.rm = TRUE),
    IN_ALIMENTACAO = sum(IN_ALIMENTACAO, na.rm = TRUE),
    IN_QUADRA_ESPORTES = sum(IN_QUADRA_ESPORTES, na.rm = TRUE),
    IN_ENERGIA_REDE_PUBLICA = sum(IN_ENERGIA_REDE_PUBLICA, na.rm = TRUE),
    IN_SALA_LEITURA = sum(IN_SALA_LEITURA, na.rm = TRUE),
    IN_BANHEIRO_PNE = sum(IN_BANHEIRO_PNE, na.rm = TRUE),
    IN_REFEITORIO = sum(IN_REFEITORIO, na.rm = TRUE),
    IN_SALA_ATENDIMENTO_ESPECIAL = sum(IN_SALA_ATENDIMENTO_ESPECIAL, na.rm = TRUE),
    .groups = "drop"
    )


## Criando os Indicadores

#1- Proporção de escolas com acesso à internet por município
indicador_internet = dados_rj |> 
  group_by(CO_MUNICIPIO) |>
  summarise(
    taxa_internet =
      round(sum(IN_INTERNET, na.rm = TRUE) / n(), 4),
    .groups = "drop"
  )
View(indicador_internet)



#2- Proporção de Escolas com Biblioteca por município 
indicador_biblioteca = dados_rj |> 
  group_by(CO_MUNICIPIO) |>
  summarise(
    taxa_biblioteca =
      round(sum(IN_BIBLIOTECA, na.rm = TRUE) / n(), 4),
    .groups = "drop"
  )
View(indicador_biblioteca)

#3- Proporção de Escolas com Laboratório de Ciências por município 
indicador_lab_ciencias = dados_rj |> 
  group_by(CO_MUNICIPIO) |>
  summarise(
    taxa_lab_ciencias =
      round(sum(IN_LABORATORIO_CIENCIAS, na.rm = TRUE) / n(), 4),
    .groups = "drop"
  )
View(indicador_lab_ciencias)


#4- Proporção de Escolas com Água Potável por município 
indicador_agua_potável = dados_rj |> 
  group_by(CO_MUNICIPIO) |>
  summarise(
    taxa_agua_potavel =
      round(sum(IN_AGUA_POTAVEL, na.rm = TRUE) / n(), 4),
    .groups = "drop"
  )
View(indicador_agua_potável)

#5- Proporção de Escolas com Sanitário Dentro da Escola por município 
indicador_banheiro = dados_rj |> 
  group_by(CO_MUNICIPIO) |>
  summarise(
    taxa_banheiro =
      round(sum(IN_BANHEIRO, na.rm = TRUE) / n(), 4),
    .groups = "drop"
  )
View(indicador_banheiro)

#6- Proporção de Escolas com Laboratório de Informática por município 
indicador_laboratorio_informatica = dados_rj |> 
  group_by(CO_MUNICIPIO) |>
  summarise(
    taxa_lab_informatica =
      round(sum(IN_LABORATORIO_INFORMATICA, na.rm = TRUE) / n(), 4),
    .groups = "drop"
  )
View(indicador_laboratorio_informatica)

#7- Proporção de Escolas sem Acessibilidade por município 
indicador_acessibilidade_inexistente = dados_rj |> 
  group_by(CO_MUNICIPIO) |>
  summarise(
    taxa_acessibilidade_inexistente =
      round(sum(IN_ACESSIBILIDADE_INEXISTENTE, na.rm = TRUE) / n(), 4),
    .groups = "drop"
  )
View(indicador_acessibilidade_inexistente)


#8- Proporção de Escolas com Alimentação Escolar por município 
indicador_alimentacao = dados_rj |> 
  group_by(CO_MUNICIPIO) |>
  summarise(
    taxa_alimentacao =
      round(sum(IN_ALIMENTACAO, na.rm = TRUE) / n(), 4),
    .groups = "drop"
  )
View(indicador_alimentacao)

#9- Proporção de Escolas com Quadra de Esportes por município 
indicador_quadra_esportes = dados_rj |> 
  group_by(CO_MUNICIPIO) |>
  summarise(
    taxa_quadra_esportes =
      round(sum(IN_QUADRA_ESPORTES, na.rm = TRUE) / n(), 4),
    .groups = "drop"
  )
View(indicador_quadra_esportes)

#10- Proporção de Escolas com Abastecimento de Energia Elétrica -Rede Pública por município 
indicador_energia_rede_publica = dados_rj |> 
  group_by(CO_MUNICIPIO) |>
  summarise(
    taxa_energia_rede_pública =
      round(sum(IN_ENERGIA_REDE_PUBLICA, na.rm = TRUE) / n(), 4),
    .groups = "drop"
  )
View(indicador_energia_rede_publica)

#11- Proporção de Escolas com Dependência Administrativa Municipal por município 
indicador_dependencia_municipal = dados_rj |> 
  group_by(CO_MUNICIPIO) |>
  summarise(
    taxa_municipal =
      round(sum(TP_DEPENDENCIA == "Municipal", na.rm = TRUE) / n(), 4),
    .groups = "drop"
  )
View(indicador_dependencia_municipal)
  

#12- Proporção de Escolas com Sala de Leitura por município 
indicador_sala_leitura = dados_rj |> 
  group_by(CO_MUNICIPIO) |>
  summarise(
    taxa_sala_leitura =
      round(sum(IN_SALA_LEITURA, na.rm = TRUE) / n(), 4),
    .groups = "drop"
  )
View(indicador_sala_leitura)

#13- Proporção de Escolas com Dependência Administrativa Estadual por município 
indicador_dependencia_estadual = dados_rj |> 
  group_by(CO_MUNICIPIO) |>
  summarise(
    taxa_estadual =
      round(sum(TP_DEPENDENCIA = 2, na.rm = TRUE) / n(), 4),
    .groups = "drop"
  )
View(indicador_dependencia_estadual)

#14- Proporção de Escolas com Banheiro Acessível por município 
indicador_banheiro_pne = dados_rj |> 
  group_by(CO_MUNICIPIO) |>
  summarise(
    taxa_banheiro_pne =
      round(sum(IN_BANHEIRO_PNE , na.rm = TRUE) / n(), 4),
    .groups = "drop"
  )
View(indicador_banheiro_pne)

#15- Proporção de Escolas com Refeitório por município 
indicador_refeitorio = dados_rj |> 
  group_by(CO_MUNICIPIO) |>
  summarise(
    taxa_refeitorio =
      round(sum(IN_REFEITORIO , na.rm = TRUE) / n(), 4),
    .groups = "drop"
  )
View(indicador_refeitorio)

#16- Proporção de Escolas com Dependência Privada por município 
indicador_dependencia_privada = dados_rj |> 
  group_by(CO_MUNICIPIO) |>
  summarise(
    taxa_privada =
      round(sum(TP_DEPENDENCIA = 4 , na.rm = TRUE) / n(), 4),
    .groups = "drop"
  )
View(indicador_dependecia_privada)

#17- Proporção de Escolas com Dependência Federal por município 
indicador_dependencia_federal = dados_rj |> 
  group_by(CO_MUNICIPIO) |>
  summarise(
    taxa_federal =
      round(sum(TP_DEPENDENCIA = 1 , na.rm = TRUE) / n(), 4),
    .groups = "drop"
  )
View(indicador_dependecia_federal)

#18- Proporção de Escolas com Sala de Atendimento Especializado (AEE) por município 
indicador_AEE = dados_rj |> 
  group_by(CO_MUNICIPIO) |>
  summarise(
    taxa_AEE =
      round(sum(IN_SALA_ATENDIMENTO_ESPECIAL , na.rm = TRUE) / n(), 4),
    .groups = "drop"
  )
View(indicador_AEE)



## Realizando as junções entre as variáveis e todos os indicadores

base_indicadores = tabela_tp_dependencia |>
  inner_join(tabela_variaveis, by = "CO_MUNICIPIO") |>
  inner_join(indicador_internet, by = "CO_MUNICIPIO") |>
  inner_join(indicador_biblioteca, by = "CO_MUNICIPIO") |>
  inner_join(indicador_lab_ciencias, by = "CO_MUNICIPIO") |>
  inner_join(indicador_agua_potável, by = "CO_MUNICIPIO") |>
  inner_join(indicador_banheiro, by = "CO_MUNICIPIO") |>
  inner_join(indicador_laboratorio_informatica, by = "CO_MUNICIPIO") |>
  inner_join(indicador_acessibilidade_inexistente, by = "CO_MUNICIPIO") |>
  inner_join(indicador_alimentacao, by = "CO_MUNICIPIO") |>
  inner_join(indicador_quadra_esportes, by = "CO_MUNICIPIO") |>
  inner_join(indicador_energia_rede_publica, by = "CO_MUNICIPIO") |>
  inner_join(indicador_dependencia_municipal, by = "CO_MUNICIPIO") |>
  inner_join(indicador_sala_leitura, by = "CO_MUNICIPIO") |>
  inner_join(indicador_dependencia_estadual, by = "CO_MUNICIPIO") |>
  inner_join(indicador_banheiro_pne, by = "CO_MUNICIPIO") |>
  inner_join(indicador_refeitorio, by = "CO_MUNICIPIO") |>
  inner_join(indicador_dependencia_privada, by = "CO_MUNICIPIO") |>
  inner_join(indicador_dependencia_federal, by = "CO_MUNICIPIO") |>
  inner_join(indicador_AEE, by = "CO_MUNICIPIO")



# Exportar a base_indicadores para um arquivo Excel
write_xlsx(base_indicadores, "base_indicadores.xlsx")
