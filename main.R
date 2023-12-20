pacman::p_load(readxl, tidyverse, ggplot2)

# Importação de dados e renomeação de variáveis
dados <- read_excel("./Dados/Dados_trabalho_20232.xlsx")

names(dados) <- c("id", "tempo_internacao", "idade", "prob_infeccao", "prop_culturas_rotina", "prop_raio_x_torax_rotina", "quant_leitos", "filiacao_escola_medicina", "regiao", "media_pacientes", "quant_enfermeiros", "servicos_disponiveis")

# Tratamento de variáveis categóricas
dados_trat <- dados %>%
    mutate(
        filiacao_escola_medicina = filiacao_escola_medicina == 1,
        regiao_ne = regiao == 1,
        regiao_nc = regiao == 2,
        regiao_s = regiao == 3,
        regiao = NULL
    )

# Separando o banco em dados de treino e de validação
set.seed(2023)
indexes <- 1:nrow(dados_trat)
index_treino <- sort(sample(indexes, 60))
index_valid <- indexes[!(indexes %in% index_treino)]
dados_treino <- dados_trat[index_treino, ]
dados_valid <- dados_trat[index_valid, ]

# Separação em variáveis respostas e explicativas
y_1 <- dados_treino[c("tempo_internacao")]

y_2 <- dados_treino[c("quant_enfermeiros")]

X <- dados_treino[!names(dados_treino) %in% c("tempo_internacao", "quant_enfermeiros")]

# Correlação das variáveis
(corr <- cor(dados_treino[-1]))

# Boxplots por variáveis categóricas
dados_treino %>% ggplot(aes(group = filiacao_escola_medicina, y = tempo_internacao)) +
    geom_boxplot()
dados_treino %>% ggplot(aes(group = regiao, y = tempo_internacao)) +
    geom_boxplot()

dados_treino %>% ggplot(aes(group = filiacao_escola_medicina, y = quant_enfermeiros)) +
    geom_boxplot()
dados_treino %>% ggplot(aes(group = regiao, y = quant_enfermeiros)) +
    geom_boxplot()
