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

# Separação em variáveis respostas e explicativas
y_1 <- dados_trat[c("tempo_internacao")]

y_2 <- dados_trat[c("quant_enfermeiros")]

X <- dados_trat[!names(dados_trat) %in% c("tempo_internacao", "quant_enfermeiros")]

# Correlação das variáveis
(corr <- cor(dados[-1]))

# Boxplots por variáveis categóricas
dados %>% ggplot(aes(group = filiacao_escola_medicina, y = tempo_internacao)) +
    geom_boxplot()
dados %>% ggplot(aes(group = regiao, y = tempo_internacao)) +
    geom_boxplot()

dados %>% ggplot(aes(group = filiacao_escola_medicina, y = quant_enfermeiros)) +
    geom_boxplot()
dados %>% ggplot(aes(group = regiao, y = quant_enfermeiros)) +
    geom_boxplot()
