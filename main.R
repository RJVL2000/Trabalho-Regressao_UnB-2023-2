pacman::p_load(readxl, tidyverse, ggplot2, MASS, DescTools, lmtest)

# Importação de dados e renomeação de variáveis
dados <- read_excel("./Dados/Dados_trabalho_20232.xlsx")[-1]

names(dados) <- c(
    "tempo_internacao", "idade", "prob_infeccao", "prop_culturas_rotina",
    "prop_raio_x_torax_rotina", "quant_leitos", "filiacao_escola_medicina", "regiao",
    "media_pacientes", "quant_enfermeiros", "servicos_disponiveis"
)

# Tratamento de variáveis categóricas
dados_trat <- dados %>%
    mutate(
        filiacao_escola_medicina = as.factor(filiacao_escola_medicina),
        regiao = as.factor(regiao)
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

# Boxplots por variáveis categóricas
dados_treino %>% ggplot(aes(group = filiacao_escola_medicina, y = tempo_internacao)) +
    geom_boxplot()
dados_treino %>% ggplot(aes(group = regiao, y = tempo_internacao)) +
    geom_boxplot()

dados_treino %>% ggplot(aes(group = filiacao_escola_medicina, y = quant_enfermeiros)) +
    geom_boxplot()
dados_treino %>% ggplot(aes(group = regiao, y = quant_enfermeiros)) +
    geom_boxplot()

# Regressão para explicar a quantidade de enfermeiros
modelo0 <- lm(log(quant_enfermeiros) ~ servicos_disponiveis**2 + regiao, data = dados_treino %>% mutate(
    tempo_internacao = NULL
))

modelo0 <- step(modelo0, direction = "both")
summary(modelo0)

# Teste dos pressupostos para o primeiro modelo
residuos0 <- modelo0$residuals
shapiro.test(residuos0)
plot(residuos0)
bptest(modelo0)

# Medidas de ajuste do primeiro modelo
rse0 <- sigma(modelo0)
rmse0 <- sqrt(mean(modelo0$residuals^2))
rsquared0 <- summary(modelo0)$r.squared
adjusted_rsquared0 <- summary(modelo0)$adj.r.squared
f_statistic0 <- summary(modelo0)$fstatistic[1]
aic0 <- AIC(modelo0)
bic0 <- BIC(modelo0)


# Regressão para explicar o tempo de internação
modelo1 <- lm(log(tempo_internacao) ~ ., data = dados_treino)
VIF(modelo1)

## Remoção de variáveis correlacionadas
modelo1 <- lm(log(tempo_internacao) ~ ., data = dados_treino %>% mutate(
    quant_leitos = NULL,
    media_pacientes = NULL,
    servicos_disponiveis = NULL,
    prob_infeccao = NULL
))
VIF(modelo1)

modelo1 <- step(modelo1, direction = "both")
summary(modelo1)

# Teste dos pressupostos para o segundo modelo
residuos1 <- modelo1$residuals
shapiro.test(residuos1)
plot(residuos1)
bptest(modelo1)

# Medidas de ajuste do segundo modelo
rse1 <- sigma(modelo1)
rmse1 <- sqrt(mean(modelo1$residuals^2))
rsquared1 <- summary(modelo1)$r.squared
adjusted_rsquared1 <- summary(modelo1)$adj.r.squared
f_statistic1 <- summary(modelo1)$fstatistic[1]
aic1 <- AIC(modelo1)
bic1 <- BIC(modelo1)


# Gerando previsões para os dados de validação
prev_tempo_internacao <- exp(predict(modelo1, newdata = dados_valid))
res_valid <- dados_valid$tempo_internacao - prev_tempo_internacao
rmse_valid <- sqrt(mean(res_valid))
shapiro.test(res_valid)
plot(res_valid)
