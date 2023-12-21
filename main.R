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


modelo0 <- lm(log(quant_enfermeiros) ~ ., data = dados_treino %>% mutate(tempo_internacao = NULL))
VIF(modelo0)

modelo0 <- lm(log(quant_enfermeiros) ~ ., data = dados_treino %>% mutate(tempo_internacao = NULL, quant_leitos = NULL))
VIF(modelo0)

modelo0 <- step(modelo0, direction = "both")
summary(modelo0)
shapiro.test(modelo0$residuals)
plot(modelo0$residuals)
bptest(modelo0)


modelo1 <- lm(log(tempo_internacao) ~ ., data = dados_treino)
VIF(modelo1)

modelo1 <- lm(log(tempo_internacao) ~ ., data = dados_treino %>% mutate(quant_leitos = NULL))
VIF(modelo1)

modelo1 <- step(modelo1, direction = "both")
summary(modelo1)
shapiro.test(modelo1$residuals)
plot(modelo1$residuals)
bptest(modelo1)

X_valid <- dados_valid %>% mutate(tempo_internacao = NULL)
Y_valid <- dados_valid$tempo_internacao
previsoes <- exp(predict(modelo1, newdata = X_valid))
res_valid <- Y_valid - previsoes
shapiro.test(res_valid)
plot(res_valid)
