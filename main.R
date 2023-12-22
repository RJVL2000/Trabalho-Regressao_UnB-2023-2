if (!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse,readxl,GGally,knitr,MASS,lmtest,DescTools)

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
        regiao = as.factor(regiao),
        servicos_disponiveis2 = servicos_disponiveis^2
        )

# Separando o banco em dados de treino e de validação
set.seed(2023)
indexes <- 1:nrow(dados_trat)
index_treino <- sort(sample(indexes, 60))
index_valid <- indexes[!(indexes %in% index_treino)]
dados_treino <- dados_trat[index_treino, ]
dados_valid <- dados_trat[index_valid, ]

# Boxplots por variáveis categóricas

dados_treino %>% ggplot(aes(x = filiacao_escola_medicina, y = tempo_internacao)) +
    geom_boxplot(fill='blue')+
    xlab('Filiação Escola de Medicina')+
    ylab('Tempo de Internação')+
    theme_classic()

dados_treino %>% ggplot(aes(x = regiao, y = tempo_internacao)) +
    geom_boxplot(fill='blue')+
    xlab('Região')+
    ylab('Tempo de Internação')+
    theme_classic()

dados_treino %>% ggplot(aes(x = filiacao_escola_medicina, y = quant_enfermeiros)) +
    geom_boxplot(fill='blue')+
    xlab('Filiação Escola de Medicina')+
    ylab('Quantidade de Enfermeiros')+
    theme_classic()

dados_treino %>% ggplot(aes(x = regiao, y = quant_enfermeiros)) +
    geom_boxplot(fill='blue')+
    xlab('Região')+
    ylab('Quantidade de Enfermeiros')+
    theme_classic()


# --------------------------------------

## Hipotose 1 - Y= quant_enfermeiros

# Modelo 1 - todas, sem trasnformação
modelo1 <- lm(quant_enfermeiros ~ servicos_disponiveis2 + regiao, dados_treino)
summary(modelo1)

residuos1 <- modelo1$residuals
plot(residuos1)
shapiro.test(residuos1) #nao atendido
bptest(modelo1) #nao atendido

# Modelo 2 - todas, log
modelo2 <- lm(log(quant_enfermeiros) ~ servicos_disponiveis2 + regiao, dados_treino)
summary(modelo2)

residuos2 <- modelo2$residuals
plot(residuos2)
shapiro.test(residuos2) #atendido
bptest(modelo2) #atendido

# Modelo 3 - servicos:regiao, log
modelo3 <- lm(log(quant_enfermeiros) ~ servicos_disponiveis2:regiao, dados_treino)
summary(modelo3)

residuos3 <- modelo3$residuals
plot(residuos3)
shapiro.test(residuos3) #atendido
bptest(modelo3) #atendido

# Medidas de ajuste do primeiro modelo
rse3 <- sigma(modelo3)
rmse3 <- sqrt(mean(modelo3$residuals^2))
rsquared3 <- summary(modelo3)$r.squared
adjusted_rsquared3 <- summary(modelo3)$adj.r.squared
f_statistic3 <- summary(modelo3)$fstatistic[1]
aic3 <- AIC(modelo3)
bic3 <- BIC(modelo3)

medidas<-data.frame(rse3,rmse3,rsquared3,adjusted_rsquared3,f_statistic3,aic3,bic3)
names(medidas) <- c('RSE','RMSE','R2','R2ad','F','AIC','BIC')
kable(medidas)

# Amostra de Validação

v_modelo3 <- lm(log(quant_enfermeiros) ~ servicos_disponiveis2:regiao, dados_valid)
summary(v_modelo3)
# --------------------------------------

# Hipotese 2 : Y = tempo_internacao
modelo12 <- lm(log(tempo_internacao) ~ ., data = dados_treino)
VIF(modelo12)

residuos12 <- modelo12$residuals
shapiro.test(residuos12)
bptest(modelo12)

pdf(file="img/res_mod12.pdf")
plot(residuos12)
dev.off()

## Remoção de variáveis correlacionadas
modelo22 <- lm(log(tempo_internacao) ~ ., data = dados_treino %>% mutate(
    quant_leitos = NULL,
    media_pacientes = NULL,
    servicos_disponiveis = NULL,
    prob_infeccao = NULL
))
VIF(modelo22)

residuos22 <- modelo22$residuals
shapiro.test(residuos22)
bptest(modelo22)

pdf(file="img/res_mod22.pdf")
plot(residuos22)
dev.off()

modelo32 <- step(modelo22, direction = "both")
#modelo32 <- lm(log(tempo_internacao) ~ idade + prop_raio_x_torax_rotina + regiao + servicos_disponiveis2,dados_treino)
summary(modelo32)

# Teste dos pressupostos para o segundo modelo
residuos32 <- modelo32$residuals
shapiro.test(residuos32)
bptest(modelo32)

pdf(file="img/res_mod32.pdf")
plot(residuos32)
dev.off()

# Medidas de ajuste do segundo modelo
rse32 <- sigma(modelo32)
rmse32 <- sqrt(mean(modelo32$residuals^2))
rsquared32 <- summary(modelo32)$r.squared
adjusted_rsquared32 <- summary(modelo32)$adj.r.squared
f_statistic32 <- summary(modelo32)$fstatistic[1]
aic32 <- AIC(modelo32)
bic32 <- BIC(modelo32)

medidas32<-data.frame(rse32,rmse32,rsquared32,adjusted_rsquared32,f_statistic32,aic32,bic32)
names(medidas32) <- c('RSE','RMSE','R2','R2ad','F','AIC','BIC')
#xtable(medidas32)


# Gerando previsões para os dados de validação
prev_tempo_internacao <- exp(predict(modelo32, newdata = dados_valid))
res_valid <- dados_valid$tempo_internacao - prev_tempo_internacao
rmse_valid <- sqrt(mean(res_valid))
shapiro.test(res_valid)
plot(res_valid)


# Amostra de Validação
v_modelo32 <- lm(log(tempo_internacao) ~ idade + prop_raio_x_torax_rotina + regiao + servicos_disponiveis2,dados_valid)
summary(v_modelo32)

# --------------------------------------
# Gráficos


#dados treino - amostra 
# fazer a exploratoria dos dados
# ------------------- TEMPO DE INTERNACAO ---------------- #
(box_1 <- ggplot(dados, aes(x=factor(""), y=tempo_internacao)) +
    geom_boxplot(fill=c("#6e00ff"), width = 0.5) +
    guides(fill=FALSE) +
    stat_summary(fun.y="mean", geom="point", shape=23, size=3, fill="white")+
    labs(x="", y="Duração da Internação")+
    theme_bw() +
    theme(axis.title.y=element_text(colour="black", size=12),
          axis.title.x = element_text(colour="black", size=12),
          axis.text = element_text(colour = "black", size=9.5),
          panel.border = element_blank(),
          axis.line.y = element_line(colour = "black")))
#ggsave(("img/box_internacao.pdf",width = 238, height = 163, units = "mm")
 
(hist1 <- ggplot(dados, aes(x=tempo_internacao)) + geom_histogram(colour = "black", fill = "#6e00ff") +
    labs(x="Duração da Internação", y="Frequência absoluta") +
    theme_bw() +
    theme(axis.title.y=element_text(colour="black", size=12),
          axis.title.x = element_text(colour="black", size=12),
          axis.text = element_text(colour = "black", size=9.5),
          panel.border = element_blank(),
          axis.line = element_line(colour = "black")))
 
#ggsave(("img/hist_internacao.pdf",width = 238, height = 163, units = "mm")
 
install.packages("e1071")
require(e1071)
 
# ---------- IDADE --------------- #
(box_2 <- ggplot(dados, aes(x=factor(""), y=idade)) +
    geom_boxplot(fill=c("#6e00ff"), width = 0.5) +
    guides(fill=FALSE) +
    stat_summary(fun.y="mean", geom="point", shape=23, size=3, fill="white")+
    labs(x="", y="Idade")+
    theme_bw() +
    theme(axis.title.y=element_text(colour="black", size=12),
          axis.title.x = element_text(colour="black", size=12),
          axis.text = element_text(colour = "black", size=9.5),
          panel.border = element_blank(),
          axis.line.y = element_line(colour = "black")))
#ggsave(("img/box_idade.pdf",width = 238, height = 163, units = "mm")
 
(hist2 <- ggplot(dados, aes(x=idade)) + geom_histogram(colour = "black", fill = "#6e00ff") +
    labs(x="Idade", y="Frequência absoluta") +
    theme_bw() +
    theme(axis.title.y=element_text(colour="black", size=12),
          axis.title.x = element_text(colour="black", size=12),
          axis.text = element_text(colour = "black", size=9.5),
          panel.border = element_blank(),
          axis.line = element_line(colour = "black")))
 
#ggsave(("img/hist_idade.pdf",width = 238, height = 163, units = "mm")
 
# ----------- PROB DE INFECCAO --------------- #
(box_3 <- ggplot(dados, aes(x=factor(""), y=prob_infeccao)) +
    geom_boxplot(fill=c("#6e00ff"), width = 0.5) +
    guides(fill=FALSE) +
    stat_summary(fun.y="mean", geom="point", shape=23, size=3, fill="white")+
    labs(x="", y="Risco de Infecção")+
    theme_bw() +
    theme(axis.title.y=element_text(colour="black", size=12),
          axis.title.x = element_text(colour="black", size=12),
          axis.text = element_text(colour = "black", size=9.5),
          panel.border = element_blank(),
          axis.line.y = element_line(colour = "black")))
#ggsave(("img/box_infeccao.pdf",width = 238, height = 163, units = "mm")
 
(hist3 <- ggplot(dados, aes(x=prob_infeccao)) + geom_histogram(colour = "black", fill = "#6e00ff") +
    labs(x="Risco de Infecção", y="Frequência absoluta") +
    theme_bw() +
    theme(axis.title.y=element_text(colour="black", size=12),
          axis.title.x = element_text(colour="black", size=12),
          axis.text = element_text(colour = "black", size=9.5),
          panel.border = element_blank(),
          axis.line = element_line(colour = "black")))
 
#ggsave(("img/hist_infeccao.pdf",width = 238, height = 163, units = "mm")
 
 
# --------------------- PROP CULTURAS DE ROTINA --------------- #
(box_4 <- ggplot(dados, aes(x=factor(""), y=prop_culturas_rotina)) +
    geom_boxplot(fill=c("#6e00ff"), width = 0.5) +
    guides(fill=FALSE) +
    stat_summary(fun.y="mean", geom="point", shape=23, size=3, fill="white")+
    labs(x="", y="Proporção de Culturas de Rotina")+
    theme_bw() +
    theme(axis.title.y=element_text(colour="black", size=12),
          axis.title.x = element_text(colour="black", size=12),
          axis.text = element_text(colour = "black", size=9.5),
          panel.border = element_blank(),
          axis.line.y = element_line(colour = "black")))
#ggsave(("img/box_prop_culturas_rotina.pdf",width = 238, height = 163, units = "mm")
 
(hist1 <- ggplot(dados, aes(x=prop_culturas_rotina)) + geom_histogram(colour = "black", fill = "#6e00ff") +
    labs(x="Proporção de Culturas de Rotina", y="Frequência absoluta") +
    theme_bw() +
    theme(axis.title.y=element_text(colour="black", size=12),
          axis.title.x = element_text(colour="black", size=12),
          axis.text = element_text(colour = "black", size=9.5),
          panel.border = element_blank(),
          axis.line = element_line(colour = "black")))
 
#ggsave(("img/hist_prop_culturas_rotina.pdf",width = 238, height = 163, units = "mm")
# ---------------------- PROP_RAIO X _ TORAX_ROTINA -------------- #
(box_5 <- ggplot(dados, aes(x=factor(""), y=prop_raio_x_torax_rotina)) +
    geom_boxplot(fill=c("#6e00ff"), width = 0.5) +
    guides(fill=FALSE) +
    stat_summary(fun.y="mean", geom="point", shape=23, size=3, fill="white")+
    labs(x="", y="Proporção de Raio X do Tórax de Rotina")+
    theme_bw() +
    theme(axis.title.y=element_text(colour="black", size=12),
          axis.title.x = element_text(colour="black", size=12),
          axis.text = element_text(colour = "black", size=9.5),
          panel.border = element_blank(),
          axis.line.y = element_line(colour = "black")))
#ggsave(("img/box_prop_raiox_torax_rotina.pdf",width = 238, height = 163, units = "mm")
 
(hist5 <- ggplot(dados, aes(x=prop_raio_x_torax_rotina)) + geom_histogram(colour = "black", fill = "#6e00ff") +
    labs(x="Proporção de Raio X do Tórax de Rotina", y="Frequência absoluta") +
    theme_bw() +
    theme(axis.title.y=element_text(colour="black", size=12),
          axis.title.x = element_text(colour="black", size=12),
          axis.text = element_text(colour = "black", size=9.5),
          panel.border = element_blank(),
          axis.line = element_line(colour = "black")))
 
#ggsave(("img/hist_prop_raio_x_torax_rotina.pdf",width = 238, height = 163, units = "mm")
 
 
# ---------------------- QUANT DE LEITOS -------------- #
(box_5 <- ggplot(dados, aes(x=factor(""), y=quant_leitos)) +
    geom_boxplot(fill=c("#6e00ff"), width = 0.5) +
    guides(fill=FALSE) +
    stat_summary(fun.y="mean", geom="point", shape=23, size=3, fill="white")+
    labs(x="", y="Quantidade de Leitos ")+
    theme_bw() +
    theme(axis.title.y=element_text(colour="black", size=12),
          axis.title.x = element_text(colour="black", size=12),
          axis.text = element_text(colour = "black", size=9.5),
          panel.border = element_blank(),
          axis.line.y = element_line(colour = "black")))
#ggsave(("img/box_quant_leitos.pdf",width = 238, height = 163, units = "mm")
 
(hist5 <- ggplot(dados, aes(x=quant_leitos)) + geom_histogram(colour = "black", fill = "#6e00ff") +
    labs(x="Quantidade de Leitos", y="Frequência absoluta") +
    theme_bw() +
    theme(axis.title.y=element_text(colour="black", size=12),
          axis.title.x = element_text(colour="black", size=12),
          axis.text = element_text(colour = "black", size=9.5),
          panel.border = element_blank(),
          axis.line = element_line(colour = "black")))
 
#ggsave(("img/hist_quant_leitos.pdf",width = 238, height = 163, units = "mm")
 
 
# --------------------- FILIACAO ESCOLA DE MEDICINA  --------------- #
# TESTE 
(box_8 <- ggplot(dados, aes(x=factor(""), y=regiao)) +
    geom_boxplot(fill=c("#6e00ff"), width = 0.5) +
    guides(fill=FALSE) +
    stat_summary(fun.y="mean", geom="point", shape=23, size=3, fill="white")+
    labs(x="", y="Região")+
    theme_bw() +
    theme(axis.title.y=element_text(colour="black", size=12),
          axis.title.x = element_text(colour="black", size=12),
          axis.text = element_text(colour = "black", size=9.5),
          panel.border = element_blank(),
          axis.line.y = element_line(colour = "black")))
#ggsave(("img/box_regiao.pdf",width = 238, height = 163, units = "mm")
 
# ---------------------- QUANT DE LEITOS -------------- #
(box_8 <- ggplot(dados, aes(x=factor(""), y=regiao)) +
    geom_boxplot(fill=c("#6e00ff"), width = 0.5) +
    guides(fill=FALSE) +
    stat_summary(fun.y="mean", geom="point", shape=23, size=3, fill="white")+
    labs(x="", y="Região")+
    theme_bw() +
    theme(axis.title.y=element_text(colour="black", size=12),
          axis.title.x = element_text(colour="black", size=12),
          axis.text = element_text(colour = "black", size=9.5),
          panel.border = element_blank(),
          axis.line.y = element_line(colour = "black")))
#ggsave(("img/box_regiao.pdf",width = 238, height = 163, units = "mm")
 
# ------------ REGIAO ------------ #
(box_8 <- ggplot(dados, aes(x=factor(""), y=regiao)) +
    geom_boxplot(fill=c("#6e00ff"), width = 0.5) +
    guides(fill=FALSE) +
    stat_summary(fun.y="mean", geom="point", shape=23, size=3, fill="white")+
    labs(x="", y="Região")+
    theme_bw() +
    theme(axis.title.y=element_text(colour="black", size=12),
          axis.title.x = element_text(colour="black", size=12),
          axis.text = element_text(colour = "black", size=9.5),
          panel.border = element_blank(),
          axis.line.y = element_line(colour = "black")))
#ggsave(("img/box_regiao.pdf",width = 238, height = 163, units = "mm")
(hist8 <- ggplot(dados, aes(x=regiao)) + geom_histogram(colour = "black", fill = "#6e00ff") +
    labs(x="Região", y="Frequência absoluta") +
    theme_bw() +
    theme(axis.title.y=element_text(colour="black", size=12),
          axis.title.x = element_text(colour="black", size=12),
          axis.text = element_text(colour = "black", size=9.5),
          panel.border = element_blank(),
          axis.line = element_line(colour = "black")))
 
#ggsave(("img/hist_regiao.pdf",width = 238, height = 163, units = "mm")
# ------------------- MEDIA PACIENTES ---------------- #
(box_9 <- ggplot(dados, aes(x=factor(""), y=media_pacientes)) +
    geom_boxplot(fill=c("#6e00ff"), width = 0.5) +
    guides(fill=FALSE) +
    stat_summary(fun.y="mean", geom="point", shape=23, size=3, fill="white")+
    labs(x="", y="Média de Pacientes")+
    theme_bw() +
    theme(axis.title.y=element_text(colour="black", size=12),
          axis.title.x = element_text(colour="black", size=12),
          axis.text = element_text(colour = "black", size=9.5),
          panel.border = element_blank(),
          axis.line.y = element_line(colour = "black")))
#ggsave(("img/box_media_pacientes.pdf",width = 238, height = 163, units = "mm")
 
(hist9 <- ggplot(dados, aes(x=media_pacientes)) + geom_histogram(colour = "black", fill = "#6e00ff") +
    labs(x="Média de Pacientes", y="Frequência absoluta") +
    theme_bw() +
    theme(axis.title.y=element_text(colour="black", size=12),
          axis.title.x = element_text(colour="black", size=12),
          axis.text = element_text(colour = "black", size=9.5),
          panel.border = element_blank(),
          axis.line = element_line(colour = "black")))
 
#ggsave(("img/hist_media_pacientes.pdf",width = 238, height = 163, units = "mm")
 
# ------------------- MEDIA PACIENTES ---------------- #
(box_10 <- ggplot(dados, aes(x=factor(""), y=quant_enfermeiros)) +
    geom_boxplot(fill=c("#6e00ff"), width = 0.5) +
    guides(fill=FALSE) +
    stat_summary(fun.y="mean", geom="point", shape=23, size=3, fill="white")+
    labs(x="", y="Quantidade de Enfermeiros")+
    theme_bw() +
    theme(axis.title.y=element_text(colour="black", size=12),
          axis.title.x = element_text(colour="black", size=12),
          axis.text = element_text(colour = "black", size=9.5),
          panel.border = element_blank(),
          axis.line.y = element_line(colour = "black")))
#ggsave(("img/box_quant_enfermeiros.pdf",width = 238, height = 163, units = "mm")
 
(hist10 <- ggplot(dados, aes(x=quant_enfermeiros)) + geom_histogram(colour = "black", fill = "#6e00ff") +
    labs(x="Quantidade de Enfermeiros", y="Frequência absoluta") +
    theme_bw() +
    theme(axis.title.y=element_text(colour="black", size=12),
          axis.title.x = element_text(colour="black", size=12),
          axis.text = element_text(colour = "black", size=9.5),
          panel.border = element_blank(),
          axis.line = element_line(colour = "black")))
 
#ggsave(("img/hist_quant_enfermeiros.pdf",width = 238, height = 163, units = "mm")
 
# ------------------- SERVICOS DISPONIVEIS ---------------- #
(box_11 <- ggplot(dados, aes(x=factor(""), y=servicos_disponiveis)) +
    geom_boxplot(fill=c("#6e00ff"), width = 0.5) +
    guides(fill=FALSE) +
    stat_summary(fun.y="mean", geom="point", shape=23, size=3, fill="white")+
    labs(x="", y="Serviços Disponíveis")+
    theme_bw() +
    theme(axis.title.y=element_text(colour="black", size=12),
          axis.title.x = element_text(colour="black", size=12),
          axis.text = element_text(colour = "black", size=9.5),
          panel.border = element_blank(),
          axis.line.y = element_line(colour = "black")))
#ggsave(("img/box_servicos_disponiveis.pdf",width = 238, height = 163, units = "mm")
 
(hist11 <- ggplot(dados, aes(x=servicos_disponiveis)) + geom_histogram(colour = "black", fill = "#6e00ff") +
    labs(x="Serviços Disponíveis", y="Frequência absoluta") +
    theme_bw() +
    theme(axis.title.y=element_text(colour="black", size=12),
          axis.title.x = element_text(colour="black", size=12),
          axis.text = element_text(colour = "black", size=9.5),
          panel.border = element_blank(),
          axis.line = element_line(colour = "black")))
 
#ggsave(("img/hist_servicos_disponiveis.pdf",width = 238, height = 163, units = "mm")
