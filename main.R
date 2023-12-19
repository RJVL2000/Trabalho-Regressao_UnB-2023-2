pacman::p_load(readxl, tidyverse)

dados <- read_excel("./Dados/Dados_trabalho_20232.xlsx")
# dados <- read_excel("./Trabalho/Trabalho-Regressao_UnB-2023-2//Dados/Dados_trabalho_20232.xlsx")

names(dados) <- c("id", "tempo_internacao", "idade", "prob_infeccao", "prop_culturas_rotina", "prop_raio_x_torax_rotina", "quant_leitos", "filiacao_escola_medicina", "regiao", "media_pacientes", "quant_enfermeiros", "servicos_disponiveis")

dados <- dados %>%
    mutate(
        filiacao_escola_medicina = filiacao_escola_medicina == 1,
        regiao_ne = regiao == 1,
        regiao_nc = regiao == 2,
        regiao_s = regiao == 3
    )

y_1 <- dados[c("tempo_internacao")]

y_2 <- dados[c("quant_enfermeiros")]

X <- dados[!names(dados) %in% c("tempo_internacao", "quant_enfermeiros")]
