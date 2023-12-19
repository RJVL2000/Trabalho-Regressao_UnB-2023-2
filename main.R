pacman::p_load(readxl)

dados <- read_excel("./Dados/Dados_trabalho_20232.xlsx")
# dados <- read_excel("./Trabalho/Trabalho-Regressao_UnB-2023-2//Dados/Dados_trabalho_20232.xlsx")

names(dados) <- c("id", "tempo_internacao", "idade", "prob_infeccao", "prop_culturas_rotina", "prop_raio_x_torax_rotina", "quant_leitos", "filiacao_escola_medicina", "regiao", "media_pacientes", "quant_enfermeiros", "servicos_disponiveis")
