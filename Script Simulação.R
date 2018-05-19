setwd("C:/Users/bkuasney/Desktop/ML/FREELA/GIT")

### ETAPAS PARA QUE O ALGORITMO FUNCIONE ####


####################################################################
# BUSCAR O VALOR DO METRO QUADRADO MÉDIO PARA CADA CIDADE E BAIRRO #
####################################################################
simulacao = read.xlsx("Simulação.xlsx", sheetIndex = 1)
# bairro
distinct(simulacao, bairro)
vetor_bairro = as.vector(simulacao$bairro)
Encoding(vetor_bairro) <- "UTF-8"
simulacao$bairro = stringi::stri_trans_general(vetor_bairro, "latin-ascii")
distinct(simulacao, bairro)

simulacao = sqldf('select cidade, case when bairro like "Cidade Industrial" then "CIC" else bairro end as bairro,
quartos, suites, banheiros, vagas, area_util, area_total from simulacao')
simulacao = sqldf('select cidade||bairro as cidade_bairro, quartos, suites, banheiros, vagas, area_util, area_total from simulacao')
names(out) = c("X", "cidade_bairro", "lpreco_medio_metro")
input = merge(simulacao, out, by = "cidade_bairro")

sqldf('select * from simulacao left join (select * from out) as lala using(cidade_bairro)')



#######################
# TRANFORMAR EM LOG10 #
#######################
predicao = transform(input,
                 larea_util = log10(area_util),
                 larea_total = log10(area_total))

# ordenando
predicao = predicao[, c(2,3,4,5,9,10,11)]
predicao = predicao[, c("quartos", "suites", "banheiros", "vagas", "larea_util", "larea_total", "lpreco_medio_metro")]

#####################
# APLICARO O MODELO #
#####################

# Randon Forest
rb = readRDS("./rb.rds")
sim_pred_rf = predict(rb, newdata = predicao)
10^predict(rb, newdata = data.frame(quartos = 1,
                                 suites = 2,
                                 banheiros = 1,
                                 vagas = 1,
                                 larea_util = log10(80),
                                 larea_total = log10(100),
                                 lpreco_medio_metro = 3.50))

# Regressao Múltipla
reg = readRDS("./reg.rds")
sim_pred_reg1 = predict(reg, newdata = predicao)
10^predict(reg, newdata = data.frame(quartos = 1,
                                    suites = 2,
                                    banheiros = 1,
                                    vagas = 1,
                                    larea_util = log10(80),
                                    larea_total = log10(100),
                                    lpreco_medio_metro = 3.50))


# Regressão "subfit"
ols = readRDS("./ols.rds")
sim_pred_reg2 = predict(ols, newdata = predicao)
10^predict(ols, newdata = data.frame(quartos = 1,
                                     suites = 2,
                                     banheiros = 1,
                                     vagas = 1,
                                     larea_util = log10(80),
                                     larea_total = log10(100),
                                     lpreco_medio_metro = 3.50))




# Regressão RIDGE
ridge = readRDS("./ridge.rds")
sim_pred_reg3 = predict(ridge, newx=as.matrix(predicao), type="response", s=0.1)
10^predict(ridge, newdata = data.frame(quartos = 1,
                                     suites = 2,
                                     banheiros = 1,
                                     vagas = 1,
                                     larea_util = log10(80),
                                     larea_total = log10(100),
                                     lpreco_medio_metro = 3.50))


######################
# SALVANDO PREDIÇÕES #
######################

set = predicao
set$fit_rf = 10^sim_pred_rf
set$reg1 = 10^sim_pred_reg1
set$reg2 = 10^sim_pred_reg2
set$reg3 = 10^sim_pred_reg3
set$areautil = 10^predicao$larea_util
set$areatotal = 10^predicao$larea_total
set$areautil = as.integer(set$areautil)
set$areatotal = as.integer(set$areatotal)
set$fit_rf = as.integer(set$fit_rf)
set$reg1 = as.integer(set$reg1)
set$reg2 = as.integer(set$reg2)
set$reg3 = as.integer(set$reg3)

write.csv(set, "fit_simulação_6_imoveis.csv")


