# Adiconando variável de preço metro quadrado médio

trat = read.csv("tratamento_outlier_fase2_CASA.csv", sep=',')
trat = sqldf('select cidade||bairro as cidade_bairro, lvalor_metro from trat', drv = 'SQLite')

regiao = distinct(trat, cidade_bairro)

for(i in 1:nrow(distinct(trat,cidade_bairro))) {
  menor = boxplot(subset(trat, cidade_bairro == regiao[i,1])[2])$stats[1,]
  maior = boxplot(subset(trat, cidade_bairro == regiao[i,1])[2])$stats[5,]
  trat$status = ifelse(trat$cidade_bairro == regiao[i,1] & trat$lvalor_metro < menor, "DESCARTAR","MANTER")
  trat = subset(trat, status == "MANTER")
  trat$status = ifelse(trat$cidade_bairro == regiao[i,1] & trat$lvalor_metro > maior, "DESCARTAR","MANTER")
  trat = subset(trat, status == "MANTER")
}

preco_medio  <- matrix(NA, ncol = 2, nrow = nrow(regiao))

for(i in 1:nrow(distinct(trat, cidade_bairro))){
  preco_medio[i,1] = regiao[i,1]
  preco_medio[i,2] = boxplot(subset(trat, cidade_bairro == regiao[i,1])[2])$stats[3,]
}

preco_medio

setwd("C:/Users/bkuasney/Desktop/ML/FREELA")
write.csv(preco_medio, "preco_medio_imoveis_CASA.csv")

