require("sqldf")
require("lattice")
require("Boruta")
require("caTools")
require("dplyr")
require("ggplot2")
require("latticeExtra")
require("caret")
require("MASS")

setwd("C:/Users/bkuasney/Desktop/ML/FREELA")
DTM = read.csv("base_tratada.csv")

str(DTM)
nrow(DTM)
ncol(DTM)
names(DTM)
head(DTM)
DTM = DTM[-1]

DTM1 = sqldf('select preco, bairro, cidade, quartos, suites, banheiros, vagas, area_util, area_total, valor_metro, url
             from DTM where tipo = 2', drv = 'SQLite')


DTM1 = sqldf('select preco, cidade, bairro, quartos, suites, banheiros, vagas, area_util, area_total, valor_metro, url from DTM1
             where quartos < 5 and suites < 4 and vagas < 3 and banheiros < 4')

# padronizando
DTM1 = transform(DTM1,
                 larea_util = log10(area_util),
                 lpreco = log10(preco),
                 larea_total = log10(area_total),
                 lvalor_metro = log10(valor_metro))

# Outliers
plot(lpreco ~ larea_util, data = DTM1)
dput(with(DTM1, identify(larea_util, lpreco)))
DTM1 = DTM1[DTM1$lpreco<DTM1[1456,]$lpreco,]

# VAGAS ##################
xyplot(lpreco ~ larea_util | cut(vagas, c(0:3, Inf)),
       data = DTM1,
       as.table = TRUE)

boxplot(lpreco ~ vagas, data = DTM1)$stats
DTM1$status = ifelse(DTM1$vagas == 2 & DTM1$lpreco > 6.139879, "DESCARTAR","MANTER")
DTM1 = subset(DTM1, status == "MANTER")
DTM1$status = ifelse(DTM1$vagas == 2 & DTM1$lpreco < 5.130334, "DESCARTAR","MANTER")
DTM1 = subset(DTM1, status == "MANTER")


# QUARTOS ##################
xyplot(lpreco ~ larea_util | cut(quartos, c(0:3, Inf)),
       data = DTM1,
       as.table = TRUE)

boxplot(lpreco ~ quartos, data = DTM1)$stats
DTM1$status = ifelse(DTM1$quartos == 2 & DTM1$lpreco > 5.694605, "DESCARTAR","MANTER")
DTM1 = subset(DTM1, status == "MANTER")
DTM1$status = ifelse(DTM1$quartos == 2 & DTM1$lpreco < 4.821514, "DESCARTAR","MANTER")
DTM1 = subset(DTM1, status == "MANTER")
DTM1$status = ifelse(DTM1$quartos == 3 & DTM1$lpreco > 6.079181, "DESCARTAR","MANTER")
DTM1 = subset(DTM1, status == "MANTER")
DTM1$status = ifelse(DTM1$quartos == 3 & DTM1$lpreco < 5.161368, "DESCARTAR","MANTER")
DTM1 = subset(DTM1, status == "MANTER")
DTM1$status = ifelse(DTM1$quartos == 4 & DTM1$lpreco < 5.301030, "DESCARTAR","MANTER")
DTM1 = subset(DTM1, status == "MANTER")


# BANHEIROS ##################
xyplot(lpreco ~ larea_util | cut(banheiros, c(0:3, Inf)),
       data = DTM1,
       as.table = TRUE)

boxplot(lpreco ~ banheiros, data = DTM1)$stats
DTM1$status = ifelse(DTM1$banheiros == 1 & DTM1$lpreco > 6.021189, "DESCARTAR","MANTER")
DTM1 = subset(DTM1, status == "MANTER")
DTM1$status = ifelse(DTM1$banheiros == 2 & DTM1$lpreco > 6.020775, "DESCARTAR","MANTER")
DTM1 = subset(DTM1, status == "MANTER")
DTM1$status = ifelse(DTM1$banheiros == 2 & DTM1$lpreco < 5.326336, "DESCARTAR","MANTER")
DTM1 = subset(DTM1, status == "MANTER")
DTM1$status = ifelse(DTM1$banheiros == 2 & DTM1$lpreco > 5.954243, "DESCARTAR","MANTER")
DTM1 = subset(DTM1, status == "MANTER")


# SUITES ##################
xyplot(lpreco ~ larea_util | cut(suites, c(0:3, Inf)),
       data = DTM1,
       as.table = TRUE)

boxplot(lpreco ~ suites, data = DTM1)$stats
DTM1$status = ifelse(DTM1$suites == 0 & DTM1$lpreco > 5.875061, "DESCARTAR","MANTER")
DTM1 = subset(DTM1, status == "MANTER")
DTM1$status = ifelse(DTM1$suites == 1 & DTM1$lpreco > 6.021189, "DESCARTAR","MANTER")
DTM1 = subset(DTM1, status == "MANTER")
DTM1$status = ifelse(DTM1$suites == 1 & DTM1$lpreco < 5.290035, "DESCARTAR","MANTER")
DTM1 = subset(DTM1, status == "MANTER")
DTM1$status = ifelse(DTM1$suites == 2 & DTM1$lpreco < 5.459392, "DESCARTAR","MANTER")
DTM1 = subset(DTM1, status == "MANTER")

setwd("C:/Users/bkuasney/Desktop/ML/FREELA")
write.csv(DTM1, "tratamento_outlier_CASA.csv")


# tratamento segunda parte

DTM1 = read.csv("tratamento_outlier_CASA.csv", sep=",")

# VAGAS ##################
boxplot(larea_util ~ vagas, data = DTM1)$stats
DTM1$status = ifelse(DTM1$vagas == 0 & DTM1$larea_util < 2.071882, "DESCARTAR","MANTER")
DTM1 = subset(DTM1, status == "MANTER")
DTM1$status = ifelse(DTM1$vagas == 1 & DTM1$larea_util < 1.278754, "DESCARTAR","MANTER")
DTM1 = subset(DTM1, status == "MANTER")
DTM1$status = ifelse(DTM1$vagas == 2 & DTM1$larea_util < 1.278754, "DESCARTAR","MANTER")
DTM1 = subset(DTM1, status == "MANTER")


# QUARTOS ##################
boxplot(larea_util ~ quartos, data = DTM1)$stats
DTM1$status = ifelse(DTM1$quartos == 2 & DTM1$larea_util < 2.752816, "DESCARTAR","MANTER")
DTM1 = subset(DTM1, status == "MANTER")
DTM1$status = ifelse(DTM1$quartos == 4 & DTM1$larea_util > 3.065206, "DESCARTAR","MANTER")
DTM1 = subset(DTM1, status == "MANTER")
DTM1$status = ifelse(DTM1$quartos == 4 & DTM1$larea_util < 1.913814, "DESCARTAR","MANTER")
DTM1 = subset(DTM1, status == "MANTER")


# BANHEIROS ##################
boxplot(larea_util ~ banheiros, data = DTM1)$stats
DTM1$status = ifelse(DTM1$banheiros == 1 & DTM1$larea_util < 2.708421, "DESCARTAR","MANTER")
DTM1 = subset(DTM1, status == "MANTER")
DTM1$status = ifelse(DTM1$banheiros == 3 & DTM1$larea_util < 1.724276, "DESCARTAR","MANTER")
DTM1 = subset(DTM1, status == "MANTER")
DTM1$status = ifelse(DTM1$banheiros == 3 & DTM1$larea_util > 2.859138, "DESCARTAR","MANTER")
DTM1 = subset(DTM1, status == "MANTER")
DTM1$status = ifelse(DTM1$banheiros == 3 & DTM1$larea_util < 1.838849, "DESCARTAR","MANTER")
DTM1 = subset(DTM1, status == "MANTER")


# SUITES ##################
boxplot(larea_util ~ suites, data = DTM1)$stats

DTM1$status = ifelse(DTM1$suites == 0 & DTM1$larea_util < 2.711807, "DESCARTAR","MANTER")
DTM1 = subset(DTM1, status == "MANTER")
DTM1$status = ifelse(DTM1$suites == 1 & DTM1$larea_util < 1.698970, "DESCARTAR","MANTER")
DTM1 = subset(DTM1, status == "MANTER")
DTM1$status = ifelse(DTM1$suites == 1 & DTM1$larea_util > 2.927883, "DESCARTAR","MANTER")
DTM1 = subset(DTM1, status == "MANTER")

setwd("C:/Users/bkuasney/Desktop/ML/FREELA")
write.csv(DTM1, "tratamento_outlier_fase2_CASA.csv")

plot(lpreco ~ larea_util, data = DTM1)
dput(with(DTM1, identify(larea_util, lpreco)))


# CHECK BASE
nrow(DTM1)

distinct(DTM1, vagas)
distinct(DTM1, quartos)
distinct(DTM1, banheiros)
distinct(DTM1, suites)

nrow(subset(DTM1, vagas == 0));nrow(subset(DTM1, vagas == 0))/nrow(DTM1)
nrow(subset(DTM1, vagas == 1));nrow(subset(DTM1, vagas == 1))/nrow(DTM1)
nrow(subset(DTM1, vagas == 2));nrow(subset(DTM1, vagas == 2))/nrow(DTM1)

nrow(subset(DTM1, quartos == 1));nrow(subset(DTM1, quartos == 1))/nrow(DTM1)
nrow(subset(DTM1, quartos == 2));nrow(subset(DTM1, quartos == 2))/nrow(DTM1)
nrow(subset(DTM1, quartos == 3));nrow(subset(DTM1, quartos == 3))/nrow(DTM1)
nrow(subset(DTM1, quartos == 4));nrow(subset(DTM1, quartos == 4))/nrow(DTM1)

nrow(subset(DTM1, banheiros == 1));nrow(subset(DTM1, banheiros == 1))/nrow(DTM1)
nrow(subset(DTM1, banheiros == 2));nrow(subset(DTM1, banheiros == 2))/nrow(DTM1)
nrow(subset(DTM1, banheiros == 3));nrow(subset(DTM1, banheiros == 3))/nrow(DTM1)

nrow(subset(DTM1, suites == 0));nrow(subset(DTM1, suites == 0))/nrow(DTM1)
nrow(subset(DTM1, suites == 1));nrow(subset(DTM1, suites == 1))/nrow(DTM1)
nrow(subset(DTM1, suites == 2));nrow(subset(DTM1, suites == 2))/nrow(DTM1)
nrow(subset(DTM1, suites == 3));nrow(subset(DTM1, suites == 3))/nrow(DTM1)



