require("sqldf")
require("lattice")
require("Boruta")
require("caTools")
require("dplyr")
require("ggplot2")
require("latticeExtra")
require("caret")
require("MASS")
require("ggplot2")

# Modelo
setwd("C:/Users/bkuasney/Desktop/ML/FREELA")
out = read.csv("preco_medio_imoveis_CASA.csv", sep=",")
DTM1 = read.csv("tratamento_outlier_fase2_CASA.csv", sep=",")
DTM1 = sqldf('select cidade||bairro as cidade_bairro, quartos, suites, banheiros, vagas, area_util, area_total, preco, url from DTM1')
names(out) = c("X", "cidade_bairro", "lpreco_medio_metro")
DTM1out = merge(DTM1, out, by = "cidade_bairro")
DTM = DTM1out

head(DTM1out)
nrow(DTM1out)
str(DTM1out)

DTM1 = sqldf('select quartos, suites, banheiros, vagas, area_util, area_total, lpreco_medio_metro, preco, url from DTM', drv = 'SQLite')
head(DTM1)
str(DTM1)

DTM1 = transform(DTM1,
                 larea_util = log10(area_util),
                 lpreco = log10(preco),
                 larea_total = log10(area_total))

DTM1 = sqldf('select quartos, suites, banheiros, vagas, larea_util, larea_total, lpreco_medio_metro, lpreco, url from DTM1', drv = 'SQLite')

# modelos
xyplot(lpreco ~ larea_util | cut(vagas, c(0:3, Inf)),
       data = DTM1,
       as.table = TRUE)

xyplot(lpreco ~ larea_util | cut(quartos, c(0:3, Inf)),
       data = DTM1,
       as.table = TRUE)

xyplot(lpreco ~ larea_util | cut(banheiros, c(0:3, Inf)),
       data = DTM1,
       as.table = TRUE)

xyplot(lpreco ~ larea_util | cut(suites, c(0:3, Inf)),
       data = DTM1,
       as.table = TRUE)

# LEMBRAR QUE NA HORA DO INPUT O VALOR DA VARIÁVEL TERÁ QUE SOFRER O LOG10 PARA ESTIMAÇÃO
DTM1M1 = DTM1[,c("quartos", "suites", "banheiros", "vagas", "larea_util", "larea_total", "lpreco_medio_metro", "lpreco", "url")]
str(DTM1M1)

DTM1M1$quartos = as.factor(DTM1M1$quartos)
DTM1M1$suites = as.factor(DTM1M1$suites)
DTM1M1$banheiros = as.factor(DTM1M1$banheiros)
DTM1M1$vagas = as.factor(DTM1M1$vagas)
names(DTM1M1)

# Avaliação Variáveis
boruta.train <- Boruta(lpreco~., data = DTM1M1[,1:8], doTrace = 2)

plot(boruta.train, xlab = "", xaxt = "n")
lz<-lapply(1:ncol(boruta.train$ImpHistory),function(i)boruta.train$ImpHistory[is.finite(boruta.train$ImpHistory[,i]),i])
names(lz) <- colnames(boruta.train$ImpHistory)
Labels <- sort(sapply(lz,median))
axis(side = 1,las=2,labels = names(Labels),at = 1:ncol(boruta.train$ImpHistory), cex.axis = 0.7)


# DESCARTANDO INFORMAÇÕES INFLUENTES
nrow(DTM1M1)
info_infl = DTM1M1
info_infl$referencia = as.numeric(rownames(DTM1M1))
influence = lm(lpreco~., data = info_infl[,c(1,2,3,4,5,6,8)])

influence_method = which(apply(influence.measures(influence)$is.inf, 1, any))
nrow(data.frame(influence_method))
influence_data_frame = data.frame(influence_method)
influence_data_frame$status = "RETIRAR"
names(influence_data_frame) = c("referencia", "status")

DTM1M1_teste = sqldf('select * from info_infl left join influence_data_frame using(referencia)', drv = 'SQLite')
DTM1M1_teste[is.na(DTM1M1_teste)] <- 0
DTM1M1_teste = subset(DTM1M1_teste, status == 0)
nrow(DTM1M1_teste)
DTM1M1 = DTM1M1_teste
DTM1M1 = DTM1M1[,1:9]

# treino e teste
DTM1M1$split <- sample.split(DTM1M1$lpreco, SplitRatio = 0.8)
training_set <- subset(DTM1M1, split == TRUE)
test_set <- subset(DTM1M1, split == FALSE)
training_set = training_set[,1:8]
test_set_bkp = test_set[,1:9]
test_set = test_set[,1:8]


# Randon Forest FIT with Boosting
require(gbm)
rb_casa = gbm(lpreco ~ .,
         data = training_set,
         distribution = "gaussian",
         n.trees = 600,
         shrinkage = 0.05,
         interaction.depth = 1,
         train.fraction = 0.7,
         n.minobsinnode = 10,
         cv.folds = 3,
         keep.data = TRUE,
         verbose = FALSE,
         n.cores = 1)
summary(rb_casa)
print(rb_casa)

yp_test = predict(rb_casa, newdata = test_set[-8])
cor(yp_test, test_set$lpreco)

xyplot(yp_test ~ test_set$lpreco, aspect = 'iso', auto.key = TRUE, type = c("p", "smooth")) +
  latticeExtra::layer(panel.abline(a=0, b=1))


# Regression without laverage point ########################
reg_casa = lm(lpreco~., data = training_set)
w_casa <- abs(rstudent(reg_casa)) < 6 & abs(cooks.distance(reg_casa)) < 7/nrow(reg_casa$model)
reg_casa <- update(reg_casa, weights=as.numeric(w_casa))
summary(reg_casa)
plot(reg_casa)
hist(reg_casa$res)

ypred = predict(reg_casa, newdata = test_set[-8])

modelo_reg = ypred - test_set$lpreco
mean(modelo_reg^2)

# Regression ##############################################
require("leaps")
subfit_casa = regsubsets(lpreco~., data=training_set)
b.sum_casa = summary(subfit_casa)
which.min(b.sum_casa$bic)
plot(b.sum_casa$bic, type="l", xlab="# of Features", ylab="BIC", main="BIC
     score by Feature Inclusion")
plot(subfit_casa, scale="bic", main="Best Subset Features")

ols_casa = lm(lpreco~., data=training_set)
plot(ols_casa$fitted.values, training_set$lpreco, xlab="Predicted", ylab="Actual",
     main="Predicted vs Actual")

pred.subfit = predict(ols_casa, newdata=test_set)
plot(pred.subfit, test_set$lpreco , xlab="Predicted", ylab="Actual",
     main="Predicted vs Actual")

resid.subfit = test_set$lpreco - pred.subfit
mean(resid.subfit^2)

# Ridge Regression #########################################
names(training_set)
x = as.matrix(training_set[,1:7])
y = training_set[ ,8]

require("glmnet")

ridge_casa = glmnet(x, y, family="gaussian", alpha=0)
print(ridge_casa)
plot(ridge_casa, label=TRUE)
plot(ridge_casa, xvar="lambda", label=TRUE)

ridge.coef = coef(ridge_casa, s=0.1, exact=TRUE)
ridge.coef
plot(ridge_casa, xvar="dev", label=TRUE)

newx = as.matrix(test_set[,1:7])
ridge.y = predict(ridge_casa, newx=newx, type="response", s=0.1)
ridge.resid = ridge.y - test_set$lpreco
mean(ridge.resid^2)


# Salvando o modelo
## Randon Forest FIT with Boosting
## Regression without laverage point
## Regression
## Ridge Regression
setwd("C:/Users/bkuasney/Desktop/ML/FREELA/GIT")
saveRDS(rb_casa,"./rb_casa.rds")
saveRDS(reg_casa,"./reg_casa.rds")
saveRDS(ols_casa,"./ols_casa.rds")
saveRDS(ridge_casa,"./ridge_casa.rds")


### Configuração
set = test_set
set$larea_util = 10^test_set$larea_util
set$larea_total = 10^test_set$larea_total
set$lpreco_medio_metro = 10^test_set$lpreco_medio_metro
set$preco = 10^test_set$lpreco
set$fit_rf = 10^yp_test
set$fit_reg1 = 10^ypred
set$fit_reg2 = 10^pred.subfit
set$fit_reg3 = 10^ridge.y
set$url = test_set_bkp$url
# as integer
set$larea_util = as.integer(set$larea_util)
set$larea_total = as.integer(set$larea_total)
set$lpreco_medio_metro = as.integer(set$lpreco_medio_metro)
set$preco = as.integer(set$preco)
set$fit_rf = as.integer(set$fit_rf)
set$fit_reg1 = as.integer(set$fit_reg1)
set$fit_reg2 = as.integer(set$fit_reg2)
set$fit_reg3 = as.integer(set$fit_reg3)

# save
setwd("C:/Users/bkuasney/Desktop/ML/FREELA")
write.csv(set, "fit_model_CASA.csv")


test_set[1,]
10^3.020361

