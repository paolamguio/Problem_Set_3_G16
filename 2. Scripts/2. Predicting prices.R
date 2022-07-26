
# Data Cleaning Base Training
# Problem_Set_3
# Grupo 16
# Andres Martinez, Paola Morales y Oscar Cortes 
#--------------------------------------------------

## preparación del espacio
rm(list = ls())
setwd("C:/Users/ocaco/OneDrive/15. Maestria Economia/9. Big Data/3. GitHub/Problem_Set_3_G16/3. Stores")


## llamado librerías de la sesión
require(pacman)
p_load(
  sf,leaflet,class,skimr, osmdata,
  tidyverse,
  rvest,
  writexl,
  rio,
  skimr,
  pastecs,
  PerformanceAnalytics,
  naniar,
  gtsummary,
  caret,
  modelsummary,
  gamlr,
  ROCR,
  pROC,
  smotefamily,
  rpart,
  randomForest,
  fastAdaboost,
  stargazer,
  randomForest,
  xgboost,
  ranger,
  SuperLearner
  )

  ### 1. llamado bases de datos ###
hogares <- readRDS("df_house_mnz2.rds")

hogares <- hogares %>% subset(base == "train")


class(hogares)
table(hogares$l3)

set.seed(777)

split1 <- createDataPartition(hogares$l3 , p = 0.7)[[1]]
length(split1)
training = hogares[split1,]
testing <- hogares[-split1,]
predict <- stats::predict

colnames(hogares)

model1 <- as.formula("price ~ bedrooms + bathrooms + surface_total + property_type + 
                     dist_bar + dist_bus_station + dist_school + dist_park + dist_parks_total + 
                     Neighborhood + parking + ascensor + balcon + terraza + remodelado + estrato")


ctrl<- trainControl(method = "cv",
                    number = 5,
                    #summaryFunction = fiveStats,
                    #classProbs = TRUE,
                    verbose=FALSE,
                    savePredictions = T)

eval_results <- function(true, predicted, df) {
  SSE <- sum((predicted - true)^2)
  SST <- sum((true - mean(true))^2)
  R_square <- 1 - SSE / SST
  RMSE = sqrt(SSE/nrow(df))
  
  
  # Medtricas del cumplimiento del model
  data.frame(
    RMSE = RMSE,
    Rsquare = R_square
  )}

## 1 Prediccion con OLS

lineal1 <- train(
  model1,
  data = training,
  method = "lm",
  trControl = ctrl,
  preProcess = c("center", "scale")
)

predLineal1 <- predict(lineal1 , testing)


## 2. Modelo lasso - datos de entrenamiento ###

lambda_grid <- 10^seq(-4, 0.01, length = 300)

set.seed(777)

lasso1 <- train(
  model1,
  data = training,
  method = "glmnet",
  trControl = ctrl,
  metric = "RMSE",
  tuneGrid = expand.grid(alpha = 0,lambda=lambda_grid),
  preProcess = c("center", "scale")
)

predLasso1<-predict(lasso1,testing)


## 3. Modelo ridge - datos de entrenamiento ###

ridge1 <- train(
  model1,
  data = training,
  method = "glmnet",
  trControl = ctrl,
  metric = "RMSE",
  tuneGrid = expand.grid(alpha = 1,lambda=lambda_grid),
  preProcess = c("center", "scale")
)

predRidge1<-predict(ridge1,testing)

## 4. Modelo elasticnet - datos de entrenamiento ###

elasticnet1 <- train(
  model1,
  data = training,
  method = "glmnet",
  trControl = ctrl,
  metric = "RMSE",
  preProcess = c("center", "scale")
)

predElasticnet1<-predict(elasticnet1,testing)

## 5. Modelo XGBoost - datos de entrenamiento ###

grid_default <- expand.grid(nrounds = c(100,250),
                            max_depth = c(4,6,8),
                            eta = c(0.01,0.3,0.5),
                            gamma = c(0,1),
                            min_child_weight = c(10, 25,50),
                            colsample_bytree = c(0.7),
                            subsample = c(0.6))

start_xg <- Sys.time()


xgboost1 <- train(
  model1,
  data = training,
  method = "xgbTree",
  trControl = ctrl,
  na.action  = na.pass,
  tuneGrid = grid_default,
  preProcess = c("center", "scale")
)
  xgboost1

predxgboost1<-predict(xgboost1,testing)

## 6. Modelo Superlernear - datos de entrenamiento ###

variablesX <- data.frame(training$bedrooms,training$bathrooms,training$surface_total,training$property_type,
                         training$dist_bar,training$dist_bus_station,training$dist_school,training$dist_park,
                         training$dist_parks_total,training$Neighborhood,training$parking,training$ascensor,
                         training$balcon,training$balcon,training$terraza,training$remodelado,training$estrato)

folds=5
index <- split(1:1000,1:folds)
precio<-training$price
length(training$price)
length(hogares$bedrooms)
superLearner1 <- SuperLearner(Y=precio, X=variablesX,
                              SL.library=c("SL.lm", "SL.ranger", "SL.xgboost"),
                              method = "method.NNLS", cvControl=list(V=folds, validRows=index))
superLearner1

### 7. Evaluación de resultados ###
tabla<-matrix(rep(0,12),nrow=6,ncol=2)
colnames(tabla)<- c("Modelo","RMSE")
#Lineal
tabla[1,1]<-"OLS"
eval<-eval_results(testing$price,predLineal1,testing)
tabla[1,2]<-eval$RMSE

#Lasso
tabla[2,1]<-"Lasso"
eval<-eval_results(testing$price,predLasso1,testing)
tabla[2,2]<-eval$RMSE

#Ridge
tabla[3,1]<-"Ridge"
eval<-eval_results(testing$price,predRidge1,testing)
tabla[3,2]<-eval$RMSE

#Elasticnet
tabla[4,1]<-"Elasticnet"
eval<-eval_results(testing$price,predElasticnet1,testing)
tabla[4,2]<-eval$RMSE

#XGBoost
tabla[5,1]<-"XGBoost"
eval<-eval_results(testing$price,predxgboost1,testing)
tabla[5,2]<-eval$RMSE


stargazer(tabla, type = "text")
tabla<-as.data.frame(tabla)
write_xlsx(tabla,"tabla_precio.xlsx")
