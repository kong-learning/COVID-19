setwd("~/R/WIE2003/Covid-19")

#Importing libraries
library(keras)
library(tensorflow)
library(data.table)
# ---------------------------Covid-19 cfr Cases----------------------------------------------------------

#Reading the structures table from cfr.csv which was created above 
cfr <- read.csv("./cfr.csv")

#Transpose the cfr data to analyze the linear model
cfr <- transpose(cfr)

write.table(cfr, "./cfr.txt", sep=" ", col.names=FALSE, quote=FALSE, row.names=FALSE)
#Reading the structures table from cfr which was created above 
cfr <- read.table("./cfr.txt", header = TRUE, na.strings = " ")

Series = cfr$Malaysia  # your time series 

#transform data to stationarity
diffed = diff(Series, differences = 1)


#create a lagged dataset, i.e to be supervised learning
lags <- function(x, k){
  lagged =  c(rep(NA, k), x[1:(length(x)-k)])
  DF = as.data.frame(cbind(lagged, x))
  colnames(DF) <- c( paste0('x-', k), 'x')
  DF[is.na(DF)] <- 0
  return(DF)
}
supervised = lags(diffed, 1)


#split into train and test sets
N = nrow(supervised)
n = round(N *0.66, digits = 0)
train = supervised[1:n, ]
test  = supervised[(n+1):N,  ]


#scale data
normalize <- function(train, test, feature_range = c(0, 1)) {
  x = train
  fr_min = feature_range[1]
  fr_max = feature_range[2]
  std_train = ((x - min(x) ) / (max(x) - min(x)  ))
  std_test  = ((test - min(x) ) / (max(x) - min(x)  ))
  
  scaled_train = std_train *(fr_max -fr_min) + fr_min
  scaled_test = std_test *(fr_max -fr_min) + fr_min
  
  return( list(scaled_train = as.vector(scaled_train), scaled_test = as.vector(scaled_test) ,scaler= c(min =min(x), max = max(x))) )
  
}


#inverse-transform
inverter = function(scaled, scaler, feature_range = c(0, 1)){
  min = scaler[1]
  max = scaler[2]
  n = length(scaled)
  mins = feature_range[1]
  maxs = feature_range[2]
  inverted_dfs = numeric(n)
  
  for( i in 1:n){
    X = (scaled[i]- mins)/(maxs - mins)
    rawValues = X *(max - min) + min
    inverted_dfs[i] <- rawValues
  }
  return(inverted_dfs)
}


Scaled = normalize(train, test, c(-1, 1))

y_train = Scaled$scaled_train[, 2]
x_train = Scaled$scaled_train[, 1]

y_test = Scaled$scaled_test[, 2]
x_test = Scaled$scaled_test[, 1]

#fit the model

dim(x_train) <- c(length(x_train), 1, 1)
dim(x_train)
X_shape2 = dim(x_train)[2]
X_shape3 = dim(x_train)[3]
batch_size = 1
units = 1

model <- keras_model_sequential() 
model%>%
  layer_lstm(units, batch_input_shape = c(batch_size, X_shape2, X_shape3), stateful= TRUE)%>%
  layer_dense(units = 1)

model %>% compile(
  loss = 'mean_squared_error',
  optimizer = optimizer_adam( lr= 0.02 , decay = 1e-6 ),  
  metrics = c('mean_squared_error')
)

summary(model)

nb_epoch = 50   
for(i in 1:nb_epoch ){
  model %>% fit(x_train, y_train, epochs=1, batch_size=batch_size, verbose=1, shuffle=FALSE)
  model %>% reset_states()
}


L = length(x_test)
dim(x_test) = c(length(x_test), 1, 1)

scaler = Scaled$scaler

predictions = numeric(L)
for(i in 1:L){
  X = x_test[i , , ]
  dim(X) = c(1,1,1)
  #forecast
  yhat = model %>% predict(X, batch_size=batch_size)
  
  #invert scaling
  yhat = inverter(yhat, scaler,  c(-1, 1))
  
  #invert differencing
  yhat  = yhat + Series[(n+i)] 
  
  #save prediction
  predictions[i] <- yhat
}















# row_num_train = nrow(cfr)-pre_day
# #Divide training set and test set
# cfr_train <- cfr[1:row_num_train, ]
# cfr_test <- cfr[(row_num_train+1):nrow(cfr), ]
# 
# 
# 
# #####  Linear Regression Start  #####  
# modelc_all = lm(Malaysia ~ ., data=cfr_train)
# 
# prediction_all = predict(modelc_all,newdata=cfr_test)
# pred_lm = as.data.frame(prediction_all)
# names(pred_lm)[names(pred_lm) == "prediction_all"] = "pred_lm"
# 
# prediction_lm_train = predict(modelc_all,newdata=cfr_train)
# lm_rmse = sqrt(mean((prediction_lm_train - cfr_train$Malaysia)^2))
# 
# #####  Linear Regression End  #####  
# 
# ##### Ridge Regression Start #####
# 
# #regularization
# 
# dummies <- dummyVars(Malaysia ~ . , data = cfr_train)
# train_dummies = predict(dummies, newdata = cfr_train)
# pred_dummies = predict(dummies, newdata = cfr_test)
# 
# train_dummies = as.matrix(train_dummies)
# pred_dummies = as.matrix(pred_dummies)
# y_train = cfr_train$Malaysia
# 
# lambdas <- 10^seq(2, -3, by = -.1)
# ridge_reg = glmnet(train_dummies, y_train, nlambda = 25, alpha = 0, family = 'gaussian', lambda = lambdas)
# 
# cv_ridge <- cv.glmnet(train_dummies, y_train, alpha = 0, lambda = lambdas)
# optimal_lambda <- cv_ridge$lambda.min
# 
# pred_ridge <- predict(ridge_reg, s = optimal_lambda, newx = pred_dummies)
# pred_ridge = as.data.frame(pred_ridge)
# names(pred_ridge)[names(pred_ridge) == "1"] = "pred_ridge"
# 
# pred_ridge_train <- predict(ridge_reg, s = optimal_lambda, newx = train_dummies)   
# ridge_rmse = sqrt(mean((pred_ridge_train - cfr_train$Malaysia)^2))
# 
# ##### Ridge Regression End #####
# 
# ##### Lasso Regression Start #####
# 
# lambdas <- 10^seq(2, -3, by = -.1)
# lasso_reg <- cv.glmnet(train_dummies, y_train, alpha = 1, lambda = lambdas, standardize = TRUE, nfolds = 5)
# 
# lambda_best <- lasso_reg$lambda.min 
# lasso_model <- glmnet(train_dummies, y_train, alpha = 1, lambda = lambda_best, standardize = TRUE)
# 
# pred_lasso <- predict(lasso_model, s = lambda_best, newx = pred_dummies)
# pred_lasso = as.data.frame(pred_lasso)
# names(pred_lasso)[names(pred_lasso) == "1"] = "pred_lasso"
# 
# pred_lasso_train <- predict(lasso_model, s = optimal_lambda, newx = train_dummies)                      
# lasso_rmse = sqrt(mean((pred_lasso_train - cfr_train$Malaysia)^2))
# ##### Ridge Regression End #####
# 
# #### Linear Regression with Dimension Reduction - PCA  Start  ####
# 
# ## read and part the data
# cfr_asean$Type = NULL
# cfr_test$Malaysia = NULL
# 
# ## Linear Regression with PCA
# trainPredictors = cfr
# trainPredictors$Malaysia = NULL
# KMO(cor(trainPredictors))
# 
# pca_facto = PCA(trainPredictors,graph = F)
# pca_facto$eig
# fviz_eig(pca_facto,ncp=3,addlabels = T)
# 
# # moving forward with only one dimension
# pca = prcomp(trainPredictors,scale. = T)
# trainPredictors = data.frame(pca$x[,1:1])
# trainPredictors2 = cbind(trainPredictors[1:row_num_train,],
#                          cfr_train[1:row_num_train,]$Malaysia)
# trainPredictors2 = as.data.frame(trainPredictors2)
# # building model
# predict_pca_1 = lm(V2~.,trainPredictors2)
# summary(predict_pca_1)
# 
# # making prediction
# trainPredictors3 = trainPredictors[(row_num_train+1):nrow(cfr), ]
# trainPredictors3 = as.data.frame(trainPredictors3)
# names(trainPredictors3)[names(trainPredictors3) == "trainPredictors3"] <- "V1"
# 
# predict_pca = predict(predict_pca_1,newdata=trainPredictors3)
# predict_pca = as.data.frame(predict_pca)
# 
# predict_pca_train = predict(predict_pca_1,newdata=trainPredictors2)
# pca_rmse = sqrt(mean((predict_pca_train-trainPredictors2$V2)^2))
# 
# #### Linear Regression with Dimension Reduction - PCA  End ####
# 
# #### FORECAST VISUALIZATION START ####
# 
# # consolidating predictions
# 
# Malaysia_lm = as.data.frame(cfr_train$Malaysia)
# names(Malaysia_lm)[names(Malaysia_lm) == "cfr_train$Malaysia"] = "pred_lm"
# lm_backward = rbind(Malaysia_lm,pred_lm)
# 
# Malaysia_ridge = as.data.frame(cfr_train$Malaysia)
# names(Malaysia_ridge)[names(Malaysia_ridge) == "cfr_train$Malaysia"] = "pred_ridge"
# lm_ridge = rbind(Malaysia_ridge,pred_ridge)
# 
# Malaysia_lasso = as.data.frame(cfr_train$Malaysia)
# names(Malaysia_lasso)[names(Malaysia_lasso) == "cfr_train$Malaysia"] = "pred_lasso"
# lm_lasso = rbind(Malaysia_lasso,pred_lasso)
# 
# Malaysia_pca = as.data.frame(cfr_train$Malaysia)
# names(Malaysia_pca)[names(Malaysia_pca) == "cfr_train$Malaysia"] = "predict_pca"
# lm_pca = rbind(Malaysia_pca,predict_pca)
# 
# days = 1:nrow(cfr)
# days = as.data.frame(days)
# 
# library(tidyverse)
# final = cbind(days,cfr_asean$Malaysia,lm_pca,lm_lasso,lm_ridge,lm_backward)
# 
# final$predict_pca = ifelse(final$days>=row_num_train+1,final$predict_pca,"")
# final$pred_lasso = ifelse(final$days>=row_num_train+1,final$pred_lasso,"")
# final$pred_ridge = ifelse(final$days>=row_num_train+1,final$pred_ridge,"")
# final$pred_lm = ifelse(final$days>=row_num_train+1,final$pred_lm,"")
# 
# final = gather(final,key = "model", value= "value",2:6)
# final$value = as.integer(final$value)
# final$linetype = ifelse(final$model == 'cfr_asean$Malaysia',1,2)
# final$linetype = as.factor(final$linetype)
# 
# ## FINAL PLOT ##
# 
# library(ggplot2);library(ggrepel)
# palette = c("yellow",'#ffa41b','#AEC4EB',
#             '#18b0b0','#fe346e',"#1f6650")
# 
# ggplot(final,aes(x=days,y=value,color=model,shape=linetype))+
#   geom_line(aes(linetype = linetype),size=1.15)+guides(linetype = FALSE)+
#   geom_text_repel(aes(label=ifelse(days>nrow(cfr_asean),as.character(round(value,0)),'')),hjust= -0.5)+
#   scale_color_manual(values=palette)+
#   scale_x_continuous(breaks=seq(0,130,10))+
#   scale_y_continuous(breaks=seq(0,10000,500))+
#   labs(title = "Malaysia Coronavirus Case Prediction Result",y= "Coronavirus Case", x = "Days")+
#   labs(colour="Models Used")+
#   scale_colour_discrete(labels = c("Actual Case", "Model 1 (lm)", "Model 2 (ridge)","Model 3 (lasso)","Model 4 (pca)"))+
#   guides(color = guide_legend(override.aes = list(linetype = 1, size=3)))+
#   theme(
#     legend.position = "right",
#     axis.text = element_text(colour = "white"),
#     axis.title.x = element_text(colour = "white", size=rel(1.7)),
#     axis.title.y = element_text(colour = "white",size=rel(1.7)),
#     panel.background = element_rect(fill="black",colour = "black"),
#     panel.grid = element_blank(),
#     plot.background = element_rect(fill="black",colour = "black"),
#     legend.key = element_rect(fill = "black",colour = "black"),
#     legend.background = element_blank(),
#     legend.text = element_text(colour="white",size = rel(1)),
#     legend.title = element_text(colour="white",size = rel(1)),
#     panel.grid.minor = element_line(colour="#202020", size=0.3),
#     plot.title = element_text(color="white", size= rel(2),hjust = 0))