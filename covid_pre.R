setwd("~/R/WIE2003/Covid-19")

#Importing libraries
library(keras)
library(tensorflow)
Sys.setenv(RETICULATE_PYTHON="/home/lk/Software/anaconda3/envs/tensorflow/bin/python3.6")
use_condaenv("tensorflow",conda ='/home/lk/Software/anaconda3/envs/tensorflow' )
library(data.table)
library(ggplot2)
# ---------------------------Covid-19 cfr Cases----------------------------------------------------------

#Reading the structures table from cfr.csv which was created above
cfr <- read.csv("./cfr.csv")

#Transpose the cfr data to analyze the linear model
cfr <- transpose(cfr)

write.table(cfr, "./cfr.txt", sep=" ", col.names=FALSE, quote=FALSE, row.names=FALSE)
#Reading the structures table from cfr which was created above
cfr <- read.table("./cfr.txt", header = TRUE, na.strings = " ")

country_name = names(cfr)

#scale data
normalize <- function(train, test, feature_range = c(0, 1)) {
  x <- train
  fr_min <- feature_range[1]
  fr_max <- feature_range[2]
  std_train <- ((x - min(x) ) / (max(x) - min(x)  ))
  std_test <- ((test - min(x) ) / (max(x) - min(x)  ))
  
  scaled_train <- std_train *(fr_max -fr_min) + fr_min
  scaled_test <-std_test *(fr_max -fr_min) + fr_min
  
  return( list(scaled_train = as.vector(scaled_train), scaled_test = as.vector(scaled_test) ,scaler= c(min =min(x), max = max(x))) )
}

#inverse-transform
inverter  <-  function(scaled, scaler, feature_range = c(0, 1)){
  min <- scaler[1]
  max <- scaler[2]
  n <- length(scaled)
  mins <- feature_range[1]
  maxs <- feature_range[2]
  inverted_dfs <- numeric(n)
  
  for( i in 1:n){
    X <- (scaled[i]- mins)/(maxs - mins)
    rawValues <- X *(max - min) + min
    inverted_dfs[i] <- rawValues
  }
  return(inverted_dfs)
}

#create a lagged dataset, i.e to be supervised learning
lags <- function(x, k){
  lagged <- c(rep(NA, k), x[1:(length(x)-k)])
  DF <- as.data.frame(cbind(lagged, x))
  colnames(DF) <- c(paste0('x-', k), 'x')
  DF[is.na(DF)] <- 0
  return(DF)
}


A <- matrix(nrow=10,ncol=28)
num <- 1
for (name in country_name) {
  

  Series <-  cfr[name] # your time series
  Series <- data.matrix(Series)
  Series <- as.numeric(Series)
  
  #transform data to stationarity
  diffed <- diff(Series, differences = 1)
  
  
  supervised <- lags(diffed, 1)
  
  #split into train and test sets
  N <- nrow(supervised)
  n <- round(N *0.8, digits = 0)
  train <- supervised[1:n, ]
  test <- supervised[(n+1):N,  ]
  
  
  Scaled <- normalize(train, test, c(-1, 1))
  
  y_train <- Scaled$scaled_train[, 2]
  x_train <- Scaled$scaled_train[, 1]
  
  y_test <- Scaled$scaled_test[,2]
  x_test <- Scaled$scaled_test[,1]
  
  #hyperparameter
  epochs <- 300
  batch_size = 1
  
  #Reshape the input to 3-dim
  dim(x_train) <- c(length(x_train), 1, 1)
  
  #fit the model
  model <- keras_model_sequential()
  model %>% layer_lstm(units = 5, batch_input_shape = c(batch_size, 1, 1), stateful= TRUE) %>% layer_dense(1)
  
  model %>% compile(
    loss = 'mean_squared_error',
    optimizer = optimizer_adam()
  )
  
  summary(model)
  
  
  model %>% fit(x=x_train, y=y_train, batch_size=batch_size,
                  epochs=epochs, verbose=1, validation_split = 0.2,
                  shuffle=FALSE, callbacks = list(
      # callback_model_checkpoint("checkpoints.h5"),
      callback_early_stopping(monitor = "val_loss", patience = 20))
      )
  
  
  
  # model %>% load_model_weights_hdf5(filepath = './checkpoints.h5')
  
  #Reshape the input to 3-dim
  L = length(x_test)
  dim(x_test) = c(length(x_test), 1, 1)
  
  scaler = Scaled$scaler
  
  # yhat <-  model %>% predict(x_test, batch_size=batch_size)
  # yhat <- inverter(yhat, scaler, c(-1,1))
  

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
    A[num,i] <- yhat

  }
  num <- num+1
}
B <- as.data.frame(A)
B <- transpose(B)

# Latitude and longitude
pre <- read.csv("./pre.csv", row.names = FALSE)
pre <- transpose(pre)

pre <- pre[nrow(pre),]
# Latitude and longitude
df <- read.csv(textConnection(
  "Name,Lat,Long
  Brunel,21.9162,95.9560
  Brunei,4.5353,114.7277
  Cambodia,12.5657,104.9910
  Indonesia,0.7893,113.9213
  Laos,19.8563,102.4955
  Malaysia,4.2105,101.9758
  Philippines,12.8797,121.7740
  Singapore,1.3521,103.8198
  Thailand,15.8700,100.9925
  Vietnam,14.0583,108.2772"
))

vis_data <-cbind(df,pre)
names(vis_data) <-c("Name","Lat","Long","cfr")

vis_data[is.na(vis_data)] <- 0

#Writing the data table in csv file 
write.csv(vis_data, "./pre.csv", sep=",", col.names=TRUE, quote=FALSE, row.names=FALSE)

