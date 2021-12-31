###########################################################################################################
# Durchfuehrung der Predictions:
###########################################################################################################

# Wichtig ist hier, je nachdem, fuer welche Datenart man hier was vorhersagen moechte, in den Code einsetzen.
# Darauf achten: Immer sowohl fuer das Training als auch die Predictions im Testset die gleiche Datenart
# verwenden!!!!
 
# nicht-transformierte Daten - normaldata
# transformierte Daten (alle transformiert bis auf historisches Beta) - test_transform
# transformierte Daten (alle transformiert bis auf historischer MPRC) - test_transform_mprc
# transformierte Daten (alle transformiert) - test_transform2
# transformierte Daten (alle transformiert) - test_transform_mprc2 


###########################################################################################################
# Berechnung der CAPM-Betas:
###########################################################################################################

# Historisches Beta als Vergleich 
historical_beta2 <- normaldata %>%
  group_by(lpermno) %>%
  do(beta = historical_beta(.[,1:45])) %>% 
  unnest(everything()) %>%
  mutate(se = se(beta_12m, beta_12m_lag))

historical_beta2 <- historical_beta2 %>%
  select(lpermno, se)
 
historical_beta2 <- pivot_wider(historical_beta2, names_from = lpermno, values_from = se, values_fn = list) %>%
  unnest(everything())

historical_beta2$mse <- rowMeans(historical_beta2)

mean(historical_beta2$mse)

### Durchfuehrung der Predictions:
# Hier werden die Objekte zugeordnet, die auch auch in den pred_Funktionen stehen (mlmmod, lassomod etc.)
## Hyperparameter-Tuning
# OLS
tic()
mlmmod <- mlm_beta(normaldata)
toc()

# LASSO
tic()
set.seed(30995)
lassomod <- lasso_beta(normaldata)
toc()

#Bagging
tic()
set.seed(30995)
bagmod <- bag_beta(normaldata)
toc()

# Random Forest
tic()
set.seed(30995)
rfmod <- rf_beta(normaldata)
toc()

# Stochastic Gradient Boosting
tic()
set.seed(30995)
sgbmod <- sgb_beta_adj(normaldata)
toc()

# Extreme Gradient Boosting
tic()
set.seed(30995)
xgbmod <- xgb_beta_adj(normaldata)
toc()

## Anwendung der finalen Modelle auf die Testdaten:
# OLS
mlm_betaUN <-  normaldata %>%  
  group_by(lpermno) %>%
  do(mlm = mlm_pred(.[,1:45])) %>%
  spread(key = lpermno, value = mlm) %>%
  unnest(cols=everything()) 

# LASSO
lasso_betaUN <- normaldata %>%  
  group_by(lpermno) %>%
  do(lasso = lasso_pred(.[,1:45])) %>%
  spread(key = lpermno, value = lasso) %>%
  unnest(cols=everything()) 

# Bagging
bag_betaUN <- normaldata %>%  
  group_by(lpermno) %>%
  do(bag = bag_pred(.[,1:45])) %>%
  spread(key = lpermno, value = bag) %>%
  unnest(cols=everything()) 

# Random Forest
rf_betaUN <- normaldata %>%  
  group_by(lpermno) %>%
  do(rf = rf_pred(.[,1:45])) %>%
  spread(key = lpermno, value = rf) %>%
  unnest(cols=everything())

# Stochastic Gradient Boosting
sgb_betaUN <- normaldata %>%  
  group_by(lpermno) %>%
  do(sgb = sgb_pred(.[,1:45])) %>%
  spread(key = lpermno, value = sgb) %>%
  unnest(cols=everything()) 

# Extreme Gradient Boosting
xgb_betaUN <- normaldata %>%  
  group_by(lpermno) %>%
  do(xgb = xgb_pred(.[,1:45])) %>%
  spread(key = lpermno, value = xgb) %>%
  unnest(cols=everything())

# Speichern der Vorhersagen und der Modelle als RDS, wegen langer Rechenzeiten
saveRDS(beta_12m_mean2, file = "actual.RDS")
saveRDS(mlm_betaUN, "mlm_betaUN.RDS")
saveRDS(lasso_betaUN, "lasso_betaUN.RDS") 
saveRDS(bag_betaUN, "bag_betaUN.RDS")
saveRDS(rf_betaUN, "rf_betaUN.RDS")
saveRDS(sgb_betaUN, "sgb_betaUN.RDS")
saveRDS(xgb_betaUN, "xgb_betaUN.RDS")
saveRDS(mlmmod, "mlmmod.RDS")
saveRDS(lassomod, "lassomod.RDS")
saveRDS(bagmod, "bagmod.RDS")
saveRDS(rfmod, "rfmod.RDS")
saveRDS(sgbmod, "sgbmod.RDS")
saveRDS(xgbmod, "xgbmod.RDS")

### Tabelle generieren der OLS und LASSO-Koeffizienten, diese ist auch in der Arbeit zu finden 
### (musste noch leicht in Excel aufbereitet werden)
d <- summary(mlmmod)

coef <- as_tibble(mlmmod$coefnames)

coef <- coef %>%
  add_row

out <- as_tibble(d[["coefficients"]][,1])
out$t_value <- d[["coefficients"]][,3]
out$p_value <- d[["coefficients"]][,4]

table_mlm <- cbind(coef, out)

write_xlsx(table_mlm, "table_mlm.xlsx")

rm(d, coef, out)

lassoimp <- varImp(lassomod, scale = FALSE)

lassoimp

coef_lasso <- coef(lassomod$finalModel, lassomod$bestTune$lambda)

tbl_lasso <- as.data.frame(as.matrix(coef_lasso))
write_xlsx(tbl_lasso, "tbl_lasso.xlsx")
rm(coef_lasso, tbl_lasso)

###########################################################################################################
# Berechnung der Aktienkurse:
###########################################################################################################
# Training der Modelle
# OLS-Regression
tic()
mlmmodm <- mlm_mprc(normaldata)
toc()

#LASSO
tic()
lassomodm <- lasso_mprc(normaldata)
toc()

# Bagging
tic()
set.seed(30995)
bagmodm <- bag_mprc(normaldata)
toc()

#Random Forest
tic()
set.seed(30995)
rfmodm <- rf_mprc(normaldata)
toc()

#Stochastic Gradient Boosting
tic()
set.seed(30995)
sgbmodm <- sgb_mprc_adj(normaldata)
toc()

# XGBoost
tic()
set.seed(30995)
xgbmodm <- xgb_mprc_adj(normaldata)
toc()

# Anwendung der generierten Modelle auf die Testdaten - Vorhersage der Aktienkurse:

# OLS
mlm_mprcUN <-  normaldata %>%  
  group_by(lpermno) %>%
  do(mlm = mlm_pred_m(.[,1:45])) %>%
  spread(key = lpermno, value = mlm) %>%
  unnest(cols=everything()) 

# LASSO
lasso_mprcUN <- normaldata %>%  
  group_by(lpermno) %>%
  do(lasso = lasso_pred_m(.[,1:45])) %>%
  spread(key = lpermno, value = lasso) %>%
  unnest(cols=everything()) 

# Bagging
bag_mprcUN <- normaldata %>%  
  group_by(lpermno) %>%
  do(bag = bag_pred_m(.[,1:45])) %>%
  spread(key = lpermno, value = bag) %>%
  unnest(cols=everything()) 

# Random Forest
rf_mprcUN <- normaldata %>%  
  group_by(lpermno) %>%
  do(rf = rf_pred_m(.[,1:45])) %>%
  spread(key = lpermno, value = rf) %>%
  unnest(cols=everything())

# Stochastic Gradient Boosting
sgb_mprcUN <- normaldata %>%  
  group_by(lpermno) %>%
  do(sgb = sgb_pred_m(.[,1:45])) %>%
  spread(key = lpermno, value = sgb) %>%
  unnest(cols=everything()) 

# Xtreme Gradient Boosting
xgb_mprcUN <- normaldata %>%  
  group_by(lpermno) %>%
  do(xgb = xgb_pred_m(.[,1:45])) %>%
  spread(key = lpermno, value = xgb) %>%
  unnest(cols=everything())

# Abspeichern der Ergebnisse und Modelle aufgrund langer Rechenzeiten
saveRDS(mprc_actual2, file = "actual_mprc.RDS")
saveRDS(mlm_mprcUN, "mlm_mprcUN.RDS")
saveRDS(lasso_mprcUN, "lasso_mprcUN.RDS")
saveRDS(bag_mprcUN, "bag_mprcUN.RDS")
saveRDS(rf_mprcUN, "rf_mprcUN.RDS")
saveRDS(sgb_mprcUN, "sgb_mprcUN.RDS")
saveRDS(xgb_mprcUN, "xgb_mprcUN.RDS")
saveRDS(mlmmodm, "mlmmodm.RDS")
saveRDS(lassomodm, "lassomodm.RDS")
saveRDS(bagmodm, "bagmodm.RDS")
saveRDS(rfmodm, "rfmodm.RDS")
saveRDS(sgbmodm, "sgbmodm.RDS")
saveRDS(xgbmodm, "xgbmodm.RDS")

### Tabelle generieren der OLS und LASSO-Koeffizienten - ist auch in der Arbeit zu finden
### (Musste in Excel noch leicht angepasst werden)
d <- summary(mlmmodm)

coef <- as_tibble(mlmmodm$coefnames)

coef <- coef %>%
  add_row

out <- as_tibble(d[["coefficients"]][,1])
out$t_value <- d[["coefficients"]][,3]
out$p_value <- d[["coefficients"]][,4]

table_mlm_m <- cbind(coef, out)

write_xlsx(table_mlm_m, "table_mlm_mprc.xlsx")

rm(d, coef, out)

lassoimpm <- varImp(lassomodm, scale = FALSE)

lassoimpm

coef_lassom <- coef(lassomodm$finalModel, lassomodm$bestTune$lambda)

tbl_lassom <- as.data.frame(as.matrix(coef_lassom))
write_xlsx(tbl_lassom, "tbl_lasso_mprc.xlsx")
rm(coef_lassom, tbl_lassom)

###########################################################################################################
# # Hier gehts dann weiter mit den Neuronalen Netzen - sowohl Betas als auch Aktienkurse:
###########################################################################################################

##################################################### Gilt sowohl fuer Betas als auch Aktienkurse:
# NN Grid
NN_grid <- expand.grid( 
  lambda = c(0.001,0.01),
  batch_size = 288,
  lr = c(0.001, 0.01), # 0.01 hat zu keinen Errors gefuehrt
  rho = 0.9,
  decay = c(1e-05, 1e-04), 
  activation = "relu" 
)


# Funktion mlpkerasdecay aus dem caret-Package abgeaendert und um Hidden Layers erweitert:

# Beginnend mit 8 Hidden Layers - in den Funktionen selbst ist dann markiert, was ich noch ergaenzt habe

# Eine Hidden Layer
mlpKerasDecayL1 <-      list(label = "Multilayer Perceptron Network with Weight Decay",
                             library = "keras",
                             loop = NULL,
                             type = c('Regression', "Classification"),
                             parameters = data.frame(
                               parameter = c('lambda', "batch_size", "lr", "rho", 
                                             "decay", "activation"),
                               class = c(rep('numeric', 5), "character"),
                               label = c('L2 Regularization', "Batch Size", "Learning Rate", "Rho", 
                                         "Learning Rate Decay", "Activation Function")
                             ),
                             grid = function(x, y, len = NULL, search = "grid") {
                               afuncs <- c("sigmoid", "relu", "tanh")
                               if(search == "grid") {
                                 out <- expand.grid(
                                   lambda = c(0, 10 ^ seq(-1, -4, length = len - 1)), 
                                   batch_size = floor(nrow(x)/3),
                                   lr = 2e-6,
                                   rho = .9,
                                   decay = 0,
                                   activation = "relu"
                                 )
                               } else {
                                 n <- nrow(x)
                                 out <- data.frame(
                                   size = sample(2:20, replace = TRUE, size = len),
                                   lambda = 10^runif(len, min = -5, 1),
                                   batch_size = floor(n*runif(len, min = .1)),
                                   lr = runif(len),
                                   rho = runif(len),
                                   decay = 10^runif(len, min = -5, 0),
                                   activation = sample(
                                     afuncs, 
                                     size = len, 
                                     replace = TRUE
                                   )
                                 )
                               }
                               out
                             },
                             fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                               require(dplyr)
                               K <- keras::backend()
                               K$clear_session()
                               if(!is.matrix(x)) x <- as.matrix(x)
                               model <- keras::keras_model_sequential()
                               model %>% 
                                 keras::layer_dense(
                                   units = 8, # wie mit Herrn Schroen besprochen direkt die 8 eingetragen, da er die Funktion sonst Probleme bei der Zuordnung hat
                                   activation = as.character(param$activation), 
                                   input_shape = ncol(x),
                                   kernel_initializer = keras::initializer_glorot_uniform(),
                                   kernel_regularizer = keras::regularizer_l2(param$lambda)
                                 ) %>%
                                 layer_batch_normalization() # Batch normalization wie in Arbeit erlaeutert - ergaenzt
                               if(is.factor(y)) {
                                 y <- class2ind(y)
                                 model %>% 
                                   keras::layer_dense(
                                     units = length(lev), 
                                     activation = 'softmax',
                                     kernel_regularizer = keras::regularizer_l2(param$lambda)
                                   ) %>% keras::compile(
                                     loss = "categorical_crossentropy",
                                     optimizer = keras::optimizer_rmsprop(
                                       lr = param$lr,
                                       rho = param$rho,
                                       decay = param$decay
                                     ),
                                     metrics = "accuracy"
                                   )
                               } else {
                                 model %>% 
                                   keras::layer_dense(
                                     units = 1, 
                                     activation = 'linear',
                                     kernel_regularizer = keras::regularizer_l2(param$lambda)
                                   ) %>% keras::compile(
                                     loss = "mean_squared_error",
                                     optimizer = keras::optimizer_rmsprop( # Hier sieht man wie in der Arbeit erlaeutert, das RMSPROP verwendet wird
                                       lr = param$lr,
                                       rho = param$rho,
                                       decay = param$decay
                                     ),
                                     metrics = "mean_squared_error"
                                   )
                               }
                               model %>% keras::fit(
                                 x = x, 
                                 y = y,
                                 batch_size = param$batch_size,
                                 ...
                               )
                               if(last)
                                 model <- keras::serialize_model(model)
                               list(object = model)
                             },
                             predict = function(modelFit, newdata, submodels = NULL) {
                               if(inherits(modelFit$object, "raw"))
                                 modelFit$object <- keras::unserialize_model(modelFit$object)
                               if(!is.matrix(newdata)) 
                                 newdata <- as.matrix(newdata)
                               out <- predict(modelFit$object, newdata)
                               ## check for model type
                               if(ncol(out) == 1) {
                                 out <- out[, 1]
                               } else {
                                 out <- modelFit$obsLevels[apply(out, 1, which.max)]
                               }
                               out
                             },
                             prob =  function(modelFit, newdata, submodels = NULL) {
                               if(inherits(modelFit$object, "raw"))
                                 modelFit$object <- keras::unserialize_model(modelFit$object)
                               if(!is.matrix(newdata)) 
                                 newdata <- as.matrix(newdata)
                               out <- predict(modelFit$object, newdata)
                               colnames(out) <- modelFit$obsLevels
                               as.data.frame(out, stringsAsFactors = TRUE)
                             },
                             varImp = NULL,
                             tags = c("Neural Network", "L2 Regularization"),
                             # sort = function(x) x[order(x$size, x$size2, -x$lambda),],
                             notes = paste("After `train` completes, the keras model object is serialized",
                                           "so that it can be used between R session. When predicting, the", 
                                           "code will temporarily unsearalize the object. To make the", 
                                           "predictions more efficient, the user might want to use ", 
                                           "`keras::unsearlize_model(object$finalModel$object)` in the current", 
                                           "R session so that that operation is only done once.",
                                           "Also, this model cannot be run in parallel due to",
                                           "the nature of how tensorflow does the computations.",
                                           
                                           "Unlike other packages used by `train`, the `dplyr`",
                                           "package is fully loaded when this model is used."),
                             check = function(pkg) {
                               testmod <- try(keras::keras_model_sequential(),
                                              silent = TRUE)
                               if(inherits(testmod, "try-error"))
                                 stop("Could not start a sequential model. ",
                                      "`tensorflow` might not be installed. ",
                                      "See `?install_tensorflow`.", 
                                      call. = FALSE)
                               TRUE
                             })

# Zwei Hidden Layers
mlpKerasDecayL2 <-      list(label = "Multilayer Perceptron Network with Weight Decay",
                             library = "keras",
                             loop = NULL,
                             type = c('Regression', "Classification"),
                             parameters = data.frame(
                               parameter = c('lambda', "batch_size", "lr", "rho", 
                                             "decay", "activation"),
                               class = c(rep('numeric', 5), "character"),
                               label = c('L2 Regularization', "Batch Size", "Learning Rate", "Rho", 
                                         "Learning Rate Decay", "Activation Function")
                             ),
                             grid = function(x, y, len = NULL, search = "grid") {
                               afuncs <- c("sigmoid", "relu", "tanh")
                               if(search == "grid") {
                                 out <- expand.grid(
                                   lambda = c(0, 10 ^ seq(-1, -4, length = len - 1)), 
                                   batch_size = floor(nrow(x)/3),
                                   lr = 2e-6,
                                   rho = .9,
                                   decay = 0,
                                   activation = "relu"
                                 )
                               } else {
                                 n <- nrow(x)
                                 out <- data.frame(
                                   size = sample(2:20, replace = TRUE, size = len),
                                   lambda = 10^runif(len, min = -5, 1),
                                   batch_size = floor(n*runif(len, min = .1)),
                                   lr = runif(len),
                                   rho = runif(len),
                                   decay = 10^runif(len, min = -5, 0),
                                   activation = sample(
                                     afuncs, 
                                     size = len, 
                                     replace = TRUE
                                   )
                                 )
                               }
                               out
                             },
                             fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                               require(dplyr)
                               K <- keras::backend()
                               K$clear_session()
                               if(!is.matrix(x)) x <- as.matrix(x)
                               model <- keras::keras_model_sequential()
                               model %>% 
                                 keras::layer_dense(
                                   units = 8, 
                                   activation = as.character(param$activation), 
                                   input_shape = ncol(x),
                                   kernel_initializer = keras::initializer_glorot_uniform(),
                                   kernel_regularizer = keras::regularizer_l2(param$lambda)
                                 ) %>%
                                 layer_batch_normalization() %>% # Batch normalization
                                 keras::layer_dense(
                                   units = 4, 
                                   activation = as.character(param$activation),
                                   kernel_initializer = keras::initializer_glorot_uniform(),
                                   kernel_regularizer = keras::regularizer_l2(param$lambda)
                                 ) %>%
                                 layer_batch_normalization() # Batch normalization
                               if(is.factor(y)) {
                                 y <- class2ind(y)
                                 model %>% 
                                   keras::layer_dense(
                                     units = length(lev), 
                                     activation = 'softmax',
                                     kernel_regularizer = keras::regularizer_l2(param$lambda)
                                   ) %>% keras::compile(
                                     loss = "categorical_crossentropy",
                                     optimizer = keras::optimizer_rmsprop(
                                       lr = param$lr,
                                       rho = param$rho,
                                       decay = param$decay
                                     ),
                                     metrics = "accuracy"
                                   )
                               } else {
                                 model %>% 
                                   keras::layer_dense(
                                     units = 1, 
                                     activation = 'linear', # lineare Aktivierungsfunktion
                                     kernel_regularizer = keras::regularizer_l2(param$lambda)
                                   ) %>% keras::compile(
                                     loss = "mean_squared_error",
                                     optimizer = keras::optimizer_rmsprop( # Hier sieht man wie in der Arbeit erlaeutert, das RMSPROP verwendet wird
                                       lr = param$lr,
                                       rho = param$rho,
                                       decay = param$decay
                                     ),
                                     metrics = "mean_squared_error"
                                   )
                               }
                               model %>% keras::fit(
                                 x = x, 
                                 y = y,
                                 batch_size = param$batch_size,
                                 ...
                               )
                               if(last)
                                 model <- keras::serialize_model(model)
                               list(object = model)
                             },
                             predict = function(modelFit, newdata, submodels = NULL) {
                               if(inherits(modelFit$object, "raw"))
                                 modelFit$object <- keras::unserialize_model(modelFit$object)
                               if(!is.matrix(newdata)) 
                                 newdata <- as.matrix(newdata)
                               out <- predict(modelFit$object, newdata)
                               ## check for model type
                               if(ncol(out) == 1) {
                                 out <- out[, 1]
                               } else {
                                 out <- modelFit$obsLevels[apply(out, 1, which.max)]
                               }
                               out
                             },
                             prob =  function(modelFit, newdata, submodels = NULL) {
                               if(inherits(modelFit$object, "raw"))
                                 modelFit$object <- keras::unserialize_model(modelFit$object)
                               if(!is.matrix(newdata)) 
                                 newdata <- as.matrix(newdata)
                               out <- predict(modelFit$object, newdata)
                               colnames(out) <- modelFit$obsLevels
                               as.data.frame(out, stringsAsFactors = TRUE)
                             },
                             varImp = NULL,
                             tags = c("Neural Network", "L2 Regularization"),
                             # sort = function(x) x[order(x$size, x$size2, -x$lambda),],
                             notes = paste("After `train` completes, the keras model object is serialized",
                                           "so that it can be used between R session. When predicting, the", 
                                           "code will temporarily unsearalize the object. To make the", 
                                           "predictions more efficient, the user might want to use ", 
                                           "`keras::unsearlize_model(object$finalModel$object)` in the current", 
                                           "R session so that that operation is only done once.",
                                           "Also, this model cannot be run in parallel due to",
                                           "the nature of how tensorflow does the computations.",
                                           
                                           "Unlike other packages used by `train`, the `dplyr`",
                                           "package is fully loaded when this model is used."),
                             check = function(pkg) {
                               testmod <- try(keras::keras_model_sequential(),
                                              silent = TRUE)
                               if(inherits(testmod, "try-error"))
                                 stop("Could not start a sequential model. ",
                                      "`tensorflow` might not be installed. ",
                                      "See `?install_tensorflow`.", 
                                      call. = FALSE)
                               TRUE
                             })


# Drei Hidden Layers
mlpKerasDecayL3 <-      list(label = "Multilayer Perceptron Network with Weight Decay",
                             library = "keras",
                             loop = NULL,
                             type = c('Regression', "Classification"),
                             parameters = data.frame(
                               parameter = c('lambda', "batch_size", "lr", "rho", 
                                             "decay", "activation"),
                               class = c(rep('numeric', 5), "character"),
                               label = c('L2 Regularization', "Batch Size", "Learning Rate", "Rho", 
                                         "Learning Rate Decay", "Activation Function")
                             ),
                             grid = function(x, y, len = NULL, search = "grid") {
                               afuncs <- c("sigmoid", "relu", "tanh")
                               if(search == "grid") {
                                 out <- expand.grid(
                                   lambda = c(0, 10 ^ seq(-1, -4, length = len - 1)), 
                                   batch_size = floor(nrow(x)/3),
                                   lr = 2e-6,
                                   rho = .9,
                                   decay = 0,
                                   activation = "relu"
                                 )
                               } else {
                                 n <- nrow(x)
                                 out <- data.frame(
                                   size = sample(2:20, replace = TRUE, size = len),
                                   lambda = 10^runif(len, min = -5, 1),
                                   batch_size = floor(n*runif(len, min = .1)),
                                   lr = runif(len),
                                   rho = runif(len),
                                   decay = 10^runif(len, min = -5, 0),
                                   activation = sample(
                                     afuncs, 
                                     size = len, 
                                     replace = TRUE
                                   )
                                 )
                               }
                               out
                             },
                             fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                               require(dplyr)
                               K <- keras::backend()
                               K$clear_session()
                               if(!is.matrix(x)) x <- as.matrix(x)
                               model <- keras::keras_model_sequential()
                               model %>% 
                                 keras::layer_dense(
                                   units = 8, 
                                   activation = as.character(param$activation), 
                                   input_shape = ncol(x),
                                   kernel_initializer = keras::initializer_glorot_uniform(),
                                   kernel_regularizer = keras::regularizer_l2(param$lambda)
                                 ) %>%
                                 layer_batch_normalization() %>% # Batch normalization
                                 keras::layer_dense(
                                   units = 4, 
                                   activation = as.character(param$activation),
                                   kernel_initializer = keras::initializer_glorot_uniform(),
                                   kernel_regularizer = keras::regularizer_l2(param$lambda)
                                 ) %>%
                                 layer_batch_normalization() %>% # Batch normalization
                                 keras::layer_dense(
                                   units = 2, 
                                   activation = as.character(param$activation),
                                   kernel_initializer = keras::initializer_glorot_uniform(),
                                   kernel_regularizer = keras::regularizer_l2(param$lambda)
                                 ) %>%
                                 layer_batch_normalization() # Batch normalization
                               if(is.factor(y)) {
                                 y <- class2ind(y)
                                 model %>% 
                                   keras::layer_dense(
                                     units = length(lev), 
                                     activation = 'softmax',
                                     kernel_regularizer = keras::regularizer_l2(param$lambda)
                                   ) %>% keras::compile(
                                     loss = "categorical_crossentropy",
                                     optimizer = keras::optimizer_rmsprop(
                                       lr = param$lr,
                                       rho = param$rho,
                                       decay = param$decay
                                     ),
                                     metrics = "accuracy"
                                   )
                               } else {
                                 model %>% 
                                   keras::layer_dense(
                                     units = 1, 
                                     activation = 'linear',
                                     kernel_regularizer = keras::regularizer_l2(param$lambda)
                                   ) %>% keras::compile(
                                     loss = "mean_squared_error",
                                     optimizer = keras::optimizer_rmsprop( # Hier sieht man wie in der Arbeit erlaeutert, das RMSPROP verwendet wird
                                       lr = param$lr,
                                       rho = param$rho,
                                       decay = param$decay
                                     ),
                                     metrics = "mean_squared_error"
                                   )
                               }
                               model %>% keras::fit(
                                 x = x, 
                                 y = y,
                                 batch_size = param$batch_size,
                                 ...
                               )
                               if(last)
                                 model <- keras::serialize_model(model)
                               list(object = model)
                             },
                             predict = function(modelFit, newdata, submodels = NULL) {
                               if(inherits(modelFit$object, "raw"))
                                 modelFit$object <- keras::unserialize_model(modelFit$object)
                               if(!is.matrix(newdata)) 
                                 newdata <- as.matrix(newdata)
                               out <- predict(modelFit$object, newdata)
                               ## check for model type
                               if(ncol(out) == 1) {
                                 out <- out[, 1]
                               } else {
                                 out <- modelFit$obsLevels[apply(out, 1, which.max)]
                               }
                               out
                             },
                             prob =  function(modelFit, newdata, submodels = NULL) {
                               if(inherits(modelFit$object, "raw"))
                                 modelFit$object <- keras::unserialize_model(modelFit$object)
                               if(!is.matrix(newdata)) 
                                 newdata <- as.matrix(newdata)
                               out <- predict(modelFit$object, newdata)
                               colnames(out) <- modelFit$obsLevels
                               as.data.frame(out, stringsAsFactors = TRUE)
                             },
                             varImp = NULL,
                             tags = c("Neural Network", "L2 Regularization"),
                             # sort = function(x) x[order(x$size, x$size2, -x$lambda),],
                             notes = paste("After `train` completes, the keras model object is serialized",
                                           "so that it can be used between R session. When predicting, the", 
                                           "code will temporarily unsearalize the object. To make the", 
                                           "predictions more efficient, the user might want to use ", 
                                           "`keras::unsearlize_model(object$finalModel$object)` in the current", 
                                           "R session so that that operation is only done once.",
                                           "Also, this model cannot be run in parallel due to",
                                           "the nature of how tensorflow does the computations.",
                                           
                                           "Unlike other packages used by `train`, the `dplyr`",
                                           "package is fully loaded when this model is used."),
                             check = function(pkg) {
                               testmod <- try(keras::keras_model_sequential(),
                                              silent = TRUE)
                               if(inherits(testmod, "try-error"))
                                 stop("Could not start a sequential model. ",
                                      "`tensorflow` might not be installed. ",
                                      "See `?install_tensorflow`.", 
                                      call. = FALSE)
                               TRUE
                             })

############################################################ Ab hier nur noch Funktionen und Prediction Betas
# Funktionen fuer die einzelnen NNs
NN1_pred1 <- function(x) {
  y_oos_test <- oos_test(x)
  
  pred <- predict(NN1_1, y_oos_test)
  
  return(pred)
}

NN1_pred2 <- function(x) {
  y_oos_test <- oos_test(x)
  
  pred <- predict(NN1_2, y_oos_test)
  
  return(pred)
}

NN1_pred3 <- function(x) {
  y_oos_test <- oos_test(x)
  
  pred <- predict(NN1_3, y_oos_test)
  
  return(pred)
}

NN1_pred4 <- function(x) {
  y_oos_test <- oos_test(x)
  
  pred <- predict(NN1_4, y_oos_test)
  
  return(pred)
}

NN1_pred5 <- function(x) {
  y_oos_test <- oos_test(x)
  
  pred <- predict(NN1_5, y_oos_test)
  
  return(pred)
}

# NN2 - Predictions!

NN2_pred1 <- function(x) {
  y_oos_test <- oos_test(x)
  
  pred <- predict(NN2_1, y_oos_test)
  
  return(pred)
}

NN2_pred2 <- function(x) {
  y_oos_test <- oos_test(x)
  
  pred <- predict(NN2_2, y_oos_test)
  
  return(pred)
}

NN2_pred3 <- function(x) {
  y_oos_test <- oos_test(x)
  
  pred <- predict(NN2_3, y_oos_test)
  
  return(pred)
}

NN2_pred4 <- function(x) {
  y_oos_test <- oos_test(x)
  
  pred <- predict(NN2_4, y_oos_test)
  
  return(pred)
}

NN2_pred5 <- function(x) {
  y_oos_test <- oos_test(x)
  
  pred <- predict(NN2_5, y_oos_test)
  
  return(pred)
}

# NN3 - Predictions!!
NN3_pred1 <- function(x) {
  y_oos_test <- oos_test(x)
  
  pred <- predict(NN3_1, y_oos_test)
  
  return(pred)
}

NN3_pred2 <- function(x) {
  y_oos_test <- oos_test(x)
  
  pred <- predict(NN3_2, y_oos_test)
  
  return(pred)
}

NN3_pred3 <- function(x) {
  y_oos_test <- oos_test(x)
  
  pred <- predict(NN3_3, y_oos_test)
  
  return(pred)
}

NN3_pred4 <- function(x) {
  y_oos_test <- oos_test(x)
  
  pred <- predict(NN3_4, y_oos_test)
  
  return(pred)
}

NN3_pred5 <- function(x) {
  y_oos_test <- oos_test(x)
  
  pred <- predict(NN3_5, y_oos_test)
  
  return(pred)
}

# Hier beginnt die Prognose:
# Unterschiedliche Neural Networks prognistizieren mit unterschiedlichen Seeds:
# NN1 - Modelle 
tic()
set.seed(199527)
NN1_1 <- NN1_beta(test_transform2)
toc()

tic()
set.seed(39955)
NN1_2 <- NN1_beta(test_transform2)
toc()

tic()
set.seed(801983)
NN1_3 <- NN1_beta(test_transform2)
toc()

tic()
set.seed(22664)
NN1_4 <- NN1_beta(test_transform2)
toc()

tic()
set.seed(391995)
NN1_5 <- NN1_beta(test_transform2)
toc()

# NN2 Modelle
tic()
set.seed(199527)
NN2_1 <- NN2_beta(test_transform2)
toc()

tic()
set.seed(39955)
NN2_2 <- NN2_beta(test_transform2)
toc()

tic()
set.seed(801983)
NN2_3 <- NN2_beta(test_transform2)
toc()

tic()
set.seed(22664)
NN2_4 <- NN2_beta(test_transform2)
toc()

tic()
set.seed(391995)
NN2_5 <- NN2_beta(test_transform2)
toc()

# NN3 - Modelle
tic()
set.seed(199527)
NN3_1 <- NN3_beta(test_transform2)
toc()

tic()
set.seed(39955)
NN3_2 <- NN3_beta(test_transform2)
toc()

tic()
set.seed(801983)
NN3_3 <- NN3_beta(test_transform2)
toc()

tic()
set.seed(22664)
NN3_4 <- NN3_beta(test_transform2)
toc()

tic()
set.seed(391995)
NN3_5 <- NN3_beta(test_transform2)
toc()

saveRDS(NN1_1, "NN1_1.RDS")
saveRDS(NN1_2, "NN1_2.RDS")
saveRDS(NN1_3, "NN1_3.RDS")
saveRDS(NN1_4, "NN1_4.RDS")
saveRDS(NN1_5, "NN1_5.RDS")
saveRDS(NN2_1, "NN2_1.RDS")
saveRDS(NN2_2, "NN2_2.RDS")
saveRDS(NN2_3, "NN2_3.RDS")
saveRDS(NN2_4, "NN2_4.RDS")
saveRDS(NN2_5, "NN2_5.RDS")
saveRDS(NN3_1, "NN3_1.RDS")
saveRDS(NN3_2, "NN3_2.RDS")
saveRDS(NN3_3, "NN3_3.RDS")
saveRDS(NN3_4, "NN3_4.RDS")
saveRDS(NN3_5, "NN3_5.RDS")

# Predictions der einzelnen NNs
# NN1 - Predictions!!
NN1_betaUN1 <- test_transform2 %>%  
  group_by(lpermno) %>%
  do(NN1 = NN1_pred1(.[,1:45])) %>%
  spread(key = lpermno, value = NN1) %>%
  unnest(cols=everything()) 

NN1_betaUN2 <- test_transform2 %>%  
  group_by(lpermno) %>%
  do(NN1 = NN1_pred2(.[,1:45])) %>%
  spread(key = lpermno, value = NN1) %>%
  unnest(cols=everything())

NN1_betaUN3 <- test_transform2 %>%  
  group_by(lpermno) %>%
  do(NN1 = NN1_pred3(.[,1:45])) %>%
  spread(key = lpermno, value = NN1) %>%
  unnest(cols=everything())

NN1_betaUN4 <- test_transform2 %>%  
  group_by(lpermno) %>%
  do(NN1 = NN1_pred4(.[,1:45])) %>%
  spread(key = lpermno, value = NN1) %>%
  unnest(cols=everything())

NN1_betaUN5 <- test_transform2 %>%  
  group_by(lpermno) %>%
  do(NN1 = NN1_pred5(.[,1:45])) %>%
  spread(key = lpermno, value = NN1) %>%
  unnest(cols=everything())

# NN2 - Predictions 
NN2_betaUN1 <- test_transform2 %>%  
  group_by(lpermno) %>%
  do(NN2 = NN2_pred1(.[,1:45])) %>%
  spread(key = lpermno, value = NN2) %>%
  unnest(cols=everything()) 

NN2_betaUN2 <- test_transform2 %>%  
  group_by(lpermno) %>%
  do(NN2 = NN2_pred2(.[,1:45])) %>%
  spread(key = lpermno, value = NN2) %>%
  unnest(cols=everything())

NN2_betaUN3 <- test_transform2 %>%  
  group_by(lpermno) %>%
  do(NN2 = NN2_pred3(.[,1:45])) %>%
  spread(key = lpermno, value = NN2) %>%
  unnest(cols=everything())

NN2_betaUN4 <- test_transform2 %>%  
  group_by(lpermno) %>%
  do(NN2 = NN2_pred4(.[,1:45])) %>%
  spread(key = lpermno, value = NN2) %>%
  unnest(cols=everything())

NN2_betaUN5 <- test_transform2 %>%  
  group_by(lpermno) %>%
  do(NN2 = NN2_pred5(.[,1:45])) %>%
  spread(key = lpermno, value = NN2) %>%
  unnest(cols=everything())

# NN3 - Predictions 
NN3_betaUN1 <- test_transform2 %>%  
  group_by(lpermno) %>%
  do(NN3 = NN3_pred1(.[,1:45])) %>%
  spread(key = lpermno, value = NN3) %>%
  unnest(cols=everything()) 

NN3_betaUN2 <- test_transform2 %>%  
  group_by(lpermno) %>%
  do(NN3 = NN3_pred2(.[,1:45])) %>%
  spread(key = lpermno, value = NN3) %>%
  unnest(cols=everything())

NN3_betaUN3 <- test_transform2 %>%  
  group_by(lpermno) %>%
  do(NN3 = NN3_pred3(.[,1:45])) %>%
  spread(key = lpermno, value = NN3) %>%
  unnest(cols=everything())

NN3_betaUN4 <- test_transform2 %>%  
  group_by(lpermno) %>%
  do(NN3 = NN3_pred4(.[,1:45])) %>%
  spread(key = lpermno, value = NN3) %>%
  unnest(cols=everything())

NN3_betaUN5 <- test_transform2 %>%  
  group_by(lpermno) %>%
  do(NN3 = NN3_pred5(.[,1:45])) %>%
  spread(key = lpermno, value = NN3) %>%
  unnest(cols=everything())

# Berechnung des Durchschnitts ueber die fuenf NNs fuer jedes Unternehmen
ensemble5_NN1 <- cbind(NN1_betaUN1,NN1_betaUN2,NN1_betaUN3,NN1_betaUN4,NN1_betaUN5)
ensemble5_NN2 <- cbind(NN2_betaUN1,NN2_betaUN2,NN2_betaUN3,NN2_betaUN4,NN2_betaUN5)
ensemble5_NN3 <- cbind(NN3_betaUN1,NN3_betaUN2,NN3_betaUN3,NN3_betaUN4,NN3_betaUN5)

# Datum als Datensatz 
date <- bigdata %>% 
  select(date) %>%
  distinct() %>% 
  filter(year(date) > 2013, year(date) < 2020)

# Funktion um den Durchschnitt fuer jedes Unternehmen und fuer jeden Zeitpunkt zu berechnen 
ensemble <- function(x) {
  
  ensemble2 <- cbind(date, x)
  
  sortedensemble <- pivot_longer(ensemble2,
                                 cols = -date,
                                 names_to = "lpermno",
                                 values_to = "value")
  
  average <-  sortedensemble %>%
    group_by(date, lpermno) %>%
    summarise(mean(value))
  
  
  average_wider <- pivot_wider(average,
                               names_from = "lpermno",
                               values_from = "mean(value)")
  
  average_wider <- average_wider[,2:403]
  
  return(average_wider)
}

# Berechnung der Ensemble-Ergebnisse
ensembleNN1 <- ensemble(ensemble5_NN1)
ensembleNN2 <- ensemble(ensemble5_NN2)
ensembleNN3 <- ensemble(ensemble5_NN3)

# Abspeichern der Ensemble-Ergebnisse
saveRDS(ensembleNN1, "ensembleNN1.RDS")
saveRDS(ensembleNN2, "ensembleNN2.RDS")
saveRDS(ensembleNN3, "ensembleNN3.RDS")

# Loeschen nicht benoetigter Dinge
rm(NN1_1, NN1_2,NN1_3,NN1_4,NN1_5,NN2_1,NN2_2,NN2_3,NN2_4,NN2_5,NN3_1,NN3_2,NN3_3,NN3_4,NN3_5, NN1_betaUN1,
   NN1_betaUN2,NN1_betaUN3,NN1_betaUN4,NN1_betaUN5,NN2_betaUN1,NN2_betaUN2,NN2_betaUN3,NN2_betaUN4,
   NN2_betaUN5,NN3_betaUN1,NN3_betaUN2,NN3_betaUN3,NN3_betaUN4,NN3_betaUN5)

############################################################ Ab hier nur noch Funktionen und Prediction Aktienkurse

###################### Funktionen fuer die einzelnen NNs

NN1_pred1_mprc <- function(x) {
  y_oos_test <- oos_test_mprc(x)
  
  pred <- predict(NN1_1_mprc, y_oos_test)
  
  return(pred)
}

NN1_pred2_mprc <- function(x) {
  y_oos_test <- oos_test_mprc(x)
  
  pred <- predict(NN1_2_mprc, y_oos_test)
  
  return(pred)
}

NN1_pred3_mprc <- function(x) {
  y_oos_test <- oos_test_mprc(x)
  
  pred <- predict(NN1_3_mprc, y_oos_test)
  
  return(pred)
}

NN1_pred4_mprc <- function(x) {
  y_oos_test <- oos_test_mprc(x)
  
  pred <- predict(NN1_4_mprc, y_oos_test)
  
  return(pred)
}

NN1_pred5_mprc <- function(x) {
  y_oos_test <- oos_test_mprc(x)
  
  pred <- predict(NN1_5_mprc, y_oos_test)
  
  return(pred)
}


# NN2 - Predictions!

NN2_pred1_mprc <- function(x) {
  y_oos_test <- oos_test_mprc(x)
  
  pred <- predict(NN2_1_mprc, y_oos_test)
  
  return(pred)
}

NN2_pred2_mprc <- function(x) {
  y_oos_test <- oos_test_mprc(x)
  
  pred <- predict(NN2_2_mprc, y_oos_test)
  
  return(pred)
}

NN2_pred3_mprc <- function(x) {
  y_oos_test <- oos_test_mprc(x)
  
  pred <- predict(NN2_3_mprc, y_oos_test)
  
  return(pred)
}

NN2_pred4_mprc <- function(x) {
  y_oos_test <- oos_test_mprc(x)
  
  pred <- predict(NN2_4_mprc, y_oos_test)
  
  return(pred)
}

NN2_pred5_mprc <- function(x) {
  y_oos_test <- oos_test_mprc(x)
  
  pred <- predict(NN2_5_mprc, y_oos_test)
  
  return(pred)
}

# NN3 - Predictions!!
NN3_pred1_mprc <- function(x) {
  y_oos_test <- oos_test_mprc(x)
  
  pred <- predict(NN3_1_mprc, y_oos_test)
  
  return(pred)
}

NN3_pred2_mprc <- function(x) {
  y_oos_test <- oos_test_mprc(x)
  
  pred <- predict(NN3_2_mprc, y_oos_test)
  
  return(pred)
}

NN3_pred3_mprc <- function(x) {
  y_oos_test <- oos_test_mprc(x)
  
  pred <- predict(NN3_3_mprc, y_oos_test)
  
  return(pred)
}

NN3_pred4_mprc <- function(x) {
  y_oos_test <- oos_test_mprc(x)
  
  pred <- predict(NN3_4_mprc, y_oos_test)
  
  return(pred)
}

NN3_pred5_mprc <- function(x) {
  y_oos_test <- oos_test_mprc(x)
  
  pred <- predict(NN3_5_mprc, y_oos_test)
  
  return(pred)
}

################################################### Hier beginnt die Prognose:

# Unterschiedliche Neural Networks prognistizieren mit unterschiedlichen Seeds:

# NN1 - Modelle 
tic()
set.seed(199527)
NN1_1_mprc <- NN1_mprc(test_transform_mprc2)
toc()

tic()
set.seed(39955)
NN1_2_mprc <- NN1_mprc(test_transform_mprc2)
toc()

tic()
set.seed(801983)
NN1_3_mprc <- NN1_mprc(test_transform_mprc2)
toc()

tic()
set.seed(22664)
NN1_4_mprc <- NN1_mprc(test_transform_mprc2)
toc()

tic()
set.seed(391995)
NN1_5_mprc <- NN1_mprc(test_transform_mprc2)
toc()

# NN2 Modelle
tic()
set.seed(199527)
NN2_1_mprc <- NN2_mprc(test_transform_mprc2)
toc()

tic()
set.seed(39955)
NN2_2_mprc <- NN2_mprc(test_transform_mprc2)
toc()

tic()
set.seed(801983)
NN2_3_mprc <- NN2_mprc(test_transform_mprc2)
toc()

tic()
set.seed(22664)
NN2_4_mprc <- NN2_mprc(test_transform_mprc2)
toc()

tic()
set.seed(391995)
NN2_5_mprc <- NN2_mprc(test_transform_mprc2)
toc()

# NN3 - Modelle
tic()
set.seed(199527)
NN3_1_mprc <- NN3_mprc(test_transform_mprc2)
toc()

tic()
set.seed(39955)
NN3_2_mprc <- NN3_mprc(test_transform_mprc2)
toc()

tic()
set.seed(801983)
NN3_3_mprc <- NN3_mprc(test_transform_mprc2)
toc()

tic()
set.seed(22664)
NN3_4_mprc <- NN3_mprc(test_transform_mprc2)
toc()

tic()
set.seed(391995)
NN3_5_mprc <- NN3_mprc(test_transform_mprc2)
toc()

# Abspeichern der Modelle
saveRDS(NN1_1_mprc, "NN1_1_mprc.RDS")
saveRDS(NN1_2_mprc, "NN1_2_mprc.RDS")
saveRDS(NN1_3_mprc, "NN1_3_mprc.RDS")
saveRDS(NN1_4_mprc, "NN1_4_mprc.RDS")
saveRDS(NN1_5_mprc, "NN1_5_mprc.RDS")
saveRDS(NN2_1_mprc, "NN2_1_mprc.RDS")
saveRDS(NN2_2_mprc, "NN2_2_mprc.RDS")
saveRDS(NN2_3_mprc, "NN2_3_mprc.RDS")
saveRDS(NN2_4_mprc, "NN2_4_mprc.RDS")
saveRDS(NN2_5_mprc, "NN2_5_mprc.RDS")
saveRDS(NN3_1_mprc, "NN3_1_mprc.RDS")
saveRDS(NN3_2_mprc, "NN3_2_mprc.RDS")
saveRDS(NN3_3_mprc, "NN3_3_mprc.RDS")
saveRDS(NN3_4_mprc, "NN3_4_mprc.RDS")
saveRDS(NN3_5_mprc, "NN3_5_mprc.RDS")

########################################################### Predictions der einzelnen NNs
# NN1 - Predictions durch die Anwendung der Modelle auf die Testdaten!!
NN1_betaUN1_mprc <- test_transform_mprc2 %>%  
  group_by(lpermno) %>%
  do(NN1 = NN1_pred1_mprc(.[,1:45])) %>%
  spread(key = lpermno, value = NN1) %>%
  unnest(cols=everything())

NN1_betaUN2_mprc <- test_transform_mprc2 %>%  
  group_by(lpermno) %>%
  do(NN1 = NN1_pred2_mprc(.[,1:45])) %>%
  spread(key = lpermno, value = NN1) %>%
  unnest(cols=everything())

NN1_betaUN3_mprc <- test_transform_mprc2 %>%  
  group_by(lpermno) %>%
  do(NN1 = NN1_pred3_mprc(.[,1:45])) %>%
  spread(key = lpermno, value = NN1) %>%
  unnest(cols=everything())

NN1_betaUN4_mprc <- test_transform_mprc2 %>%  
  group_by(lpermno) %>%
  do(NN1 = NN1_pred4_mprc(.[,1:45])) %>%
  spread(key = lpermno, value = NN1) %>%
  unnest(cols=everything())

NN1_betaUN5_mprc <- test_transform_mprc2 %>%  
  group_by(lpermno) %>%
  do(NN1 = NN1_pred5_mprc(.[,1:45])) %>%
  spread(key = lpermno, value = NN1) %>%
  unnest(cols=everything())

# NN2 - Predictions 
NN2_betaUN1_mprc <- test_transform_mprc2 %>%  
  group_by(lpermno) %>%
  do(NN2 = NN2_pred1_mprc(.[,1:45])) %>%
  spread(key = lpermno, value = NN2) %>%
  unnest(cols=everything()) 

NN2_betaUN2_mprc <- test_transform_mprc2 %>%  
  group_by(lpermno) %>%
  do(NN2 = NN2_pred2_mprc(.[,1:45])) %>%
  spread(key = lpermno, value = NN2) %>%
  unnest(cols=everything())

NN2_betaUN3_mprc <- test_transform_mprc2 %>%  
  group_by(lpermno) %>%
  do(NN2 = NN2_pred3_mprc(.[,1:45])) %>%
  spread(key = lpermno, value = NN2) %>%
  unnest(cols=everything())

NN2_betaUN4_mprc <- test_transform_mprc2 %>%  
  group_by(lpermno) %>%
  do(NN2 = NN2_pred4_mprc(.[,1:45])) %>%
  spread(key = lpermno, value = NN2) %>%
  unnest(cols=everything())

NN2_betaUN5_mprc <- test_transform_mprc2 %>%  
  group_by(lpermno) %>%
  do(NN2 = NN2_pred5_mprc(.[,1:45])) %>%
  spread(key = lpermno, value = NN2) %>%
  unnest(cols=everything())

# NN3 - Predictions 
NN3_betaUN1_mprc <- test_transform_mprc2 %>%  
  group_by(lpermno) %>%
  do(NN3 = NN3_pred1_mprc(.[,1:45])) %>%
  spread(key = lpermno, value = NN3) %>%
  unnest(cols=everything()) 

NN3_betaUN2_mprc <- test_transform_mprc2 %>%  
  group_by(lpermno) %>%
  do(NN3 = NN3_pred2_mprc(.[,1:45])) %>%
  spread(key = lpermno, value = NN3) %>%
  unnest(cols=everything())

NN3_betaUN3_mprc <- test_transform_mprc2 %>%  
  group_by(lpermno) %>%
  do(NN3 = NN3_pred3_mprc(.[,1:45])) %>%
  spread(key = lpermno, value = NN3) %>%
  unnest(cols=everything())

NN3_betaUN4_mprc <- test_transform_mprc2 %>%  
  group_by(lpermno) %>%
  do(NN3 = NN3_pred4_mprc(.[,1:45])) %>%
  spread(key = lpermno, value = NN3) %>%
  unnest(cols=everything())

NN3_betaUN5_mprc <- test_transform_mprc2 %>%  
  group_by(lpermno) %>%
  do(NN3 = NN3_pred5_mprc(.[,1:45])) %>%
  spread(key = lpermno, value = NN3) %>%
  unnest(cols=everything())


######################################### Berechnung des Durchschnitts ueber die fuenf NNs fuer jedes Unternehmen
ensemble5_NN1_mprc <- cbind(NN1_betaUN1_mprc,NN1_betaUN2_mprc,NN1_betaUN3_mprc,NN1_betaUN4_mprc,NN1_betaUN5_mprc)
ensemble5_NN2_mprc <- cbind(NN2_betaUN1_mprc,NN2_betaUN2_mprc,NN2_betaUN3_mprc,NN2_betaUN4_mprc,NN2_betaUN5_mprc)
ensemble5_NN3_mprc <- cbind(NN3_betaUN1_mprc,NN3_betaUN2_mprc,NN3_betaUN3_mprc,NN3_betaUN4_mprc,NN3_betaUN5_mprc)

# Datum als Datensatz 
date <- bigdata %>% 
  select(date) %>%
  distinct() %>% 
  filter(year(date) > 2013, year(date) < 2020)

# Funktion um den Durchschnitt fuer jedes Unternehmen und fuer jeden Zeitpunkt zu berechnen 
ensemble <- function(x) {
  
  ensemble2 <- cbind(date, x)
  
  sortedensemble <- pivot_longer(ensemble2,
                                 cols = -date,
                                 names_to = "lpermno",
                                 values_to = "value")
  
  average <-  sortedensemble %>%
    group_by(date, lpermno) %>%
    summarise(mean(value))
  
  
  average_wider <- pivot_wider(average,
                               names_from = "lpermno",
                               values_from = "mean(value)")
  
  average_wider <- average_wider[,2:403]
  
  return(average_wider)
}

# Berechnung der Ensemble-Ergebnisse durch Verwendung der ensemble-Funktion
ensembleNN1_mprc <- ensemble(ensemble5_NN1_mprc)
ensembleNN2_mprc <- ensemble(ensemble5_NN2_mprc)
ensembleNN3_mprc <- ensemble(ensemble5_NN3_mprc)

# Daten abspeichern:
saveRDS(ensembleNN1_mprc, "ensembleNN1_mprc.RDS")
saveRDS(ensembleNN2_mprc, "ensembleNN2_mprc.RDS")
saveRDS(ensembleNN3_mprc, "ensembleNN3_mprc.RDS") 
