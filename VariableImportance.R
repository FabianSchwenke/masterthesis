###########################################################################################################
# Package fuer die Visualisierung: 
###########################################################################################################

# Package danach wieder rausnehmen, da es Ueberschneidungen mit Funktionen von dplyr hat:
# install.packages("gridExtra")
library(gridExtra) 
 
###########################################################################################################
# Einlesen der abgespeicherten RDS-Dateien - sonst muesste man die Modelle immer wieder durchlaufen lassen
###########################################################################################################

# Einlesen der abgespeicherten Modelle: (alle Modelle durchlaufen zu lassen dauert sehr lange)
mlmmod <- readRDS("mlmmod_normal.RDS")
lassomod <- readRDS("lassomod_normal.RDS") 
bagmod <- readRDS("bagmod_normal.RDS")
rfmod <- readRDS("rfmod_normal.RDS")
sgbmod <- readRDS("sgbmod_normal.RDS")
xgbmod <- readRDS("xgbmod_normal.RDS") 

###########################################################################################################
# CAPM: Berechnung der Trainingsergebnisse!!
###########################################################################################################

# CAPM-Betas:
# Funktion von caret:
resamps <- resamples(list(LASSO = lassomod,
                          Bagging = bagmod,
                          RandomForest = rfmod,
                          GBM = sgbmod,
                          XGBoost = xgbmod))

trellis.par.set(caretTheme())
train1 <- dotplot(resamps, metric = "Rsquared")

trellis.par.set(caretTheme())
train2 <- dotplot(resamps, metric = "RMSE")

png(filename ="Train_CAPM.png", width=800, height=400)
gridExtra::grid.arrange(train1, train2, nrow = 1)
dev.off()

###########################################################################################################
# Aktienkurse: Berechnung der Trainingsergebnisse!!
###########################################################################################################

# Einlesen der abgespeicherten Modelle: (alle Modelle durchlaufen zu lassen dauert sehr lange)
mlmmodm <- readRDS("mlmmodm_normal.RDS")
lassomodm <- readRDS("lassomodm_normal.RDS")
bagmodm <- readRDS("bagmodm_normal.RDS")
rfmodm <- readRDS("rfmodm_normal.RDS")
sgbmodm <- readRDS("sgbmodm_normal.RDS")
xgbmodm <- readRDS("xgbmodm_normal.RDS")

resamps <- resamples(list(LASSO = lassomodm,
                          Bagging = bagmodm,
                          RandomForest = rfmodm,
                          GBM = sgbmodm,
                          XGBoost = xgbmodm))

trellis.par.set(caretTheme())
dotplot(resamps, metric = "Rsquared")
#train3 <- dotplot(resamps, metric = "Rsquared")

trellis.par.set(caretTheme())
dotplot(resamps, metric = "RMSE")
#train4 <- dotplot(resamps, metric = "RMSE")

png(filename ="Train_MPRC.png", width=800, height=400)
gridExtra::grid.arrange(train3, train4, nrow = 1)
dev.off()

###########################################################################################################
# CAPM-Betas: Berechnung der Variablen-Wichtigkeit
###########################################################################################################
# Fuer die Methoden OLS, LASSO, Bagging, RF, GBM, XGBoost ist es relativ simpel, da hier bereits Methoden integriert
# sind, die helfen das ganze umzusetzen:
#mlmimp <- varImp(mlmmod)
#lassoimp <- varImp(lassomod)

# Methoden muessen erst mit readRDS geladen werden:

bagimp <- varImp(bagmod, scale = TRUE)
rfimp <- varImp(rfmod, scale = TRUE)
sgbimp <- varImp(sgbmod, scale = TRUE)
xgbimp <- varImp(xgbmod, scale = TRUE)

# Habe die eigentlichen plots vom caret-Paket noch etwas abgeaendert, damit es mit meinen anderen Grafiken
# zusammenpasst
bagplot <- ggplot(bagimp, top = 10) +
  geom_bar(stat = "identity", fill = "#000000") +
  ylab("Variablen-Wichtigkeit (Bagging)") +
  theme(legend.position = "bottom", legend.title = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(color = "black"), axis.text = element_text(color = "black"),
        axis.title.y = element_blank())

rfplot <- ggplot(rfimp, top = 10) +
  geom_bar(stat = "identity", fill = "#000000") +
  ylab("Variablen-Wichtigkeit (Random Forest)") +
  theme(legend.position = "bottom", legend.title = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(color = "black"), axis.text = element_text(color = "black"),
        axis.title.y = element_blank())

gbmplot <- ggplot(sgbimp, top = 10) +
  geom_bar(stat = "identity", fill = "#000000") +
  ylab("Variablen-Wichtigkeit (GBM)") +
  theme(legend.position = "bottom", legend.title = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(color = "black"), axis.text = element_text(color = "black"),
        axis.title.y = element_blank())

xgbplot <- ggplot(xgbimp, top = 10) +
  geom_bar(stat = "identity", fill = "#000000") +
  ylab("Variablen-Wichtigkeit (XGBoost)") +
  theme(legend.position = "bottom", legend.title = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(color = "black"), axis.text = element_text(color = "black"),
        axis.title.y = element_blank())

png(filename ="Variablen-Wichtigkeit.png", width=800, height=400)
gridExtra::grid.arrange(bagplot, gbmplot, rfplot, xgbplot, nrow = 2)
dev.off()

###########################################################################################################
# CAPM-Betas: Berechnung der Variablen-Wichtigkeit - NEURONALE NETZE - Shapley-Wert!
###########################################################################################################

######################################################################### SHAPLEY-WERT

# Das package Shapper (wrapper vom Python-Paket SHAP) hat leider nicht funktioniert. Daher konnte ich nur
# die Shapley-Werte fuer einzelne Beobachtungen darstellen:

# Obwohl Date bei der Regression ausgeschlossen wird, macht es Probleme, daher fuehre ich hier nochmal ein NN
# durch ohne Date in den Daten.

# Muss nicht nochmal durchgefuehrt werden... Dauert ungefaehr 50 Minuten, deswegen als RDS-Datei gespeichert.
# Daten ohne Date
shap_data <- test_transform[,-1]

NN_shap_beta <- function(x){
  y_in_sample <- in_sample(x)
  
  NN2 <- caret::train(beta_12m ~ ., # hier nicht -date, da es jetzt nicht mehr in Daten ist!
                      data = na.omit(y_in_sample),
                      method = mlpKerasDecayL2,
                      trControl = cv_train,
                      tuneGrid = NN_grid,
                      epochs = 100,
                      verbose = FALSE
  )
} 

tic()
set.seed(199527)
NN_shap <- NN_shap_beta(shap_data)
toc()

# Hier wird die RDS-Datei abgespeichert!
saveRDS(NN_shap, "NN_shap.RDS")

# Daten aufbereiten:
beta <- in_sample(test_transform) 

beta <- beta[,-1]

X <- in_sample(test_transform) %>%
  select(-beta_12m)

X <- X[,-1]

# mit dem IML-Package
# install.packages("iml")
library(iml)

# Hier kann das Modell ohne date eingelesen werden!
NN_shap <- readRDS("NN_shap.RDS")

# Predictor auf Basis des iml-packages berechnen, bringt es in einer Form, sodass die weiteren Funktionen verwendet
# werden koennen
predictor <- Predictor$new(NN_shap, data = X, y = beta$beta_12m)

# Berechnung vereinzelter Shapley-Werte!
shap_pred <- Shapley$new(predictor, x.interest = X[10, ])
shap_pred2 <- Shapley$new(predictor, x.interest = X[10580, ])
shap_pred3 <- Shapley$new(predictor, x.interest = X[311, ])

# Da der iml-Plot nicht ins Masterarbeit-Design passt, habe ich hier Daten noch etwas anders aufbereitet.
# Diese Funktionen nimmt aus dem Shap-Environment die results. Anschließend werden die features und values
# auf Basis des Seperators = getrennt und dann die Dezimalstellen auf 4 geaendert (vorher sehr lange Zahlen)
# Abschließend werden sie wieder zusammengefuegt und nach dem Phi-Wert sortiert.
data_plot_shap <- function(data) {
  
  output <- as.data.frame(data$results) %>%
    separate(feature.value, c("feature2", "value"), sep = "=") %>%
    mutate(value = as.numeric(value)) %>%
    mutate(value = as.numeric(round(value, digits = 4))) %>%
    unite("feature.value", feature2, value, sep = "=") %>%
    arrange(desc(phi))
  
  return(output)
}

# Daten fuer die Grafiken mit der erlaeuterten Funktion generieren
shap_plot <- data_plot_shap(shap_pred)
shap_plot2 <- data_plot_shap(shap_pred2)
shap_plot3 <- data_plot_shap(shap_pred3)

# Plots 
plot1 <- ggplot(shap_plot, aes(x = phi, y = reorder(feature.value,phi))) +
  geom_bar(stat = "identity", fill = "black") +
  theme(legend.title = element_blank(),axis.title.y = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), axis.text = element_text(colour = "black"),
        legend.key = element_blank())

plot2 <- ggplot(shap_plot2, aes(x = phi, y = reorder(feature.value,phi))) + 
  geom_bar(stat = "identity", fill = "black") +
  theme(legend.title = element_blank(),axis.title.y = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), axis.text = element_text(colour = "black"),
        legend.key = element_blank())

plot3 <- ggplot(shap_plot3, aes(x = phi, y = reorder(feature.value,phi))) + 
  geom_bar(stat = "identity", fill = "black") +
  theme(legend.title = element_blank(),axis.title.y = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), axis.text = element_text(colour = "black"),
        legend.key = element_blank())

# zum Abschluss werden die Plots innerhalb einer Grafik dargestellt.
png(filename ="Shapley.png", width=800, height=400)
gridExtra::grid.arrange(plot1, plot2, plot3, nrow = 1)
dev.off()
