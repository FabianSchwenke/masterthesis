###########################################################################################################
# Vorhersage-Ergebnisse einlesen:
###########################################################################################################
## Betas
actual_beta <- readRDS("actual.RDS")
mlm_beta <- readRDS("mlm_betaUN_normal.RDS")
lasso_beta <- readRDS("lasso_betaUN_normal.RDS")  
bag_beta <- readRDS("bag_betaUN_normal.RDS") 
rf_beta <- readRDS("rf_betaUN_normal.RDS")
sgb_beta <- readRDS("sgb_betaUN_normal.RDS")
xgb_beta <- readRDS("xgb_betaUN_normal.RDS")
NN1_beta <- readRDS("ensembleNN1_transform.RDS")
NN2_beta <- readRDS("ensembleNN2_transform.RDS") 
NN3_beta <- readRDS("ensembleNN3_transform.RDS") 

# Aktienkurse:
actual_mprc <- mprc_actual2
mlm_mprc <- readRDS("mlm_mprcUN_normal.RDS")
lasso_mprc <- readRDS("lasso_mprcUN_normal.RDS")
bag_mprc <- readRDS("bag_mprcUN_normal.RDS")
rf_mprc <- readRDS("rf_mprcUN_normal.RDS")
sgb_mprc <- readRDS("sgb_mprcUN_normal.RDS")
xgb_mprc <- readRDS("xgb_mprcUN_normal.RDS")
NN1_mprc <- readRDS("ensembleNN1_mprc_transform.RDS")
NN2_mprc <- readRDS("ensembleNN2_mprc_transform.RDS")
NN3_mprc <- readRDS("ensembleNN3_mprc_transform.RDS")

###########################################################################################################
# Funktionen kann man einfach durchlaufen lassen:
###########################################################################################################

#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# IM WEITEREN STEHT x IMMER FUER DATA in den Funktionen!!!!!!
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

# Funktion, um die Dateien fuer anderen Datenaufbau zu generieren:
generate <- function(x) {
  # mit Datum verbinden, um pivot longer anwenden zu koenne
  output <- cbind(date2, x)
  
  output <- output %>%
    pivot_longer(-date)
  
  output <- output %>%
    transmute(date = ymd(date),
              lpermno = as.numeric(name),
              prediction = as.numeric(value))
  
  return(output)
}

generate_annual <- function(x) {
  # sortieren nach lpermno und date, um sicher zu gehen, dass alle die gleiche Reihenfolge haben, um sie spaeter
  # zusammenzuefuegen
  output <- x %>%
    arrange(lpermno, date)
  
  output <- output[,2:3]
  
  return(output)
}

# Funktion fuer die Berechnung der Korrelationen:
correlation <- function(x,y) { 
  
  output <- left_join(y,x, by=c("date","lpermno"))
  
  names(output)[3] <- "actual"
  names(output)[4] <- "prediction"
  
  output <- output %>%
    group_by(date) %>%
    summarise(cor(actual, prediction))
  
  names(output)[2] <- deparse(substitute(x))
  
  output <- output %>%
    select(-date)
  
  return(output)
}

# Funktion fuer die Berechnung der Bestimmtheitsmasse
rsq_data <- function(x,y) { 
  
  output <- left_join(y,x, by=c("date","lpermno"))
  
  names(output)[3] <- "actual"
  names(output)[4] <- "prediction"
  
  output <- output %>%
    group_by(date) %>%
    summarise(cor(actual, prediction)^2)
  
  names(output)[2] <- deparse(substitute(x))
  
  output <- output %>%
    select(-date)
  
  return(output)
}

# Funktion fuer die Berechnung der MSE-Werte
mse_data <- function(x,y) { 
  
  output <- left_join(y,x, by=c("date","lpermno"))
  
  names(output)[3] <- "actual"
  names(output)[4] <- "prediction"
  
  output <- output %>%
    mutate(se = as.numeric((actual-prediction)^2))
  
  output <- output %>%
    group_by(date) %>%
    summarise(mean(se))
  
  names(output)[2] <- deparse(substitute(x))
  
  output <- output %>%
    select(-date)
  
  return(output)
}

# Funktion fuer die Berechnung der Genauigkeit
compare <- function(x,y) {
  c <- ifelse(x > 1 & y > 1 | x < 1 & y < 1, 1, 0) 
  s <- sum(c)
  p <- (s/402) # hier schauen, je nachdem wie viele Unternehmen man hat (koennte auch nrow nehmen)
}

# Hier wird dann die compare Funktion mit angewendet, um die Vorhersage-Genauigkeit zu eroerten
accuracy_data <- function(x,y) { 
  
  output <- left_join(y,x, by=c("date","lpermno"))
  
  names(output)[3] <- "actual"
  names(output)[4] <- "prediction"
  
  output <- output %>%
    group_by(date) %>%
    summarise(compare(actual, prediction))
  
  names(output)[2] <- deparse(substitute(x))
  
  output <- output %>%
    select(-date)
  
  return(output)
}

# Korrelation auf jaehrlicher Basis
cor_annual <- function(x) {
  method <- x %>%
    rename("lpermno1" = "lpermno")
  
  method <- cbind(date, method)
  
  # Cross-Sectional Korrelation, indem ich die Monate aus dem Jahr 2019 herausnehme
  # (hier gibt es ja dann kein Jahr spaeter mehr)
  method <- method %>%
    filter(year(date) < 2019)
  
  # Fuegt date wieder hinzu, was ich wieder rausnehme
  method <- method[,2:3]
  
  # Gleiches gilt fuer Beta indem ich hier die Monate aus dem Jahr 2014 herausnehme
  beta <- cbind(date, beta_12m_mean)
  
  beta <- beta %>%
    filter(year(date) > 2014)
  
  beta <- beta[,2:3]
  
  # Dann generiere ich einen Datum-Vektor, der innerhalb der Grafik angezeigt wird
  inter <- scaled_data %>%
    select(date) %>%
    filter(year(date) > 2014)
  
  # Dann kombiniere ich die beiden dfs miteinander
  output <- cbind(beta, method) %>%
    select(-lpermno1)
  
  # Ergaenze hier das Datum (2015 - 2019). Ich habe eine monatliche Vorhersage, die ich mit dem beta_12m ein Jahr
  # voraus vergleichen moechte. Daher habe ich bei method das Jahr 2019 herausgenommen, weil es kein Jahr 2020 gibt
  # mit dem es verglichen werden kann. Bei den tatsaechlichen Betas habe ich das Jahr 2014 herausgenommen, da es
  # kein Jahr 2013 gibt. Somit sind dann die jeweiligen Betas immer auf einer hoehe, sodass ich eine Cross-Sectional
  # Korrelation messen kann!
  output2 <- cbind(inter, output)
  
  names(output2)[4] <- "prediction"
  
  # Hier wird dann die Korrelation ueber alle Unternehmen berechnet.
  final <- output2 %>%
    group_by(date) %>%
    summarise(cor(beta_12m, prediction))
  
  names(final)[2] <- deparse(substitute(x))
  
  final <- final %>%
    select(-date)
  
  return(final)
}

# Genauigkeit auf jaehrlicher Basis
acc_annual <- function(x) {
  method <- x %>%
    rename("lpermno1" = "lpermno")
  
  method <- cbind(date, method)
  
  # Cross-Sectional Korrelation, indem ich die Monate aus dem Jahr 2019 herausnehme
  # (hier gibt es ja dann kein Jahr spaeter mehr)
  method <- method %>%
    filter(year(date) < 2019)
  
  # Fuegt date wieder hinzu, was ich wieder rausnehme
  method <- method[,2:3]
  
  # Gleiches gilt fuer Beta indem ich hier die Monate aus dem Jahr 2014 herausnehme
  beta <- cbind(date, beta_12m_mean)
  
  beta <- beta %>%
    filter(year(date) > 2014)
  
  beta <- beta[,2:3]
  
  # Dann generiere ich einen Datum-Vektor, der innerhalb der Grafik angezeigt wird
  inter <- scaled_data %>%
    select(date) %>%
    filter(year(date) > 2014)
  
  # Dann kombiniere ich die beiden dfs miteinander
  output <- cbind(beta, method) %>%
    select(-lpermno1)
  
  # Ergaenze hier das Datum (2015 - 2019). Ich habe eine monatliche Vorhersage, die ich mit dem beta_12m ein Jahr
  # voraus vergleichen moechte. Daher habe ich bei method das Jahr 2019 herausgenommen, weil es kein Jahr 2020 gibt
  # mit dem es verglichen werden kann. Bei den tatsaechlichen Betas habe ich das Jahr 2014 herausgenommen, da es
  # kein Jahr 2013 gibt. Somit sind dann die jeweiligen Betas immer auf einer hoehe, sodass ich eine Cross-Sectional
  # Korrelation messen kann!
  output2 <- cbind(inter, output)
  
  names(output2)[4] <- "prediction"
  
  # Hier wird dann die Korrelation ueber alle Unternehmen berechnet.
  final <- output2 %>%
    group_by(date) %>%
    summarise(compare(beta_12m, prediction))
  
  names(final)[2] <- deparse(substitute(x))
  
  final <- final %>%
    select(-date)
  
  return(final)
}

###########################################################################################################
# Erstellung der Dateien - Datenaufbau muss etwas veraendert werden fuer die Berechnung der Performancemaße
###########################################################################################################
# Erstellung der Beta-Dateien, indem generate() verwendet wird
actual_cor <- generate(actual_beta)
mlm_cor <- generate(mlm_beta)
lasso_cor <- generate(lasso_beta)
bag_cor <- generate(bag_beta)
rf_cor <- generate(rf_beta)
sgb_cor <- generate(sgb_beta)
xgb_cor <- generate(xgb_beta)
NN1_cor <- generate(NN1_beta)
NN2_cor <- generate(NN2_beta)
NN3_cor <- generate(NN3_beta)

# Erstellung der Aktienkurs-Dateien, indem generate() verwendet wird
actual_corm <- generate(actual_mprc)
mlm_corm <- generate(mlm_mprc)
lasso_corm <- generate(lasso_mprc)
bag_corm <- generate(bag_mprc)
rf_corm <- generate(rf_mprc)
sgb_corm <- generate(sgb_mprc)
xgb_corm <- generate(xgb_mprc)
NN1_corm <- generate(NN1_mprc)
NN2_corm <- generate(NN2_mprc) 
NN3_corm <- generate(NN3_mprc)


###########################################################################################################
# Betrachtung der CAPM-Betas
###########################################################################################################
################################################## Berechnung auf monatlicher Basis!!!!!!
################################################## Hier Pruefung der monatlichen Vorhersagegenauigkeit

#################################################################### Header
#################################################################### Berechnung der Cross-Sectional MSE:
# Berechnung der MSE-Werte, mit Hilfe der festgelegten Funktion mse_data
hist_mse <- mse_data(beta_hist1, actual_cor)
mlm_mse <- mse_data(mlm_cor, actual_cor)
lasso_mse <- mse_data(lasso_cor, actual_cor)
bag_mse <- mse_data(bag_cor, actual_cor)
rf_mse <- mse_data(rf_cor, actual_cor)
sgb_mse <- mse_data(sgb_cor, actual_cor)
xgb_mse <- mse_data(xgb_cor, actual_cor)
NN1_mse <- mse_data(NN1_cor, actual_cor)
NN2_mse <- mse_data(NN2_cor, actual_cor)
NN3_mse <- mse_data(NN3_cor, actual_cor)

# monatliche cross-sectionally MSE berechnen:
mse_beta_monthly <- cbind(mlm_mse, lasso_mse, bag_mse, rf_mse, sgb_mse, xgb_mse, NN1_mse, NN2_mse, NN3_mse)
mse_beta_all <- cbind(hist_mse, mlm_mse, lasso_mse, bag_mse, rf_mse, sgb_mse, xgb_mse, NN1_mse, NN2_mse, NN3_mse)

rm(hist_mse, mlm_mse, lasso_mse, bag_mse, rf_mse, sgb_mse, xgb_mse, NN1_mse, NN2_mse, NN3_mse)

# monatliche MSE darstellen:
mse_beta_monthly <- cbind(date2, mse_beta_monthly) %>%
  rename("OLS" = "mlm_cor",
         "LASSO" = "lasso_cor",
         "Bagging" = "bag_cor",
         "RandomForest" = "rf_cor",
         "GBM" = "sgb_cor",
         "XGBoost" = "xgb_cor",
         "NN1" = "NN1_cor",
         "NN2" = "NN2_cor",
         "NN3" = "NN3_cor")

mse_beta_monthly <- mse_beta_monthly %>%  
  select(date, OLS, LASSO, Bagging, RandomForest, GBM, XGBoost, NN1, NN2, NN3) %>%
  pivot_longer(-date) %>%
  rename("mse" = "value")

# Reihenfolge in der Grafik festlegen
mse_beta_monthly$name <- factor(mse_beta_monthly$name, levels = c("OLS", "LASSO", "Bagging", "RandomForest", "GBM", "XGBoost","NN1", "NN2", "NN3"))

# Grafik ersetellen, mit automatischer png. generierung
png(filename ="OOS-MSE (monthly, 750x300).png", width=750, height=300)
ggplot(data=mse_beta_monthly, aes(x=date, y = mse, color = name)) + 
  geom_line() + 
  ylab("OOS-MSE") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") + 
  theme(legend.position = "bottom", legend.title = element_blank(), 
        panel.grid.major.x = element_blank(), axis.title.x = element_blank(),
        panel.background = element_blank(), 
        panel.grid.major.y = element_line(linetype = "dashed", colour = "black"), 
        axis.line = element_line(colour = "black"), axis.text = element_text(colour = "black"),
        legend.background = element_blank(), legend.key = element_blank()) +
  scale_colour_brewer(palette = "Paired")
dev.off()

# Berechnung der means der jeweiligen Methode und Darstellung in einer Grafik:
mse_beta_mean <- mse_beta_all %>%
  summarise(colMeans(mse_beta_all[,1:10]))

names(mse_beta_mean)[1] <- "oos_mse"

# Vektor erstellen mit den Methoden und mit den Means verknuepfen:
methods_mean <- c("HIST", "OLS", "LASSO", "Bagging","RandomForest", "GBM", "XGBoost","NN1", "NN2", "NN3")

mse_beta_mean <- cbind(methods_mean, mse_beta_mean)

# Reihenfolge der Methoden in der Grafik festlegen
mse_beta_mean$methods_mean <- factor(mse_beta_mean$methods_mean, levels = c("HIST","OLS", "LASSO", "Bagging","RandomForest", "GBM","XGBoost","NN1", "NN2", "NN3"))

# Grafik ersetellen, mit automatischer png. generierung
png(filename ="OOS-MSE (meanxmonthly, 750x300).png", width=750, height=300)
ggplot(data=mse_beta_mean, aes(x = methods_mean, y = oos_mse)) + 
  geom_col(width = 0.5, fill = "#000000") +
  ylab("OOS-MSE") +
  coord_cartesian(ylim = c(0.007, 0.009)) +
  theme(legend.position = "bottom", legend.title = element_blank(), 
        panel.background = element_blank(), 
        panel.grid.major.y = element_line(linetype = "dashed", color = "black"), 
        axis.line = element_line(color = "black"), axis.text = element_text(color = "black"),
        axis.title.y = element_text(color = "black"), axis.title.x = element_blank())
dev.off()

################################################################## Header
################################################################## Berechnung der Cross-Sectional Correlation:
# Berechnung der Korrelation mit Hilfe der Funktion correlation()
hist_correlation <- correlation(beta_hist1, actual_cor)
mlm_correlation <- correlation(mlm_cor, actual_cor)
lasso_correlation <- correlation(lasso_cor, actual_cor)
bag_correlation <- correlation(bag_cor, actual_cor)
rf_correlation <- correlation(rf_cor, actual_cor)
sgb_correlation <- correlation(sgb_cor, actual_cor)
xgb_correlation <- correlation(xgb_cor, actual_cor)
NN1_correlation <- correlation(NN1_cor, actual_cor)
NN2_correlation <- correlation(NN2_cor, actual_cor)
NN3_correlation <- correlation(NN3_cor, actual_cor)

# monatliche cross-sectionally Correlation berechnen:
cor_beta_monthly <- cbind(mlm_correlation, lasso_correlation, bag_correlation, rf_correlation, sgb_correlation, xgb_correlation, NN1_correlation, NN2_correlation, NN3_correlation)

rm(hist_correlation, mlm_correlation, lasso_correlation, bag_correlation, rf_correlation, sgb_correlation, xgb_correlation, NN1_correlation, NN2_correlation, NN3_correlation)

# monatliche correlation darstellen:
cor_beta_monthly <- cbind(date2, cor_beta_monthly) %>%
  rename("OLS" = "mlm_cor",
         "LASSO" = "lasso_cor",
         "Bagging" = "bag_cor",
         "RandomForest" = "rf_cor",
         "GBM" = "sgb_cor",
         "XGBoost" = "xgb_cor",
         "NN1" = "NN1_cor",
         "NN2" = "NN2_cor",
         "NN3" = "NN3_cor")

cor_beta_monthly <- cor_beta_monthly %>%  
  select(date, OLS, LASSO, Bagging, RandomForest, GBM, XGBoost, NN1, NN2, NN3) %>%
  pivot_longer(-date) %>%
  rename("cor" = "value")

# Reihenfolge der Methoden in der Grafik festlegen
cor_beta_monthly$name <- factor(cor_beta_monthly$name, levels = c("OLS", "LASSO", "Bagging", "RandomForest", "GBM", "XGBoost","NN1", "NN2", "NN3"))

# Grafik ersetellen, mit automatischer png. generierung
png(filename ="OOS-Korrelation (monthly, 750x300).png", width=750, height=300)
ggplot(data=cor_beta_monthly, aes(x=date, y = cor, color = name)) + 
  geom_line() + 
  ylab("OOS-Korrelation") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") + 
  theme(legend.position = "bottom", legend.title = element_blank(), 
        panel.grid.major.x = element_blank(), axis.title.x = element_blank(),
        panel.background = element_blank(), 
        panel.grid.major.y = element_line(linetype = "dashed", colour = "black"), 
        axis.line = element_line(colour = "black"), axis.text = element_text(colour = "black"),
        legend.background = element_blank(), legend.key = element_blank()) +
  scale_colour_brewer(palette = "Paired")
dev.off()

####################################################### Header
####################################################### Berechnung der Cross-Sectional R-Squared:
# Bestimmtheitsmass berechnen mit der festgelegten Funktion rsq_data()
hist_rsq <- rsq_data(beta_hist1, actual_cor)
mlm_rsq <- rsq_data(mlm_cor, actual_cor)
lasso_rsq <- rsq_data(lasso_cor, actual_cor)
bag_rsq <- rsq_data(bag_cor, actual_cor)
rf_rsq <- rsq_data(rf_cor, actual_cor)
sgb_rsq <- rsq_data(sgb_cor, actual_cor)
xgb_rsq <- rsq_data(xgb_cor, actual_cor)
NN1_rsq <- rsq_data(NN1_cor, actual_cor)
NN2_rsq <- rsq_data(NN2_cor, actual_cor)
NN3_rsq <- rsq_data(NN3_cor, actual_cor)

# monatliche cross-sectionally rsq_data berechnen:
rsq_beta_monthly <- cbind(mlm_rsq, lasso_rsq, bag_rsq, rf_rsq, sgb_rsq, xgb_rsq, NN1_rsq, NN2_rsq, NN3_rsq)
rsq_beta_all <- cbind(hist_rsq, mlm_rsq, lasso_rsq, bag_rsq, rf_rsq, sgb_rsq, xgb_rsq, NN1_rsq, NN2_rsq, NN3_rsq)
rm(hist_rsq, mlm_rsq, lasso_rsq, bag_rsq, rf_rsq, sgb_rsq, xgb_rsq, NN1_rsq, NN2_rsq, NN3_rsq)

# Berechnung der means der jeweiligen Methode und Darstellung in einer Grafik:
rsq_beta_mean <- rsq_beta_all %>%
  summarise(colMeans(rsq_beta_all[,1:10]))

names(rsq_beta_mean)[1] <- "oos_rsq"

methods_mean <- c("HIST", "OLS", "LASSO", "Bagging","RandomForest", "GBM", "XGBoost","NN1", "NN2", "NN3")

rsq_beta_mean <- cbind(methods_mean, rsq_beta_mean)

# Reihenfolge der Methoden in der Grafik festlegen
rsq_beta_mean$methods_mean <- factor(rsq_beta_mean$methods_mean, levels = c("HIST","OLS", "LASSO", "Bagging","RandomForest", "GBM","XGBoost","NN1", "NN2", "NN3"))

# Grafik ersetellen, mit automatischer png. generierung
png(filename ="OOS-Bestimmtheitsmaß (meanxmonthly, 750x300).png", width=750, height=300)
ggplot(data=rsq_beta_mean, aes(x = methods_mean, y = oos_rsq)) + 
  geom_col(width = 0.5, fill = "#000000") +
  ylab("OOS-Bestimmtheitsmaß") +
  coord_cartesian(ylim = c(0.96, 0.97)) +
  theme(legend.position = "bottom", legend.title = element_blank(), 
        panel.background = element_blank(), 
        panel.grid.major.y = element_line(linetype = "dashed", color = "black"), 
        axis.line = element_line(color = "black"), axis.text = element_text(color = "black"),
        axis.title.y = element_text(color = "black"), axis.title.x = element_blank())
dev.off()

####################################################### Header
####################################################### Berechnung der Cross-Sectional Accuracy:
# Genauigkeit berechnen mit Hilfe von accuracy_data()
hist_acc <- accuracy_data(beta_hist1, actual_cor)
mlm_acc <- accuracy_data(mlm_cor, actual_cor)
lasso_acc <- accuracy_data(lasso_cor, actual_cor)
bag_acc <- accuracy_data(bag_cor, actual_cor)
rf_acc <- accuracy_data(rf_cor, actual_cor)
sgb_acc <- accuracy_data(sgb_cor, actual_cor)
xgb_acc <- accuracy_data(xgb_cor, actual_cor)
NN1_acc <- accuracy_data(NN1_cor, actual_cor)
NN2_acc <- accuracy_data(NN2_cor, actual_cor)
NN3_acc <- accuracy_data(NN3_cor, actual_cor)

# monatliche cross-sectionally acc_data berechnen:
acc_beta_monthly <- cbind(mlm_acc, lasso_acc, bag_acc, rf_acc, sgb_acc, xgb_acc, NN1_acc, NN2_acc, NN3_acc)
acc_beta_all <- cbind(hist_acc, mlm_acc, lasso_acc, bag_acc, rf_acc, sgb_acc, xgb_acc, NN1_acc, NN2_acc, NN3_acc)
rm(hist_acc, mlm_acc, lasso_acc, bag_acc, rf_acc, sgb_acc, xgb_acc, NN1_acc, NN2_acc, NN3_acc)

# monatliche accuracy darstellen:
acc_beta_monthly <- cbind(date2, acc_beta_monthly) %>%
  rename("OLS" = "mlm_cor",
         "LASSO" = "lasso_cor",
         "Bagging" = "bag_cor",
         "RandomForest" = "rf_cor",
         "GBM" = "sgb_cor",
         "XGBoost" = "xgb_cor",
         "NN1" = "NN1_cor",
         "NN2" = "NN2_cor",
         "NN3" = "NN3_cor")

acc_beta_monthly <- acc_beta_monthly %>%  
  select(date, OLS, LASSO, Bagging, RandomForest, GBM, XGBoost, NN1, NN2, NN3) %>%
  pivot_longer(-date) %>%
  rename("acc" = "value")

# Reihenfolge der Methoden in der Grafik festlegen
acc_beta_monthly$name <- factor(acc_beta_monthly$name, levels = c("OLS", "LASSO", "Bagging", "RandomForest", "GBM", "XGBoost","NN1", "NN2", "NN3"))

# Grafik ersetellen, mit automatischer png. generierung
png(filename ="OOS-Genauigkeit (monthly, 750x300).png", width=750, height=300)
ggplot(data=acc_beta_monthly, aes(x=date, y = acc, color = name)) + 
  geom_line() + 
  ylab("OOS-Genauigkeit") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") + 
  theme(legend.position = "bottom", legend.title = element_blank(), 
        panel.grid.major.x = element_blank(), axis.title.x = element_blank(),
        panel.background = element_blank(), 
        panel.grid.major.y = element_line(linetype = "dashed", colour = "black"), 
        axis.line = element_line(colour = "black"), axis.text = element_text(colour = "black"),
        legend.background = element_blank(), legend.key = element_blank()) +
  scale_colour_brewer(palette = "Paired")
dev.off()

# Berechnung der means der jeweiligen Methode und Darstellung in einer Grafik:
acc_beta_mean <- acc_beta_all %>%
  summarise(colMeans(acc_beta_all[,1:10]))

names(acc_beta_mean)[1] <- "oos_acc"

# Vektor mit den einzelenen ML-Methoden erstellen und mit Ergebnissen verbinden
methods_mean <- c("HIST", "OLS", "LASSO", "Bagging","RandomForest", "GBM", "XGBoost","NN1", "NN2", "NN3")

acc_beta_mean <- cbind(methods_mean, acc_beta_mean)

# Reihenfolge der Methoden in der Grafik festlegen
acc_beta_mean$methods_mean <- factor(acc_beta_mean$methods_mean, levels = c("HIST","OLS", "LASSO", "Bagging","RandomForest", "GBM","XGBoost","NN1", "NN2", "NN3"))

# Grafik ersetellen, mit automatischer png. generierung
png(filename ="OOS-Genauigkeit (meanxmonthly, 750x300).png", width=750, height=300)
ggplot(data=acc_beta_mean, aes(x = methods_mean, y = oos_acc)) + 
  geom_col(width = 0.5, fill = "#000000") +
  ylab("OOS-Genauigkeit") +
  coord_cartesian(ylim = c(0.945, 0.955)) +
  theme(legend.position = "bottom", legend.title = element_blank(), 
        panel.background = element_blank(), 
        panel.grid.major.y = element_line(linetype = "dashed", color = "black"), 
        axis.line = element_line(color = "black"), axis.text = element_text(color = "black"),
        axis.title.y = element_text(color = "black"), axis.title.x = element_blank())
dev.off()

################################################## Vergleich der monatlich vorhergesagten Betas mit den Betas ein Jahr spaeter!!!!!!
################################################## Hier Pruefung der jaehrlichen Vorhersagegenauigkeit

# Daten generieren fuer die Bestimmung der Vorhersagefaehigkeit der monatlichen Betas ein Jahr im Voraus
# jaehrliche Daten vorbereiten mit generate_annual()
mlm_ann <- generate_annual(mlm_cor)
lasso_ann <- generate_annual(lasso_cor)
bag_ann <- generate_annual(bag_cor)
rf_ann <- generate_annual(rf_cor)
sgb_ann <- generate_annual(sgb_cor)
xgb_ann <- generate_annual(xgb_cor)
NN1_ann <- generate_annual(NN1_cor)
NN2_ann <- generate_annual(NN2_cor)
NN3_ann <- generate_annual(NN3_cor)

################################################################## Header
################################################################## Berechnung der Cross-Sectional Correlation:
# Berechnung der monatlichen Korrelationen 
# Hier beta_test vs. beta_hist! beta_test benutzt das tatsaechliche Beta aus dem jeweiligen Monat und 
# vergleicht es mit dem tatsaechlichen Beta ein Jahr spaeter (12M). Bei beta_hist2 wird das historische Beta
# in diesem Monat benutzt (13M), da das ja auch eigentlich fuer den anderen Monat herangezogen wird.
hist_cor_ann <- cor_annual(beta_test)
mlm_cor_ann <- cor_annual(mlm_ann)
lasso_cor_ann <- cor_annual(lasso_ann)
bag_cor_ann <- cor_annual(bag_ann)
rf_cor_ann <- cor_annual(rf_ann)
sgb_cor_ann <- cor_annual(sgb_ann)
xgb_cor_ann <- cor_annual(xgb_ann)
NN1_cor_ann <- cor_annual(NN1_ann)
NN2_cor_ann <- cor_annual(NN2_ann)
NN3_cor_ann <- cor_annual(NN3_ann)

# Kombinieren der Ergebnisse aller Methoden bis auf HIST - Korrelationen:
cor_annual2 <- cbind(mlm_cor_ann, lasso_cor_ann, bag_cor_ann, rf_cor_ann, sgb_cor_ann, xgb_cor_ann, NN1_cor_ann, NN2_cor_ann, NN3_cor_ann) %>%
  rename("OLS" = "mlm_ann",
         "LASSO" = "lasso_ann",
         "Bagging" = "bag_ann",
         "RandomForest" = "rf_ann",
         "GBM" = "sgb_ann", 
         "XGBoost" = "xgb_ann",
         "NN1" = "NN1_ann",
         "NN2" = "NN2_ann",
         "NN3" = "NN3_ann")

# Kombinieren der Ergebnisse aller Methoden - Korrelationen:
cor_annual_all <- cbind(hist_cor_ann, mlm_cor_ann, lasso_cor_ann, bag_cor_ann, rf_cor_ann, sgb_cor_ann, xgb_cor_ann, NN1_cor_ann, NN2_cor_ann, NN3_cor_ann) %>%
  rename("HIST" = "beta_test",
         "OLS" = "mlm_ann",
         "LASSO" = "lasso_ann",
         "Bagging" = "bag_ann",
         "RandomForest" = "rf_ann",
         "GBM" = "sgb_ann", 
         "XGBoost" = "xgb_ann",
         "NN1" = "NN1_ann",
         "NN2" = "NN2_ann",
         "NN3" = "NN3_ann")

rm(hist_cor_ann, mlm_cor_ann, lasso_cor_ann, bag_cor_ann, rf_cor_ann, sgb_cor_ann, xgb_cor_ann, NN1_cor_ann, NN2_cor_ann, NN3_cor_ann)

# monatliche correlation darstellen:
date3 <- scaled_data %>%
  select(date) %>%
  filter(year(date) > 2014, year(date) < 2020) %>%
  distinct()

cor_annual_mon <- cbind(date3, cor_annual2)

cor_annual_mon <- cor_annual_mon %>%  
  select(date, OLS, LASSO, Bagging, RandomForest, GBM, XGBoost, NN1, NN2, NN3) %>%
  pivot_longer(-date) %>%
  rename("cor" = "value")

# Reihenfolge der Methoden in der Grafik festlegen
cor_annual_mon$name <- factor(cor_annual_mon$name, levels = c("OLS", "LASSO", "Bagging", "RandomForest", "GBM", "XGBoost","NN1", "NN2", "NN3"))

# Grafik ersetellen, mit automatischer png. generierung
png(filename ="OOS-Korrelation (annualxmonthly, 750x300).png", width=750, height=300)
ggplot(data=cor_annual_mon, aes(x=date, y = cor, color = name)) + 
  geom_line() + 
  ylab("OOS-Korrelation") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") + 
  theme(legend.position = "bottom", legend.title = element_blank(), 
        panel.grid.major.x = element_blank(), axis.title.x = element_blank(),
        panel.background = element_blank(), 
        panel.grid.major.y = element_line(linetype = "dashed", colour = "black"), 
        axis.line = element_line(colour = "black"), axis.text = element_text(colour = "black"),
        legend.background = element_blank(), legend.key = element_blank()) +
  scale_colour_brewer(palette = "Paired")
dev.off()

################################################################## Header
################################################################## Berechnung des Cross-Sectional R-Squared:
# Kombinieren der Ergebnisse aller Methoden - R-Squared:
rsq_annual <- cor_annual_all %>%
  transmute(HIST = as.numeric(HIST^2),
            OLS = as.numeric(OLS^2),
            LASSO = as.numeric(LASSO^2),
            Bagging = as.numeric(Bagging^2),
            RandomForest = as.numeric(RandomForest^2),
            GBM = as.numeric(GBM^2),
            XGBoost = as.numeric(XGBoost^2),
            NN1 = as.numeric(NN1^2),
            NN2 = as.numeric(NN2^2),
            NN3 = as.numeric(NN3^2))

rsq_annual_mean <- rsq_annual %>%
  summarise(colMeans(rsq_annual[,1:10]))

names(rsq_annual_mean)[1] <- "oos_rsq"

# Vektor mit ML-Methoden bestimmen
methods_mean <- c("HIST", "OLS", "LASSO", "Bagging","RandomForest", "GBM", "XGBoost","NN1", "NN2", "NN3")

rsq_annual_mean <- cbind(methods_mean, rsq_annual_mean)

# Reihenfolge der Methoden in der Grafik festlegen
rsq_annual_mean$methods_mean <- factor(rsq_annual_mean$methods_mean, levels = c("HIST","OLS", "LASSO", "Bagging","RandomForest", "GBM","XGBoost","NN1", "NN2", "NN3"))

# Grafik ersetellen, mit automatischer png. generierung
png(filename ="OOS-Bestimmtheitsmaß (meanxannual, 750x300).png", width=750, height=300)
ggplot(data=rsq_annual_mean, aes(x = methods_mean, y = oos_rsq)) + 
  geom_col(width = 0.5, fill = "#000000") +
  ylab("OOS-Bestimmtheitsmaß") +
  coord_cartesian(ylim = c(0.49, 0.52)) +
  theme(legend.position = "bottom", legend.title = element_blank(), 
        panel.background = element_blank(), 
        panel.grid.major.y = element_line(linetype = "dashed", color = "black"), 
        axis.line = element_line(color = "black"), axis.text = element_text(color = "black"),
        axis.title.y = element_text(color = "black"), axis.title.x = element_blank())
dev.off()

################################################################## Header
################################################################## Berechnung des Cross-Sectional Accuracy:
# Berechnung der jaehrlichen Accuracy 
hist_acc_ann <- acc_annual(beta_test)
mlm_acc_ann <- acc_annual(mlm_ann)
lasso_acc_ann <- acc_annual(lasso_ann)
bag_acc_ann <- acc_annual(bag_ann)
rf_acc_ann <- acc_annual(rf_ann)
sgb_acc_ann <- acc_annual(sgb_ann)
xgb_acc_ann <- acc_annual(xgb_ann)
NN1_acc_ann <- acc_annual(NN1_ann)
NN2_acc_ann <- acc_annual(NN2_ann)
NN3_acc_ann <- acc_annual(NN3_ann)

# Kombinieren der Ergebnisse aller Methoden bis auf HIST - Accuracy:
acc_annual2 <- cbind(mlm_acc_ann, lasso_acc_ann, bag_acc_ann, rf_acc_ann, sgb_acc_ann, xgb_acc_ann, NN1_acc_ann, NN2_acc_ann, NN3_acc_ann) %>%
  rename("OLS" = "mlm_ann",
         "LASSO" = "lasso_ann",
         "Bagging" = "bag_ann",
         "RandomForest" = "rf_ann",
         "GBM" = "sgb_ann", 
         "XGBoost" = "xgb_ann",
         "NN1" = "NN1_ann",
         "NN2" = "NN2_ann",
         "NN3" = "NN3_ann")

# Kombinieren der Ergebnisse aller Methoden - Accuracy:
acc_annual_all <- cbind(hist_acc_ann, mlm_acc_ann, lasso_acc_ann, bag_acc_ann, rf_acc_ann, sgb_acc_ann, xgb_acc_ann, NN1_acc_ann, NN2_acc_ann, NN3_acc_ann) %>%
  rename("HIST" = "beta_test",
         "OLS" = "mlm_ann",
         "LASSO" = "lasso_ann",
         "Bagging" = "bag_ann",
         "RandomForest" = "rf_ann",
         "GBM" = "sgb_ann", 
         "XGBoost" = "xgb_ann",
         "NN1" = "NN1_ann",
         "NN2" = "NN2_ann",
         "NN3" = "NN3_ann")

rm(hist_acc_ann, mlm_acc_ann, lasso_acc_ann, bag_acc_ann, rf_acc_ann, sgb_acc_ann, xgb_acc_ann, NN1_acc_ann, 
   NN2_acc_ann, NN3_acc_ann)

# monatliche correlation darstellen:
date3 <- scaled_data %>%
  select(date) %>%
  filter(year(date) > 2014, year(date) < 2020) %>%
  distinct()

acc_annual_mon <- cbind(date3, acc_annual2)

acc_annual_mon <- acc_annual_mon %>%  
  select(date, OLS, LASSO, Bagging, RandomForest, GBM, XGBoost, NN1, NN2, NN3) %>%
  pivot_longer(-date) %>%
  rename("acc" = "value")

# Reihenfolge der Methoden in der Grafik festlegen
acc_annual_mon$name <- factor(acc_annual_mon$name, levels = c("OLS", "LASSO", "Bagging", "RandomForest", "GBM", "XGBoost","NN1", "NN2", "NN3"))

# Grafik ersetellen, mit automatischer png. generierung
png(filename ="OOS-Genauigkeit (annualxmonthly, 750x300).png", width=750, height=300)
ggplot(data=acc_annual_mon, aes(x=date, y = acc, color = name)) + 
  geom_line() + 
  ylab("OOS-Genauigkeit") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") + 
  theme(legend.position = "bottom", legend.title = element_blank(), 
        panel.grid.major.x = element_blank(), axis.title.x = element_blank(),
        panel.background = element_blank(), 
        panel.grid.major.y = element_line(linetype = "dashed", colour = "black"), 
        axis.line = element_line(colour = "black"), axis.text = element_text(colour = "black"),
        legend.background = element_blank(), legend.key = element_blank()) +
  scale_colour_brewer(palette = "Paired")
dev.off()

# Kombinieren der Ergebnisse aller Methoden - Accuracy:
acc_annual_mean <- acc_annual_all %>%
  summarise(colMeans(acc_annual_all[,1:10]))

names(acc_annual_mean)[1] <- "oos_acc"

methods_mean <- c("HIST", "OLS", "LASSO", "Bagging","RandomForest", "GBM", "XGBoost","NN1", "NN2", "NN3")

acc_annual_mean <- cbind(methods_mean, acc_annual_mean)

acc_annual_mean$methods_mean <- factor(acc_annual_mean$methods_mean, levels = c("HIST","OLS", "LASSO", "Bagging","RandomForest", "GBM","XGBoost","NN1", "NN2", "NN3"))

png(filename ="OOS-Genauigkeit (meanxannual, 750x300).png", width=750, height=300)
ggplot(data=acc_annual_mean, aes(x = methods_mean, y = oos_acc)) + 
  geom_col(width = 0.5, fill = "#000000") +
  ylab("OOS-Genauigkeit") +
  coord_cartesian(ylim = c(0.72, 0.73)) +
  theme(legend.position = "bottom", legend.title = element_blank(), 
        panel.background = element_blank(), 
        panel.grid.major.y = element_line(linetype = "dashed", color = "black"), 
        axis.line = element_line(color = "black"), axis.text = element_text(color = "black"),
        axis.title.y = element_text(color = "black"), axis.title.x = element_blank())
dev.off()
###########################################################################################################
# Betrachtung der Aktienkurse:
###########################################################################################################

# Hier wird jetzt das gleiche gemacht wie mit den Ergebnisse von den Betas, weswegen hier nicht mehr alles
# so stark kommentiert wird.

#################################################################### Header
#################################################################### Berechnung der Cross-Sectional MSE:
hist_mse <- mse_data(mprc_hist, actual_corm)
mlm_mse <- mse_data(mlm_corm, actual_corm)
lasso_mse <- mse_data(lasso_corm, actual_corm)
bag_mse <- mse_data(bag_corm, actual_corm)
rf_mse <- mse_data(rf_corm, actual_corm)
sgb_mse <- mse_data(sgb_corm, actual_corm)
xgb_mse <- mse_data(xgb_corm, actual_corm)
NN1_mse <- mse_data(NN1_corm, actual_corm)
NN2_mse <- mse_data(NN2_corm, actual_corm)
NN3_mse <- mse_data(NN3_corm, actual_corm)

# monatliche cross-sectionally MSE berechnen:
mse_mprc_monthly <- cbind(mlm_mse, lasso_mse, bag_mse, rf_mse, sgb_mse, xgb_mse, NN1_mse, NN2_mse, NN3_mse)
mse_mprc_all <- cbind(hist_mse, mlm_mse, lasso_mse, bag_mse, rf_mse, sgb_mse, xgb_mse, NN1_mse, NN2_mse, NN3_mse)

rm(hist_mse, mlm_mse, lasso_mse, bag_mse, rf_mse, sgb_mse, xgb_mse, NN1_mse, NN2_mse, NN3_mse)

# monatliche MSE darstellen:
mse_mprc_monthly <- cbind(date2, mse_mprc_monthly) %>%
  rename("OLS" = "mlm_corm",
         "LASSO" = "lasso_corm",
         "Bagging" = "bag_corm",
         "RandomForest" = "rf_corm",
         "GBM" = "sgb_corm",
         "XGBoost" = "xgb_corm",
         "NN1" = "NN1_corm",
         "NN2" = "NN2_corm",
         "NN3" = "NN3_corm")

mse_mprc_monthly <- mse_mprc_monthly %>%  
  select(date, OLS, LASSO, Bagging, RandomForest, GBM, XGBoost, NN1, NN2, NN3) %>%
  pivot_longer(-date) %>%
  rename("mse" = "value")

mse_mprc_monthly$name <- factor(mse_mprc_monthly$name, levels = c("OLS", "LASSO", "Bagging", "RandomForest", "GBM", "XGBoost","NN1", "NN2", "NN3"))

ggplot(data=mse_mprc_monthly, aes(x=date, y = mse, color = name)) + 
  geom_line() + 
  ylab("OOS-MSE") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") + 
  theme(legend.position = "bottom", legend.title = element_blank(), 
        panel.grid.major.x = element_blank(), axis.title.x = element_blank(),
        panel.background = element_blank(), 
        panel.grid.major.y = element_line(linetype = "dashed", colour = "black"), 
        axis.line = element_line(colour = "black"), axis.text = element_text(colour = "black"),
        legend.background = element_blank(), legend.key = element_blank()) +
  scale_colour_brewer(palette = "Paired")

# Berechnung der means der jeweiligen Methode und Darstellung in einer Grafik:
mse_mprc_mean <- mse_mprc_all %>%
  summarise(colMeans(mse_mprc_all[,1:10]))

names(mse_mprc_mean)[1] <- "oos_mse"

methods_mean <- c("HIST", "OLS", "LASSO", "Bagging","RandomForest", "GBM", "XGBoost","NN1", "NN2", "NN3")

mse_mprc_mean <- cbind(methods_mean, mse_mprc_mean)

mse_mprc_mean$methods_mean <- factor(mse_mprc_mean$methods_mean, levels = c("HIST","OLS", "LASSO", "Bagging","RandomForest", "GBM","XGBoost","NN1", "NN2", "NN3"))

ggplot(data=mse_mprc_mean, aes(x = methods_mean, y = oos_mse)) + 
  geom_col(width = 0.5, fill = "#000000") +
  ylab("OOS-MSE") +
  coord_cartesian(ylim = c(0, 12000)) +
  theme(legend.position = "bottom", legend.title = element_blank(), 
        panel.background = element_blank(), 
        panel.grid.major.y = element_line(linetype = "dashed", color = "black"), 
        axis.line = element_line(color = "black"), axis.text = element_text(color = "black"),
        axis.title.y = element_text(color = "black"), axis.title.x = element_blank())

################################################################## Header
################################################################## Berechnung der Cross-Sectional Correlation:
hist_correlation <- correlation(mprc_hist, actual_corm)
mlm_correlation <- correlation(mlm_corm, actual_corm)
lasso_correlation <- correlation(lasso_corm, actual_corm)
bag_correlation <- correlation(bag_corm, actual_corm)
rf_correlation <- correlation(rf_corm, actual_corm)
sgb_correlation <- correlation(sgb_corm, actual_corm)
xgb_correlation <- correlation(xgb_corm, actual_corm)
NN1_correlation <- correlation(NN1_corm, actual_corm)
NN2_correlation <- correlation(NN2_corm, actual_corm)
NN3_correlation <- correlation(NN3_corm, actual_corm)

# monatliche cross-sectionally Correlation berechnen:
cor_mprc_monthly <- cbind(mlm_correlation, lasso_correlation, bag_correlation, rf_correlation, sgb_correlation, xgb_correlation, NN1_correlation, NN2_correlation, NN3_correlation)

rm(hist_correlation, mlm_correlation, lasso_correlation, bag_correlation, rf_correlation, sgb_correlation, xgb_correlation, NN1_correlation, NN2_correlation, NN3_correlation)

# monatliche correlation darstellen:
cor_mprc_monthly <- cbind(date2, cor_mprc_monthly) %>%
  rename("OLS" = "mlm_corm",
         "LASSO" = "lasso_corm",
         "Bagging" = "bag_corm",
         "RandomForest" = "rf_corm",
         "GBM" = "sgb_corm",
         "XGBoost" = "xgb_corm",
         "NN1" = "NN1_corm",
         "NN2" = "NN2_corm",
         "NN3" = "NN3_corm")

cor_mprc_monthly <- cor_mprc_monthly %>%  
  select(date, OLS, LASSO, Bagging, RandomForest, GBM, XGBoost, NN1, NN2, NN3) %>%
  pivot_longer(-date) %>%
  rename("cor" = "value")

cor_mprc_monthly$name <- factor(cor_mprc_monthly$name, levels = c("OLS", "LASSO", "Bagging", "RandomForest", "GBM", "XGBoost","NN1", "NN2", "NN3"))

####################################################### Header
####################################################### Berechnung der Cross-Sectional R-Squared:
hist_rsq <- rsq_data(mprc_hist, actual_corm)
mlm_rsq <- rsq_data(mlm_corm, actual_corm)
lasso_rsq <- rsq_data(lasso_corm, actual_corm)
bag_rsq <- rsq_data(bag_corm, actual_corm)
rf_rsq <- rsq_data(rf_corm, actual_corm)
sgb_rsq <- rsq_data(sgb_corm, actual_corm)
xgb_rsq <- rsq_data(xgb_corm, actual_corm)
NN1_rsq <- rsq_data(NN1_corm, actual_corm)
NN2_rsq <- rsq_data(NN2_corm, actual_corm)
NN3_rsq <- rsq_data(NN3_corm, actual_corm)

# monatliche cross-sectionally rsq_data berechnen:
rsq_mprc_monthly <- cbind(mlm_rsq, lasso_rsq, bag_rsq, rf_rsq, sgb_rsq, xgb_rsq, NN1_rsq, NN2_rsq, NN3_rsq)
rsq_mprc_all <- cbind(hist_rsq, mlm_rsq, lasso_rsq, bag_rsq, rf_rsq, sgb_rsq, xgb_rsq, NN1_rsq, NN2_rsq, NN3_rsq)
rm(hist_rsq, mlm_rsq, lasso_rsq, bag_rsq, rf_rsq, sgb_rsq, xgb_rsq, NN1_rsq, NN2_rsq, NN3_rsq)

# Berechnung der means der jeweiligen Methode und Darstellung in einer Grafik:
rsq_mprc_mean <- rsq_mprc_all %>%
  summarise(colMeans(rsq_mprc_all[,1:10]))

names(rsq_mprc_mean)[1] <- "oos_rsq"

methods_mean <- c("HIST", "OLS", "LASSO", "Bagging","RandomForest", "GBM", "XGBoost","NN1", "NN2", "NN3")

rsq_mprc_mean <- cbind(methods_mean, rsq_mprc_mean)

rsq_mprc_mean$methods_mean <- factor(rsq_mprc_mean$methods_mean, levels = c("HIST","OLS", "LASSO", "Bagging","RandomForest", "GBM","XGBoost","NN1", "NN2", "NN3"))

