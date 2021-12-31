###########################################################################################################
# Prognostizierten Beta-Daten und Aktienkurse nochmal einlesen
###########################################################################################################
## Betas
actual_data_calc <- readRDS("actual.RDS")
mlm_data_calc <- readRDS("mlm_betaUN_normal.RDS") 
lasso_data_calc <- readRDS("lasso_betaUN_normal.RDS") 
bag_data_calc <- readRDS("bag_betaUN_normal.RDS") 
rf_data_calc <- readRDS("rf_betaUN_normal.RDS")
sgb_data_calc <- readRDS("sgb_betaUN_normal.RDS")
xgb_data_calc <- readRDS("xgb_betaUN_normal.RDS")
NN1_data_calc <- readRDS("ensembleNN1_transform.RDS") 
NN2_data_calc <- readRDS("ensembleNN2_transform.RDS") 
NN3_data_calc <- readRDS("ensembleNN3_transform.RDS") 

## Aktienkurse
actual_mprc_data <- mprc_actual2
mlm_mprc_data <- readRDS("mlm_mprcUN_normal.RDS")
lasso_mprc_data <- readRDS("lasso_mprcUN_normal.RDS")
bag_mprc_data <- readRDS("bag_mprcUN_normal.RDS")
rf_mprc_data <- readRDS("rf_mprcUN_normal.RDS")
sgb_mprc_data <- readRDS("sgb_mprcUN_normal.RDS")
xgb_mprc_data <- readRDS("xgb_mprcUN_normal.RDS")
NN1_mprc_data <- readRDS("ensembleNN1_mprc_transform.RDS")
NN2_mprc_data <- readRDS("ensembleNN2_mprc_transform.RDS")
NN3_mprc_data <- readRDS("ensembleNN3_mprc_transform.RDS")

###########################################################################################################
# Funktionen zur Berechnung der verschiedenen Parameter:
###########################################################################################################
#### Berechnung der Renditen auf Basis des CAPMs
returnscapm <- function(x,y) {
  # Hier erstmal x und y miteinadern verbinden, um die Daten nach dem Datum zu joinen
  zwischen <- cbind(y,x)
  
  # Daten mit FF rm-rf verbinden
  data <- left_join(zwischen, rp_rf_ff, by = "date")
  
  # Datum wieder rausnehmen
  data <- data[,2:405]
  
  # Schleife durchfuehren, sodass fuer jedes Beta 
  output <- as_tibble(y)
  for (i in 1:(ncol(x))) {
    output[,i+1] <- data$rf + data[,i] * data$mrp
  }
  
  output <- output %>%
    pivot_longer(-date)
  
  names(output)[2] <- "lpermno"
  names(output)[3] <- "return"
  
  output <- output %>%
    transmute(date = ymd(date),
              lpermno = as.numeric(lpermno),
              return = as.numeric(return))
  
  return(output)
}


#### Berechnung der returns auf Basis der Aktienkurse:
return_mprc <- function(x) {
  # Mit Datum kombinieren, um pivot_longer verwenden zu koennen und anschließend zu joinen
  output <- cbind(date2, x)
  
  # Pivot_longer durchfuehren, um es in Datenform 2 bringen
  output <- pivot_longer(output, -date)
  
  # Daten in richtiges Format bringen
  output <- output %>%
    transmute(date = ymd(date),
              lpermno = as.numeric(name),
              prediction = as.numeric(value)) 
  
  # Daten auf Basis von lpermno und date joinen
  output <- left_join(output, mprc_lag, by=c("lpermno", "date"))
  
  # Rendite berechnen 
  output <- output %>%
    transmute(date = ymd(date),
              lpermno = as.numeric(lpermno),
              return = as.numeric((prediction - mprc_lag) / mprc_lag) * 100)
  
  return(output)
}

##### Hier moechte ich die Renditen miteinandere vergleichen, tatsaechliche Rendite vs. vorhergesagte Rendite 
##### Vergleich der Renditen - Berechnung der Korrelation

compare_returns <- function(x,y) {
  
  # Zuerst die beiden Datensaetze joinen:
  data <- left_join(y,x, by = c("lpermno", "date")) %>%
    select(-lpermno)
  
  # Anschließend dem Datensatz x einen neuen Namen geben, damit er jedesmal gleich ist, sonst gibt es Probleme bei Berechnug
  names(data)[3] <- "pred_return"
  
  # Anschließend wird das monatliche R^2 berechnet:
  output <- data %>%
    group_by(date) %>%
    summarise(cor(MRET, pred_return))
  
  output <- output[,2]
  
  names(output)[1] <- deparse(substitute(x))
  
  return(output)
}

###### Berechnung des MSE
mse_returns <- function(x,y) {
  
  # Zuerst die beiden Datensaetze joinen:
  data <- left_join(y,x, by = c("lpermno", "date")) %>%
    select(-lpermno)
  
  # Anschließend dem Datensatz x einen neuen Namen geben, damit er jedesmal gleich ist, sonst gibt es Probleme bei Berechnug
  names(data)[3] <- "pred_return"
  
  # Anschließend wird das monatliche R^2 berechnet:
  output <- data %>%
    group_by(date) %>%
    summarise(mean((MRET-pred_return)^2))
  
  output <- output[,2]
  
  names(output)[1] <- deparse(substitute(x))
  
  return(output)
}

###########################################################################################################
# Berechnung der Renditen und Performancemaße auf Basis des CAPM-Betas
###########################################################################################################
# tasaechlichen Renditen vorbereiten:
monthlyprimary <- readRDS("crsp_students_mth.RDS") 

monthlydata <- monthlyprimary %>%
  select(MCALDT, KYPERMNO, MRET) %>%
  rename(date = MCALDT,
         lpermno = KYPERMNO) %>%
  mutate(date = ymd(date),
         lpermno = as.numeric(lpermno),
         MRET = as.numeric(MRET) * 100) # * 100 damit es eine Prozentzahl ist.

calc_mprem <- monthlydata %>%
  filter(!lpermno %in% without) %>%
  filter(year(date) >= 2014, year(date) < 2020) %>%
  arrange(lpermno, date)

# Berechnung der Cross-Sectional Rendite
avg_mret <- calc_mprem %>%
  group_by(date) %>%
  summarise(mean(MRET))

# Andere Variante: Mkt_Rf von Fama/French verwenden monthly:
monthlymktrp <- read_csv("ThreeFactors_monthly.CSV")

datamktrp <- monthlymktrp %>%
  transmute(date = ym(date),
            mrp = as.numeric(`Mkt-RF`),
            rf = as.numeric(RF)) %>%
  filter(year(date) > 1984)

monthlymean <- as.data.frame(roll_mean(datamktrp$mrp, width = 60))

names(monthlymean)[1] <- "mrp"

monthlymean$rf <- roll_mean(datamktrp$rf, width = 60)

monthlymean <- cbind(datamktrp$date, monthlymean)

names(monthlymean)[1] <- "date" 

rp_rf_ff <- monthlymean %>%
  filter(year(date) > 1989, year(date) < 2020)

# Ich nehme das Datum raus (immer zum Ersten) und ergaenze das Datum fuer den
# Zusammenschluss in der Funktion (immer zum Letzten des Monats) - somit ist auch der mrp jeden Monat gelagged
# Ich simuliere also, dass ich mich zu Beginn des Monats befinde (mrp ist gelagged), ein Beta fuer den Monat 
# prognostiziere und daraus dann eine monatliche erwartete Rendite berechne

rp_rf_ff <- rp_rf_ff[,2:3]

inter <- scaled_data_mprc %>%
  select(date) %>%
  distinct()

rp_rf_ff <- cbind(inter, rp_rf_ff)

rm(inter, monthlymean, monthlymktrp, datamktrp)

###########################################################################################################
# Berechnung der Renditen:
hist_return <- returnscapm(beta_hist3, date2)
mlm_return <- returnscapm(mlm_data_calc, date2)
lasso_return <- returnscapm(lasso_data_calc, date2) 
bag_return <- returnscapm(bag_data_calc, date2)
rf_return <- returnscapm(rf_data_calc, date2)
sgb_return <- returnscapm(sgb_data_calc, date2)
xgb_return <- returnscapm(xgb_data_calc, date2)
NN1_return <- returnscapm(NN1_data_calc, date2)
NN2_return <- returnscapm(NN2_data_calc, date2)
NN3_return <- returnscapm(NN3_data_calc, date2)

rm(mlm_data_calc, lasso_data_calc, bag_data_calc, rf_data_calc, sgb_data_calc, xgb_data_calc,
   NN1_data_calc, NN2_data_calc, NN3_data_calc, rp_rf_ff, monthlydata, monthlyprimary)

mean_return <- mean(hist_return$return)
names(mean_return)[1] <- "HIST"
mean_return$OLS <- mean(mlm_return$return)
mean_return$LASSO <- mean(lasso_return$return)
mean_return$Bagging <- mean(bag_return$return)
mean_return$RandomForest <- mean(rf_return$return)
mean_return$GBM <- mean(sgb_return$return)
mean_return$XGBoost <- mean(xgb_return$return)
mean_return$NN1 <- mean(NN1_return$return)  
mean_return$NN2 <- mean(NN2_return$return)
mean_return$NN3 <- mean(NN3_return$return)  
  
  
###########################################################################################################
# Berechnung der Korrelationen mit Hilfe von compare_returns()
hist_return_cor <- compare_returns(hist_return, calc_mprem)
mlm_return_cor <- compare_returns(mlm_return, calc_mprem)
lasso_return_cor <- compare_returns(lasso_return, calc_mprem)
bag_return_cor <- compare_returns(bag_return, calc_mprem)
rf_return_cor <- compare_returns(rf_return, calc_mprem)
sgb_return_cor <- compare_returns(sgb_return, calc_mprem)
xgb_return_cor <- compare_returns(xgb_return, calc_mprem)
NN1_return_cor <- compare_returns(NN1_return, calc_mprem)
NN2_return_cor <- compare_returns(NN2_return, calc_mprem)
NN3_return_cor <- compare_returns(NN3_return, calc_mprem)

return_cor_all_monthly <- cbind(mlm_return_cor, lasso_return_cor, bag_return_cor, rf_return_cor, sgb_return_cor, xgb_return_cor,
                                NN1_return_cor, NN2_return_cor, NN3_return_cor)

return_cor_all_mean <- cbind(hist_return_cor, mlm_return_cor, lasso_return_cor, bag_return_cor, rf_return_cor, sgb_return_cor, xgb_return_cor,
                             NN1_return_cor, NN2_return_cor, NN3_return_cor)


# Ableitung des Bestimmtheitsmasses
# monatliches Bestimmtheitsmass berechnen, indem die Korrelationnen quadriert werden
return_rsq_all <- return_cor_all_mean %>%
  transmute(hist_rsq = as.numeric(hist_return^2),
            mlm_rsq = as.numeric(mlm_return^2),
            lasso_rsq = as.numeric(lasso_return^2),
            bag_rsq = as.numeric(bag_return^2),
            rf_rsq = as.numeric(rf_return^2),
            sgb_rsq = as.numeric(sgb_return^2),
            xgb_rsq = as.numeric(xgb_return^2), 
            NN1_rsq = as.numeric(NN1_return^2),
            NN2_rsq = as.numeric(NN2_return^2),
            NN3_rsq = as.numeric(NN3_return^2))

# Durchschnitt der ML-Methoden berechnen
return_rsq_mean <- return_rsq_all %>%
  summarise(colMeans(return_rsq_all[,1:10]))

names(return_rsq_mean)[1] <- "oos_rsquared"

# Vektor erstellen mit ML-Methoden
methods <- c("HIST","OLS", "LASSO", "Bagging", "RandomForest", "GBM", "XGBoost", "NN1", "NN2", "NN3")

return_rsq_mean <- cbind(methods, return_rsq_mean)

# Reihenfolge der ML-Methoden festlegen
return_rsq_mean$methods <- factor(return_rsq_mean$methods, levels = c("HIST","OLS", "LASSO", "Bagging","RandomForest", "GBM","XGBoost","NN1", "NN2", "NN3"))

# Grafik erstellen
ggplot(data=return_rsq_mean, aes(x = methods, y = oos_rsquared)) + 
  geom_col(width = 0.5, fill = "#000000") +
  ylab("OOS-Bestimmtheitsmaß") +
  coord_cartesian(ylim = c(0.026,0.028)) +
  theme(legend.position = "bottom", legend.title = element_blank(), 
        panel.background = element_blank(), 
        panel.grid.major.y = element_line(linetype = "dashed", color = "black"), 
        axis.line = element_line(color = "black"), axis.text = element_text(color = "black"),
        axis.title.y = element_text(color = "black"), axis.title.x = element_blank())


###########################################################################################################
# Berechnung des MSE
hist_return_mse <- mse_returns(hist_return, calc_mprem)
mlm_return_mse <- mse_returns(mlm_return, calc_mprem)
lasso_return_mse <- mse_returns(lasso_return, calc_mprem)
bag_return_mse <- mse_returns(bag_return, calc_mprem)
rf_return_mse <- mse_returns(rf_return, calc_mprem)
sgb_return_mse <- mse_returns(sgb_return, calc_mprem)
xgb_return_mse <- mse_returns(xgb_return, calc_mprem)
NN1_return_mse <- mse_returns(NN1_return, calc_mprem)
NN2_return_mse <- mse_returns(NN2_return, calc_mprem)
NN3_return_mse <- mse_returns(NN3_return, calc_mprem)

mse_return_capm <- cbind(hist_return_mse, mlm_return_mse, lasso_return_mse, bag_return_mse, rf_return_mse, sgb_return_mse, xgb_return_mse, NN1_return_mse, NN2_return_mse, NN3_return_mse)

mse_mean_capm <- mse_return_capm %>%
  summarise(colMeans(mse_return_capm[,1:10]))

mse_mean_capm <- cbind(methods, mse_mean_capm)
names(mse_mean_capm)[2] <- "MSE"

rm(hist_return_mse, mlm_return_mse, lasso_return_mse, bag_return_mse, rf_return_mse, sgb_return_mse, xgb_return_mse, NN1_return_mse, NN2_return_mse, NN3_return_mse)
rm(mlm_return, lasso_return, bag_return, rf_return, sgb_return, xgb_return, NN1_return, NN2_return, NN3_return)


###########################################################################################################
# Berechnung der Renditen auf Basis der Aktienkurse
###########################################################################################################
actual_mprc_lag <- function(x) {
  t <- x %>%
    filter(year > 2013, year < 2020) %>%
    select(date, mprc_lag)
  return(t)
}

mprc_lag <- normaldata %>%
  group_by(lpermno) %>%
  do(beta = actual_mprc_lag(.[,1:45])) %>%
  unnest(everything())

mprc_lag2 <- pivot_wider(mprc_lag, names_from = lpermno, values_from = mprc_lag, values_fn = list) %>%
  unnest(everything())

mprc_lag2 <- mprc_lag2[,2:403]

###########################################################################################################
# Berechnung der Renditen:
# Hier auch Hist integrieren, auch wenn das bedeuten wuerde, dass die vorhergesagte Rendite 0% entsprechen wird
hist_mprc_return <- return_mprc(mprc_lag2)
mlm_mprc_return <- return_mprc(mlm_mprc_data)
lasso_mprc_return <- return_mprc(lasso_mprc_data)
bag_mprc_return <- return_mprc(bag_mprc_data)
rf_mprc_return <- return_mprc(rf_mprc_data)
sgb_mprc_return <- return_mprc(sgb_mprc_data)
xgb_mprc_return <- return_mprc(xgb_mprc_data)
NN1_mprc_return <- return_mprc(NN1_mprc_data)
NN2_mprc_return <- return_mprc(NN2_mprc_data)
NN3_mprc_return <- return_mprc(NN3_mprc_data) 

###########################################################################################################
# Kumulierten taegliche Renditen einlesen und vorbereiten:
aggr_ret <- readRDS("crsp_students_agg_mth.RDS") # Renditen ohne Dividenden

aggr_ret <- aggr_ret %>%
  select(MCALDT, KYPERMNO, MTHRETX) %>%
  rename(date = MCALDT,
         lpermno = KYPERMNO) %>%
  transmute(date = ymd(date),
            lpermno = as.numeric(lpermno),
            MRET = as.numeric(MTHRETX)*100)

aggr_ret <- aggr_ret %>%
  filter(!lpermno %in% without) %>%
  filter(year(date) >= 2014, year(date) < 2020) %>%
  arrange(lpermno, date)

# Im Vergleich dazu noch die Renditen aus Aktienkursen berechnen. (fuer einen Vergleich zwischen den beiden Renditen)
return_data <- normaldata %>%
  select(date, lpermno, MPRC, mprc_lag) %>%
  mutate(MRET = as.numeric((MPRC-mprc_lag)/mprc_lag) * 100) %>%
  filter(year(date) >= 2014, year(date) < 2020) %>%
  select(date,lpermno, MRET)

rsquaredtest <- left_join(aggr_ret, return_data, by=c("lpermno", "date"))
names(rsquaredtest)[3] <- "aggr_mret"
names(rsquaredtest)[4] <- "mprc_mret"

# Test dafuer wie stark die Renditen miteinander korrelieren
rsquaredtest <- rsquaredtest %>%
  group_by(date) %>%
  summarise(cor(aggr_mret,mprc_mret)^2)

###########################################################################################################
###### Entweder oder durchfuehren:
# Korrelationen berechnen, wieder mit Hilfe von compare_returns
hist_cor_mprc <- compare_returns(hist_mprc_return, aggr_ret)
mlm_cor_mprc <- compare_returns(mlm_mprc_return, aggr_ret)
lasso_cor_mprc <- compare_returns(lasso_mprc_return, aggr_ret)
bag_cor_mprc <- compare_returns(bag_mprc_return, aggr_ret)
rf_cor_mprc <- compare_returns(rf_mprc_return, aggr_ret)
sgb_cor_mprc <- compare_returns(sgb_mprc_return, aggr_ret)
xgb_cor_mprc <- compare_returns(xgb_mprc_return, aggr_ret)
NN1_cor_mprc <- compare_returns(NN1_mprc_return, aggr_ret)
NN2_cor_mprc <- compare_returns(NN2_mprc_return, aggr_ret)
NN3_cor_mprc <- compare_returns(NN3_mprc_return, aggr_ret)

# Auf Basis der Renditen abgeleitet aus Aktienkursen
#hist_cor_mprc <- compare_returns(hist_mprc_return, return_data)
#mlm_cor_mprc <- compare_returns(mlm_mprc_return, return_data)
#lasso_cor_mprc <- compare_returns(lasso_mprc_return, return_data)
#bag_cor_mprc <- compare_returns(bag_mprc_return, return_data)
#rf_cor_mprc <- compare_returns(rf_mprc_return, return_data)
#sgb_cor_mprc <- compare_returns(sgb_mprc_return, return_data)
#xgb_cor_mprc <- compare_returns(xgb_mprc_return, return_data)
#NN1_cor_mprc <- compare_returns(NN1_mprc_return, return_data)
#NN2_cor_mprc <- compare_returns(NN2_mprc_return, return_data)
#NN3_cor_mprc <- compare_returns(NN3_mprc_return, return_data)

# Ergebnisse aller ML-Methoden miteinander verbinden
return_cor_mprc <- cbind(mlm_cor_mprc, lasso_cor_mprc, bag_cor_mprc, rf_cor_mprc, sgb_cor_mprc, xgb_cor_mprc,
                         NN1_cor_mprc, NN2_cor_mprc, NN3_cor_mprc)

# Durchschnitte berechnene
return_cor_mprc_mean <- return_cor_mprc %>%
  summarise(colMeans(return_cor_mprc[,1:9]))

# Vektor aller ML-Methoden ohne HIST
methods2 <- c("OLS", "LASSO", "Bagging", "RandomForest", "GBM", "XGBoost", "NN1", "NN2", "NN3")

# miteinander verbinden
return_cor_mprc_mean <- cbind(methods2, return_cor_mprc_mean)

names(return_cor_mprc_mean)[2] <- "oos_cor"

# nicht benoetigte data loeschen
rm(mlm_cor_mprc, lasso_cor_mprc, bag_cor_mprc, rf_cor_mprc, sgb_cor_mprc, xgb_cor_mprc,
   NN1_cor_mprc, NN2_cor_mprc, NN3_cor_mprc)

view(return_cor_mprc_mean)

# Aus den Korrelationen das Bestimmtheitsmass ableiten:
return_rsq_mprc <- return_cor_mprc %>%
  transmute(mlm_rsq = as.numeric(mlm_mprc_return^2),
            lasso_rsq = as.numeric(lasso_mprc_return^2),
            bag_rsq = as.numeric(bag_mprc_return^2),
            rf_rsq = as.numeric(rf_mprc_return^2),
            sgb_rsq = as.numeric(sgb_mprc_return^2),
            xgb_rsq = as.numeric(xgb_mprc_return^2), 
            NN1_rsq = as.numeric(NN1_mprc_return^2),
            NN2_rsq = as.numeric(NN2_mprc_return^2),
            NN3_rsq = as.numeric(NN3_mprc_return^2))

# Durchschnitte berechnen
return_rsq_mprc_mean <- return_rsq_mprc %>%
  summarise(colMeans(return_rsq_mprc[,1:9]))

# Vektor mit ML-Methoden
methods2 <- c("OLS", "LASSO", "Bagging", "RandomForest", "GBM", "XGBoost", "NN1", "NN2", "NN3")

# Ergebnisse und Methoden miteiander verbinden
return_rsq_mprc_mean <- cbind(methods2, return_rsq_mprc_mean)

names(return_rsq_mprc_mean)[2] <- "oos_rsq_cor^2"

view(return_rsq_mprc_mean)

###########################################################################################################
################## Nur eins von beiden durchfuehren
## MSE Berechnung:
hist_mse_mprc <- mse_returns(hist_mprc_return, aggr_ret)
mlm_mse_mprc <- mse_returns(mlm_mprc_return, aggr_ret)
lasso_mse_mprc <- mse_returns(lasso_mprc_return, aggr_ret)
bag_mse_mprc <- mse_returns(bag_mprc_return, aggr_ret)
rf_mse_mprc <- mse_returns(rf_mprc_return, aggr_ret)
sgb_mse_mprc <- mse_returns(sgb_mprc_return, aggr_ret)
xgb_mse_mprc <- mse_returns(xgb_mprc_return, aggr_ret)
NN1_mse_mprc <- mse_returns(NN1_mprc_return, aggr_ret)
NN2_mse_mprc <- mse_returns(NN2_mprc_return, aggr_ret)
NN3_mse_mprc <- mse_returns(NN3_mprc_return, aggr_ret)

# MSE-Berechnung (Renditen aus Aktienkursen abgeleitet)
#hist_mse_mprc <- mse_returns(hist_mprc_return, return_data)
#mlm_mse_mprc <- mse_returns(mlm_mprc_return, return_data)
#lasso_mse_mprc <- mse_returns(lasso_mprc_return, return_data)
#bag_mse_mprc <- mse_returns(bag_mprc_return, return_data)
#rf_mse_mprc <- mse_returns(rf_mprc_return, return_data)
#sgb_mse_mprc <- mse_returns(sgb_mprc_return, return_data)
#xgb_mse_mprc <- mse_returns(xgb_mprc_return, return_data)
#NN1_mse_mprc <- mse_returns(NN1_mprc_return, return_data)
#NN2_mse_mprc <- mse_returns(NN2_mprc_return, return_data)
#NN3_mse_mprc <- mse_returns(NN3_mprc_return, return_data)

mse_all_mprc <- cbind(mlm_mse_mprc, lasso_mse_mprc, bag_mse_mprc, rf_mse_mprc, sgb_mse_mprc, xgb_mse_mprc, NN1_mse_mprc,NN2_mse_mprc,NN3_mse_mprc)
rm(mlm_mse_mprc, lasso_mse_mprc, bag_mse_mprc, rf_mse_mprc, sgb_mse_mprc, xgb_mse_mprc, NN1_mse_mprc,NN2_mse_mprc,NN3_mse_mprc)

mse_mean_mprc <- mse_all_mprc %>%
  summarise(colMeans(mse_all_mprc[,1:9]))

mse_mean_mprc <- cbind(methods2, mse_mean_mprc)

names(mse_mean_mprc)[2] <- "oos_mse"

view(mse_mean_mprc)