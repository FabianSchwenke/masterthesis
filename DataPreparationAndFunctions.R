###########################################################################################################
# Funktionen und benoetigten Libraries
###########################################################################################################
# Global Environment bereinigen
rm(list=ls()) 

# Wenn die Fehlermeldung tar: Failed to set default locale (Scheint ein Mac-Problem zu sein)
# Diesen Code ausfuehren und neustarten. Dann gehts!
# system('defaults write org.R-project.R force.LANG en_US.UTF-8')

###########################################################################################################
# Pakete ausfuehren:
###########################################################################################################
# Konsole auf Englisch stellen
Sys.setenv(LANG = "en")

# Pakete abrufen
library(tidyverse)
library(scales)
library(BBmisc)
library(viridis)
library(lubridate)
library(readxl)
library(tibbletime) 
library(slider)
library(moments)
library(corrr)
library(kableExtra)
library(broom)
library(sandwich)
library(ggplot2)
library(lattice)
library(caret)
library(forecast)
library(gbm)
library(xgboost)
library(keras)
library(doParallel) # fuer Windows
library(doMC) # fuer Mac
library(tensorflow)
library(base)
library(writexl)
library(zoo)
library(roll)
library(reshape2)
library(tictoc)
library(matrixStats)
library(vroom)
library(broom) 
library(rpart)
library(devtools) 

###########################################################################################################
# Funktionen ausfuehren: (einfach durchlaufen lassen)
###########################################################################################################

#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# IM WEITEREN STEHT x IMMER FUER DATA!!!!!!
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

############################################################## Beta-Berechnung

# Funktion von Christopher Scheuch (leicht abgeaendert), habe ich aber mit einzelnen Tests aber auch nochmal gecheckt
# rollierende Bestimmung der CAPM-Betas ueber alle Unternehmen 
fun.capm_regression <- function(x, window, freq) {
  # Drop missing values
  x <- na.omit(x)
  
  # Determine minimum number of observations depending on window size and data frequency
  if (freq == "monthly") {
    if (window == 12) {check <- 10}
    if (window == 24) {check <- 20}
    if (window == 36) {check <- 24}
    if (window == 60) {check <- 24}
  }
  if (freq == "daily") {
    if (window == 1) {check <- 15}
    if (window == 3) {check <- 50}
    if (window == 6) {check <- 100}
    if (window == 12) {check <- 200}
    if (window == 24) {check <- 450}
  }
  
  # Check if minimum number of observations is satisfied
  if (nrow(x) < check) {
    return(as.numeric(NA))
  } else {
    reg <- lm(rp ~ mkt_rp, data = x)
    return(as.numeric(reg$coefficients[2]))
  }
}

# Rollierendes Beta
fun.rolling_capm_regression <- function(x, window, freq) {
  # Nested tibbles of daily data need to be binded
  if (freq == "daily") {
    x <- bind_rows(x)
  }
  out <- slide_period_vec(.x = x, .i = x$month, .period = "month",
                          .f = function(x) {fun.capm_regression(x, window, freq)},
                          .before = (window-1), .complete = FALSE)
  return(out)
}

############################################################## Variablenberechnung fuer jedes Unternehmen

# Berechnung der Variablen auf taeglicher, monatlicher und jaehrlicher Datenbasis 
# und am Ende werden die data.frames gemerged.
# Nicht die effizienteste bzw. ziemlich lange Funktion, weswegen sie innerhalb der Datenaufbereitung
# relativ lange durchlaeuft!

output <- function(x) {
  # Kennzahlen auf jaehrlicher Basis
  annual <- tbl_balanceincomeqrt %>%
    filter(lpermno == x) %>%
    transmute(lpermno = as.integer(lpermno),
              year = as.numeric(date),
              month = as.numeric(month),
              ROE = as.numeric(IB/shr_equity) , # Return on Equity
              ROA = as.numeric(IB/AT) , # ROA
              ROIC = as.numeric((EBIT-TXT)/ICAPT) , # Return on Invested Capital
              oper_prof = as.numeric(EBIT/REVT), # Operating Profitability
              sales_gr = as.numeric((SALE-lag(SALE, n = 1L))/lag(SALE, n=1L)), # Sales growth in %, hier hat abs keinen Einfluss, da Umsaetze eh nur positiv sind
              asset_gr = as.numeric((AT-lag(AT, n = 1L))/lag(AT, n = 1L)), # Asset growth in %, hier ABS keinen Einfluss, da Assets eh immer positiv sind
              NetD_to_EBITDA = as.numeric((DLTT-CHEQ)/EBITDA), # Net Debt / EBITDA Verhaeltnis
              accr_ratio = as.numeric((NI-FCF)/AT), # Berechnung der Accrual Ratio
              BM = as.numeric(shr_equity/mcap), # Book to Market
              EPS = as.numeric(IB/CSHO), # Berechnung EPS
              earn_to_price = as.numeric(IB/mcap), # Earn to price
              sales_to_price = as.numeric(SALE/mcap), # Sales to price
              EBITDA_EV = as.numeric(EBITDA/EV), # EBITDA to Enterprise Value 
              crnt_ratio = as.numeric(ACT/LCT),# Current Ratio
              quick_ratio = as.numeric((ACT-INVT)/LCT), # Quick Ratio
              DA = as.numeric(DLTT/AT), # Long-Term Debt / Assets
              asset_lev = as.numeric((AT/shr_equity)), # Berechnung Asset Leverage
    )  %>%
    mutate(ROE_gr = as.numeric((ROE-lag(ROE, n = 1L))/abs(lag(ROE, n=1L))), # Wachstum in % ROE, hier mit abs aufgrund der Interpretation
           EPS_gr = as.numeric((EPS-lag(EPS, n = 1L))/abs(lag(EPS, n=1L)))  # Wachstum in % EPS, hier mit abs aufgrund der Interpretation
    ) %>%
    filter(year >= 1987)
  
  # Variablen auf jaehrlicher Basis
  daily <- tbl_dailyreturn %>%
    filter(lpermno == x) %>%
    group_by(year = year(date), month = month(date)) %>%
    summarise(sd = sd(DRET))
  
  # Variablen auf monatlicher Basis
  monthly <- monthlyprimary %>%
    filter(KYPERMNO == x, year(MCALDT) >= 1987) %>%
    transmute(lpermno = as.integer(KYPERMNO),
              date = ymd(MCALDT),
              year = year(MCALDT),
              month = month(MCALDT),
              MVOL = as.numeric(MVOL), # Monatliches Volumen
              MCAP = as.numeric(log(lag(MTCAP, n = 1L))), # monatliche Marktkapitalisierung (logarithmiert)
              MRET = as.numeric(MRET))%>%
    mutate(CUMRET12 = rollsum(log(1+MRET), 11, na.pad=TRUE, align="right"), # Berechnung der mom3m, mom12m, mom6m
           CUMRET12 = exp(CUMRET12)-1,
           mom12m = lag(CUMRET12, 2),
           CUMRET6 =  rollsum(log(1+MRET), 5, na.pad=TRUE, align="right"),
           CUMRET6 = exp(CUMRET6)-1,
           mom6m = lag(CUMRET6, 2),
           CUMRET3 = rollsum(log(1+MRET), 2, na.pad=TRUE, align="right"),
           CUMRET3 = exp(CUMRET3)-1,
           mom3m_lag = lag(CUMRET3, 1)
    ) %>% 
    filter(year(date) >= 1987) %>%
    select(date, year, month, MVOL, MCAP, mom12m, mom6m, mom3m_lag)
  
  # MPRC anpassen fuer das jeweilige Unternehmen
  dis <- distributions %>%
    select(KYPERMNO, DIVAMT, FACPR, EXDT) %>%
    transmute(lpermno = as.numeric(KYPERMNO),
              divamt = as.numeric(DIVAMT),
              facpr = as.numeric(FACPR),
              date = ymd(EXDT)) %>%
    filter(year(date) >= 1987) 
  
  dis <- dis %>%
    filter(lpermno == x) %>%
    select(-lpermno)
  
  daily_prc <- daily_return %>%
    filter(lpermno == x) %>%
    transmute(lpermno = as.numeric(lpermno),
              date = ymd(date),
              DPRC = as.numeric(DPRC))%>%
    arrange(desc(date))
  
  adjustment <- full_join(daily_prc, dis, by = "date")
  
  adjustment$facpr <- ifelse(is.na(adjustment$facpr), 0, adjustment$facpr)
  adjustment$divamt <- ifelse(is.na(adjustment$divamt), 0, adjustment$divamt)
  
  # Hier findet dann die tatsaechliche Aufbereitung statt. Dafuer wird der Anpassungsfaktor +1 gerechnet
  # anschließend wird eine Hilfszeile erstellt, wobei der taegliche Kurs durch die kumulierten Produkte
  # des Anpassungsfaktor geteilt wird. Ausserdem wird es hier noch um die Dividenden angepasst, jedoch 
  # wird im Endeffekt der Aktienkurs verwendet, der nur um den Anpassungsfaktor bereinigt ist
  # innerhalb des Vorgangs werden die Daten unterschiedlich angeordnet, um die jeweiligen Berechnungen durchzuefuehren
  adjustment2 <- adjustment %>%
    mutate(inter = facpr + 1) %>%
    mutate(cumfacpr = cumprod(inter)) %>%
    mutate(DPRC_adj = DPRC/cumfacpr) %>%
    arrange(date) %>%
    mutate(divfac = divamt/lag(DPRC_adj, n=1L)) %>%
    mutate(inter2 = divfac + 1) %>%
    filter(year(date) >= 1987) %>%
    arrange(desc(date)) %>%
    mutate(cumdivfac = cumprod(inter2)) %>%
    mutate(cumdivfac_lag = lag(cumdivfac)) %>%
    mutate(DPRC_adj_sd = DPRC_adj/cumdivfac_lag) %>%
    arrange(date)
  
  adjustment2$DPRC_adj_sd <- ifelse(is.na(adjustment2$DPRC_adj_sd), adjustment2$DPRC_adj, adjustment2$DPRC_adj_sd)
  
  adjustment2$mon_yr = format(adjustment2$date, "%Y-%m")
  
  # Hier liegen dann die Ergebnisse der Anpassungen vor. Es wird aber DPRC adjusted verwendet.
  monthly_prc <- adjustment2 %>%
    group_by(mon_yr) %>%
    filter(date == max(date)) %>%
    transmute(lpermno = as.numeric(lpermno),
              MPRC_raw = as.numeric(DPRC),
              MPRC = as.numeric(DPRC_adj),
              MPRC_exdiv = as.numeric(DPRC_adj_sd),
              year = year(date),
              month = month(date))
  
  monthly_prc$mon_yr <- NULL
  
  # Die beiden Monatsdaten miteinander verknuepfen
  monthly_adj <- full_join(monthly, monthly_prc, by = c("year", "month"))
  
  # hier wird MPRC_lag generiert - und am Ende alle benoetigten Variablen selektiert
  monthly_adj <- monthly_adj %>%
    mutate(mprc_lag = as.numeric(lag(MPRC))) %>%
    select(date, year, month, MPRC, mprc_lag, MVOL, MCAP, mom12m, mom6m, mom3m_lag)
  
  # Verknuepfung zu einem Data_frame
  tbl_mvar <- full_join(monthly_adj, daily, by = c("year", "month"))
  
  tbl_mvar <- tbl_mvar %>%
    mutate(sd_lag = as.numeric(lag(sd, n=1L)/100)) %>%
    #select(-sd) %>%
    filter(year(date) >= 1987)
  
  betas <- tbl_betas_daily %>%
    filter(lpermno == x)
  
  # Hier werden einfach alle erstellten data_frames gejoined, um mit dem kompletten Datensatz arbeiten zu koennen:
  tbl_one <- full_join(tbl_mvar, betas, by = c("year", "month"))
  tbl_two <- inner_join(tbl_one, empirical_lag, c("year", "month"))
  
  tbl_two <- tbl_two %>%
    arrange(lpermno) %>%
    select(-date) %>%
    mutate(date = date.x) %>%
    mutate(month = month(date)) %>%
    select(date, year, month, lpermno, beta_12m, MPRC, mprc_lag, MVOL, MCAP, mom12m, mom6m, mom3m_lag, sd_lag, beta_12m_lag, M1_lag, CPI_lag, TB3_lag, TB6_lag, DGS10_lag, DYS_lag, DYS_BAA_TB3_lag, UNEMPLOY_lag, ntis_lag, bm_macro_lag, corpr_lag)%>%
    distinct(date, .keep_all=TRUE)
  
  tbl_three <- left_join(tbl_two, annual)
  
  # So werden immer ab dem Ende des Fiskal-Jahres die Werte ausgegeben, anschließend werden die Daten mit jaehrlicher
  # Frequenz um 7 Monate (t-6) nach Gu et al. (2020) gelagged
  annualandmonthly <- tbl_three %>%
    fill(ROE, ROA, ROIC, oper_prof, sales_gr, asset_gr, NetD_to_EBITDA, 
         accr_ratio, BM, EPS, earn_to_price, sales_to_price, EBITDA_EV, ROE_gr, EPS_gr,
         crnt_ratio,quick_ratio,DA, asset_lev,
         .direction = "down") %>%
    mutate(ROE = lag(as.numeric(ROE), n = 7L),
           ROA = lag(as.numeric(ROA), n = 7L),
           ROIC = lag(as.numeric(ROIC), n = 7L),
           oper_prof = lag(as.numeric(oper_prof), n = 7L),
           sales_gr = lag(as.numeric(sales_gr), n = 7L),
           asset_gr = lag(as.numeric(asset_gr), n = 7L),
           NetD_to_EBITDA = lag(as.numeric(NetD_to_EBITDA), n = 7L),
           accr_ratio = lag(as.numeric(accr_ratio), n = 7L),
           BM = lag(as.numeric(BM), n = 7L),
           EPS = lag(as.numeric(EPS), n = 7L),
           earn_to_price = lag(as.numeric(earn_to_price), n = 7L),
           sales_to_price = lag(as.numeric(sales_to_price), n = 7L),
           EBITDA_EV = lag(as.numeric(EBITDA_EV), n = 7L),
           ROE_gr = lag(as.numeric(ROE_gr), n = 7L),
           EPS_gr = lag(as.numeric(EPS_gr), n = 7L),
           crnt_ratio = lag(as.numeric(crnt_ratio), n = 7L),
           quick_ratio = lag(as.numeric(quick_ratio), n = 7L),
           DA = lag(as.numeric(DA), n = 7L),
           asset_lev = lag(as.numeric(asset_lev), n = 7L)
    )
  
  # Daten noch umbenennen, damit auch klar wird in der Variablen-Bezeichnung, dass die Variablen gelagged sind,
  # um den Forward looking bias zu verhindern.
  df_check <- annualandmonthly %>%
    filter(year(date) < 2020, year(date) >= 1990) %>%
    separate(year, c("year", "quarter"), sep = 4) %>%
    select(-quarter) %>%
    rename(MVOL_lag = MVOL,
           MCAP_lag = MCAP,
           ROE_lag = ROE,
           ROA_lag = ROA,
           DA_lag = DA,
           EPS_lag = EPS,
           asset_lev_lag = asset_lev,
           ROIC_lag = ROIC,
           oper_prof_lag = oper_prof,
           sales_gr_lag = sales_gr,
           asset_gr_lag = asset_gr,
           NetD_to_EBITDA_lag = NetD_to_EBITDA,
           accr_ratio_lag = accr_ratio,
           BM_lag = BM,
           earn_to_price_lag = earn_to_price,
           sales_to_price_lag = sales_to_price,
           EBITDA_EV_lag = EBITDA_EV,
           ROE_gr_lag = ROE_gr,
           EPS_gr_lag = EPS_gr) %>%
    select(-month)
  
  return(df_check)
}

############################################################## Datenaufbereitung nach Gu et al. (2020) - NNs
# Funktionen zur Aufbereitung wie Gu et al. (2020)
replace_nas <- function(x){
  med <- median(x, na.rm = TRUE)
  x[is.na(x)] <- med
  return(x)
}

# Hier werden zunaechst die Werte der Variablen Cross-Sectional geranked (bekommen einen Rang nach hoehe des Wertes)
# Dann werden noch die NA-Werte beruecksichtigt, ist aber eigentlich zu vernachlaessigen, da vorher die NAs durch
# den Cross-Sectional Median ersetzt werden
transform_data <- function(x){
  rx <- rank(x, na.last = TRUE)
  non_nas <- sum(!is.na(x))
  rx[rx>non_nas] <- NA
  2*(rx/non_nas - 0.5)
}

############################################################## Funktionen fuer die Durchfuehrung der ML-Methoden - Betas

########################## In-Sample und OOS-Datensatz festlegen und die anderen Funktionen
in_sample <- function(x) {
  tr <- x %>%
    filter(year <= 2013) %>%
    select(-year, -MPRC, -lpermno, -mprc_lag)
  return(tr)
}

oos_test <- function(x) {
  t <- x %>%
    filter(year > 2013, year < 2020) %>%
    select(-year, -lpermno, -MPRC, -mprc_lag)
  return(t)
}

actual_func <- function(x) {
  t <- x %>%
    filter(year > 2013, year < 2020) %>%
    select(beta_12m)
  return(t)
}

# Fuer die MSE-Berechnung
historical_beta <- function(x) {
  t <- x %>%
    filter(year > 2013, year < 2020) %>%
    select(beta_12m, beta_12m_lag)
  return(t)
}

########################## Prediction-Funktionen basierend auf dem caret-package

# TrainControl mit Hilfe der k-fold Cross Validation
cv_train <- trainControl(method = "cv",
                         number = 5,
                         verboseIter = FALSE)  

# Multivariate Regression
mlm_beta <- function(x) {
  y_in_sample <- in_sample(x)
  
  # Hyperparamtertuning und Training
  mlmmod <- caret::train(beta_12m ~ . -date, data = na.omit(y_in_sample), method = "lm")
}

lasso_beta <- function(x) {
  y_in_sample <- in_sample(x)
  
  # Tuningparameter
  lassoGrid <- expand.grid(alpha = 1, lambda = seq(0.0001, 0.1, by = 5e-04))
  
  # Hyperparamtertuning und Training
  lassomod <- caret::train(beta_12m ~ . -date, 
                           data = na.omit(y_in_sample), 
                           method = "glmnet", 
                           tuneGrid = lassoGrid, 
                           trControl = cv_train)
}

# Bagging_Beta:
bag_beta <- function(x) {
  y_in_sample <- in_sample(x)
  
  # m = p, dann entspricht Random Forest dem Bagging 
  bagGrid <- expand.grid(
    mtry = 39)
  
  # Hyperparamtertuning und Training
  rfmod <- caret::train(beta_12m ~ . -date,
                        data = na.omit(y_in_sample),
                        method = "rf",
                        trControl = cv_train,
                        tuneGrid = bagGrid,
                        ntree = 100,
                        verbose = FALSE)

}

# Random Forest
rf_beta <- function(x) {
  y_in_sample <- in_sample(x)
  
  rfGrid <- expand.grid(
      mtry = c(6,13))
  
  # laut James et al. (2017) und weiteren Quellen m ungefaehr die Wurzel aus P, 
  # somit habe ich einen Wert darueber und einen darunter dazugenommen
  # Problem ist hier aber, dass bei Regressionen die Wurzel schlechter abschneidet als /3
  # Daher nehme ich Werte in dem Bereich
  
  # Hyperparamtertuning und Training
  rfmod <- caret::train(beta_12m ~ . -date,
                        data = na.omit(y_in_sample),
                        method = "rf",
                        trControl = cv_train,
                        tuneGrid = rfGrid,
                        ntree = 100,
                        verbose = FALSE)
}

# Stochastig Gradient Boosting angepasst: 
sgb_beta_adj <- function(x) {
  y_in_sample <- in_sample(x)
  
  # Erlaueterung in der Masterarbeit - ist ein Mix aus Trial-and-Error und Literatur
  sgb_grid <- expand_grid(
    n.trees = seq(1, 1000, by = 1),
    interaction.depth = c(1,2),
    shrinkage = c(0.001,0.01),
    n.minobsinnode = c(6,8,10)
  )
  
  # Hyperparamtertuning und Training
  sgbmod <- caret::train(beta_12m ~ . -date, 
                         data = na.omit(y_in_sample), 
                         method = "gbm", 
                         trControl = cv_train,
                         tuneGrid = sgb_grid,
                         verbose = FALSE)
}

# XGBoost-Methode:
xgb_beta_adj <- function(x) {
  y_in_sample <- in_sample(x)
  
  # Erlaueterung in der Masterarbeit - ist ein Mix aus Trial-and-Error und Literatur
  xgbmod_final <- expand.grid(
    list(
      nrounds = seq(1, 1000, by = 1),
      max_depth = c(1,2),
      colsample_bytree = 1, 
      eta = c(0.001,0.01), # nach viel ausprobieren fuer 0.01 entschieden, anstatt bspw. 0.001 oder 0.1 (bei beiden schlechtere Ergebnisse)
      gamma =  0.05, # hoehere Werte fuehren zu schlechteren Ergebnissen
      min_child_weight = 6, # 6 fuehrt zu besseren Ergebnissen
      subsample = 0.5 # verhindern von Overfitting 
    ))
  
  # Hyperparamtertuning und Training
  xgbmod <- caret::train(beta_12m ~ . -date, 
                         data = na.omit(y_in_sample), 
                         method = "xgbTree", 
                         trControl = cv_train,
                         tuneGrid = xgbmod_final,
                         verbose = TRUE)
}

NN1_beta <- function(x){
  y_in_sample <- in_sample(x)
  
  # Tuningparameter fuer die NNs werden direkt im Skript Predictions festgelegt
  
  # Hyperparamtertuning und Training
  NN1 <- caret::train(beta_12m ~ . -date,
                      data = na.omit(y_in_sample),
                      method = mlpKerasDecayL1, # Ist die caret-Funktion, die leicht getweaked wurde - eine Schicht
                      trControl = cv_train,
                      tuneGrid = NN_grid,
                      epochs = 100,
                      verbose = FALSE
  )
} 

NN2_beta <- function(x){
  y_in_sample <- in_sample(x)
  
  # Tuningparameter fuer die NNs werden direkt im Skript Predictions festgelegt
  
  # Hyperparamtertuning und Training
  NN2 <- caret::train(beta_12m ~ . -date,
                      data = na.omit(y_in_sample),
                      method = mlpKerasDecayL2, # Ist die caret-Funktion, die leicht getweaked wurde - zwei Schichten
                      trControl = cv_train,
                      tuneGrid = NN_grid,
                      epochs = 100,
                      verbose = FALSE
  )
}

NN3_beta <- function(x){ 
  y_in_sample <- in_sample(x)
  
  # Tuningparameter fuer die NNs werden direkt im Skript Predictions festgelegt
  
  # Hyperparamtertuning und Training
  NN3 <- caret::train(beta_12m ~ . -date,
                      data = na.omit(y_in_sample),
                      method = mlpKerasDecayL3, # Ist die caret-Funktion, die leicht getweaked wurde - drei Schichten
                      trControl = cv_train,
                      tuneGrid = NN_grid,
                      epochs = 100,
                      verbose = FALSE
  )
}

########################## Hier werden dann die generierten Modelle fuer die Prediction verwendet:

mlm_pred <- function(x) {
  y_oos_test <- oos_test(x)
  
  mlm_pred <- predict(mlmmod, y_oos_test)
  
  return(mlm_pred)
}

lasso_pred <- function(x) {
  y_oos_test <- oos_test(x)
  
  lasso_pred <- predict(lassomod, y_oos_test)
  
  return(lasso_pred)
}

bag_pred <- function(x) {
  y_oos_test <- oos_test(x)
  
  #Prediction und measuring accuracy
  pred <- predict(bagmod, y_oos_test)
  
  return(pred)
}

rf_pred <- function(x) {
  y_oos_test <- oos_test(x)
  
  #Prediction und measuring accuracy
  pred <- predict(rfmod, y_oos_test)
  
  return(pred)
}

sgb_pred <- function(x) {
  y_oos_test <- oos_test(x)
  
  #Prediction und measuring accuracy
  pred <- predict(sgbmod, y_oos_test)
  
  return(pred)
}

xgb_pred <- function(x) {
  y_oos_test <- oos_test(x)
  
  #Prediction und measuring accuracy
  pred <- predict(xgbmod, y_oos_test)
  
  return(pred)
}


############################################################## Funktionen fuer die Durchfuehrung der ML-Methoden - Betas

# In-Sample und OOS-Datensatz festlegen:
in_sample_mprc <- function(x) {
  tr <- x %>%
    filter(year <= 2013) %>%
    select(-year, -lpermno, -beta_12m)
  return(tr) 
}

oos_test_mprc <- function(x) {
  t <- x %>%
    filter(year > 2013, year < 2020) %>%
    select(-year, -lpermno, -beta_12m)
  return(t)
}

# tatsaechlichen Aktienkurse
actual_func_mprc <- function(x) {
  t <- x %>%
    filter(year > 2013, year < 2020) %>%
    select(MPRC)
  return(t)
}

# Multivariate Regression
mlm_mprc <- function(x) {
  y_in_sample <- in_sample_mprc(x)
  
  # Hyperparamtertuning und Training
  mlmmod <- caret::train(MPRC ~ . -date,
                         data = na.omit(y_in_sample), 
                         method = "lm")
}

lasso_mprc <- function(x) {
  y_in_sample <- in_sample_mprc(x)
  
  # Tuningparameter
  lassoGrid <- expand.grid(alpha = 1, lambda = seq(0.0001, 0.1, by = 5e-04))
  
  # Hyperparamtertuning und Training
  lassomod <- caret::train(MPRC ~ . -date, 
                           data = na.omit(y_in_sample), 
                           method = "glmnet", 
                           tuneGrid = lassoGrid, 
                           trControl = cv_train)
}

# Bagging_Beta:
bag_mprc <- function(x) {
  y_in_sample <- in_sample_mprc(x)
  
  # Tuningparameter
  baggrid <- expand.grid(mtry = 40)
  
  # Hyperparamtertuning und Training
  bagmod <- caret::train(MPRC ~ . -date,
                         data = na.omit(y_in_sample),
                         method = "rf",
                         trControl = cv_train,
                         ntree = 100,
                         verbose = FALSE)
}

# Random Forest
rf_mprc <- function(x) {
  y_in_sample <- in_sample_mprc(x)
  
  # Tuningparameter
  rfGrid_1 <- expand.grid(
    mtry = c(6,13)) # laut James et al. (2017) m ungefaehr die Wurzel aus P, somit habe ich einen Wert darueber und einen darunter dazugenommen
  
  # Hyperparamtertuning und Training
  rfmod <- caret::train(MPRC ~ . -date,
                        data = na.omit(y_in_sample),
                        method = "rf",
                        trControl = cv_train,
                        tuneGrid = rfGrid_1,
                        ntree = 100,
                        verbose = FALSE)
  }

# Stochastig Gradient Boosting angepasst: 
sgb_mprc_adj <- function(x) {
  y_in_sample <- in_sample_mprc(x)
  
  # Tuningparameter
  sgb_grid <- expand_grid(
    n.trees = seq(1, 1000, by = 1),
    interaction.depth = c(1,2),
    shrinkage = c(0.001,0.01),
    n.minobsinnode = c(6,8,10)
  )
  
  # Hyperparamtertuning und Training
  sgbmod <- caret::train(MPRC ~ . -date, 
                         data = na.omit(y_in_sample), 
                         method = "gbm", 
                         trControl = cv_train,
                         tuneGrid = sgb_grid,
                         verbose = FALSE)
}

# XGBoost angepasst:
xgb_mprc_adj <- function(x) {
  y_in_sample <- in_sample_mprc(x) 
  
  # Tuningparameter
  xgbmod_final <- expand.grid(
      list(
        nrounds = seq(1, 1000, by = 1),
        max_depth = c(1,2),
        colsample_bytree = 1, #
        eta = c(0.001,0.01), # nach viel ausprobieren fuer 0.01 entschieden, anstatt bspw. 0.001 oder 0.1 (bei beiden schlechtere Ergebnisse)
        gamma =  0.05, # hoehere Werte fuehren zu schlechteren Ergebnissen
        min_child_weight = 6, # 6 fuehrt zu besseren Ergebnissen
        subsample = 0.5 #verhindern von Overfitting 
      ))
  
  # Hyperparamtertuning und Training
  xgbmod <- caret::train(MPRC ~ . -date, 
                         data = na.omit(y_in_sample), 
                         method = "xgbTree", 
                         trControl = cv_train,
                         tuneGrid = xgbmod_final,
                         verbose = TRUE)
}

NN1_mprc <- function(x){
  y_in_sample <- in_sample_mprc(x)
  
  # Tuningparameter fuer die NNs werden direkt im Skript Predictions festgelegt
  
  # Hyperparamtertuning und Training
  NN1 <- caret::train(MPRC ~ . -date,
                      data = na.omit(y_in_sample),
                      method = mlpKerasDecayL1,
                      trControl = cv_train,
                      tuneGrid = NN_grid,
                      epochs = 100,
                      verbose = FALSE
  )
} 

NN2_mprc <- function(x){
  y_in_sample <- in_sample_mprc(x)
  
  # Tuningparameter fuer die NNs werden direkt im Skript Predictions festgelegt
  
  # Hyperparamtertuning und Training
  NN2 <- caret::train(MPRC ~ . -date,
                      data = na.omit(y_in_sample),
                      method = mlpKerasDecayL2,
                      trControl = cv_train,
                      tuneGrid = NN_grid,
                      epochs = 100,
                      verbose = FALSE
  )
}

NN3_mprc <- function(x){
  y_in_sample <- in_sample_mprc(x)
  
  # Tuningparameter fuer die NNs werden direkt im Skript Predictions festgelegt
  
  # Hyperparamtertuning und Training
  NN3 <- caret::train(MPRC ~ . -date,
                      data = na.omit(y_in_sample),
                      method = mlpKerasDecayL3,
                      trControl = cv_train,
                      tuneGrid = NN_grid,
                      epochs = 100,
                      verbose = FALSE
  )
}

########################## Hier werden dann die generierten Modelle fuer die Prediction verwendet: - Aktienkurse

# sowie sie hier heißen mlmmodm, lassomodm etc. wird in Predictions definiert!
# Habe ich in Funktionen geschrieben, damit ich es in der do-Funktion in Predictions verwenden kann.
mlm_pred_m <- function(x) {
  y_oos_test <- oos_test_mprc(x)
  
  lasso_pred <- predict(mlmmodm, y_oos_test)
  
  return(lasso_pred)
}

lasso_pred_m <- function(x) {
  y_oos_test <- oos_test_mprc(x)
  
  lasso_pred <- predict(lassomodm, y_oos_test)
  
  return(lasso_pred)
}

bag_pred_m <- function(x) {
  y_oos_test <- oos_test_mprc(x)
  
  pred <- predict(bagmodm, y_oos_test)
  
  return(pred)
}

rf_pred_m <- function(x) {
  y_oos_test <- oos_test_mprc(x)
  
  pred <- predict(rfmodm, y_oos_test)
  
  return(pred)
}

sgb_pred_m <- function(x) {
  y_oos_test <- oos_test_mprc(x)
  
  pred <- predict(sgbmodm, y_oos_test)
  
  return(pred)
}

xgb_pred_m <- function(x) {
  y_oos_test <- oos_test_mprc(x)
  
  pred <- predict(xgbmodm, y_oos_test)
  
  return(pred)
} 

# Allgemeine Funktionen fuer die Berechnung des MSE und 
mse <- function(x,y) {
  mean((x - y)^2)
}

se <- function(x,y) {
  (x - y)^2
}

###########################################################################################################
# Daten fuer die letztendliche Durchfuehrung der output-Funktion (siehe Funktionen) vorbereiten
###########################################################################################################
# Daten einlesen
dailyprimary <- readRDS("crsp_students_dly.RDS")
monthlyprimary <- readRDS("crsp_students_mth.RDS")
incomeqtrly <- readRDS("compustat_incomestatementquarterly.RDS")
incomeindustrialannual <- readRDS("compustat_incomestatementindustrialannual.RDS")
incomequarterly <- readRDS("compustat_incomestatementquarterly.RDS")
balancesheet <- readRDS("compustat_balancesheet_quartelry.RDS")
balanceindustrialannual <- readRDS("compustat_balancesheetindustrialannual.RDS")
cashflowannual <- readRDS("compustat_cashflowannual.RDS")
shares <- readRDS("crsp_sfz_shr.RDS")
empirical <- read_xlsx("empirical.xlsx")
market <- read.csv("ThreeFactors.csv")
hdr <- readRDS("sfz_hdr.RDS")
distributions <- readRDS("crsp_students_sfz_dis.RDS") 

# Daten aufbereiten
incomeannual2 <- select(incomeindustrialannual, lpermno, FYYYY, fyra, SALE, REVT, NI, EBITDA, EBIT, TXT, OIADP, OIBDP, IB, COGS, RDIP, DVT, XINT)
rm(incomeindustrialannual)
incomeqrtrly <- select(incomequarterly, lpermno, FYYYYQ, fyrq, SALEQ, REVTQ, NIQ, TXTQ, OIADPQ, OIBDPQ, IBQ, COGSQ, RDIPQ, XINTQ)
rm(incomequarterly)
balancesheet2 <- select(balanceindustrialannual, lpermno, FYYYY, fyra, LT, LSE, DLC, WCAP, AT, ACT, DPACT, INVT, CH, LCT, DLTT, ICAPT, BKVLPS, PPEGT, XPP, CSHO)
rm(balanceindustrialannual)
balancesheet_qrtrly <- select(balancesheet, lpermno, FYYYYQ, fyrq, CHEQ, LTQ, LSEQ, DLCQ, WCAPQ, ATQ, ACTQ, DPACTQ, INVTQ, LCTQ, DLTTQ, ICAPTQ, PPEGTQ, XPPQ, CSHOQ)
rm(balancesheet)
cashflow <- select(cashflowannual, lpermno, lpermco, FYYYY, fyra, OANCF, CAPX, DV)
rm(cashflowannual)
monthlyprimaryx <- select(monthlyprimary, KYPERMNO, MCALDT, MTCAP)

# Duplikate entfernen
tbl_income <- incomeannual2 %>%
  distinct(lpermno, FYYYY, .keep_all = TRUE)
tbl_balance <- balancesheet2 %>%
  distinct(lpermno, FYYYY, .keep_all = TRUE)
tbl_cashflow <- cashflow %>%
  distinct(lpermno, FYYYY, .keep_all = TRUE)
tbl_balanceqrtrly <- balancesheet_qrtrly %>%
  distinct(lpermno, FYYYYQ, .keep_all=TRUE)
tbl_incomeqrtrly <- incomeqrtrly %>%
  distinct(lpermno, FYYYYQ, .keep_all=TRUE)
monthlyprimary2 <- monthlyprimaryx %>%
  distinct(KYPERMNO, MCALDT, .keep_all = TRUE)
monthlyprimary <- monthlyprimary %>% # enthaelt keine Duplikate (Bei Vergleich mit monthly anstatt monthlyprimary siehe Stand 12.05.2021)
  distinct(KYPERMNO, MCALDT,.keep_all = TRUE)
dailyprimary <- dailyprimary %>%
  distinct(KYPERMNO, CALDT, .keep_all = TRUE)

# Negative Vorzeichen vor den Aktienkursen entfernen: Wenn der Schlusskurs an einem bestimmten Handelstag 
# nicht verfuegbar ist, hat die Zahl im Kursfeld ein negatives Vorzeichen, um anzuzeigen, dass es sich um einen 
# Geld-/Brief-Durchschnitt und nicht um einen tatsaechlichen Schlusskurs handelt. Bitte beachten Sie, dass das negative 
# Vorzeichen in diesem Feld ein Symbol ist und dass der Wert des Geld-/Briefdurchschnitts nicht negativ ist.
monthlyprimary$MPRC <- ifelse(monthlyprimary$MPRC < 0, monthlyprimary$MPRC*(-1), monthlyprimary$MPRC*1)

daily_return <- dailyprimary %>%
  select(KYPERMNO, CALDT, PRC) %>%
  transmute(lpermno = as.numeric(KYPERMNO),
            date = ymd(CALDT),
            DPRC = as.numeric(PRC))

daily_return$DPRC <- ifelse(daily_return$DPRC < 0, daily_return$DPRC*(-1), daily_return$DPRC*1)

# Datensaetze vorbereiten und miteinander verbinden:
# Berechne hier zwar die Quartalsdaten, werde sie aber nicht weiter verwenden, da sie bei Ueberpruefungen ziemlich
# fehlerhaft sind
tbl_balanceincome <- merge(tbl_income, tbl_balance)
rm(tbl_income)
rm(tbl_balance)

tbl_balanceincome <- tbl_balanceincome %>%
  filter(FYYYY >= 1987) %>%
  rename(date = FYYYY)

cash_annual <- tbl_cashflow %>%
  filter(FYYYY >= 1987) %>%
  mutate(date = as.numeric(FYYYY),
         FCF = as.numeric(OANCF-CAPX)) # Berechnung des Free Cashflows (lt. https://corporatefinanceinstitute.com/resources/knowledge/valuation/fcf-formula-free-cash-flow/)

mcap_annual <- monthlyprimary2 %>%
  separate(MCALDT, c("date", "month", "day"), sep = "-") %>%
  transmute(lpermno = as.integer(KYPERMNO),
            date = as.numeric(date),
            month = as.numeric(month),
            mcap = as.numeric(MTCAP)
  )

quarterly <- tbl_balanceqrtrly %>%
  separate(FYYYYQ, c("date", "quarter"), sep = 4) %>%
  transmute(lpermno = as.integer(lpermno),
            date = as.numeric(date),
            quarter = as.numeric(quarter),
            CHEQ = as.numeric(CHEQ),
            #LTQ = as.numeric(LTQ), # Werte von LTQ (Q4) und LT (Annual) stimmen ueberein (zumindest bei Stichprobe)
            #LSEQ = as.numeric(LSEQ) # Werte von LSEQ (Q4) und LSE (Annual) stimmen ueberein (zumindest bei Stichprobe)
  ) %>%
  filter(quarter == 4, date >= 1987)

# noch die Quartalsdate - zur Veranschaulichung fehlerhafter Werte fuer ROE
balancequarterly_t <- tbl_balanceqrtrly %>%
  separate(FYYYYQ, c("date", "quarter"), sep = 4) %>%
  transmute(lpermno = as.integer(lpermno),
            date = as.numeric(date),
            quarter = as.numeric(quarter),
            month = as.numeric(fyrq),
            LTQ = as.numeric(LTQ),
            LSEQ = as.numeric(LSEQ),
            DLCQ = as.numeric(DLCQ),
            ATQ = as.numeric(ATQ),
            ACTQ = as.numeric(ACTQ),
            INVTQ = as.numeric(INVTQ),
            LCTQ = as.numeric(LCTQ),
            DLTTQ = as.numeric(DLTTQ),
            ICAPTQ = as.numeric(ICAPTQ),
            CHEQ = as.numeric(CHEQ),
            CSHOQ = as.numeric(CSHOQ) # in Mio.
  ) %>%
  filter(date >= 1987, date < 2020)

incomequarterly_t <- tbl_incomeqrtrly %>%
  separate(FYYYYQ, c("date", "quarter"), sep = 4) %>%
  transmute(lpermno = as.integer(lpermno),
            date = as.numeric(date),
            quarter = as.numeric(quarter),
            month = as.numeric(fyrq),
            SALEQ = as.numeric(SALEQ),
            REVTQ = as.numeric(REVTQ),
            NIQ = as.numeric(NIQ),
            TXTQ = as.numeric(TXTQ),
            IBQ = as.numeric(IBQ),
            XINTQ = as.numeric(XINTQ)
  ) %>%
  filter(date >= 1987, date < 2020)

# Zusammenfuegen der drei Tabellen, ohne Quartalsdaten, da dort viele falsche Werte fuer IB und 
# die Bilanzdaten hat Gu et al. (2020) bspw. auch nicht auf Quartalsbasis bestimmt (siehe Appendix)
x <- full_join(tbl_balanceincome, quarterly) %>%
  rename(month = fyra)

y <- left_join(x, mcap_annual, by = c("lpermno", "date", "month"))

tbl_balanceincomeqrt <- full_join(y, cash_annual)

# Daily Returns aufbereiten
market <- market %>%
  transmute(date = ymd(date),
            mkt_rp = as.numeric(Mkt.RF), # Marktrisikopraemie in %
            rf = as.numeric(RF)
  ) %>%
  filter(year(date) >= 1987)

daily_all <- dailyprimary %>%
  filter(year(CALDT) >= 1987) %>%
  transmute(lpermno = as.integer(KYPERMNO),
            date = ymd(CALDT),
            DRET = as.numeric(RET) * 100 # taegliche Rendite in %
  ) %>%
  arrange(date)

tbl_dailyreturn <- inner_join(daily_all, market, by = "date")

tbl_dailyreturn <- tbl_dailyreturn %>%
  mutate(rp = as.numeric(DRET-rf),
  ) %>%
  select(-rf) %>%
  arrange(lpermno)

# Betas berechnen mit der Funktion von Christoph Scheuch:
beta_calc <- tbl_dailyreturn %>%
  mutate(month = floor_date(date, "month")) %>%
  select(lpermno, month, mkt_rp, rp) 

tbl.crsp_daily_nested <- beta_calc %>%
  group_by(lpermno, month_group = month) %>%
  nest(daily_data = c(month, mkt_rp, rp)) %>% 
  ungroup()

tbl_betas_daily <- tbl.crsp_daily_nested %>% 
  group_by(lpermno) %>% 
  arrange(month_group) %>%
  mutate(beta_12m = fun.rolling_capm_regression(daily_data, window = 12, freq = "daily"),
         beta_24m = fun.rolling_capm_regression(daily_data, window = 24, freq = "daily")
  ) %>%
  ungroup() %>% 
  select(lpermno, month = month_group, beta_12m, beta_24m) %>% 
  arrange(lpermno, month) %>%
  mutate(date = ymd(month)) %>%
  transmute(lpermno = as.integer(lpermno), 
            date = ymd(date),
            year = year(date), 
            month = month(date),
            beta_12m = as.numeric(beta_12m),
            beta_12m_lag = lag(beta_12m, n = 1L)) %>%
  filter(year(date) >= 1987)

################################# Check, ob die Betas uebereinstimmen - passt
#test <- tbl_dailyreturn %>%
#  filter(year(date) == 1990, lpermno == 10032)

#testreg <- lm(rp ~ mkt_rp, test)

#test2 <- tbl_dailyreturn %>%
#  filter(year(date) == 1995, lpermno == 10104)

#testreg2 <- lm(rp ~ mkt_rp, test2)

#rm(testreg, testreg2)

# Emprical Data vorbereiten
empirical_lag <- empirical %>%
  mutate(year = year(date)) %>%
  transmute(date = ymd(date),
            year = year(date),
            month = month(date),
            M1_lag = as.numeric(lag(M1, n = 2L)), # gehoert zu den 15 ausgewaehlten Variablen von Enke et al., S. 932
            CPI_lag = as.numeric(lag(CPI, n = 2L)), # Goyal und Welch (2008)
            TB6_lag = as.numeric(lag(TB6, n = 1L)), # gehoert zu den 15 ausgewaehlten Variablen von Enke et al., S. 932
            TB3_lag = as.numeric(lag(TB3, n = 1L)), # gehoert zu den 15 ausgewaehlten Variablen von Enke et al., S. 932
            DGS10_lag = as.numeric(lag(DGS10, n = 1L)), # gehoert zu den 15 ausgewaehlten Variablen von Enke et al., S. 932
            DYS_lag = as.numeric(lag(DYS, n = 1L)), 
            DYS_BAA_TB3_lag = as.numeric(lag(`DYS_(BAA-TB3)`, n = 1L)), # gehoert zu den 15 ausgewaehlten Variablen von Enke et al., S. 932
            TS_DGS10_TB3_lag = as.numeric(lag(`TS_(DGS10-TB3)`, n = 1L)), # gehoert zu den 15 ausgewaehlten Variablen von Enke et al., S. 932
            UNEMPLOY_lag = as.numeric(lag(UNRATE, n = 2L)), 
            ntis_lag = as.numeric(lag(ntis, n = 1L)), # Goyal und Welch (2008)
            bm_macro_lag = as.numeric(lag(bm_macro, n = 1L)), # Goyal und Welch (2008)
            corpr_lag = as.numeric(lag(corpr), n = 1L) # Goyal und Welch (2008)
  ) %>%
  mutate(M1_dif_lag = as.numeric(M1_lag-lag(M1_lag)),
         CPI_dif_lag = as.numeric(CPI_lag-lag(CPI_lag)),
         TB6_dif_lag = as.numeric(TB6_lag - lag(TB6_lag)),
         TB3_dif_lag = as.numeric(TB3_lag - lag(TB3_lag)),
         DGS10_dif_lag = as.numeric(DGS10_lag - lag(DGS10_lag)),
         DYS_dif_lag = as.numeric(DYS_lag - lag(DYS_lag)),
         DYS_BAA_TB3_dif_lag = as.numeric(DYS_BAA_TB3_lag - lag(DYS_BAA_TB3_lag)),
         TS_DGS10_TB3_dif_lag = as.numeric(TS_DGS10_TB3_lag - lag(TS_DGS10_TB3_lag))
  ) %>% # Verwende die Differenzen aber nicht
  filter(year >= 1987)

# Shareholdersequity selber berechnen - Liablitities & Shareholders Equity (LSE) - Liabilitites - Total (LT)
tbl_balanceincomeqrt <- tbl_balanceincomeqrt %>%
  transmute(lpermno = as.integer(lpermno),
            date = as.numeric(date),
            month = as.numeric(month),
            SALE = as.numeric(SALE), # Sale in Mio.
            REVT = as.numeric(REVT), # Umsatz in Mio.
            NI = as.numeric(NI), # Net Income in Mio.
            EBITDA = as.numeric(EBITDA), # EBITDA in Mio.
            EBIT = as.numeric(EBIT), # EBIT in Mio.
            TXT = as.numeric(TXT), # Income Taxes - Total
            IB = as.numeric(IB), # Income before Extraord. items in Mio.
            XINT = as.numeric(XINT), # Interest & Related Expense - Total in Mio.
            LT = as.numeric(LT), # Liabilities - Total in Mio.
            LSE = as.numeric(LSE), # Liabilities & Sharesholders Equity - Total in Mio.
            DLC = as.numeric(DLC), # Debt in Current Liabilities - Total (Short Term Debt) in Mio.
            AT = as.numeric(AT), # Total Assets in Mio.
            ACT = as.numeric(ACT), # Current Assets - Total in Mio.
            INVT = as.numeric(INVT), # Inventories - Total in Mio.
            LCT = as.numeric(LCT), # Current Liabilities - Total in Mio.
            DLTT = as.numeric(DLTT), # Long-Term Debt Total in Mio.
            ICAPT = as.numeric(ICAPT), # Invested Capital - Total in Mio.
            CHEQ = as.numeric(CHEQ), # Cash & Equivalents - Total in Mio. (aus Q4)
            mcap = as.numeric(mcap/1000), # Marketcap in Mio. (basierend auf dem letzten Monat des Jahres)
            CSHO = as.numeric(CSHO), # Common Shares Outstanding in Mio.
            FCF = as.numeric(FCF) # Selbst berechneter Free Cashflow in Mio.
  ) %>%
  mutate(shr_equity = as.numeric(LSE-LT),
         EV = as.numeric(mcap+DLC+DLTT-CHEQ)) %>% # https://www.investopedia.com/terms/e/enterprisevalue.asp
  filter(date >= 1987) %>%
  arrange(lpermno, date)

# unnoetigen data frames loeschen
rm(x) 
rm(y)
rm(incomeannual2)
rm(balancesheet2)
rm(balancesheet_qrtrly)
rm(cashflow)
rm(tbl_balanceincome)
rm(quarterly)
rm(cash_annual)
rm(monthlyprimaryx)
rm(monthlyprimary2)
rm(mcap_annual)

###########################################################################################################
# Einzelnen Variablen berechnen fuer die unterschiedlichen Unternehmen
###########################################################################################################
hdr_filter <- hdr %>%
  transmute(lpermno = as.numeric(KYPERMNO),
            CUSIP9 = as.character(CUSIP9),
            HSICCD = as.numeric(HSICCD),
            BEGDT = ymd(BEGDT),
            ENDDT = ymd(ENDDT),
            HCOMNAM = as.character(HCOMNAM),
            HSHRCD = as.numeric(HSHRCD)) %>%
  filter(HSHRCD < 12, HSHRCD > 9) %>% # 10 und 11 
  filter(year(BEGDT) < 1989, year(ENDDT) == 2020, day(ENDDT) >= 30, month(ENDDT) == 12)

manufacturing <- hdr_filter %>%
  filter(HSICCD >= 2000, HSICCD < 4000) %>%
  distinct(HCOMNAM, .keep_all = TRUE) %>%
  select(lpermno, HSICCD)

retail <- hdr_filter %>%
  filter(HSICCD >= 5200, HSICCD < 6000) %>%
  distinct(HCOMNAM, .keep_all = TRUE) %>%
  select(lpermno, HSICCD)

services <- hdr_filter %>%
  filter(HSICCD >= 7000, HSICCD < 9000) %>%
  distinct(HCOMNAM, .keep_all = TRUE) %>%
  select(lpermno, HSICCD)

permnolist <- rbind(manufacturing, retail, services) %>%
  select(lpermno) %>%
  arrange(lpermno)

###########################################################################################################
# Berechnung der verschiedenen Daten - brauche am Ende nur fullclean, scaled_data und normaldata
###########################################################################################################
bigdata <- apply(permnolist, 1, output) %>%
  bind_rows()

# Generiere nochmal Platzhalter, dass wenn irgendwas schief geht bigdata nicht tangiert wird
zwischen <- bigdata

is.na(zwischen) <- sapply(zwischen, is.infinite)

# Daten missing values ersetzen
clean_data <- zwischen %>%
  group_by(date) %>%
  mutate_at(vars(-lpermno, -date, -year, -beta_12m, -MPRC), replace_nas)

# Generierung der Dummy-Variablen
dummy_data <- rbind(manufacturing, retail, services)

dummy_data_clean <- left_join(clean_data, dummy_data, by = c("lpermno"))

dummy_data_clean$manufacturing <- ifelse(dummy_data_clean$HSICCD >= 2000 & dummy_data_clean$HSICCD < 4000, 1, 0)
dummy_data_clean$retail <- ifelse(dummy_data_clean$HSICCD >=5200 & dummy_data_clean$HSICCD < 6000, 1, 0)
dummy_data_clean$services <- ifelse(dummy_data_clean$HSICCD >=7000 & dummy_data_clean$HSICCD < 9000, 1, 0)

clean_data <- dummy_data_clean %>%
  select(-HSICCD)

# loeschen von nicht benoetigten dataframes
rm(dummy_data,
   manufacturing,
   hdr_filter,
   retail,
   services)

# Diese Unternehmen muessen raus (sehr starke Ausreißer, fehlende Betas und fehlende Aktienkurse)
without <- c(10253,10661,10693,10779,10966,11275,11511,11636,
             11803,11896,12146,12266,12503,12570,14761,18148,20670,21178,22323,
             22825,24476,27167,29058,34666,34673,37154,37381,43634,44813,48072,	
             49488,50017,52250,55862,56945,59256,60038,61508,62958,63546, 64742,
             64899,65429,65700,67029,68145,72304,74836,78664,78960,
             79573,81190,86290,86474,87725,92583)

fullclean <- clean_data %>%
  filter(!lpermno %in% without)

# Normale Daten ohne services
normaldata <- fullclean %>%
  select(-services)

############################################### Daten transformieren wie Gu et al (2020) fuer die Beta-Vorhersage - NNs

# Daten transformieren, außer Makro-Daten und historisches Beta
scaled_data <- fullclean %>%
  group_by(date) %>%
  mutate_at(vars(-lpermno, -date, -year, -beta_12m, -MPRC, -beta_12m_lag, -M1_lag, -CPI_lag, -TB3_lag,
                 -TB6_lag, -DGS10_lag, -DYS_lag, -DYS_BAA_TB3_lag, -UNEMPLOY_lag, -ntis_lag, -bm_macro_lag, 
                 -corpr_lag, -manufacturing, -retail, -services), 
            transform_data) %>%
  select(-services) # um eine Dummy-Falle zu verhindern

# Alle Variablen transformiert bis auf beta_12m_lag
test_transform <- scaled_data %>%
  group_by(lpermno) %>%
  mutate_at(vars(M1_lag, CPI_lag, TB3_lag, TB6_lag, DGS10_lag, DYS_lag, DYS_BAA_TB3_lag, UNEMPLOY_lag, ntis_lag, 
                 bm_macro_lag, corpr_lag), 
            transform_data) %>%
  ungroup()

# Alle Variablen transformiert:
test_transform2 <- test_transform %>%
  group_by(date) %>%
  mutate_at(vars(beta_12m_lag), 
            transform_data) %>%
  ungroup()

# Test fuer das Ranking:
#test_ranking <- fullclean %>%
 # filter(year(date) == 1990, month(date) == 1) %>%
 #select(-date, -lpermno, -year, -manufacturing, -services, -retail)

#test_ranking <- test_ranking[,-1] %>%
 # select(EPS_gr_lag)

#rx <- rank(test_ranking, na.last = TRUE)
#non_nas <- sum(!is.na(test_ranking))
#rx[rx>non_nas] <- NA
#calculation_test <- 2*((rx/non_nas) - 0.5)

############################################### Daten transformieren wie Gu et al (2020) fuer die MPRC-Vorhersage - NNs

# Daten transformieren, außer Makro-Daten und historischer Aktienkurs
scaled_data_mprc <- fullclean %>%
  group_by(date) %>%
  mutate_at(vars(-lpermno, -date, -year, -beta_12m, -MPRC, -mprc_lag,-M1_lag, -CPI_lag, -TB3_lag,
                 -TB6_lag, -DGS10_lag, -DYS_lag, -DYS_BAA_TB3_lag, -UNEMPLOY_lag, -ntis_lag, -bm_macro_lag, 
                 -corpr_lag, -manufacturing, -retail, -services),
            transform_data) %>%
  select(-services)

# Alle Variablen transformiert bis auf mprc_lag
test_transform_mprc <- scaled_data_mprc %>%
  group_by(lpermno) %>%
  mutate_at(vars(M1_lag, CPI_lag, TB3_lag, TB6_lag, DGS10_lag, DYS_lag, DYS_BAA_TB3_lag, UNEMPLOY_lag, ntis_lag, 
                 bm_macro_lag, corpr_lag), 
            transform_data) %>%
  ungroup()

# Alle Variablen transformiert:
test_transform_mprc2 <- test_transform_mprc %>%
  group_by(date) %>%
  mutate_at(vars(mprc_lag), 
            transform_data) %>%
  ungroup()

# Habe es getestet, macht keinen großen Unterschied, ob ich erst replace und dann scale
# oder erst scale und dann replace

# restlichen data.frames loeschen (werden nicht mehr benoetigt)
rm(beta_calc, daily_all, dailyprimary, empirical, empirical_lag, incomeqrtrly, incomeqtrly, monthlyprimary,
   shares, tbl_balanceincomeqrt, tbl_balanceqrtrly, tbl_betas_daily, tbl_cashflow, tbl_dailyreturn, 
   tbl.crsp_daily_nested, market, distributions, daily_return, tbl_incomeqrtrly, test2,
   balancequarterly_t, incomequarterly_t,clean_data,zwischen,wins_data,dummy_data_clean,test)

# Datum-Vektor erstellen
# brauche ich fuer spaetere Daten-Generierung:
date <- scaled_data %>%
  select(date) %>%
  filter(year(date) > 2013, year(date) < 2020)

date2 <- scaled_data %>%
  select(date) %>%
  filter(year(date) > 2013, year(date) < 2020) %>%
  distinct()

# Durchschnitt der Actual betas
beta_12m_mean <- normaldata %>%
  group_by(lpermno) %>%
  do(beta = actual_func(.[,1:45])) %>%
  unnest(everything())

beta_12m_mean2 <- pivot_wider(beta_12m_mean, names_from = lpermno, values_from = beta_12m, values_fn = list) %>%
  unnest(everything())

# Durchschnitt der tatsaechlichen Aktienkurse
mprc_actual <- scaled_data_mprc %>%
  group_by(lpermno) %>%
  do(beta = actual_func_mprc(.[,1:45])) %>%
  unnest(everything())

# Fuer die Performancemaße noch in andere Form bringen:
mprc_actual2 <- pivot_wider(mprc_actual, names_from = lpermno, values_from = MPRC, values_fn = list) %>%
  unnest(everything())

# Beta-Datei erstellen, um es in der Datenaufbereitung zu verwenden:
beta_test <- beta_12m_mean %>%
  rename("beta_hist" = "beta_12m") 

# historisches Beta
beta_hist1 <- normaldata %>%
  select(date, lpermno, beta_12m_lag) %>%
  filter(year(date) > 2013, year(date) < 2020)

# historisches Beta fuer Datenaufbau 2:
beta_hist2 <- normaldata %>%
  select(date, lpermno, beta_12m_lag) %>%
  filter(year(date) > 2013, year(date) < 2020) %>%
  arrange(lpermno, date)

beta_hist2 <- beta_hist2[,2:3]

# Fuer die Rendite-Berechnung auf Basis der CAPM-Betas
beta_hist3 <- pivot_wider(beta_hist2, names_from = lpermno, values_from = beta_12m_lag, values_fn = list) %>%
  unnest(everything())

# historischer Aktienkurs
mprc_hist <- scaled_data_mprc %>%
  select(date, lpermno, mprc_lag) %>%
  filter(year(date) > 2013, year(date) < 2020)

###########################################################################################################
# Export Summary Data - Abbildung der Aktiencharakteristika in der Masterarbeit
###########################################################################################################
summary_data <- as.data.frame(summary(normaldata))

summary_data <- summary_data[19:276,2:3]

summary_data <- summary_data %>%
  separate(Freq, c("name", "value"), sep = ":")

summary_data$value <- gsub("\\.", ",", summary_data$value)

summary_data <- summary_data %>%
  pivot_wider(-name, values_fn = list)

summary_data <- summary_data[1:42, 1:7]

summary_data <- as.data.frame(summary_data) %>%
  rename("Charakteristik" = "Var2")