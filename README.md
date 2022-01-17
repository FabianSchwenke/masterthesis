# masterthesis
Kurze Erläuterung des Codes meiner Masterarbeit.

Die Datenanalyse wurde in fünf verschiedene Skripte eingeteilt:

DataPreparationAndFunctions - Dieses Skript kann man einfach komplett durchlaufen lassen. Hier laufen erst die Funktionen durch 
(u.a. für die Datenaufbereitung selbst aber auch für die ML-Methoden). Anschließend wird die Datenaufbereitung durchgeführt.

Predictions - Hier werden die eigentlichen Prozesse ausgeführt. Dabei ist es wichtig, die jeweiligen Methoden mit dem gleichen Datentyp zu trainieren 
und zu testen (spielt aber auch nur bei den NNs eine Rolle). Der Rest der Methoden wird mit den normalen Daten durchgeführt. 
Welcher Datensatz, was enthält, wird auch im Code beschrieben. Sowohl für die CAPM-Betas als auch Aktienkurse werden dann die Modelle und Ergebnisse 
in Form einer RDS gespeichert. Die adjustierte caret-Funktion zum Neuronalen Netz befindet sich auch in diesem Skript. Hier wurden Dinge wie die 
Batch Normalization ergänzt. Außerdem ist noch eine Funktion enthalten, die für jedes Modell die Ergebnisse und den Durchschnitt der jeweiligen 
Ergebnisse berechnet. (NN-Ensemble)

PerformanceMeasuresAndGraphics - Hier werden zunächst die als RDS-Datei gespeicherten Ergebnisse eingelesen. Anschließend kann man wieder die Funktionen 
(MSE, Korrelation, Rsquared und die Genauigkeit) durchlaufen lassen. Grafiken werden automatisch erstellt. Die Bereiche sind eingeteilt. Es gibt einen 
Aktienkurs-Bereich und einen CAPM-Beta-Bereich.

ComparisonOfReturns - Hier erfolgen die Berechnungen der Renditen aus den prognostizierten Betas und Aktienkursen. Außerdem gibt es wieder einige Funktionen, 
die zunächst ausgeführt werden müssen. Abschließend werden die Rendite und die zugehörigen Performancemaße berechnet.

VariableImportance - Die Methoden Bagging, Random Forest, XGBoost und GBM haben schon vorinstallierte Variablen-Wichtigkeitsmaße, die verwendet wurden. 
Dafür müssen aber zunächst die Modelle (xgbmod, lassomod etc.) in das R-Environment mit readRDS geladen werden. Für die NNs musste ich eine Lösung finden,
da es sich um ein Custom-Modell von caret handelt. Im Endeffekt habe ich mich für Shapley-Werte entschieden. Da SHAP (oder in R shapper, ein wrapper von Python) 
leider nicht funktioniert hat, konnte ich hier nur einzelne Shapley-Werte mit dem iml-Package von Christoph Molnar bestimmen.
