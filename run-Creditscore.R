# Setzen des Pfades und Einlesen der Daten
setwd("C:/Users/Roman/OneDrive/Dokumente/1 Uni/3.Semester/4 Assistenzsysteme/Hable/Credit score classification")
Daten <- read.csv("clean_credit_score_classification_good_is_Standard.csv",header=TRUE,sep=";",fill=TRUE,stringsAsFactors=TRUE)


# Berechnung der Klassifikation
model <- glm(Credit_Score ~ Age + Annual_Income + Num_Bank_Accounts + Num_Credit_Card + Num_of_Delayed_Payment + Num_of_credit_requests  + Outstanding_Debt  
             + Credit_History_Age  + Monthly_rate + Amount_invested_monthly  + Monthly_Balance, data=Daten,binomial(link = "logit"))


# Starten der Shiny-App
library(shiny)
runApp("App-Creditscore")
