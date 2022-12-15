library(caret)
library(shiny)
library(shinyWidgets)


# Setzen des Pfades und Einlesen der Daten
setwd("C:/Users/gerlo/OneDrive/Dokumente/1 Uni/3.Semester/4 Assistenzsysteme/Creditscoreclassification/Creditscoreclassification")
Daten <- read.csv("clean_credit_score_classification_3.csv",header=TRUE,sep=";",fill=TRUE,stringsAsFactors=TRUE)


data <- subset(Daten,select =  c("Age","Annual_Income","Num_Bank_Accounts","Num_Credit_Card","Num_of_Delayed_Payment","Num_of_credit_requests","Outstanding_Debt",
                                 "Credit_History_Age","Monthly_rate","Amount_invested_monthly","Monthly_Balance","Credit_Score"))

data_extraction <- function(data,class_name) {
  data_ext <- data 
  data_ext[,"Credit_Score"] <- ifelse(data_ext[,"Credit_Score"] == class_name, 1, 0)
  data_ext[,"Credit_Score"] <- as.factor(data_ext[,"Credit_Score"])
  data_ext
}

data_poor <- data_extraction(data,"Poor")
data_standard <- data_extraction(data,"Standard")
data_good <- data_extraction(data,"Good")



generate_model  <- function(data){
  model <- glm(Credit_Score ~ Age + Annual_Income + Num_Bank_Accounts + Num_Credit_Card + Num_of_Delayed_Payment + Num_of_credit_requests  + Outstanding_Debt  
               + Credit_History_Age  + Monthly_rate + Amount_invested_monthly  + Monthly_Balance, data = data,binomial(link = "logit"))
}

model_poor <- generate_model(data_poor)
model_standard <- generate_model(data_standard)
model_good <- generate_model(data_good)



# Starten der Shiny-App
library(shiny)
runApp("App-Creditscore")
