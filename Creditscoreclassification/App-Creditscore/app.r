
first_bs5_theme <- bs_theme(
  version = 5
)

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "stylesheet.css")
  ),
  navbarPage(
    "Credit Score Classicifation",
    tabPanel("Calculator"),
    tabPanel("Information"),
    tabPanel("About"),
    position = "fixed-top"
  ),
  # App title
  titlePanel(p("Credit Score", id="title"), "Credit Score"),
  
  # Hint on Credit Score
  tags$div(
    id="info",
    p("Ein Credit Score ist ein Wert, der die Kreditwürdigkeit von Dir bewertet.", class="info-creditscore")
    # p("The higher the score, the better a borrower looks to potential lenders.", class = "info-creditscore"),
  ),
  

  
  # Sidebar layout with input and output definitions
  sidebarLayout(
    
    # Sidebar panel for inputs
    sidebarPanel(
      h2("Parameter", id="sidepanel-title"),
      p("Ändern Sie die verschiedenen Parameter für Ihren Anwendungsfall", class="hint"),
      # Title for input section
      
      #<div class="tooltip">Hover over me
      #<span class="tooltiptext">Tooltip text</span>
      #  </div>
      
      tags$div(
        class="tooltip",
          "Alter",
        tags$span(
          class="tooltiptext",
          "Gib hier dein Alter an"
        )
      ),
      sliderInput("Age", label="", min = 18, max = 99, value = 40, step = 1),
      tags$div(
        class="tooltip",
        "Jährliches Einkommen",
        tags$span(
          class="tooltiptext",
          "Gib hier dein Jährliches Einkommen an"
        )
      ),
      numericInput("Annual_Income", label = "", value = 1000, min = 0, max = 1000000),
      tags$div(
        class="tooltip",
        "Anzahl Bankkonten",
        tags$span(
          class="tooltiptext",
          "Gib hier die Anzahl Deiner Bankkonten an",
        )
      ),
      numericInput("Num_Bank_Accounts", label = "", value = 1, min = 1, max = 15),
      tags$div(
        class="tooltip",
        "Anzahl Kreditkarten",
        tags$span(
          class="tooltiptext",
          "Gib hier die Anzahl Deiner Kreditkarten an"
        )
      ),
      numericInput("Num_Credit_Card", label = "", value = 1, min = 1, max = 15),
      tags$div(
        class="tooltip",
        "Anzahl verspäteter Zahlungen",
        tags$span(
          class="tooltiptext",
          "Gib hier die Anzahl Deiner verspäteten Zahlungen an"
        )
      ),
      numericInput("Num_of_Delayed_Payment", label = "", value = 0, min = 0, max = 50),
      tags$div(
        class="tooltip",
        "Kreditanträge",
        tags$span(
          class="tooltiptext",
          "Gib hier an, wie oft du einen Kredit beantragt hast"
        )
      ),
      numericInput("Num_of_credit_requests", label = "", value = 0, min = 0, max = 50),
      tags$div(
        class="tooltip",
        "Ausstehende Schulden",
        tags$span(
          class="tooltiptext",
          "Gib hier an, wie hoch d´Deine Schulden sind"
        )
      ),
      numericInput("Outstanding_Debt", label = "", value = 0, min = 0, max = 10000),
      tags$div(
        class="tooltip",
        "Kredit Alter",
        tags$span(
          class="tooltiptext",
          "???????"
        )
      ),
      numericInput("Credit_History_Age", label = "", value = 1, min = 1, max = 500),
      tags$div(
        class="tooltip",
        "Monatliche Rate",
        tags$span(
          class="tooltiptext",
          "Gib hier an, wie viel Geld Du pro Monat zurückzahlst"
        )
      ),
      numericInput("Monthly_rate", label = "", value = 0, min = 0, max = 5000),
      tags$div(
        class="tooltip",
        "Investierter montlicher Betrag",
        tags$span(
          class="tooltiptext",
          "Gib hier an, wie viel Du monatlich Investiierst",
        )
      ),
      numericInput("Amount_invested_monthly", label = "", value = 0, min = 0, max = 100000),
      tags$div(
        class="tooltip",
        "Monatliche Bilanz",
        tags$span(
          class="tooltiptext",
          "????????????"
        )
      ),
      numericInput("Monthly_Balance",labe = "", value = 0,min = 0, 100000)
    ),
    # Main panel for outputs
    mainPanel(
      # Title for output section
      h2("Output", id="mainpanel-title"),
      p("Zeigt die Ausgabe in Abhängigkeit von den Parametern an", class="hint"),
      textOutput("Prognose_Values"),
      textOutput("Prognose_Highest"),
      textOutput("confusionMatrix"),
    )
  )
)





# Define server logic ----
server <- function(input, output) {
  
  calculate_probability <- function(model,vektor){
    prognosevektor <- predict(model,vektor)
    prog <- prognosevektor[1]
    prog <- round(prog,digits=2)
    prog <- exp(prog)/(1+exp(prog))
    prog <- round(prog,digits = 4)
    prog
  }
  
  top_class <-function(){
    numbers <- prognose()
    class <- ifelse(max(numbers) == numbers[1,3], "Good",
                    ifelse(max(numbers) == numbers[1,2], "Standard", "Poor"))
    class
  }
  
  generate_confusionMatrix <- function (data,model){
    y <- data[,"Credit_Score"]
    Prognosen <- round(model$fitted.values) 
    Prognosen <- as.factor(Prognosen)
    confusion_matrix <- confusionMatrix(data = y, reference = Prognosen)
    confusion_matrix
  }
  
  # Folgende Funktion berechnet die Prognose für die eingegeben Werte  
  prognose <- reactive({
    
    # Speichere die Daten der Einflussvariablen in ein Objekt X
    X <- Daten[,c("Age","Annual_Income","Num_Bank_Accounts","Num_Credit_Card","Num_of_Delayed_Payment","Num_of_credit_requests","Outstanding_Debt",
                  "Credit_History_Age","Monthly_rate","Amount_invested_monthly","Monthly_Balance")]

    # Ersetze die erste Zeile in X nun mit den neuen, eingegebenen Werten
    X[1,"Age"] <- input$Age
    X[1,"Annual_Income"] <- input$Annual_Income
    X[1,"Num_Bank_Accounts"] <- input$Num_Bank_Accounts
    X[1,"Num_Credit_Card"] <- input$Num_Credit_Card
    X[1,"Num_of_Delayed_Payment"] <- input$Num_of_Delayed_Payment
    X[1,"Num_of_credit_requests"] <- input$Num_of_credit_requests
    X[1,"Outstanding_Debt"] <- input$Outstanding_Debt
    X[1,"Credit_History_Age"] <- input$Credit_History_Age
    X[1,"Monthly_rate"] <- input$Monthly_rate
    X[1,"Amount_invested_monthly"] <- input$Amount_invested_monthly
    X[1,"Monthly_Balance"] <- input$Monthly_Balance
    
    prog_poor <- calculate_probability(model_poor,X)
    prog_standard <- calculate_probability(model_standard,X)
    prog_good <- calculate_probability(model_good,X)
    
    prog <- data.frame(prog_poor,prog_standard,prog_good)
    prog
  })
  
  output$Prognose_Values <- renderText({
    prog <- prognose()
    poor <- prog[1,1]*100
    standard <- prog[1,2]*100
    good <- prog[1,3]*100
    Ausgabe <- paste0(" poor: ",poor,"%, standard: ",standard,"%, good: ",good)
  })
  
  output$Prognose_Highest <-renderText({
    Ausgabe <- top_class()
  })
  
  
  
  
  output$confusionMatrix <- renderText({
    poor <- generate_confusionMatrix(data_poor,model_poor)
    standard <- generate_confusionMatrix(data_standard,model_standard)
    good <- generate_confusionMatrix(data_good,model_good)
    # Create a confusion matrix using the confusionMatrix function
    Ausgabe <- paste0("poor: ",poor$overall[1]*100,"% , standard: ",standard$overall[1]*100,"% , good: ",good$overall[1]*100,"%")
  })

}

# Run the app ----
shinyApp(ui = ui, server = server)
