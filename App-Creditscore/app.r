library("shiny")
library("shinyWidgets")

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # Set gradient background color
  setBackgroundColor(
    color = c("#a7aabd"),
  ),
  
  # App title and styling
  titlePanel(p("CREDIT SCORE", style = "color:#006ad2")),
  
  # Sidebar layout with input and output definitions
  sidebarLayout(
    # Sidebar panel for inputs
    sidebarPanel(
      # Title for input section
      h1("Parameter", style = "color:#434656"),
      sliderInput("Age", label = "Alter", min = 18, max = 99, value = 40, step = 1),
      numericInput("Annual_Income", label = "Jaehrliches Einkommen", value = 1000, min = 0, max = 1000000),
      numericInput("Num_Bank_Accounts", label = "Anzahl Bankkonten", value = 1, min = 1, max = 15),
      numericInput("Num_Credit_Card", label = "Anzahl Kreditkarten", value = 1, min = 1, max = 15),
      numericInput("Num_of_Delayed_Payment", label = "Anzahl verspaeteter Zahlungen", value = 0, min = 0, max = 50),
      numericInput("Num_of_credit_requests", label = "Anzahl der Kreditantraege", value = 0, min = 0, max = 50),
      numericInput("Outstanding_Debt", label = "Ausstehende Schulden", value = 0, min = 0, max = 10000),
      numericInput("Credit_History_Age", label = "Kredit Alter", value = 1, min = 1, max = 500),
      numericInput("Monthly_rate", label = "Monatliche Rate", value = 0, min = 0, max = 5000),
      numericInput("Amount_invested_monthly", label = "Investierter Monatlicher Betrag", value = 0, min = 0, max = 100000),
      numericInput("Monthly_Balance",labe = "Monatliche Bilanz", value = 0,min = 0, 100000)
    ),
    # Main panel for outputs
    mainPanel(
      # Title for output section
      h1("Main Panel"),
      textOutput("PrognoseRaw"),
      textOutput("Prognose"),
      imageOutput("num_image"),
    )
  )
)

# Define server logic ----
server <- function(input, output) {
  
  # Das Ergebnis ist ein Vielfaches von 10
  nearest_multiple_of_10 <- function(x) {
    # Compute the difference between x and the nearest multiple of 10
    diff <- x %% 10
    
    # If the difference is less than 5, the nearest multiple of 10 is x - diff,
    # otherwise it is x + (10 - diff)
    if (diff <= 5) {
      x <- (x - diff)
    } else {
      x <- (x + (10 - diff))
    }
    x
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
    
    
    # Berechne die Prognosen für X
    # die Prognose der neuen, eingegebenen Werte stehen im ersten Eintrag des Prognosevektors
    prognosevektor <- predict(model,X)
    prog <- prognosevektor[1]
    
    # der Prognosewert wird noch auf 2 Stellen hinter dem Komma (digits=2) gerundet.
    prog <- round(prog,digits=2)
    prog <- exp(prog)/(1+exp(prog))
    
    # der errechnete Wert soll als Ergebnis der Funktion zurückgegeben werden
    prog
  })   
  
  output$Prognose <- renderText({
    
    # der Wert der Prognose aus der Funktion prognose()
    prog <- prognose()*100
    Ausgabe <- nearest_multiple_of_10(prog)
  })
  
  output$PrognoseRaw <- renderText({
    prog <- prognose()*100
    Ausgabe <- prog
  })
  
  
  
  img_file <- reactive({
    paste0("images/", nearest_multiple_of_10(prognose()*100), ".png")
  })
  
  # Output the image file
  output$num_image <- renderImage({
    list(src = img_file(), contentType = "image/png", width = "100%", alt = "Image of the number entered")
  }, deleteFile = FALSE)
}

# Run the app ----
shinyApp(ui = ui, server = server)
