# Load the necessary library
library(shiny)
library(ggplot2)

# Function fo rate of interest based on credit score(cr_score)
interest_func <- function(cr_score) {
  if (cr_score < 693) {
    return(NULL)  #below 693, customer is not eligible for loan
  } else if (cr_score <= 742) {
    return(10)  #ROI 10% for credit score 742 and less
  } else if (cr_score <= 789) {
    return(9.5)  #ROI 9.5% for credit score 789to 743
  } else {
    return(9)  #ROI 9.5% for credit score 790 and above
  }
}

# Function to calculate Equated Monthly Installment (EMI)
calculate_emi <- function(loan_amount, interest_rate, loan_period = 24) {
  monthly_interest_rate <- interest_rate / 12 / 100
  emi <- loan_amount * monthly_interest_rate * ((1 + monthly_interest_rate) ^ loan_period) / ((1 + monthly_interest_rate) ^ loan_period - 1)
  return(emi)
}

# Function to assess credit risk
credit_risk <- function(cr_score, loan_amount, monthly_income, loan_period) {
  interest_rate <- interest_func(cr_score)
  if (is.null(interest_rate)) {
    return("No loan can be sanctioned due to poor credit score")
  }
  emi <- calculate_emi(loan_amount, interest_rate, loan_period)
  nmi_ratio <- (emi / monthly_income) * 100
  # Assessment of credit risk
  if (nmi_ratio > 50) {
    return("Loan amount cannot be given due to low income (high EMI/NMI ratio)")
  } else if (cr_score <= 742) {
    return("High chances of default. Please do proper due diligence and collect proper documents before sanctioning the loan. Check for any written-off loans")
  } else if (cr_score <= 789) {
    return("Medium chances of default. Please do proper due diligence and collect proper documents before sanctioning the loan")
  } else {
    return("Low chances of default")
  }
}



ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=Yusei+Magic&display=swap');
      body {
        background-color: Lightblue;
        color: Darkblue;
      }
      h2 {
        font-family: 'Times New Roman', Times;
      }
      .shiny-input-container {
        color: #333333;
      }
      .shiny-text-output {
        font-size: 18px;
        margin-bottom: 20px;  # Add space below the risk assessment text
      }
    "))
  ),
  titlePanel("Credit Risk Assessment"),
  sidebarLayout(
    sidebarPanel(
      
      numericInput("cr_score", "Credit Score:", value = 750, min = 300, max = 900),
      numericInput("monthly_income", "Monthly Income after deductions ($):", value = 4000, min = 0),
      numericInput("loan_amount", "Loan Amount ($):", value = 25000, min = 0),
      numericInput("loan_period", "Loan Period (Months):", value = 24, min = 1, max = 360),
      actionButton("assess", "Assess Risk")
    ),
    mainPanel(
      textOutput("risk_assessment"), # The risk assessment text
      plotOutput("credit_score_bar") # The scatter plot
    )
  )
)



server <- function(input, output) {
  observeEvent(input$assess, {
    risk <- credit_risk(input$cr_score, input$loan_amount, input$monthly_income, input$loan_period)
    output$risk_assessment <- renderText({
      risk
    })
    
    # Render the credit score comparison plot
    output$credit_score_bar <- renderPlot({
      credit_data <- data.frame(
        Category = factor(c("Total Score", "Cut-off Score", "Score of the Customer")),
        Score = c(900, 693, input$cr_score),
        Labels = c(900, 693, paste("Customer:", input$cr_score))
      )
      
      ggplot(credit_data, aes(x = Category, y = Score)) +
        geom_line(group = 1, color = "darkblue") +
        geom_point(size = 5, color = "darkblue") +
        geom_text(aes(label = Labels), nudge_y = 15, color = "darkblue", size = 6) +
        labs(title = "Credit Score Comparison", x = "", y = "Credit Score") +
        theme_minimal() +
        theme(
          plot.title = element_text(hjust = 0.5, size = 16),
          axis.text.y = element_text(color = "darkblue", size = 14),
          axis.title.y = element_text(size = 14)
        )
    }, height = 250)
  })
}

# Run the application with the defined UI and server logic
shinyApp(ui = ui, server = server)

