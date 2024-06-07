library(shiny)

ui <- fluidPage(
  titlePanel("Quiz with Branching Logic"),
  uiOutput("questionUI"),
  actionButton("nextBtn", "Next")
)

server <- function(input, output, session) {
  questions <- list(
    list(question = "Question 1: What is your favorite color?", choices = c("Red", "Blue", "Green"), next_q = c(2, 3, 4)),
    list(question = "Question 2: What is your favorite animal?", choices = c("Cat", "Dog"), next_q = c(5, 6)),
    list(question = "Question 3: What is your favorite food?", choices = c("Pizza", "Burger"), next_q = c(5, 6))
  )
  
  current_question <- reactiveVal(1)
  
  output$questionUI <- renderUI({
    question <- questions[[current_question()]]
    radioButtons("response", question$question, choices = question$choices)
  })
  
  observeEvent(input$nextBtn, {
    user_response <- input$response
    next_question <- questions[[current_question()]]$next_q[match(user_response, questions[[current_question()]]$choices)]
    current_question(next_question)
  })
}


shinyApp(ui, server)
