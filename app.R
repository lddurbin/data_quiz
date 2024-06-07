library(shiny)

ui <- fluidPage(
  titlePanel("Who's Your Fantasy Data Character?"),
  uiOutput("questionUI"),
  actionButton("nextBtn", "Next")
)

server <- function(input, output, session) {
  questions <- list(
    list(
      question = "Choose your era",
      choices = c(
        "The Elemental Age: \"I am younger than 26.\"",
        "The Age of Discovery: \"I'm aged 26 to 39.\"",
        "The Golden Age: \"I'm aged 40 to 55.\"",
        "The Age of the Sages: \"I'm 56 or older.\""
      ),
      next_q = c(2, 2, 2, 2)),
    list(
      question = "Are you a spellcaster?",
      choices = c(
        "Yes, I regularly weave powerful lines of code to manipulate the forces of data",
        "No, I do not pursue the magical arts of the coders"
      ),
      next_q = c(3, 4)
    ),
    list(
      question = "Choose your weapon(s)",
      choices = c(
        "Staff of Pythons: \"I weave spells using Python, casting fiery arrays and tuples\"",
        "Grimoire of R: \"I weave spells using R, summoning shadowy forces from the Tidyverse\"",
        "Sceptre of SQL: \"I weave spells using SQL, selecting arcane forces from the stars\""
      )
    ),
    list(
      question = "Choose your weapon(s)",
      choices = c(
        "Excelibur, Greatsword of Heroes: \"My sword slices through data in spreadsheets\"",
        "Dragon-Drop Daggers: \"I pierce through my data with cutting-edge tools like Alteryx, Tableau Prep, or Power Query\"",
        "Bow of Pure Intuition: \"I do not use data. I rely on my inner wisdom and experience to hit my mark\""
      )
    )
  )
  
  current_question <- reactiveVal(1)
  
  output$questionUI <- renderUI({
    question <- questions[[current_question()]]
    radioButtons("response", question$question, choices = question$choices)
  })
  
  results <- reactiveValues(responses = list())
  
  observeEvent(input$nextBtn, {
    user_response <- input$response
    next_question <- questions[[current_question()]]$next_q[match(user_response, questions[[current_question()]]$choices)]
    current_question(next_question)
    
    if (is.null(next_question)) {
      # Display results
      output$questionUI <- renderUI({
        result_text <- paste("Your responses:", paste(unlist(results$responses), collapse = ", "))
        h3(result_text)
      })
    }
  })
}


shinyApp(ui, server)
