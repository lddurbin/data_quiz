library(tidyverse)
library(shiny)

source("functions.R")

# Define questions as a list
quiz_data <- readr::read_csv(here::here("data/quiz_questions.csv"))
questions <- df_to_list(quiz_data)

# Function to get question by ID
get_question_by_id <- function(id) {
  for (question in questions) {
    if (question$id == id) {
      return(question)
    }
  }
  return(NULL)
}

# Function to generate valid IDs
generate_id <- function(text) {
  gsub("[^a-zA-Z0-9]", "_", text)
}

ui <- fluidPage(
  titlePanel("Who's Your Fantasy Data Character?"),
  uiOutput("questionUI"),
  actionButton("nextBtn", "Next")
)

server <- function(input, output, session) {
  current_question <- reactiveVal(1)
  results <- reactiveValues(responses = list())
  
  output$questionUI <- renderUI({
    question <- get_question_by_id(current_question())
    if (is.null(question)) return(NULL)
    
    choices_ui <- lapply(question$choices, function(choice) {
      tags$div(class = "radio-inline",
               tags$input(type = "radio", name = "response", value = generate_id(choice$text), id = generate_id(choice$text)),
               tags$label(`for` = generate_id(choice$text),
                          imageOutput(paste0("img_", generate_id(choice$text)), width = 150, height = 150),
                          tags$div(choice$text)
               )
      )
    })
    
    tagList(
      h3(question$question),
      fluidRow(choices_ui)
    )
  })
  
  lapply(questions, function(question) {
    lapply(question$choices, function(choice) {
      local({
        choice_text <- generate_id(choice$text)
        output[[paste0("img_", choice_text)]] <- renderImage({
          list(
            src = file.path("images", choice$image),
            contentType = 'image/png',
            width = 150,
            height = 150,
            alt = choice$text
          )
        }, deleteFile = FALSE)
      })
    })
  })
  
  observeEvent(input$nextBtn, {
    user_response <- input$response
    question <- get_question_by_id(current_question())
    next_question <- NULL
    
    for (choice in question$choices) {
      if (generate_id(choice$text) == user_response) {
        next_question <- choice$next_q
        break
      }
    }
    
    results$responses[[length(results$responses) + 1]] <- user_response
    
    if (!is.null(next_question)) {
      current_question(next_question)
    } else {
      output$questionUI <- renderUI({
        result_text <- paste("Your responses:", paste(unlist(results$responses), collapse = ", "))
        h3(result_text)
      })
    }
  })
}

shinyApp(ui, server)
