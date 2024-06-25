library(shiny)

# Define questions as a list
questions <- list(
  list(
    id = 1,
    question = "Choose your era",
    choices = list(
      list(
        text = "The Elemental Age: I am younger than 26.",
        image = "age_of_elements.png",
        next_q = 2
      ),
      list(
        text = "The Age of Discovery: I'm aged 26 to 39.",
        image = "age_of_discovery.png",
        next_q = 2
      ),
      list(
        text = "The Golden Age: I'm aged 40 to 55.",
        image = "golden_age.png",
        next_q = 2
      ),
      list(
        text = "The Age of the Sages: I'm 56 or older.",
        image = "age_of_sages.png",
        next_q = 2
      )
    )
  ),
  list(
    id = 2,
    question = "Are you a spellcaster?",
    choices = list(
      list(
        text = "Yes, I regularly weave powerful lines of code to manipulate the forces of data",
        image = "spellcaster_yes.png",
        next_q = 3
      ),
      list(
        text = "No, I do not pursue the magical arts of the coders",
        image = "spellcaster_no.png",
        next_q = 4
      )
    )
  ),
  list(
    id = 3,
    question = "Choose your weapon(s)",
    choices = list(
      list(
        text = "Staff of Pythons: I weave spells using Python, casting fiery arrays and tuples",
        image = "staff_of_pythons.png",
        next_q = NULL
      ),
      list(
        text = "Grimoire of R: I weave spells using R, summoning shadowy forces from the Tidyverse",
        image = "grimoire_of_r.png",
        next_q = NULL
      ),
      list(
        text = "Sceptre of SQL: I weave spells using SQL, selecting arcane forces from the stars",
        image = "sceptre_of_sql.png",
        next_q = NULL
      )
    )
  ),
  list(
    id = 4,
    question = "Choose your weapon(s)",
    choices = list(
      list(
        text = "Excelibur, Greatsword of Heroes: My sword slices through data in spreadsheets",
        image = "excelibur.png",
        next_q = NULL
      ),
      list(
        text = "Dragon-Drop Daggers: I pierce through my data with cutting-edge tools like Alteryx, Tableau Prep, or Power Query",
        image = "dragon_drop_daggers.png",
        next_q = NULL
      ),
      list(
        text = "Bow of Pure Intuition: I do not use data. I rely on my inner wisdom and experience to hit my mark",
        image = "bow_of_pure_intuition.png",
        next_q = NULL
      )
    )
  )
)

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
