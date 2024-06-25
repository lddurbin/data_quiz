df_to_list <- function(df) {
  list <- df |> 
    group_by(id, question) |> 
    nest() |> 
    list_choices() |> 
    select(-data) |> 
    pmap(list)
  
  return(list)
}

list_choices <- function(df) {
  choices <- df |> 
    mutate(choices = map(data, ~{
      .x |> 
        mutate(
          text = choice_text,
          image = choice_image,
          next_q = map(choice_next_q, ~ if(is.na(.x)) NULL else .x)
        ) |> 
        select(text, image, next_q) |> 
        pmap(list)
    }))
  
  return(choices)
}
