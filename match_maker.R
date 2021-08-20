## Oates Family secret santa match maker

library(tidyverse)

xmas_names <- read_csv(here::here("xmas_names.csv")) %>% 
  select(Person, Group)


match_maker <- function() {
  
  dim_names <- xmas_names
  matches <- vector(mode = "character", length = 13)
  already_matched <- vector(mode = "character")
  
  for (i in 1:13) {
    temp_names <- xmas_names %>%
      filter(Group != dim_names$Group[i]) %>% 
      filter(!Person %in% already_matched)
    
    sample_number <- sample(1:nrow(temp_names), 1)
    
    
    xmas_match <- temp_names$Person[sample_number]
    xmas_pair <- c(paste(dim_names[i, 1], "+", xmas_match))
    matchee <- dim_names[i, 1]
    xmas_vec <- c(matchee, xmas_match)
    
    matches[i] <- xmas_pair 
    
    already_matched <- append(already_matched, xmas_vec)
    
    dim_names <- dim_names %>% 
      filter(Person != xmas_match) 
  }
  
  return(matches)
  
}

match_maker()






