## Oates Family secret santa match maker

library(tidyverse)
library(here)

xmas_names <- read_csv(here::here("xmas_names.csv")) %>% 
  select(Person, Group) 


match_maker <- function() {

  output_df <- data.frame(Giver = seq(1:nrow(xmas_names)), Recipient = seq(1:nrow(xmas_names)))
  already_matched <- vector(mode = "character")
  
  for (i in 1:nrow(xmas_names)) {
    temp_names <- xmas_names %>%
      filter(Group != xmas_names$Group[i]) %>% 
      filter(!Person %in% already_matched)
    
    if (nrow(temp_names) == 0) {
      return(match_maker())
    }
    
    sample_number <- sample(1:nrow(temp_names), 1)
    
    xmas_match <- temp_names$Person[sample_number]
    
    matchee <- xmas_names[[i, 1]]
    
    already_matched <- append(already_matched, xmas_match)
    
    output_df$Giver[i] <- matchee
    output_df$Recipient[i] <- xmas_match
    
  }
  
  return(output_df)
  
}

match_maker()




