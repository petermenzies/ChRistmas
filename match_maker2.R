## Oates Family secret santa match maker

library(tidyverse)
library(here)

xmas_names <- read_csv(here::here("xmas_names.csv")) %>% 
  select(Person, Group) 


match_maker2 <- function() {
  
  dim_names <- xmas_names
  already_matched <- vector(mode = "character")
  output_df1 <- data.frame(Giver = seq(1, 26), Recipient = seq(1, 26))
  
  final_matches <- vector(mode = "character")
  
  for (i in 1:26) {
    temp_names <- xmas_names %>%
      filter(Group != dim_names$Group[i]) %>% 
      filter(!Person %in% already_matched)
    
    if (nrow(temp_names) == 0) {
      return(match_maker2())
    }
    
    sample_number <- sample(1:nrow(temp_names), 1)
    
    xmas_match <- temp_names$Person[sample_number]
    
    matchee <- dim_names[[i, 1]]
    xmas_vec <- c(matchee, xmas_match)
    
    already_matched <- append(already_matched, xmas_match)
    
    output_df1$Giver[i] <- matchee
    output_df1$Recipient[i] <- xmas_match
    
  }
  
  return(output_df1)
  
}

match_maker2()




