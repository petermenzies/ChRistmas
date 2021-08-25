## Oates Family secret santa match maker

library(tidyverse)
library(here)

xmas_names <- read_csv(here::here("xmas_names.csv")) %>% 
  select(Person, Group) 


match_maker <- function() {
  
  dim_names <- xmas_names
  matches <- vector(mode = "character", length = 13)
  already_matched <- vector(mode = "character")
  xmas_match_vec <- vector(mode = "character")
  output_df1 <- data.frame(Giver = seq(1, 26), Receiver = seq(1, 26))
  
  final_matches <- vector(mode = "character")
  
  for (i in 1:13) {
    temp_names <- xmas_names %>%
      filter(Group != dim_names$Group[i]) %>% 
      filter(!Person %in% already_matched)
    
    if (nrow(temp_names) == 0) {
      return(match_maker())
    }
    
    sample_number <- sample(1:nrow(temp_names), 1, replace = FALSE)
    
    xmas_match <- temp_names$Person[sample_number]
    
    xmas_pair <- c(paste(dim_names[i, 1], ">", xmas_match))
    matchee <- dim_names[[i, 1]]
    xmas_vec <- c(matchee, xmas_match)
    
    matches[i] <- xmas_pair
    
    already_matched <- append(already_matched, xmas_vec)
    
    xmas_match_vec <- append(xmas_match_vec, xmas_match)
    
    output_df1$Giver[i] <- matchee
    output_df1$Receiver[i] <- xmas_match
    
    dim_names <- dim_names %>% 
      filter(Person != xmas_match)
    
  }
  
  final_matches <- append(final_matches, matches)
  
  dim_names2 <- xmas_names %>%
    filter(Person %in% xmas_match_vec)
  
  matches2 <- vector(mode = "character", length = 13)
  already_matched2 <- vector(mode = "character")
  
  for (i in 1:13) {
    temp_names2 <- xmas_names %>%
      filter(Group != dim_names2$Group[i]) %>%
      filter(!Person %in% xmas_match_vec) %>% 
      filter(!Person %in% already_matched2) %>% 
      filter(Person != output_df1$Receiver[i])
    
    
    if (nrow(temp_names2) == 0) {
      return(match_maker())
    }
    
    sample_number <- sample(1:nrow(temp_names2), 1, replace = FALSE)
    
    xmas_match <- temp_names2$Person[sample_number]
    
    xmas_pair <- c(paste(dim_names2[i, 1], ">", xmas_match))
    matchee <- dim_names2[[i, 1]]
    xmas_vec <- c(matchee, xmas_match)
    
    matches2[i] <- xmas_pair
    
    already_matched2 <- append(already_matched2, xmas_vec)
    
    output_df1$Giver[i + 13] <- matchee
    output_df1$Receiver[i + 13] <- xmas_match
    
  }
  
  final_matches <- append(final_matches, matches2)
  
  return(output_df1)
  
}

match_maker()




