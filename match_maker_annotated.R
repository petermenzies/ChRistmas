## Oates Family secret santa match maker

library(tidyverse)
library(here)

xmas_names <- read_csv(here::here("xmas_names.csv")) %>% 
  select(Person, Group)


match_maker <- function() {
  
  # dim_names (diminishing) is the df that will be iterated over and indexed with i and will have the random match filtered out at the end of each iteration. Creating dim_names allows us to iterate through the list without using any names that have already been selected, and that is separate from temp_names, which will change each iteration and is used strictly to create the random match. It also allows xmas_names to remain unaffected.
  dim_names <- xmas_names
  matches <- vector(mode = "character", length = 13)
  already_matched <- vector(mode = "character")
  
  for (i in 1:13) {
    temp_names <- xmas_names %>%
      # temp_names (for pulling the random match from) is filtered each iteration to remove members of current
      # family unit and anyone in the already_matched vector we add to each iteration
      filter(Group != dim_names$Group[i]) %>% 
      filter(!Person %in% already_matched)
    
    # depending on the order of matching, sometimes only members of the same family unit are left at the end and
    # this results in an error or an NA match - still not sure why there are different outcomes
    # this if statement recursively runs the function if temp_names = 0 (meaning there are no viable matches left)
    if (nrow(temp_names) == 0) {
      return(match_maker())
    }
    
    sample_number <- sample(1:nrow(temp_names), 1, replace = FALSE)
    
    xmas_match <- temp_names$Person[sample_number]
    
    xmas_pair <- c(paste(dim_names[i, 1], "+", xmas_match))
    matchee <- dim_names[[i, 1]]
    xmas_vec <- c(matchee, xmas_match)
    
    matches[i] <- xmas_pair
    
    already_matched <- append(already_matched, xmas_vec)
    
    
    dim_names <- dim_names %>% 
      filter(Person != xmas_match)
    
  }
  
  return(matches)
  
}

match_maker()




