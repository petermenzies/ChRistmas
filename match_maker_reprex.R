# Reprex names tibble creation

reprex_names <- tibble::tribble(
                   ~Person,      ~Group,
                   "Peter",    "my_fam",
                    "Holt",    "my_fam",
                  "Austin",   "rob_fam",
                   "Jesse",   "rob_fam",
                    "Justin", "steve_fam",
                  "Adam", "steve_fam",
                  )

match_maker_reprex <- function() {
  
  dim_names <- reprex_names
  matches <- vector(mode = "character", length = 3)
  already_matched <- vector(mode = "character")
  
  for (i in 1:3) {
    temp_names <- reprex_names %>%
      filter(Group != dim_names$Group[i]) %>% 
      filter(!Person %in% already_matched)
    
    if (nrow(temp_names) == 0) {
      return(match_maker_reprex())
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


match_maker_reprex()


