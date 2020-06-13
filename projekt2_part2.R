
## 
## NOTA 1: dobrze dobrany zbior danych by duzo pomogl xD

is.contained <- function(list1,list2) {
# check if list1 is contained in list2
  x <- FALSE
  if (length(list2) > 0) {
    if (length(list1) > 0) {
      for (i in 1:length(list2)) {
        x <- TRUE
        for (j in 1:length(list1)) {
          x <- (list1[[j]] %in% list2[[i]]) & x
        }
        if(x == TRUE) {
          break
        }
      }
    }
    else {
      x <- TRUE
    }
  }
  return(x)
}


forward_chaining <- function(fact = c()) {
# assert all known facts (and/or add new)
  if (!is.contained(fact, facts)){
    facts[length(facts) + 1] <<- list(fact)
    is_new <<- TRUE
  }
  
  while(is_new) {
    is_new <<- FALSE
    for(assertion in facts) {
      if(assertion[1] == 'mammal')
        forward_chaining(c('vertebrate', assertion[2]))
      if(assertion[1] == 'vertebrate')
        forward_chaining(c('animal', assertion[2]))
      if(assertion[1] == 'vertebrate' & is.contained(c('flying', assertion[2]), facts))
        forward_chaining(c('bird', assertion[2]))
    }
    
  }
}


#=================== TABELA 1 =========================================== 

# Wnioskowanie w przod
facts <- list()
facts[[1]] <- list('vertebrate','duck')
facts[[2]] <- list('flying','duck')
facts[[3]] <- list('mammal','cat')

is_new <- TRUE
forward_chaining()
print(facts)






