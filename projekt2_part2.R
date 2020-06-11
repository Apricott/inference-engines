
##
## NA RAZIE NIE MA TU NIC PEWNEGO, TYLKO TESTY JAK TO MOZE DZIALAC 
## NOTA 1: dobrze dobrany zbior danych by duzo pomogl xD


#=================== TABELA 1 =========================================== 
# skopiowane ze stacka i przerobione na R (ale nie dziala na razie xd)

facts <- list()
is_changed <- FALSE

facts[[1]] <- list('vertebrate','duck')
facts[[2]] <- list('flying','duck')
facts[[3]] <- list('mammal','cat')

assert_fact <- function(fact) {
  # w tego ifa zawsze wpada, trzeba jakosc inaczej iterowac po zagdniezdzonych listach, pewnie w petli
  # i powinno by git
  if (!fact %in% facts){
    facts[length(facts) + 1] <- list(fact)
    is_changed = TRUE
  }
  
  while(is_changed) {
    is_changed = FALSE
    for(A1 in facts) {
      if(A1[1] == 'mammal')
        assert_fact(c('vertebrate', A1[2]))
      if(A1[1] == 'vertebrate')
        assert_fact(c('animal', A1[2]))
      if(A1[1] == 'vertebrate' & c('flying', A1[2]) %in% facts)
        assert_fact(c('bird', A1[2]))
    }
    
  }
  print(facts)
}

assert_fact(facts[[1]])







