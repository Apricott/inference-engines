
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
      # Rules
      if(assertion[1] == 'cylinders' & assertion[2] > 5 & is.contained(list('hp', '>100'), facts))
        forward_chaining(list('brand', 'US.'))
      if(assertion[1] == 'cubicinches' & assertion[2] == '<199')
        forward_chaining(list('weightlbs', '~2500'))
      if(assertion[1] == 'cubicinches' & assertion[2] == '>199')
        forward_chaining(list('cylinders', 8))
      if((assertion[1] == 'brand' & assertion[2] == 'US.'))
        forward_chaining(list('mpg', '~20'))
      if((assertion[1] == 'mpg' & assertion[2] != '~20'))
        forward_chaining(list('year', '>1977'))
      if((assertion[1] == 'cylinders' & assertion[2] > 5))
        forward_chaining(list('year', '<1977'))
      if((assertion[1] == 'brand' & assertion[2] == 'Europe.') | (is.contained(list('weightlbs','~2500'), facts)))
        forward_chaining(list('mpg', '~25'))
      
    }
    
  }
}


cars_data <- read.csv('cars.csv')
sum(is.na(cars_data))
# only 5 NAs in the data frame so we can delete them
cars_data <- na.omit(cars_data)

#=================== Wnioskowanie 1 =========================================== 
print('Przewidywanie mpg na podstawie liczby cylindrow, pojemnosci silnika i mocy') 

for(i in 65:67) {
  facts <- list()
  facts[[1]] <- list('cylinders', cars_data[i, 2])
  facts[[2]] <- list('cubicinches', ifelse(cars_data[i, 3] < 199, '<199', '>199'))
  facts[[3]] <- list('hp', ifelse(cars_data[i, 4] < 100, '<100', '>100'))

  is_new <- TRUE
  forward_chaining()
  print(facts)
}


print('=================================================================================')

#=================== Wnioskowanie 2 =========================================== 
print('Przewidywanie roku produkcji na podstawie kraju pochodzenia i pojemnosci silnika ')

for(i in 65:67) {
  facts <- list()
  facts[[1]] <- list('brand', cars_data[i, 8])
  facts[[2]] <- list('cubicinches', ifelse(cars_data[i, 3] < 199, '<199', '>199'))

  is_new <- TRUE
  forward_chaining()
  print(facts)
}



