get_entropy<-function(C)
{
  # function returns entropy of subset given the vector of classes C of the examples in this subset
  n <- length(C)
  unique_c <- (unique(C))
  nc <- array(numeric())
  for (i in 1:length(unique_c)) {
    if (sum(C == unique_c[i]) > 0) {
      nc <- rbind(nc, sum(C == unique_c[i]))
    }
  }
  # vectorized entropy
  p <- nc/n
  ent <- t(-p)%*%log2(p)
  
  # convert ent to numeric because matrix operations always return a matrix 
  return(as.numeric(ent))
}

get_split_entropy<-function(X,C,node)
{
  n <- length(X)
  pos <- which(X <= X[node]) # Decision rule
  
  X1<-X[pos];  X2<-X[-pos] # Split X by set node
  C1<-C[pos];  C2<-C[-pos]; # Split C by set node
  
  #Entropy X1 I+
  I_plus <- 0
  n1 <- length(X1)
  if(n1 != 0) 
    I_plus <- get_entropy(C1)
  #Entropy X2 I-
  I_minus <- 0
  n2 <- length(X2)
  if(n2 != 0) 
    I_minus <- get_entropy(C2)
  #Split entropy
  E <- (n1/n * I_plus) + (n2/n * I_minus)
  return(E)
}

fnd_node<-function(X, C, I)
{
  # function finds the node (i-th row and j-th column) by which the table is to be divided
  node_j <- 1
  node_i <- 1
  max_inf <- 0
  
  for(j in 1:ncol(X)) {
    column_X <- X[,j]
    # j_th column of X with distinct values only
    unique_X <- unique(column_X)
    # do not divide the table on the max value, because it'l result in an empty table
    if(length(unique_X) > 1) {
      unique_X <- unique_X[which(unique_X < max(unique_X))]
    }
    # same values of Xs column return the same entropy thus we can calculate it only once for every distinct value
    for(i in 1:length(unique_X))
    {
      # get the first occurence of the unique value in the original column  
      pos <- which(column_X == unique_X[i])[1]
      pos <- as.numeric(pos)
      ent<-get_split_entropy(column_X, C, pos)
      
      # save indices for the node with the maximum amount of information
      if(I - ent > max_inf) {
        max_inf <- I - ent
        node_i <- pos
        node_j <- j
      }
    }
    
  }
  return(list(node_i, node_j))
}


crt_dtree<-function(X,C)
{
  lstX <- list(X) # list of splitted tables
  lstC <- list(C) # list of splitted vectors of classess for the matching table
  PosN <- rbind("root") # Position of the node in the current data set
  ValN <- rbind("-") # Value of the node
  HF <- rbind("r") # Hierarchical flag; x and y for the 'left' and 'right' branch, r for 'root'
  node_pos <- list() # node indices in the table
  i <- 1
  while(i <= length(lstX))
  {
    ent <- get_entropy(lstC[[i]]) # calculate entropy
    # if entropy > 0 find the node to split the current table
    if (ent != 0) { 
      node_pos <- fnd_node(lstX[[i]],lstC[[i]], ent) 
    }
    # if entropy > 0 perform the split
    if(ent != 0)
    {
      #Split the table by node
      node_val <- lstX[[i]][node_pos[[1]], node_pos[[2]]]
      pos <- which(lstX[[i]][, node_pos[[2]]] <= node_val)
      
      X1 <- lstX[[i]][pos,]; X2 <- lstX[[i]][-pos,];
      C1 <- lstC[[i]][pos]; C2 <- lstC[[i]][-pos];
      
      #Add new tables to the list 
      lstX[length(lstX) + 1] <- list(X1)
      lstX[length(lstX) + 1] <- list(X2)
      lstC[length(lstC) + 1] <- list(C1)
      lstC[length(lstC) + 1] <- list(C2)
      #Save information
      HF <- rbind(HF, paste0(HF[i], "x"))
      HF <- rbind(HF, paste0(HF[i], "y"))
      PosN <- c(PosN, list(node_pos))  # save the indices of node the split was performed on
      ValN <- c(ValN, round(node_val, 3)) # save value of node the split was performed on
    }
    # otherwise save the class of the examples in the table as a 'leaf' element
    else{
      ValN<-c(ValN, unique(lstC[[i]]))
    }
    i <- i + 1
  }
  tree <- list(HF, PosN, ValN) # list of lists with information about the tree
  names(tree) <- c("HF", "PosN", "ValN")
  return(tree)
}


cars_data <- read.csv('cars.csv')
sum(is.na(cars_data))
# only 5 NAs in the data frame so we can delete them
cars_data <- na.omit(cars_data)

y_name <- "brand"
X <- cars_data[, !(names(cars_data) %in% y_name)]
X <- data.matrix(X)

C <- cars_data[y_name]
C <- as.factor(C[,1])
C <- levels(C)[as.numeric(C)]

tree<-crt_dtree(X,C)
tree[3]

# Poprzez zmapowanie 3 list z informacjami z obiektu 'tree' mozna okreslic wartosci atrybutow dzielacych drzewo na 'galezie', 
# elementy bedace 'liscmi', a takze strukture drzewa


