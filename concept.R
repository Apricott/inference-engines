get_entropy<-function(X,C)
{
  n<-length(X)
  
  nc1<-sum(C==1);nc2<-sum(C==2) #number of classes 1 and 2 in X
  
  p1<-nc1/n;  p2<-nc2/n
  ent<- -p1*ifelse(p1>0,log2(p1),0)-p2*ifelse(p2>0,log2(p2),0)
  return(ent)
}

get_split_entropy<-function(X,C,node)
{
  n<-length(X)
  pos<-which(X<=X[node]) #Decision rule

  X1<-X[pos];  X2<-X[-pos] #Split X by set node
  C1<-C[pos];  C2<-C[-pos]; #Split C by set node
  
  #Entropy X1
  ent1<-0; n1<-length(X1)
  if(n1!=0) ent1<-get_entropy(X1,C1)
  #Entropy X2
  ent2<-0;  n2<-length(X2)
  if(n2!=0) ent2<-get_entropy(X2,C2)
  #Split entropy
  s_ent<- ent1*n1/n+ent2*n2/n
  return (s_ent)
}

fnd_node<-function(X,C)
{
  node<-1
  min_ent<-get_split_entropy(X,C,node)
  for(i in 2:length(X))
  {
    ent<-get_split_entropy(X,C,i)  
    if(ent<min_ent) {min_ent<-ent;node<-i}
  }
  return(node)
}


crt_dtree<-function(X,C)
{
  lstX<-list(X) #List of splitted variables datasets
  lstC<-list(C) #List of splitted classes datasets
  PosN<-rbind("root") #Position of node in current data set
  ValN<-rbind("-") #Value of node
  HF<-rbind("r") #Hierarchical flag
  i<-1
  while(i<=length(lstX))
  {
    node<-fnd_node(lstX[[i]],lstC[[i]]) #find node for current dataset
    ent<-get_entropy(lstX[[i]],lstC[[i]]) #calculate entropy
    if(ent!=0)
    {
      #Split data set by node
      pos<-which(lstX[[i]]<=lstX[[i]][node])
      X1<-lstX[[i]][pos];X2<-lstX[[i]][-pos];
      C1<-lstC[[i]][pos];C2<-lstC[[i]][-pos];
      #Add to list new datasets
      lstX[length(lstX)+1]<-list(X1)
      lstX[length(lstX)+1]<-list(X2)
      lstC[length(lstC)+1]<-list(C1)
      lstC[length(lstC)+1]<-list(C2)
      #Save information
      HF<-rbind(HF,paste0(HF[i],"x"))
      HF<-rbind(HF,paste0(HF[i],"y"))
      PosN<-c(PosN,node) 
      ValN<-c(ValN,round(lstX[[i]][node],3))
    }
    else
      ValN<-c(ValN,round(unique(lstC[[i]]),3))
    i<-i+1
  }
  tree<-list(HF,PosN,ValN) #Key information about tree
  names(tree)<-c("HF","PosN","ValN")
  return(tree)
}


set.seed(6) #We set seed for freezing results
n1<-10;n2<-10

#Variables from Gaussian distribution with different parameters
X<-c(rnorm(n1,0),rnorm(n2,1))
#Classes
C<-c(rep(1,n1),rep(2,n2))
#Create a tree
tree<-crt_dtree(X,C)

tree[3]
