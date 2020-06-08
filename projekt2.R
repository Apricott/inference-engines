get_entropy<-function(X,C)
{
  # function returns entropy of X's subset given the vector of classes C of the examples in this subset
  
  n <- length(X)
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
  # to jest dobre dla ciaglych wartosci cech, dla tabel 0-1 trzeba chyba napisac te funckje inaczej
  # np. Decision Rule jako dzielenie na przyklady z 0 i 1 w danym atrybucie
  
  n<-length(X)
  pos<-which(X<=X[node]) #Decision rule
  
  # to [-pos] chyba trzeba przerobic, bo dla innych zestawow danych moze sie tak ladnie nie podzielic (?)
  X1<-X[pos];  X2<-X[-pos] #Split X by set node
  C1<-C[pos];  C2<-C[-pos]; #Split C by set node
  
  # ewentualnie mozna rozwazyc dodanie warunku stopu, gdy dlugosc wektora C1 lub C2 bedzie mniejsza 
  # niz podana przez uzytkownika ='min_leaf' ,co moze przeciwdzialac przeuczeniu drzewa dla konkretnego zbioru danych
  # wtedy trzeba zmienic przyporzadkowanie liscia do konkretnej klasy w funkcji fnd_node np. poprzez prawdopodobienstwo 
  
  #Entropy X1 I+
  ent1<-0
  n1<-length(X1)
  if(n1!=0) 
    ent1<-get_entropy(X1,C1)
  #Entropy X2 I-
  ent2<-0
  n2<-length(X2)
  if(n2!=0) 
    ent2<-get_entropy(X2,C2)
  #Split entropy
  # laczna entropia po ocenie warunku/atrybutu j (podzialu na warunki jeszcze nie ma :)) 
  # moze jako X w tej funkcji zostawic tylko wektor z tym warunkiem, a ogarniecie wielu warunkow w fnd_node?
  
  # E = n1/n * I+ + n2/2 * I-
  s_ent<- ent1*n1/n+ent2*n2/n
  return (s_ent)
}

fnd_node<-function(X,C)
{
  # funkcja sprawdza wobec ktorego atrybutu nalezy podzielic w danym kroku tabele
  # trzeba to zrobic na wszystkich kolumnach tabeli, teraz jest tylko na 1, bo X ma tylko jedna
  node<-1
  min_ent<-get_split_entropy(X,C,node)
  for(i in 2:length(X))
  {
    ent<-get_split_entropy(X,C,i)  
    # poniewaz szukamy max(I-Ej), jest to rownoznaczne z szukaniem min(Ej)
    # zeby zrobic to jak w prezentacji trzeba policzyc I = -1/2*log(1/2)-1/2*log(1/2) = 1, (poniewaz mamy 2 klasy, 
    # do ktorych nalezy po 10 przykladow, czyli prawdopodobienstwo trafienia na kazda klase to 1/2) 
    # (mozna to zrobic funkcja get_entropy() z gory) 
    
    # i od tego odejmowac ent i sprawdzic, dla podzialu wedlug ktorego atrybutu/warunku wynik jest najwiekszy
    if(ent<min_ent) {
      min_ent<-ent
      node<-i
      }
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
    # node to atrybut/warunek, wedlug ktorego dzielimy tabele
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
      HF<-rbind(HF,paste0(HF[i],"x")) # jedna strona rozgalezienia
      HF<-rbind(HF,paste0(HF[i],"y")) # i druga
      PosN<-c(PosN,node)  # zapisz nr atrybutu, wedlug ktorego podzielilismy tabele
      ValN<-c(ValN,round(lstX[[i]][node],3)) # zapisz wartosc atrybutu dzielacego tabele <- czy to potzebne?
    }
    else{
      # listC[[i]] w tym przypadku powinno zawierac juz tylko wystapienia jednej klasy
      ValN<-c(ValN,round(unique(lstC[[i]]),3)) # zapisz przyporzadkowanie do klasy, "lisc"
    }
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
