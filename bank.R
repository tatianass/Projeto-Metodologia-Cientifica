#checa se possui a biblioteca, caso contrÃ¡rio a instala
if (!require("doParallel")) {
  install.packages("doParallel", repos="http://cran.rstudio.com/") 
}

library(foreach)
library(doParallel)

#start time
strt<-Sys.time()

data <- read.csv("bank-full.csv", header = T, sep = ";")
data_100 <- data[0:100,]
data_1000 <- data[0:1000,]
data_10000 <- data[0:10000,]

input <- as.character(data_10000$job)
#seq_sort(input)
pl_sort(input)

#write(x = as.character(data_100$job), file = "d100.txt", sep = "\t", append = T)
#write(x = as.character(data_1000$job), file = "d1000.txt", sep = "\t", append = T)
#write(x = as.character(data_10000$job), file = "d10000.txt", sep = "\t", append = T)


#merge sort
pl_mmerge<-function(a,b) {
  r<-numeric(length(a)+length(b))
  ai<-1; bi<-1;
  j<-1
  #for(j in 1:length(r))  {
  foreach(j= 1:length(r)) %dopar% {
    if((ai<=length(a) && a[ai]<b[bi]) || bi>length(b)) {
      r[j] <- a[ai]
      ai <- ai+1
    } else {
      r[j] <- b[bi]
      bi <- bi+1          
    }
  }
  r
}

#merge sort
seq_mmerge<-function(a,b) {
  r<-numeric(length(a)+length(b))
  ai<-1; bi<-1;
  j<-1
  foreach(j= 1:length(r)) %do% {
    if((ai<=length(a) && a[ai]<b[bi]) || bi>length(b)) {
      r[j] <- a[ai]
      ai <- ai+1
    } else {
      r[j] <- b[bi]
      bi <- bi+1          
    }
  }
  r
}

pl_mmergesort<-function(A) {
  if(length(A)>1) {
    q <- ceiling(length(A)/2)
    a <- pl_mmergesort(A[1:q])
    b <- pl_mmergesort(A[(q+1):length(A)])
    pl_mmerge(a,b)
  } else {
    return(A)
  }
}

seq_mmergesort<-function(A) {
  if(length(A)>1) {
    q <- ceiling(length(A)/2)
    a <- seq_mmergesort(A[1:q])
    b <- seq_mmergesort(A[(q+1):length(A)])
    seq_mmerge(a,b)
  } else {
    return(A)
  }
}

seq_sort<-function(input){
  #sort
  ordenado <- seq_mmergesort(input)
  write(x = ordenado, file = "output.txt", sep = "\t", append = T)
}

pl_sort<-function(input){
  #number of clusters
  cl <- makeCluster(100)
  registerDoParallel(cl)
  
  #sort
  ordenado <- pl_mmergesort(input)
  ordenado
  write(x = ordenado, file = "output.txt", sep = "\t", append = T)
  
  #stop parallel
  stopImplicitCluster()
}

#end time
print(Sys.time()-strt)