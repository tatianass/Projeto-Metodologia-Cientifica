#checa se possui a biblioteca, caso contrÃ¡rio a instala
if (!require("doParallel")) {
  install.packages("doParallel", repos="http://cran.rstudio.com/") 
}
if (!require("doMC")) {
  install.packages("doMC", repos="http://cran.rstudio.com/") 
}

library(foreach)
library(doParallel)
library(doMC)

#run experiments with different data sizes for parallel algorithm
experiment_seq<-function(data){
  #start time
  strt<-Sys.time()
  
  seq_sort(data)
  
  #end time
  now <- Sys.time()
  t <- as.numeric(difftime(now, strt, units = "secs"))
  return(t) 
  
}

#run experiments with different data sizes for sequencial algorithm
experiment_pl<-function(data){
  #start time
  strt<-Sys.time()
  
  pl_sort(data)
  
  #end time
  now <- Sys.time()
  t <- as.numeric(difftime(now, strt, units = "secs"))
  return(t)
  
}

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
}

pl_sort<-function(input){
  
  #sort
  ordenado <- pl_mmergesort(input)
  
  
  
}

save_result<-function(data){
  write.table(data, "result.csv", quote = F, row.names = F, sep=";")
}

data <- read.csv("bank-full.csv", header = T, sep = ";")


#metrics
sizes <- c(1000, 2000, 3000, 4000, 5000, 6000, 7000, 8000, 9000, 10000)
clusters <- c(2, 4, 6)

df <- data.frame(type= character(0), size= numeric(0), time = numeric(0), cluster = numeric(0),stringsAsFactors=FALSE)

for(s in sizes) {
  data1 <- data[0:s,]
  data1 <- as.character(data1$job)
  
  v1 <- c()
  time1 <- experiment_seq(data1)
  
  v1 <- c("sequencial", s, time1, 1)
  df[nrow(df) + 1, ] <- v1
  
}

for(c in clusters){
  #number of clusters
  registerDoParallel(c)
  
  for(s in sizes) {
    data1 <- data[0:s,]
    data1 <- as.character(data1$job)
    
    v2 <- c()
    time2 <- experiment_pl(data1)
    
    v2 <- c("parallel", s, time2, c)
    df[nrow(df) + 1, ] <- v2
    
  }
  
  
  #stop parallel
  stopImplicitCluster()
}
save_result(data = df)