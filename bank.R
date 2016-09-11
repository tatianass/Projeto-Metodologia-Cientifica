#checa se possui a biblioteca, caso contrÃ¡rio a instala
if (!require("doParallel")) {
  install.packages("doParallel", repos="http://cran.rstudio.com/") 
}

library(foreach)
library(doParallel)

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

df_append <- function(v){
  df[nrow(df) + 1, ] <- v
}

do_treatment <- function(t, data, r){
  time <- 0
  algorithm <- ""
  size1 <- 0
  size2 <- 0
  
  switch(t, 
         t1={
           algorithm <- "sequencial"
           size1 <- 100
           size2 <- 1000
         },
         t2={
           algorithm <- "parallel"
           size1 <- 100
           size2 <- 1000
           
         },
         t3={
           algorithm <- "sequencial"
           size1 <- 1000
           size2 <- 10000
           
         },
         t4={
           algorithm <- "parallel"
           size1 <- 1000
           size2 <- 10000
           
         },
         t5={
           algorithm <- "sequencial"
           size1 <- 100
           size2 <- 10000
           
         },
         t6={
           algorithm <- "parallel"
           size1 <- 100
           size2 <- 10000
           
         })
  
  
  data1 <- data[0:size1,]
  data1 <- as.character(data1$job)
  
  data2 <- data[0:size2,]
  data2 <- as.character(data2$job)
  
  v1 <- c()
  v2 <- c()
  
  if(algorithm == "sequencial") {
    experiment_seq(data1)
    time1 <- experiment_seq(data1)
    time2 <- experiment_seq(data2)
  }
  else{
    time1 <- experiment_pl(data1)
    time2 <- experiment_pl(data2)
    
  } 
  v1 <- c(algorithm, size1, time1, t, r)
  v2 <- c(algorithm, size2, time2, t, r)
  df_append(v1)
  df_append(v2)
  
}

do_repetition <- function(treatment, n_repetition, data){
  for(r in n_repetition){
    for (t in treatment) {
      do_treatment(t, data, r)
    }  
  }
  
}

data <- read.csv("bank-full.csv", header = T, sep = ";")


#metrics
sizes <- c(100,1000,10000)
algorithm <- c("sequencial", "parallel")
treatment <- c("t1", "t2", "t3", "t4", "t5", "t6")
#treatment <- c("t2")
repetition <- c(1,2,3,4,5,6,7,8,9,10)

df <- data.frame(type= character(0), size= numeric(0), time = numeric(0), n_treatment = character(0) ,n_repetition = numeric(0), stringsAsFactors=FALSE)


#number of clusters
registerDoParallel(4)

for(r in repetition){
  for (t in treatment) {
    #do_repetition(treatment, repetition, data, df)
    time <- 0
    algorithm <- ""
    size1 <- 0
    size2 <- 0
    
    switch(t, 
           t1={
             algorithm <- "sequencial"
             size1 <- 100
             size2 <- 1000
           },
           t2={
             algorithm <- "parallel"
             size1 <- 100
             size2 <- 1000
             
           },
           t3={
             algorithm <- "sequencial"
             size1 <- 1000
             size2 <- 10000
             
           },
           t4={
             algorithm <- "parallel"
             size1 <- 1000
             size2 <- 10000
             
           },
           t5={
             algorithm <- "sequencial"
             size1 <- 100
             size2 <- 10000
             
           },
           t6={
             algorithm <- "parallel"
             size1 <- 100
             size2 <- 10000
             
           })
    
    
    data1 <- data[0:size1,]
    data1 <- as.character(data1$job)
    
    data2 <- data[0:size2,]
    data2 <- as.character(data2$job)
    
    v1 <- c()
    v2 <- c()
    
    if(algorithm == "sequencial") {
      time1 <- experiment_seq(data1)
      time2 <- experiment_seq(data2)
    }else{
      time1 <- experiment_pl(data1)
      time2 <- experiment_pl(data2)
    } 
    v1 <- c(algorithm, size1, time1, t, r)
    v2 <- c(algorithm, size2, time2, t, r)
    df[nrow(df) + 1, ] <- v1
    df[nrow(df) + 1, ] <- v2
    
  }  
}
#stop parallel
stopImplicitCluster()

save_result(data = df)