name <- 'KANGBO CHEN'
liuid <- 'kanch152'
#1.1.1
my_num_vector <- function(){c(log10(11),cos(pi/5),exp(pi/3),(1173 %% 7)/19)
}


#1.1.2
filter_my_vector <- function(x,leq){
  y <- x
  for(i in 1:length(x)){
    if((x[i]>=leq)){y[i] <- NA}
  }
  y
}

#1.1.3
dot_prod <- function(a,b){
  sum <- 0
    for(i in 1:length(a)){sum <- sum+a[i]*b[i]}
  return(sum)
}

#1.1.4
approx_e <- function(N){
  a <- c()
  for(i in 0:N){
    a <- append(a,(1/gamma(i+1))) #gamma result in n!
  }
  return(sum(a))
}

#1.2.1
my_magic_matrix <- function(){
  mat <- matrix(c(4,9,2,3,5,7,8,1,6),3,3,byrow=TRUE)
  return(mat)
}

#1.2.2
calculate_elements <- function(A){
  a <- dim(A)
  size <- a[1]*a[2]
  return(size)
}

#1.2.3
row_to_zero <- function(A,i){
  A[i,] <- 0
  return(A)
}

#1.2.4
add_elements_to_matrix <- function(A,x,i,j){  #
  A[i,j] <- A[i,j]+x
  return(A)
}

#1.3.1
my_magic_list <- function(){
  l <- list(info="my own list",my_num_vector(),my_magic_matrix())
  return(l)
}

#1.3.2
change_info <- function(x,text){
  x$info <- text
  return(x)
}

#1.3.3
add_note <- function(x,note){
  x[['note']] <- note
  return(x)
}


#1.3.4
sum_numeric_parts <- function(x){
  tem <- 0
  i <- 1
  while(i<=length(x)){
    if(mode(x[[i]])=="numeric"){
      tem <- tem+sum(x[[i]])
    }
    i <- i+1
  }
  return(tem)
}

#1.4.1
my_data.frame <- function(){
  df <- data.frame(id=c(1,2,3),name=c('John','Lisa','Azra')
                   ,income=c(7.30,0.00,15.21),rich=c(FALSE,FALSE,TRUE))
  return(df)
}

#1.4.2
sort_head <- function(df,var.name,n){
  tem <- df[order(df[[var.name]],decreasing=TRUE),]
  return(head(tem,n))
}

#1.4.3
add_median_variable <- function(df,j){
  median1 <- median(df[[j]])
  aaa <- dim(df)
  com <- data.frame('compared_to_median'=rep(NA,aaa[1]))
  for(i in 1:aaa[1]){
    if(df[[j]][[i]]>median1){
      com[[1]][i] <- 'Greater' 
    }else if(df[[j]][[i]]<median1){
      com[[1]][i] <- 'Smaller'
    }else if(df[[j]][[i]]==median1){
      com[[1]][i] <- 'Median'
    }
  }
  A <- cbind(df,com)
  return(A)
}



#1.4.4
analyze_columns <- function(df,j){
  col1_data <- c(mean(df[[j[1]]]),median(df[[j[1]]]),sd(df[[j[1]]]))
  col2_data <- c(mean(df[[j[2]]]),median(df[[j[2]]]),sd(df[[j[2]]]))
  
  names(col1_data) <- c('mean','median','sd')
  names(col2_data) <- c('mean','median','sd')
  
  col1 <- colnames(df[j[1]])
  col2 <- colnames(df[j[2]])
  col3 <- 'correlation_matrix'
  
  a <- data.frame(col1=df[[j[1]]],col2=df[[j[2]]])
  names(a)[1] <- colnames(df[j[1]])
  names(a)[2] <- colnames(df[j[2]])
  
  cor1 <- cor(a)
  
  l <- list(col1=col1_data,col2=col2_data,col3=cor1)
  names(l)[1] <- colnames(df[j[1]])
  names(l)[2] <- colnames(df[j[2]])
  names(l)[3] <- 'correlation_matrix'
  return(l)
}