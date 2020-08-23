ScoreGrid<-function(homeXg,awayXg){
  
  A <- as.numeric()
  B <- as.numeric()
  
  for(i in 0:9) {
    A[(i+1)] <- dpois(i,homeXg)
    B[(i+1)] <- dpois(i,awayXg)
  }
  
  A[11] <- 1 - sum(A[1:10])
  B[11] <- 1 - sum(B[1:10])
  
  name <- c("0","1","2","3","4","5","6","7","8","9","10+")
  zero <- mat.or.vec(11,1)
  
  C <- data.frame(row.names=name, "0"=zero, "1"=zero, "2"=zero, "3"=zero, "4"=zero,
                  "5"=zero, "6"=zero, "7"=zero,"8"=zero,"9"=zero,"10+"=zero)
  
  for(j in 1:11) {
    for(k in 1:11) {
      C[j,k] <- A[k]*B[j]
    }
  }
  
  colnames(C) <- name
  
  return(round(C*100,2)/100)
}