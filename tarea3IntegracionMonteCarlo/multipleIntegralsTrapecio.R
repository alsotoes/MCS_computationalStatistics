trapezoid <- function(N, a, b, FUN){
  #FUN is the function for integrate: R^di --> R
  #a,b are dim-dimensional vectors specifying the limits of integration
  dim <- length(a)
  x <- seq(a[1], b[1], (b[1]-a[1])/N)
  if(dim == 1){
    fi <- sapply(x, FUN)
  } else{
    fi <- sapply(x, function(x){
      trapezoid(N, a[-1], b[-1], function(y) FUN(c(x,y)))
    })
  }
  return(((b[1]-a[1])/(2*N))*sum(fi[-1]+fi[-(N+1)]))
}

print(paste("Real volume: ", (4*pi/3)))
print(paste("Approximated volume: ", 
            trapezoid(20, c(-1,-1,-1), c(1,1,1), function(x) {sum(x^2) <= 1})))
