fun <- function (x) {
  return(4/(1+x^2))
  }

trapezoid <- function(N, a, b, FUN){
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

from=c(0)
to = c(1)
n = 100
print(trapezoid(n, from, to, fun))
