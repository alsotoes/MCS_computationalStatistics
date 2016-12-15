#https://www.r-bloggers.com/using-apply-sapply-lapply-in-r/
m <- matrix(data=cbind(rnorm(30,0), rnorm(30,2), rnorm(30,5)), nrow = 30, ncol=3)

#Using apply
apply(m, 1, mean) #apply mean function to all rows
apply(m, 2, mean) #apply mean function to all columns
apply(m, 2, function(x) length(x[x<0])) #defining own function


#Using sapply and lapply
#apply a function to each element of a data set
sapply(seq(1,3, by=1), function(x) x^2) #return a VECTOR
sapply(seq(1,3, by=1), function(x) x^2, simplify = F) #return a list
lapply(seq(1,3, by=1), function(x) x^2) #return a LIST
unlist(lapply(seq(1,3, by=1), function(x) x^2)) #return a vector

sapply(seq(1,3, by=1), function(x) mean(m[,x]))
sapply(seq(1,3,1), function(x,y) mean(y[,x]), y=m)

#tapply
n <- 17
fac <- factor(rep(1:3, length = n), levels = 1:5)
table(fac)
tapply(1:n, fac, sum)
tapply(1:n, fac, range)

ind <- list(c(1,2,2), c("A", "A", "B"))
table(ind)
tapply(1:3, ind)
tapply(1:3, ind, sum)


#mapply
mapply(rep, 1:4, 4:1)
mapply(rep, times=1:4, x=4:1)
mapply(rep, times=1:4, MoreArgs=list(x=42))

mapply(function(x, y) seq_len(x) + y,
       c(a =  1, b = 2, c = 3),  # names from first
       c(A = 10, B = 0, C = -10))
