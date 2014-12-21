##Functions that demonstrate the value of caching data vs computing each time .
##The Functions below are ussing matrix. <<- operator and z < funtion() w .
## Matrix inversion is usually a costly computation and there may be some benefit
## to caching the inverse of a matrix rather than compute it repeatedly. The
## following two functions -cache the inverse of a matrix.

## makeCacheMatrix creates a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
# 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
invr <- NULL
set <- function(y) {
x <<- y
invr <<- NULL
}
get <- function() x
setinverse <- function(inverse) invr <<- inverse
getinverse <- function() invr
list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## Function assumes that the matrix is always invertible.
## If in case the matrix is not invertable then it returns an error. 
## Works for square matrix! None Square matrix return an error : Error in solve.default(data) : 'a' (n x r) must be square 
## The following function returns the inverse of the matrix. It first checks if
## the inverse has already been computed. If so, it gets the result and skips the
##computation. If not, it computes the inverse, sets the value in the cache via
## setinverse function.

cacheSolve <- function(x, ...) {
invr <- x$getinverse()
if(!is.null(invr)) {
message("getting data from cache.")
return(invr)
}
data <- x$get()
invr <- solve(data)
x$setinverse(invr)
invr
}

## Sample :

## Creates invert square metrix 
##> x = rbind(c(-1/8,7,2), c(2,7,-1/8),c(7,2,-1/8))
##> m = makeCacheMatrix(x)
##> m$get()
##       [,1] [,2]   [,3]
##[1,] -0.125    7  2.000
##[2,]  2.000    7 -0.125
##[3,]  7.000    2 -0.125


##First run of cacheSolve() caclulates the invert square metrix and stores the values to cache
##> cacheSolve(m)
##            [,1]        [,2]        [,3]
##[1,] 0.006628003 -0.05169843  0.15774648
##[2,] 0.006628003  0.14830157 -0.04225352
##[3,] 0.477216239 -0.52228666  0.15774648

##Second time run of cacheSolve() pulls the values from cache and displays - no calculations occur. 
##> cacheSolve(m)
##getting data from cache.
##            [,1]        [,2]        [,3]
##[1,] 0.006628003 -0.05169843  0.15774648
##[2,] 0.006628003  0.14830157 -0.04225352
##[3,] 0.477216239 -0.52228666  0.15774648
