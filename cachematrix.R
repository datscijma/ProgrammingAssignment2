## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it 
## repeatedly. Below are a pair of functions that are used to create a 
## special object that stores a matrix and caches its inverse.

## This function creates a special "matrix" object that can cache its 
## inverse.

makeCacheMatrix <- function(x = matrix()) {
     s <- NULL
     set <- function(y) {
          x <<- y
          s <<- NULL
     }
     get <- function() x
     setinv <- function(solve) s <<- solve
     getinv <- function() s
     list(set = set, get = get,
          setinv = setinv,
          getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and 
## the matrix has not changed), then cacheSolve should retrieve the inverse ## from the cache.

cacheSolve <- function(x, ...) {
     ## Return a matrix that is the inverse of 'x'
     s <- x$getinv()
     if(!is.null(s)) {
          message("getting cached data")
          return(s)
     }
     data <- x$get()
     s <- solve(data, ...)
     x$setinv(s)
     s
}

## sample
## create a matrix
# > mat1<-matrix(c(4,2,7,6), nrow=2, ncol=2)
## store matrix in makeCacheMatrix
# > test1<-makeCacheMatrix(mat1)
## find inverse of matrix and store in makeCacheMatrix
# > cacheSolve(test1)
# [,1] [,2]
# [1,]  0.6 -0.7
# [2,] -0.2  0.4
## retrieve cached inverse data
# > cacheSolve(test1)
# getting cached data
# [,1] [,2]
# [1,]  0.6 -0.7
# [2,] -0.2  0.4