## Matrix inversion is usually a costly computation and there may be 
## some benefit to caching the inverse of a matrix rather than computing it repeatedly

## makeCacheMatrix function creates a list that has functions to perform the following:
# 1. set to set the value of the matrix
# 2. get to get the value of the matrix
# 3. setinverse to set the value of inverse of the matrix
# 4. getinverse to get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
INV = NULL
  set<- function(y){
    x<<-y
    INV<<-NULL
  }
  get<-function() x
  setinverse<-function(inverse) INV<<-inverse
    getinverse<-function() INV
    list(set=set, get = get, setinverse= setinverse, getinverse = getinverse)
}


## The following function returns the inverse of a matrix. It first checks if
# the inverse has already been computed. If so, it gets the result and skips the
# computation. If not, it computes the inverse and sets the value in the cache by
# setinverse function.

cacheSolve <- function(x, ...) {
        INV<-x$getinverse()
        if(!is.null(INV)){
          message("Getting Cached data")
          return(INV)
        }
        data<-x$get()
        INV<-solve(data)
        x$setinverse(INV)
        INV
}
## Example Run
## x<- matrix(1:4, 2, 2)
## > x
## [,1] [,2]
## [1,]    1    3
## [2,]    2    4
## y<- makeCacheMatrix(x)
## y$get()
## [,1] [,2]
## [1,]    1    3
## [2,]    2    4
## > cacheSolve(y)
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## Repeating inverse for the same matrix uses cache data
## > cacheSolve(y)
## Getting Cached data
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
