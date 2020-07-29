## Programming Assignment 2: Lexical Scoping
##____________________________________________
## The functions bellow cache the inverse of a matrix previously calculated
##____________________________________________
## First function: makeCacheMatrix - creates a matrix that can cache its inverse, follwing the 4 steps:
##    1. set the value of the matrix
##    2. get the value of the matrix
##    3. set the value of the inverse matrix
##    4. get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setinv <- function(solve) m <<- solve
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

##____________________________________________
## The second function: cacheSolve: computes the inverse of the matrix 
##returned by the first function.
## If the inverse has already been calculated for the same matrix, 
##this function retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
     m <- x$getinv()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}  
  
  
    
