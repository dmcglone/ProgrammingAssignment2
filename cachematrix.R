## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
 m <- NULL ## m is mean reset to NULL
 set <- function(y) {
   x <<- y
   m <<- NULL
 }
 get <-function() x ## returns value of original matrix
 setmatrix <- function(solve) m <<- solve ## applies solve function to matrix to get inverse
 getmatrix <- function() m ## gets matrix inverse
 list(set=set, get=get,
      setmatrix=setmatrix,
      getmatrix=getmatrix)
}


## Write a short comment describing this function

cacheSolve <- function(x=matrix(), ...) {  ## Function to return a matrix that is the inverse of 'x'
        m <- x$getmatrix()
        if(!is.null(m)){
          message("getting cached matrix")
          return(m)
        }
        matrix <- x$get()
        m <- solve(matrix, ...)
        x$setmatrix(m)
        m
}
