## This function takes a matrix and generates the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
 m <- NULL ## m is mean reset to NULL
 set <- function(y) {
   x <<- y
   m <<- NULL
 }
 get <-function() x ## returns value of original matrix
 setmatrix <- function(solve) m <<- solve ## applies solve function to matrix to get inverse
 getmatrix <- function() m ## gets matrix inverse sets to m will return the cached value
 list(set=set, get=get, ##new object is created each time that this function is called
      setmatrix=setmatrix,
      getmatrix=getmatrix)
}


## This function retrieves the inverse of the previous matrix

cacheSolve <- function(x=matrix(), ...) {  ## Function to return a matrix that is the inverse of 'x'
        m <- x$getmatrix()
        if(!is.null(m)){ ## if statement if matrix is not null
          message("getting cached matrix") ## then return message that it's getting the matrix
          return(m) ## and return the function
        }
        matrix <- x$get() ## if the code returns null
        m <- solve(matrix, ...) ## then need to calculate inverse
        x$setmatrix(m) ## and store it
        m ## return the matrix
}
