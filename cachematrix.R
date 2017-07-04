## The first function creates a matrix that can cache its inverse
##The second function takes what is returned from the first function and computes the inverse

## The function essentially creates a special matrix that can cache its inverse
##it calculates the inverse of the matrix 
##sets the value of the inverse in the cache via the setmean function
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y 
      m <<- NULL
    }
    get <- function() x
    setmatrix <- function(solve) m<<- solve
    getmatrix <- function() m
    list(set = set, get = get, 
         setmatrix = setmatrix, 
         getmatrix = getmatrix)
  }


## This function computes the inverse of what was returned from the previous function
##

cacheSolve <- function(x, ...) {
  m <- x$getmatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  matrix <- x$get()
  m <- solve(matrix, ...)
  x$setmatrix(m)
  m
        ## Return a matrix that is the inverse of 'x'
}

