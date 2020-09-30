## In this project, we implement caching the inverse of a matrix rather than
## compute it repeatedly. This is done in order to reduce costly computations

## The following function creates a "matrix object" than can cash its inverse

makeCacheMatrix <- function(x = matrix()) {

  x_inverse <- NULL
  ## x - stores the original matrix
  ## x_inverse - cached inverted matrix
  
  setmatrix <- function(x_new) {
    x <<- x_new
    x_inverse <<- NULL
  }
  getmatrix <- function() x
  setinverse <- function(inverse_new) x_inverse <<- inverse_new
  getinverse <- function() x_inverse
  
  ## return a list of 4 functions which operate with the matrix and the inverse
  list(
    set = setmatrix,
    get = getmatrix,
    set.inverse = setinverse,
    get.inverse = getinverse
  )
}


## The following function computes the inverse of the "matrix" returned by
## makeCacheMatrix. If the inverse has already been calculated and the matrix 
## has not changed, then the function retrieves the cached inverse

cacheSolve <- function(x, ...) {
        
  result <- x$get.inverse()
  if (is.null(result)) {
    ## cached data unavailable
    message("Computing the inverse of matrix...")
    data <- x$get()
    ## we pass one argument to the 'solve' function, and it returns the inverse 
    result <- solve(data)
    x$set.inverse(result)
  }
  else {
    ## cached data available
    message("Getting the inverse of matrix from cache ...")
  }
  result
}
