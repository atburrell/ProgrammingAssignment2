## Programming assignment #2 for Coursera:
## Goal is to create a script that can cache the inverse of a matrix.
## When the inverse is called again, the function will first check the cache to
##   see if the value is already available and, if possible, return it.
## If the value is not available, then it will be computed.



## The first function will deal only with caching the matrix.
## This function does not actually solve the inverse.
## This function contains a list of functions that set or show matrix cached values.


makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL ## inverse is included as an internal variable for this function.
  
  set_matrix <- function(input_matrix) { ## Use this subfunction to cache the starting matrix.
    x <<- input_matrix ## x is cache value for input matrix.
    inverse <<- NULL ## inverse value is intially cached as null, but will eventually contain calc'd value.
  }
  show_matrix <- function() x ## Use this subfunction to call the starting matrix
  
  set_inverse_matrix <- function(solve) inverse <<- solve ## This will store a value as the solved matrix.
                                                    ## This does not actually solve the matrix!
  
  show_inverse_matrix <- function() inverse ## Use this subfunction to call the inverse matrix.
  
  ## This stores the above four subfunctions in an accessible list
  list(set_matrix= set_matrix, show_matrix = show_matrix,
       set_inverse_matrix = set_inverse_matrix,
       show_inverse_matrix = show_inverse_matrix)  
}

## Second function will actually solve for the inverse matrix.
## Before executing the solve function, it will first check the cache to see if was already computed.
## If the cached value exists, then it will return the solved matrix and exit the function.
## If the cached value is still null, then it will solve for the inverse matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  inverse <- x$show_inverse_matrix() ## Use subfunction of makeCacheMatrix to call the inverse value
  if(!is.null(inverse)) { ## If it has not been calculated yet, it is stored as NULL. See line 19.
    message("getting cached data") ## If m is not equal to null, then this message displays
    return(inverse) ## Return the cached inverse matrix and exit cacheSolve immediately.
  }
  data <- x$show_matrix() ## If inverse is NULL then call the original matrix
                          ## using a subfunction of makeCacheMatrix
 
  inverse <- solve(data, ...) ## Solve the called matrix for the inverse and set it to m
                        ## This is done as an internal variable
  
  x$set_inverse_matrix(inverse) ## Store the inverse in cache using a subfunction of makeCacheMatrix
  inverse ## Return the inverse matrix
}