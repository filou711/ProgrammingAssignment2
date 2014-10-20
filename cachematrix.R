## The script contains :
##     -A function (makeCacheMatrix) to create a special Matrix object
##     -A function (cache solve) to compute the inverse matrix


## The function makeCacheMatrix creates  a special Matrix object
makeCacheMatrix <- function(x = matrix()) {
  ## Initialize Matrix variable
  matrix_cache <- NULL
  
  ## Function to set the value of the Matrix
  setMatrix <- function(y) {
    x <<- y
    matrix_cache <<- NULL
  }
  
  ## Function to get the value of the Matrix
  getMatrix <- function() x
  
  ## Function to set the value of the Inverse Matrix
  setInverseMatrix <- function(Inverse_matrix) matrix_cache <<- Inverse_matrix
  
  ## Function to fet the value of the Inverse Matrix
  getInverseMatrix <- function() matrix_cache
  
  ## List of function
  list(setMatrix  = setMatrix , getMatrix = getMatrix,
       setInverseMatrix  = setInverseMatrix ,
       getInverseMatrix = getInverseMatrix)
}


## The function cacheSolve computes the inverse matrix
cacheSolve <- function(x, ...) {

## Check if the Matrix Inverse has already been calculated, so we return the matrix cache
  matrix_cache <- x$getInverseMatrix()
  if(!is.null(matrix_cache )) {
    message("getting cached square matrix")
    return(matrix_cache)
  }
 

## Chexk if no Matrix Inverse is in the cache, so we compute the matrix inverse by use solve function and load the 
## result in the cache
  XMatrix <- x$getMatrix()
  matrix_cache <- solve(XMatrix, ...)
  x$setInverseMatrix(matrix_cache)
  
 ## Display the Matrix Inverse in the cache 
  matrix_cache

}
        
