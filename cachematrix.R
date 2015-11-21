## Contains two functions
## makeCacheMatrix - Returns list of functions which can be used to store a matrix
## and its associated inverse matrix
##
## cacheSolve - Uses makeCacheMatrix() returned list to calculate and cache a
## martrix and associated inverse

## makeCacheMatrix has four functions that all user of this function to
## cache inverse of the matrix. Assumption is that matrix stored is an
## square invertible. This function returns a list containing
## following functions
##
## setMatrix(matrix) - This function takes a matrix and updates the 
## internally stored matrix value. It will also set the inverse matrix
## value to NULL, as the associated matrix value is getting changed.
##
## getMatrix() - This function returns the internally stored matrix.
##
## setInverseMatrix(matrix) - This function takes a matrix and updates the 
## internally stored inverse matrix value. 
##
## getInverseMatrix() - This function returns the internally stored inverse matrix.
##

makeCacheMatrix <- function(inputMatrix = matrix()) {
  # Set to NULL, so that inverse matrix can be calculted later.
  # Note <- is used as we are still in the function's context.
  invMatrix <- NULL
  
  # Allows the caller to update the matrix value in function's environment
  setMatrix <- function(updateMatrix) {
    # Note that <<- is use so that function environment variable is updated
    inputMatrix <<- updateMatrix
    
    # Enables recalculation of inverse matrix after the matrix is updated
    invMatrix <<- NULL
  }
  
  # Returns the value of the matrix stored inside function's environment
  getMatrix <- function() inputMatrix
  
  # Note that <<- is used to assign the new inverse matrix. It updates
  # the function environment with the new value and hence available in
  # subsequent calls to getInverseMatrix()
  setInverseMatrix <- function(updateInvMatrix) invMatrix <<- updateInvMatrix

  # Returns the value of the inverse matrix stored inside function's environment
  getInverseMatrix <- function() invMatrix
  
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setInverseMatrix = setInverseMatrix,
       getInverseMatrix = getInverseMatrix)
}


## Uses the makeCacheMatrix provided list of function to cache matrix and its
## associated inverse matrix. It will calculate the inverse when the cached 
## inverse matrix is NULL. This condition is true in two scenarios
## 1. When inverse matrix is calculated for the first time
## 2. When the matrix is updated and hence inverse has to be recalculated
## Return a matrix that is the inverse of 'iMatrix'.
##
## Assume that the matrix supplied is always square invertible.

cacheSolve <- function(iMatrix, ...) {
  # Get the inverse matrix stored in cache, iMatrix is a collection of functions
  invMatrix <- iMatrix$getInverseMatrix()
  if(!is.null(invMatrix)) {
    message("getting cached data")
    return(invMatrix)
  }
  inpMatrix <- iMatrix$getMatrix()
  invMatrix <- solve(inpMatrix,...)
  iMatrix$setInverseMatrix(invMatrix)
  invMatrix
}
