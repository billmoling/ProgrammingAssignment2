## Coursera, R Programming, April 2014
## Assignment: Caching the Inverse of a Matrix
## Matrix inversion is usually a costly computation and their may be some 
## benefit to caching the inverse of a matrix rather than compute it 
## repeatedly. The assignment is to write a pair of functions that cache 
## the inverse of a matrix.  

## Function:makeCacheMatrix
## Description: This function creates a special "matrix" object that can cache it inverse

makeCacheMatrix <- function(x = matrix()) {
    #invMatrix - inverse matrix value
    invMatrix <- NULL #default value - NULL
    
    #setMatrix function: matrix and reset invMatrix
    setMatrix <- function(y){
      matrix <<- y
      invMatrix <<- NULL
    }
    #getMatrix function: get matrix
    getMatrix <- function() matrix
    
    #setInvMatrix function: set inverse matrix
    setInvMatrix <- function(inverseMatrix) invMatrix <<- inverseMatrix
    
    #getInvMatrix function: get inverse matrix
    getInvMatrix <- function() invMatrix
    
    list(setMatrix = setMatrix, getMatrix=getMatrix,
         setInvMatrix = setInvMatrix,
         getInvMatrix = getInvMatrix)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    #get invMatrix
    invMatrix <- matrix$getInvMatrix()
    
    #check if it is not null
    if (!is.null(invMatrix)){
      message("getting cached data")
      return (invMatrix)
    }
    
    #if it is null, calculate the inverse
    data <- matrix$getMatrix()
    inv <- solve(data, ...)
    matrix$setInvMatrix(inv) #save it
    inv
}
