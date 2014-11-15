## Coursera, R Programming, 2014
## Assignment: Caching the Inverse of a Matrix
## Matrix inversion is usually a costly computation and their may be some 
## benefit to caching the inverse of a matrix rather than compute it 
## repeatedly. The assignment is to write a pair of functions that cache 
## the inverse of a matrix.  



## This function creates a special "matrix" object that can cache it inverse
#
# Args:
#   x: The matrix which inverse to be calculated
# 
# Returns:
#   The object that can cache the inverse of the matrix.  
makeCacheMatrix <- function(x = matrix()) {
    #invMatrix - inverse matrix value
    invMatrix <- NULL #default value - NULL
    
    # set the value of the matrix and clear the value of inverse matrix
    # The "<<-" operator used to set variable that already exists 
    # in the parent environment.
    set <- function(y){
      x <<- y
      invMatrix <<- NULL
    }
    
    #getMatrix function: get matrix
    get <- function() x
    
    #setInvMatrix function: set inverse matrix
    setInvMatrix <- function(inverseMatrix) invMatrix <<- inverseMatrix
    
    #getInvMatrix function: get inverse matrix
    getInvMatrix <- function() invMatrix
    
    # create the list to access properties with $ sign. 
    list(set = set, get=get,
         setInvMatrix = setInvMatrix,
         getInvMatrix = getInvMatrix)
}


## Return a matrix that is the inverse of 'x'
#
# Args:
#   x: The makeCacheMatrix object.
# 
# Returns:
#   The inverse matrix.
cacheSolve <- function(x, ...) {
        
    #get invMatrix
    invMatrix <- x$getInvMatrix()
    
    #check if it is not null
    if (!is.null(invMatrix)){
      message("getting cached data")
      return (invMatrix)
    }
    
    #if it is null, calculate the inverse
    data <- x$get()
    inv <- solve(data, ...)   # Inverse matrix
    x$setInvMatrix(inv) #save it
    inv # return inverse matrix
}

# test
# m <- makeCacheMatrix(matrix(1:4, nrow = 2, ncol = 2))
# cacheSolve(m)



