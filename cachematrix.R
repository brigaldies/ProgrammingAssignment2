## ----------------------------------------------------------------------------
## A cached matrix is a square matrix that caches its calculated inversion
## after it is calculated the first time. Subsequent reuest for its inversion
## returns the cached inversion.
## ----------------------------------------------------------------------------

## ----------------------------------------------------------------------------
## Function makeCacheMatrix: Creates a cache matrix, using R's lexical scoping
## which allows functions to be "mutable". In this example, the mutable
## information is both the matrix and the its calculated and cached inversion.
## ----------------------------------------------------------------------------
makeCacheMatrix <- function(x = matrix()) {
    
    # Argument checks.
    # Not in assignment's scope, but implemented for education.
    if (!is.matrix(x)) {
        stop('Argument is not a matrix!')
    }
    dims <- dim(x)
    if (dims[1] != dims[2]) {
        stop('Argument is not a square matrix')  
    }
        
    # Cached matrix inverse
    inverseMatrix <- NULL
    
    # Set the matrix
    set <- function(y) {
        if (!is.matrix(y)) {
            stop('Argument is not a matrix!')
        }
        dims <- dim(y)
        if (dims[1] != dims[2]) {
            stop('Argument is not a square matrix')  
        }
        
        # Mutate the free variables using the <<- operator 
        # (Both 'x' and inverseMatrix are defined outside of this function).
        x <<- y
        inverseMatrix <<- NULL
    }
    
    # Get the matrix
    get <- function() {
        x
    }
    
    # Set the inverse
    setinverse <- function(inverse) {        
        inverseMatrix <<- inverse
        message("Matrix inverse cached.")
    }
    
    # Get the inverse
    getinverse <- function() {
        inverseMatrix
    }
    
    # List of functions
    list(set = set, get = get,
         setInverse = setinverse,
         getInverse = getinverse)
}


## Function cacheSolve: solves a cached matrix. The function assumes
## that the passed in matrix was built using the above makeCacheMatrix
## "factory".
## TO DO: How does one check that the passed in variable is a cached matrix.
cacheSolve <- function(x, ...) {
    
    inverseFromCache <- x$getInverse()
    if(!is.null(inverseFromCache)) {
        message("Retried the matrix inverse from cache.")
        return(inverseFromCache)
    }
    
    # Retrieve the matrix, inverse it, cache it, and return it to the caller.
    matrix <- x$get()
    inverse <- solve(matrix, ...)
    x$setInverse(inverse)
    inverse
}
