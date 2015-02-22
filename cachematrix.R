## ----------------------------------------------------------------------------
## A "cached matrix" is a square matrix that caches its calculated inversion
## after it is calculated the first time. Subsequent requests for its inversion
## returns the cached inversed matrix.
## ----------------------------------------------------------------------------

## ----------------------------------------------------------------------------
## Function makeCacheMatrix: Creates a cached matrix, using R's lexical scoping
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
        
    # Initialize the cached matrix inverse
    inverseMatrix <- NULL
    
    # The "setter"
    set <- function(y) {
        # Same arguments checks as above
        if (!is.matrix(y)) {
            stop('Argument is not a matrix!')
        }
        dims <- dim(y)
        if (dims[1] != dims[2]) {
            stop('Argument is not a square matrix')  
        }
        
        # Mutate the free variables using the <<- operator 
        # (Both 'x' and inverseMatrix are defined outside of this function, hence
        # they are "free variables").
        x <<- y
        inverseMatrix <<- NULL
    }
    
    # The "getter"
    get <- function() {
        x
    }
    
    # Set the inversed matrix (to be cached)
    setinverse <- function(inverse) {        
        inverseMatrix <<- inverse
        message("Matrix inverse cached.")
    }
    
    # Get the inversed matrix
    getinverse <- function() {
        inverseMatrix
    }
    
    # List of functions (i.e., the cached matrix's API)
    list(set = set, get = get,
         setInverse = setinverse,
         getInverse = getinverse)
}


## ----------------------------------------------------------------------------
## Function cacheSolve: "solves" (matrix inverses) a cached matrix object.
## ----------------------------------------------------------------------------
cacheSolve <- function(x, ...) {
    # Attempt to check that 'x' is a cached matrix object
    if (!is.list(x) || !identical(names(x), c('set', 'get', 'setInverse', 'getInverse'))) {
        stop('Argument does not seem to be a cached matrix')
    }
    
    inverseFromCache <- x$getInverse()
    if(!is.null(inverseFromCache)) {
        message("Retrieved the matrix inverse from cache.")
        return(inverseFromCache)
    }
    
    # Retrieve the matrix, inverse it, cache it, and return it to the caller.
    matrix <- x$get()
    inverse <- solve(matrix, ...)
    x$setInverse(inverse)
    inverse
}
