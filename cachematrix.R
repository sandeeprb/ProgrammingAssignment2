
## makeCacheMatrix function takes a matrix as input, and returns a list of four function objects.
## This function caches the invese of input matrix if it is not already available.

makeCacheMatrix <- function(x = matrix()) {
        invMatrix <- NULL
        set <- function(y = matrix()){
                x <<- solve(y)
                invMatrix <<- NULL
        }
        get <- function() x
        setInverse <- function(inverseMatrix = matrix()) invMatrix <<- inverseMatrix
        getInverse <- function() invMatrix
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## cacheSolve function looks for an inverse of a given matrix in cache and returns the 
## value from cache if already available. If inverse is not found in cache, it calculates
## new inverse and stores it in cache for further computation.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invMatrix <- x$getInverse()
        if(!is.null(invMatrix)) {
                message("getting cached data")
                return(invMatrix)
        }
        data <- x$get()
        invMatrix <- solve(data, ...)
        x$setInverse(invMatrix)
        invMatrix                
}
