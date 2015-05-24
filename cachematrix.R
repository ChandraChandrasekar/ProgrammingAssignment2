## The functions in this file help define a special matrix object with associated operations  (somewhat like an abstract data type),
## which allows for its inverse to be cached (or 'memoized', in LISP lingo); if the inverse function 
## is invoked more than once, the cached value is returned if the matrix is not changed. These functions
## assume that any matrix provided as input is invertible.


## Define the special matrix object and associated functions to set/get values, set/get inverses and
## to keep track of if the matric was modified after the inverse was computed

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL      # internal variable to store the inverse of matrix x
        
        set <- function(y) {
                x <<- y
                inv <<- NULL  # any time set is called, inv is set to NULL
        }
        get <- function() x
        
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Function to get inverse of special matrix object; if the cached value of the inverse is available, that is returned;
## else the value is computed using 'solve' and cached
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        inver <- x$getInverse()
        if(!is.null(inver)) {
                message("Getting the cached inverse value")
                return(inver)
        }
        # else if inver is null, which is true if the matrix has been reset and/or if the inverse has not been cached so far, compute & cache it
        dataObject <- x$get()
        inver <- solve(dataObject, ...)
        x$setInverse(inver)
        inver
}
