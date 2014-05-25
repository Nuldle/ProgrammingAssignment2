## The two functions here enable caching the inverse of a square invertible matrix.
## This avoids costly recalculation each time the inverse is needed.

## The function makeCacheMatrix creates a special "matrix" object that can cache its inverse.
## The object is a list that includes functions that:
## set the original matrix in the object, 
## get the matrix,,
## set the inverse of the matrix, and
## get the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
        ## calling the function without arguments initializes the object
        invsq <- NULL
        set <- function(y) {
                x <<- y
                invsq <<- NULL
        }
        get <- function() x
        setInvsq <- function(solve) invsq <<- solve
        getInvsq <- function() invsq
        list(set = set, get = get,
             setInvsq = setInvsq,
             getInvsq = getInvsq)
}


## The function cacheSolve computes the inverse of the special "matrix" 
## returned by makeCacheMatrix. If the inverse has already been calculated 
## (and the matrix has not changed), 
## then cacheSolve retrieves the inverse from the cache.
## (Note: Checking if matrix has changed is not explicitly addressed.)

cacheSolve <- function(x, ...) {
        invsq <- x$getInvsq()
        if(!is.null(invsq)) {
                message("getting cached inverse matrix")
                return(invsq)
        }
        myMatrix <- x$get()
        invsq <- solve(myMatrix, ...)
        x$setInvsq(invsq)
        invsq
}
