## Put comments here that give an overall description of what your
## functions do

##  Function name: makecacheMatrix
##  Description: create a special "matrix" object that can cache its inverse.
##  create the following functions
##    set         - saves the matrix 
##    get         - fetches the matrix  
##    setinverse  - stores the inverse matrix 
##    getinverse  - fetches the inverse matrix previously computed
##      
##    MBJ  4/22/14  version 1
##
makeCacheMatrix <- function(x = matrix()) {
        matinv <- NULL
        set <- function(y) {
                x <<- y
                matinv <<- NULL
        }
        get <- function() x
        setinverse <- function(sqmatinv) matinv <<- sqmatinv
        getinverse <- function() matinv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
}


##  function name:  cachesolve
##  Description: Return a matrix that is the inverse of 'x' 
##  Note: If the inverse has already been calculated 
##  (and the matrix has not changed), then the cachesolve retrieves 
##  the inverse from the cache.
##      
##    MBJ  4/22/14  version 1
##
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        matrxinv <- x$getinverse()
        if(!is.null(matrxinv)) {
                message("getting cached data")
                return(matrxinv)
        }
        ## inverse not cached - get matrix and do inverse using solve function
        ## Note: if X is a square invertible matrix, then solve(X) returns its inverse
        data <- x$get()
        matrxinv <- solve(data, ...)
        ## cache inverse
        x$setinverse(matrxinv)
        matrxinv
}
