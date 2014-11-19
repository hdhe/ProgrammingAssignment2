## This function calculates the inverse of a matriz and saves the results
## in a cache array, so if the inverse has been calculated it is not recalculated again


## This function make a Cache Matrix to save the results of matrix inverse calculation

makeCacheMatrix <- function(x = matrix()) {

        #We inicialize le inverse matriz "inV" as null
        inv <- NULL
        
        #We define the initial set
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        ## We define the function to be used in the cacheSolve function
        get <- function() x
        setinv <- function(solve) inv <<- solve
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## This calculate the inverse of a matrix created with the function makeCacheMatrix

cacheSolve <- function(x, ...) {
        
        ## This identify if there is a calculated inverse for the matrix. If there is
        ## returns the cache inverse and a message
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting inverse matrix from cached data")
                return(inv)
        }
        
        ## If there is no inverse in the cache, this calculates the inverse and saves it in cache
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        ## Return a matrix that is the inverse of 'x'
        inv
}
