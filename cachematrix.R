## Functions to cache the inverse of a matrix so that it doesn´t need to be 
# calculated every time.

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        #null matrix
        z <- NULL
        # set the matrix
        set <- function(y) {
                x <<- y
                z <<- NULL
        }
        #get the matrix
        get <- function () x
        #set the inverse matrix
        setInverse <- function (inverse) z <<- inverse
        # get the inverse matrix
        getInverse <- function () z
        list(set=set, get=get,
             setInverse = setInverse,
             getInverse = getInverse)
        
}


## This function computes the inverse of the special "matrix" returned 
# by makeCacheMatrix above. If the inverse has already been calculated 
# (and the matrix has not changed), then the cachesolve should retrieve 
# the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        z <- x$getInverse()
        #return the inverse if it is already set
        if(!is.null(z)) {
                message("getting cached data")
                return(z)
        }
        # get matrix from object
        data <- x$get ()
        #calculate the inverse using solve()
        z <- solve(data, ...)
        #set inverse to the object
        x$setInverse(z)
        #return matrix
        z
}




