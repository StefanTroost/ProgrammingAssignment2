## The methods in this script are meant to:
## Create the inverse of a matrix and cache it into a special matrix object: makeCacheMatrix
## Caculate or cache the inverse of the matrix
## 
## Example of use would be:
## y<-makeCacheMatrix(x)
## xInv<-cacheSolve(y)
##
## Restriction:
## the argument of the function must be a square invertible matrix
##

## This function creates the special "matrix" object that caches its inverse
makeCacheMatrix <- function(x = matrix()) {
            m <- NULL
            set <- function(y) {
                    x <<- y
                    m <<- NULL
            }
            get <- function() x
            setInverse <- function(solve) m <<- solve
            getInverse <- function() m
            list(set = set, get = get,
                 setInverse = setInverse,
                 getInverse = getInverse)

}


## This function calculates the inverse of the matrix unless already cached. 
## In that case it takes the cached inverse matrix 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
            m <- x$getInverse()
            if(!is.null(m)) {
                    message("getting cached data")
                    return(m)
            }
            data <- x$get()
            m <- solve(data, ...)
            x$setInverse(m)
            m
       
}
