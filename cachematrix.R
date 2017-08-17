## My first function takes a given matrix and caches its invers. The second function enables the computation of the inverted matrix to the first function

## The function that creates a matrix and caches its inverse

makeCacheMatrix <- function(x = matrix()) {
        I <- NULL
        set <- function(y) {
                x <<- y
                I <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) I <<- inverse
        getInverse <- function() I
        list(set = set, 
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## The function that calculates the inverted matrix if it was not calculated in the first function. If ut was calculated, the function will retreive it from the cache 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
          I <- x$getInverse()
        if(!is.null(I)) {
                message("getting cached data")
                return(I)
        }
        data <- x$get()
        I <- solve(data, ...)
        x$setInverse(I)
        I
}
