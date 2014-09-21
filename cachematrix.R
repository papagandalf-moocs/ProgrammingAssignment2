## implementation of the "special" matrix, whose inverse we can cache
## 

## creates the "special" matrix in a way similar to the one given in the vector example

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) m <<- inverse
        getInverse <- function() m
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## returns the inverse of the matrix, after checking if it has been calculated and cached before

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        m <- x$getInverse()
        if(!is.null(m))
                return(m)
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m
}

