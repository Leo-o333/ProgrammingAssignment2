## The first function will create a inverse matrix and store it. Everytime when another matrix was input, it will 
## decide whether this matrix is inverted before, and if it is, the functions will get result from it cache.

## it will create a inverse function of the input function

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


##It will compute the inverse matrix returned from previous function. If it is calculated before, it will draw from its memory directly

cacheSolve <- function(x, ...) {
           m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- inverse(data, ...)
        x$setinverse(m)
        m
        
}
