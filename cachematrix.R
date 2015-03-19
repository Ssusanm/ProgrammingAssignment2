#to take a matrix and return the inverse. Once calculated, store 
#in cache so that it wont be recalculated.
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL    #assigning null value        
        set <- function(y) {
                x <<- y #using a different environment to store it for cache purpose
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}
cacheinverse <- function(x, ...) {
        m <- x$getinverse() #gets the matrix if already existing- if yes donot calculate
        if(!is.null(m)) {    
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}