## Calculate inverse of a matrix and retrieve cached inverse 
## if the matrix is unchanged

## makeCacheMatrix is a function object for caching the inverse of the 
## passed-in matrix
makeCacheMatrix <- function(x = matrix()) {
        invx <- NULL

        ## Set matrix
        set <- function(y) {
                x <<- y
                invx <<- NULL
        }
        
        ## Get matrix
        get <- function() x
        
        ## Set inverse of matrix
        setinverse <- function(inverse) {
                invx <<- inverse
        }
        
        ## Get inverse matrix
        getinverse <- function() invx
        
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve function calculates and saves the inverse of matrix to the 
## function object passed in as a parameter
cacheSolve <- function(fn) {
        ## Return a matrix that is the inverse of 'x'
        invx <- fn$getinverse()
        if(!is.null(invx)) {
                message("getting cached inverse matrix")
                return(invx)
        }
        matrix <- fn$get()
        invx <- solve(matrix)
        fn$setinverse(invx)
        invx
}
