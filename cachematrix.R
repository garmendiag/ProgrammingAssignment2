## R function that is able to cache potentially time-consuming computations
## Take advantage of the scoping rules of the R language and how they can be 
## manipulated to preserve state inside of an R object.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    invr <- NULL
    set <- function(y)  {
        x <<- y
        invr <<- NULL
        
    }

    get <- function() x
    setInverseCat <- function(inverse) invr <<- inverse
    getInverseCat <- function() invr
    list(set = set, get = get,
         setInverseCat = setInverseCat,
         getInverseCat = getInverseCat)
    
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above


cacheSolve <- function(x, ...) {
    invr <- x$getInverseCat()
    if(!is.null(invr)) {
        message("getting cached data")
        return(invr)
    }
    matr <- x$get()
    invr <- solve(matr, ...)
    x$setInverseCat(invr)
    invr
    
    ## Return a matrix that is the inverse of 'x'
}
    
       

