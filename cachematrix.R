## Stores a matrx´s inverse in cache in order to have available
## to use without having to re-calculate it

## The first function, makeVector creates a special "matrix", 
## which is really a list containing a function to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

## HOLA

makeCacheMatrix <- function(x = matrix()) {
    xInv <- NULL
    set <- function(y) {
        x <<- y
        xInv <<- NULL
    }
    get <- function() x
    setInv <- function(solve) xInv <<- solve
    getInv <- function() xInv
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
}


## Calculates the inverse of the special "matrix" created with the above function
## It first checks to see if the inverse has already been calculated
## If so, it gets the inverse from the cache and skips the computation
## Otherwise, it calculates the inverse of the data and sets the 
## value of the inverse in the cache via the setInv function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    xInv <- x$getInv()
    if(!is.null(xInv)) {
        message("getting cached data")
        return(xInv)
    }
    data <- x$get()
    xInv <- solve(data, ...)
    x$setInv(xInv)
    xInv
}
