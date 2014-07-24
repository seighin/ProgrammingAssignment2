## A pair a functions to calculate inverses of matrix, 
## using cached solution if availaale


## Returns an object that maintains a cached matrix inverse
## Use:  m <- makeCacheMatrix(x), create object with matrix x
##       m$set(x), assign matrix x to object
##       x = m$get(), retrieve matrix
##       m$setInverse(inv), store the inverse of matrix
##       inv = m$getInverse(), retrieve stored inverse, might be null

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    
    set <- function(y) { x <<- y
                        i <<- NULL}
    
    get <- function() x
    
    setInverse <- function(inv) i <<- inv
    
    getInverse <- function() i
    
    list(set=set, get=get, setInverse=setInverse, getInverse=getInverse )
}


## Retrieve the inverse of matrix from object created with makeCacheMatrix
## Will used cached inverse if possible
## Use: inv <- cacheSolve(m)

cacheSolve <- function(x, ...) {
    i <- x$getInverse()
    
    if (is.null(i)){
        message("Solving inverse...")
        i <- solve(x$get(), ...)
        x$setInverse(i)
    }
    else {
        message("Retrieving cached inverse...")
    }
    
    i
}
