## The pair of the functions (makeCacheMatrix, cacheSolve) in this file 
## provides a way to cache the inverse of a matrix to avoid recomputing it. 
##
## The example below shows how to use them.
## 1. create a matrix called A:
## A <- matrix(c(2,0,0,2), nrow=2, ncol=2)
## 2. Using makeCacheMatrix to create a spceical "matrix" object called B:
## B <- makeCacheMatrix(A)
## 3. Using cacheSolve, via B, calculate A's inverse matrix called C:
## C <- cacheSolve(B)
## 4. To recalculate the inverse of A, the cached inverse matrix will be used:
## D <-	 cacheSolve(B)
## The message "getting cached inverse matrix" will be printed


## Function Name: makeCacheMatrix
## Description  : Take the input matrix and create a special matix for 
##                cacheSolve to use.
## Input        : x - an inversable matrix
## Output       : special matrix with a list of functions
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    ## initialize the special matrix
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    ## get the real matrix of this special "matrix"
    get <- function() x
    ## function to cache the inverse matrix
    setinverse <- function(inv) m <<- inv
    ## function to get the cached inverse matrix
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## Function Name: cacheSolve
## Description  : Calculate the inverse of the special matrix that is
##                created using makeCacheMatrix. If the cached inverse
##                exists, the cached inverse matrix will be returned;
##                Otherwise, the inverse matrix will be computed using
##                solve function call.
## Input        : x - a special "matrix" created by calling makeCacheMatrix
## Output       : the inverse matrix of the original inversable matrix
cacheSolve <- function(x, ...) {
    ## get the cached inverse matrix
    m <- x$getinverse()
    if(!is.null(m)) {
        ## the inverse is cached so reuse it
        message("getting cached inverse matrix")
        return(m)
    }
    ## there is no cached inverse. Get the real matrix of the special "matrix"
    data <- x$get()
    # take inverse
    m <- solve(data, ...)
    ## cached the inverse matrix
    x$setinverse(m)
    m
}

