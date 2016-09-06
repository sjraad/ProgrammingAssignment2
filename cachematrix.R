## In this assignemnet, we will write two functions to cache potentially
## time-consuming computations by caching the the inverse of a Matrix
## rather than computinmg it repeatedly.

## The first function, makeCacheMatrix, creates a special "Matrix" 
## that can cache its inverse. It creates a a list containing a 
## function to:
## 1. Set the value of the Matrix
## 2. Get the value of the Matrix
## 3. Set the value of the Inverse of the Matrix
## 4. Get the value of the Inverse of the Matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(inv) m <<- inv
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## The second function, casheSolve, computes the inverse of the special 
## "Matrix" returned by the makeCacheMatrix function above.  If the
## inverse has already been calculated, it retrieves it from cache otherwise
## it computes the inverse.

cacheSolve <- function(x, ...) {
    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}

> x <- matrix(1:4, 2, 2)
> m = makeCacheMatrix(x)
> m$get()
     [,1] [,2]
[1,]    1    3
[2,]    2    4
> cacheSolve(m)
     [,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5
> cacheSolve(m)
getting cached data
     [,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5
