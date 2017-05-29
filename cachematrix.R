## Put comments here that give an overall description of what your
## functions do

# This function creates a special matrix object that can cache its inverse. The form of the function follows the example of caching the mean shown by Dr. Peng, except this function caches the inverse of a matrix.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL                         # create an empty matrix
        set <- function(y) {                
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Write a short comment describing this function. This function returns the invese of a matrix, but first checks to see if the inverse of that matrix has been calculated already and is stored (or cached) in a different environment. If so, then the function simply returns the cached value. If not, then the function generates the inverse. 

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data.")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}

x = rbind(c(1, -1/4), c(-1/4, 1))
m = makeCacheMatrix(x)
m$get()

