## Put comments here that give an overall description of what your
## functions do


## nikosvalet wrote
## This R file contains two functions that can be used to 
## solve the inverse of a Matrix many times where the inverse is calculated just once
## Inverse Matrix is stored and recalled


## Write a short comment describing this function

## This function is called first
## For example we make a matrix M <- matrix(1:4,c(2,2))
## Then we call makeCacheMatrix function e.g. CacheM <- makeCacheMatrix(M)


makeCacheMatrix <- function(x = matrix()) {
        cachedInv <- NULL
        set <- function(y) {
                x <<- y
                cachedInv <<- NULL
        }
        get <- function() x
        setInv <- function(inverse) cachedInv <<- inverse
        getInv <- function() cachedInv
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}
## Write a short comment describing this function

## nikosvalet wrote
## This function returns the inverse of the cached matrix
## This function is called second
## For example cacheSolve(CacheM) returns the inverse of M
## But is not calculating it second time as it is calling the stored inverse


cacheSolve <- function(x, ...) {
        inv <- x$getInv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        my_data <- x$get()
        inv <- solve(my_data, ...)
        x$setInv(inv)
        inv
}