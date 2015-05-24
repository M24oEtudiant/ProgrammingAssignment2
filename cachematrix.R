## Matrix inversion is usually a costly computation and there may be some benefit
## to caching the inverse of a matrix rather than computing it repeatedly. With
## this assignment work, a pair of functions are written to cache the inverse of a
## matrix.

## The first function 'makeCacheMatrix' creates a special "matrix" object, which
## is really a list containing a function to

# 1.  set the value of the matrix
# 2.  get the value of the matrix
# 3.  set the value of the matrix's inverse
# 4.  get the value of the matrix's inverse

makeCacheMatrix <- function(x = matrix()) {
        ## Creates a special "matrix" object that can cache its inverse.
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(i) inv <<- i
        getinv <- function() inv
        list(set = set, 
             get = get,
             setinv = setinv,
             getinv = getinv)
}


## The second function 'cacheSolve' computes the inverse of the matrix in the
## special "object" created with the above function. However, it first checks to
## see if the inverse has already been computed. If so, it get's the inverse
## from the cache and skips the computation. Otherwise, it computes the inverse
## of the matrix and sets the value of the inverse in the cache.

## For inverse computation 'solve' function has been used. For example, if 'x'
## is a square invertible matrix, then this function will return its inverse.

## Note: Given assumption for this assignment is square invertible matrix will 
## always be supplied.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}

## -----------
## Sample run:
## -----------

## > x <- matrix(c(2,1,1,2,0,1,1,1,1), nrow=3, ncol=3) 
## Inversable matrix is defined

## > m <- makeCacheMatrix(x)
## Created the special 'object'

## > m$get()
##      [,1] [,2] [,3]
## [1,]    2    2    1
## [2,]    1    0    1
## [3,]    1    1    1
## Matrix is cached

## > m$getinv()
## NULL
## Inverse is not cached

## > cacheSolve(m)
## [,1] [,2] [,3]
## [1,]    1    1   -2
## [2,]    0   -1    1
## [3,]   -1    0    2
## With the first call ... computed inverse is returned

## > m$getinv()
## [,1] [,2] [,3]
## [1,]    1    1   -2
## [2,]    0   -1    1
## [3,]   -1    0    2
## Computed inverse is now cached

## > cacheSolve(m)
## getting cached data
## [,1] [,2] [,3]
## [1,]    1    1   -2
## [2,]    0   -1    1
## [3,]   -1    0    2
## With the second call ... cached inverse is returned