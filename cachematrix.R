# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. The
# following two functions are used to cache the inverse of a matrix.

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) m <<- inverse
    getInverse <- function() m
    list(set = set, get = get, setInverse = setInverse,  getInverse = getInverse)
}

# Return a matrix that is the inverse of 'x'
# Checks if the inverse has already been computed. If so, return the already inversed matrix. 
# If not, it computes the inverse, sets the value in the cache via setinverse function.
# This function assumes that the matrix is always invertible.
cacheSolve <- function(x, ...) {
    m <- x$getInverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$setInverse(m)
    m
}

# Running example
## Create a invertible 2x2 matrix
##  m <- matrix(c(2,3,2,2),2,2)
##  m
#   [,1] [,2]
#   [1,]    2    2
#   [2,]    3    2
## Create a cache-able matrix object
##  m2 <- makeCacheMatrix(m)
## Inverse the matrix the first time
##  m3 <- cacheSolve(m2)
##  m3
#   [,1] [,2]
#   [1,] -1.0    1
#   [2,]  1.5   -1
##  m3 <- cacheSolve(m2)
# Solving the matrix the second time prints a message
#getting cached data
##  m3
#   [,1] [,2]
#   [1,] -1.0    1
#   [2,]  1.5   -1
##  
