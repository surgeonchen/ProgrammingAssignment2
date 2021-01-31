## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) inverse <<- solve
        getinverse <- function() inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}



## Write a short comment describing this function
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setsolve
        inverse
}


### Tests

newmatrix <- matrix(1:4, 2, 2) ## Assign a new matrix to subject "newmatrix"

# [,1] [,2]
# [1,]    1    3
# [2,]    2    4

makeCacheMatrix(newmatrix)  ## cache matrix reverse

# $set
# function(y) {
#         x <<- y
#         inverse <<- NULL
# }
# <environment: 0x000002450ab00ed0>

#         $get
# function() x
# <environment: 0x000002450ab00ed0>

#         $setinverse
# function(solve) inverse <<- solve
# <environment: 0x000002450ab00ed0>

#         $getinverse
# function() inverse
# <environment: 0x000002450ab00ed0>


cacheSolve(makeCacheMatrix(newmatrix)) ## return matrix reverse

# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5

