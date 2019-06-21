## Put comments here that give an overall description of what your
## functions do

#The following two functions take the matrix as an argument and calculate the inverse matrix. The result of the calculation can be stored in the cache memory, allowing efficient calculation to recall the saved value.

## Write a short comment describing this function
# The makeCacheMatrix function takes a matrix as an argument and stores it in x. If it is a matrix that already computes an inverse matrix, this function has its cache data.
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) i <<- solve
        getsolve <- function() i
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## Write a short comment describing this function
# The cacheSolve function receives the matrix argument as the return value of makeCacheMatrix and computes the inverse matrix. If the cache data already contains a calculated value, a message is displayed.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i<- x$getsolve()
        if(!is.null(i)) {
                message("getting cached matrix")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setsolve(i)
        i
}
