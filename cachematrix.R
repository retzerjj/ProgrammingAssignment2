## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# This function will create a list of functions which will create
# a "special" matrix that can cache its inverse.
# Specifically, it will:
#    1. set the value of a matrix
#    2. get the value of a matrix
#    3. set the value of a matrix inverse
#    4. get the value of a matrix inverse

makeCacheMatrix <- function(x = matrix()) {
        myInv <- NULL
        set <- function(y) {
                x <<- y
               myInv <<- NULL
        }
        get <- function() x
        setInv <- function(solve) myInv <<- solve 
        getInv <- function() myInv 
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}


## Write a short comment describing this function

# This function first checks to see if the inverse of x already exits.
# If it does, it returns the cached inverse, if not, the inverse is calculated and returned.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        myInv <- x$getInv()
        if(!is.null(myInv)) {
                message("getting cached data")
                return(myInv)
        }
        data <- x$get()
        myInv <- solve(data, ...)
        x$setInv(myInv)
        myInv 
}
