## Put comments here that give an overall description of what your
## functions do

# returns a special matrix object (a list of functions) that caches the
# expensive calculation of the matrix inverse
makeCacheMatrix <- function(x = matrix()) {
    inverse  <- NULL
    set <- function(y) {
        # assign y to x in the parent environment
        x <<- y
        # reset inverse in the parent environment. Invalidates the cache
        inverse <<- NULL
    }
    # simple "getter" function to return the actual matrix object
    get <- function() {
        x
    }
    # "setter" function
    setinverse <- function(new_inverse) {
        # assign new_inverse to the inverse defined in the parent environment
        inverse <<- new_inverse
    }
    # simple "getter" function to retrieve the inverse
    getinverse <- function() {
        inverse
    }

    list(set = set, get = get,
         setinverse = setinverse, getinverse = getinverse)
}


# calculates the inverse of the matrix if it's not already cached
cacheSolve <- function(x, ...) {
    # get the inverse
    inverse <- x$getinverse()
    # if it's already calculated return it early
    if(!is.null(inverse)) {
        message('fetching inverse from cache')
        return(inverse)
    }

    # retrieve the actual matrix object
    data <- x$get()
    #calculate the inverse with the internal solve function
    inverse <- solve(data, ...)
    # store the calculated inverse in the special matrix object
    x$setinverse(inverse)
    inverse
}

# to test this run the following on the command line:
message('creating the cache matrix object for the following matrix:')
a_matrix <- matrix(c(4,2,7,6), nrow=2)
print(a_matrix)
caching_matrix <- makeCacheMatrix(a_matrix)
cacheSolve(caching_matrix)
message('the inverse matrix is:')
print(caching_matrix$getinverse())
message("what's the inverse again?:")
cacheSolve(cachingMatrix)
print(caching_matrix$getinverse())
