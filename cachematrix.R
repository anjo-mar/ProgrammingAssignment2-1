# makeCacheMatrix is a function to store a list containing a function to:
# 1 - set the value of the matrix
# 2 - get the value of the matrix
# 3 - set the value of the inverse of the matrix
# 4 - get the value of the inverse of the matrix
makeCacheMatrix <- function(x=matrix()) {
    i <- NULL
    
    # set is a function that changes the matrix stored in the main function
    # we only use this function if we want to change the matrix.
    set <- function(y) {
        x <<- y             # replace the matrix 'x' with the value of the input 'y' in the main function
        i <<- NULL          # restores to null the value of the inverse 'i'. Since the matrix 'x' is new, 
                            # the new inverse of the matrix needs to be recalculated through the function cacheman. 
    }
    
    # get is a function that returns the matrix 'x' stored in the main function
    get <- function() {
        x   
    }
    
    # setinverse is a function to store the value of the input in a variable 'i' with the inverse of the matrix
    setinverse <- function(inverse) {
        i <<- inverse   
    }
    
    # getinverse is a function that returns the inverse of the matrix 'i' in the main function
    getinverse <- function() {
        i   
    }
    
    # store the 4 functions in the function makeCacheMatrix, we need a function list(), 
    # so that when we assign makeCacheMatrix to an object, the object has all the 4 functions
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

# Calculate the inverse of the 'matrix' created with the function called 'makeCacheMatrix'.
# It first check to see if the inverse of the matrix already been computed. 
# If so, it gets the inverse of the matrix from the cache and skips the computation.
# Otherwise, it calculates the inverse of the matrix of the data and sets the value of the inverse 
# of the matrix in the cache via the the setinverse function. 
cacheSolve <- function(x, ...) {
    # set value to m with current value in cache
    i <- x$getinverse()
    # if we have a value in cache then return value in cache and we don't calculate the inverse of the matrix
    if(!is.null(i)) {
        # returns the value in cache and exit the function
        return(i)   
    }
    # We need to calculate the inverse of the matrix, for that we need to get the matrix data.
    data <- x$get()
    i <- solve(data, ...)                   # compute the inverse of the matrix
    x$setinverse(m)                         # store the inverse of the matrix into the function 'makeCacheMatrix'
    i                                       # return the inverse of the matrix
}
