## Put comments here that give an overall description of what your
## functions do

# The following functions take in a matrix, 
# calculate the inverse of the matrix, 
# and cache the inverse matrix so that if the 
# value of the inverse matrix is requested again,
# it can look to see if the value is cached
# before running the time consuming calculation 
# of finding the inverse of the matrix


## Write a short comment describing this function

# In the following function, makeCacheMatrix, we 
# set the value of the matrix, 
# get the value of the matrix
# set the value of the inverse matrix, and 
# get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    # matrix x is the original matrix
    # matrix m is the inverse of matrix x
    # set the value of m to NULL to be used if cacheSolve has not yet run 
    m <- NULL
    
    # set the value of the matrix, and cache the matrix
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    # get the value of the matrix, x 
    get <- function() x
    
    # set the value of the inverse matrix, m
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function()m
    
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function

# In the following function, cacheSolve, 
# the function checks to see if the inverse matrix has 
# already been computed.  If it has, it gets the value
# of the inverse matrix from the cache and returns it 
# If it has not, it computes the inverse of the matrix and
# sets the value of the inverse in the cache in the
# 'setinverse' function, and returns the inverse matrix

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    # matrix x is the original matrix
    # matrix m is the inverse of matrix x
    # this function returns m, the inverse matrix
    
    m <- x$getinverse()
    
    # first, check to see if the inverse matrix has alreay been calculated
    # if it has, the inverse computation can bee skipped
    # and the inverse matrix can be immediately returned
    
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    # if m is null (the inverse has not yet been computed), 
    # calculate and return the inverse of matrix x
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
