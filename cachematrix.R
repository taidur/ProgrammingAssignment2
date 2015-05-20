# Functions in this scirpt create matrix which is able to cache it's inverse (makeCacheMatrix)
# and then it's possible to get inverse of the matrix from the cache if it's
# already calculated (cacheSolve)


## makeCahceMatrix creates matrix that can cache it's inverse

makeCacheMatrix <- function(x = matrix()) {
    # define variable for inverse matrix
    i  <- NULL
    # function to set matrix
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    # function to get matrix 
    get <- function() x
    # function to set inverse matrix
    set_inverse <- function(inverse) i <<- inverse
    # function to get inverse matrix
    get_inverse <- function() i
    #list of functions
    list(set = set, get = get,set_inverse = set_inverse,get_inverse = get_inverse)
    
}


## cacheSolve returns invese matrix from cahce if it's already calculated
## or solvs it when it's not available in cace

cacheSolve <- function(x, ...) {
    
    # get inverse matrix
    i <- x$get_inverse()
    # if inverse matrix is in cache return it from cahce
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    # Otherwise calculate inverse matrix
    # setp 1: initialize data (assign matrix to variable "data")
    data <- x$get()
    # step 2: caluclate inverse matrix using solve
    i <- solve(data)
    # step 3: put calcucalted inverse matrix into cache
    x$set_inverse(i)
    # return inverse matrix
    i
}
