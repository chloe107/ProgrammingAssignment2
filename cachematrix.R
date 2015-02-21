## program assignment 2
## chloe107, 19-02-2015
## caching the inverse of a matrix


## makeCacheMatrix
## this function creates a special "matrix" object that can cache its inverse
## the function returns a list of functions:
## [set the value of the matrix, get the value of the matrix, set the value of the inverse, get the value of the inverse]

makeCacheMatrix <- function(x = matrix()) {
    # declare a variable (inverse_matrix) for the inverse of the matrix
    inverse_matrix <- NULL

    # set the value of the matrix
    set <- function(y) {
        x <<- y
        inverse_matrix <<- NULL
    }
	
    # get the value of the matrix
    get <- function() x

    # set the value of the inverse matrix
    set_inverse <- function(inverse) inverse_matrix <<- inverse
	
	# get the value of the inverse matrix
    get_inverse <- function() inverse_matrix

    # return the matrix
    list(set = set, get = get, set_inverse = set_inverse, get_inverse = get_inverse)
}


## cacheSolve
## function calculate the inverse of the special "matrix" returned by makeCacheMatrix above 
## return cached inverse of the matrix if the inverse of the matrix has already been calculated

cacheSolve <- function(x, ...) {
    # return a matrix that is the inverse of 'x'
	inverse_matrix <- x$get_inverse()

    # return the inverse matrix if it has been calculated
    if (!is.null(inverse_matrix)) {
        return(inverse_matrix)    }

    # calculate the inverse matrix
    data <- x$get()
    inverse_matrix <- solve(data, ...)

    # cache the inverse matrix
    x$set_inverse(inverse_matrix)

    # return the inverse matrix
    inverse_matrix
}