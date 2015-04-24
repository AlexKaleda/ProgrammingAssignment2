## Two functions to calculate inverse of matrix and store cached result

## Defines 4 subfunctions for getting and storing data

makeCacheMatrix <- function(x = matrix()) {
    matrCached <- NULL
    set <- function(y) {
        x <<- y
        matr <<- NULL
    }
    get <- function() x
    setmatrix <- function(y) matrCached <<- y
    getmatrix <- function() matrCached
    list(set = set, get = get,
         setmatrix = setmatrix,
         getmatrix = getmatrix)
}


## This function checks if there is already calculated matrix and returns it
## If not, then calculate it and store.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getmatrix()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setmatrix(m)
    m
}

## test it:
# > test <- makeCacheMatrix(matrix(c(4,3,3,2),2,2))
# > cacheSolve(test)
# [,1] [,2]
# [1,]   -2    3
# [2,]    3   -4
# > cacheSolve(test)
# getting cached data
# [,1] [,2]
# [1,]   -2    3
# [2,]    3   -4