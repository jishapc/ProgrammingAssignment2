

## The 2 functions are created to call inverse of a matrix that's cached in 1 function  
## and used in another function

## This function sets a matrix and get the value of the matrix in get function in parent environment. 
## The function then sets the inverse of the matrix (which is calculated and cached in another function) 
## and sets the value in a matrix

makeCacheMatrix <- function(x = matrix()) {
    inverseM <- NULL
    set <- function(y) {
        x <<- y
        inverseM <<- NULL
    }
    get <- function() x
    setinverse <- function(solveMatrix) inverseM <<- solveMatrix
    getinverse <- function() inverseM
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function uses the matrix from parent environment
## and finds the inverse of the matrix using solve() function
## everytime while the function is called it checks whether the inverse matrix is available in cache
## if no then first time it calculates and sets the value which is then cached

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inverseM <- x$getinverse()
    if(!is.null(inverseM)) {
        message("getting cached inverse of matrix")
        return(inverseM)
    }
    data <- x$get()
    inverseM <- solve(data, ...)
    x$setinverse(inverseM)
    inverseM
}
