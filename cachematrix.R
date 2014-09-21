## The functions makeCacheMatrix and cacheSolve accepts a matrix as input, calculate the inverse of the matrix, and then cache the inverse. 

## makeCacheMatrix accepts a matrix as input and creates an object with the matrix that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    invMatrix <- NULL ## stes the inversed matrix object to NULL  
    
    ## below we define a function for setting the matrix to be inverted
    set <- function(y = matrix()) {
        x <<- y ## creates a global variable x with values y
        invMatrix <<- NULL ## the set function creates a new matrix to be inverted, and so the inversed object matrix must be reset to NULL
    }
    get <- function() x ## returns the matrix x
    setInvMatrix <- function(a) invMatrix <<- a ## caches the inverse matrix
    getInvMatrix <- function() invMatrix ## returns the cached inverse matrix
    list(set = set, get = get,
         setInvMatrix = setInvMatrix,
         getInvMatrix = getInvMatrix)
}



## cacheSolve calculates the inverse of the matrix object created by makeCacheMatrix after first checking whether the inverse has already been calculated.

cacheSolve <- function(x, ...) {
    invMatrix <- x$getInvMatrix() ## calls the cached inversed matrix using the getInvMatrix function from the vector x 
    if(!is.null(invMatrix)) {
        message("getting cached data")
        return(invMatrix)
    }  ## checks whether an inversed matrix has been cached; returns the cached inversed matrix if there is one
    data <- x$get() ## gets the original matrix using the get function from the vector x
    invMatrix <- solve(data, ...) ## calculates the inverse matrix
    x$setInvMatrix(invMatrix) ## outputs the inversed matrix using the setInverseMatrix function from x
    invMatrix ## returns the inversed matrix
}
