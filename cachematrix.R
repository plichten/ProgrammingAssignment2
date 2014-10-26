## create a function that is a list containing a function to 
## set the value of a matrix
## get the value of a matrix
## set the value of the inverse of the matrix
## get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    ## create a null matrix
    inverseMatrix <- NULL
    
    ## create a function to set the value of the matrix
    set <- function(y = matrix()) {
        ## make sure the scope of the matrix and its inverse is such
        ## that it preserves the state inside of the "set" object
        x <<- y
        inverseMatrix <<- NULL
    }
    
    ## create a function to get the value of the matrix
    get <- function() x
    
    ## create a function to set the value of the inverse matrix
    ## make sure that it's scoped to preserve the state inside the
    ## "setInverse" object
    setInverse <- function(inverse) inverseMatrix <<- inverse
    
    ## create a function to get the value of the inverse matrix
    getInverse <- function() inverseMatrix
    
    ## return a list of the makeCacheMatrix functions
    list(set = set, get = get, 
         setInverse = setInverse, 
         getInverse = getInverse)
}

## create a function that checks to see if the inverse of
## the matrix has been calculated, if not it calculates it
cacheSolve <- function(x, ...) {
    ## call the getInverse function from x
    inverseMatrix <- x$getInverse()
    
    ## check to see whether the call returned an empty matrix
    ## if it is not empty, return a message and the inverse matrix
    if(!is.null(inverseMatrix)) {
        message("getting cached data")
        return(inverseMatrix)
        
    }
    
    ## if it is empty, retrieve the original matrix using the 
    ## "get" function from x  
    data <- x$get()
    
    ## calculate the inverse
    inverseMatrix <- solve(data, ...)
    
    ## set the inverse using the "setInverse" function from x
    x$setInverse(inverseMatrix)
    inverseMatrix
}
