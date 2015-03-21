## makeCacheMatrix creates an empty matrix, sets the value of the matrix, gets the value
## of the matrix, sets the value of the inverse, and gets the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
    mat <- is.null(matrix)                ## initialize an empty matrix
    set <- function(y)  {                         ## y is the argument passed to mCM
        x <<- y                                    ## set x for the function environment to y
        mat <<- is.null(matrix)         ## set mat for the mCM environment to y
    }             
    get <- function() x                            ## create function called get in the mCM env and assign to it
    setinv <- function(inv) mat <<- inv            ## take value and set to mat
    getinv <- function() mat                       ## return value 
    list(set = set, get = get, setinv = setinv,    ## list values of functions in mCM
         getinv = getinv)
}

## cacheSolve calculates the inverse of the matrix returned by makeCacheMatrix.

cacheSolve <- function(x, ...) {
    mat <- x$getinv()                              ## go to the x environment, assign mat value to this env
    if(!is.null(mat)){                             ## if there is something in mat, print message and value of mat
        message("getting cached data")
        return(mat)
    }
    else {
        data <- x$get()                            ## if mat does not have a value, pull x into a local variable called data
        mat <- solve(data, ...)                    ## take the inverse of x by calling solve on local data variable
        x$setinv(mat)                              ## assign the calculated inverse to x environment using setinv
        return(mat)                                        ## Return a matrix that is the inverse of 'x'
    }
}