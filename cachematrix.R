## This two functions will create special matrix objects that contain 
##   the original data (a matrix), derived data (its inverse) and several 
##   associated methods to interact with these data (set/get methods)

# makeCacheMatrix stores a given matrix into a special object that will allow its 
#   inverse to be stored 
makeCacheMatrix <- function(x = matrix()) {
    # by default when the matrix is created, the inverse is not computed
    inv <- NULL
    # method set() definition
    set <- function(y) {
        if (!identical(x,y) && inv != NULL){
            x <<- y
            # inv needs to be set to null *everytime* the original data changes
            inv <<- NULL
        }
        else {
            message('identical matrix copied, so previously cached inversed is kept')
        }
    }
    # other methods definitions
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    # Return list object with several methods to set/get data and inverses
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Write a short comment describing this function
## cacheSolve returns the inverse of a matrix, either by computing it 
##   or by returning a previously computed and cached version
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    ## Verify if this cached matrix has had its inverse computed previously
    inv <- x$getinv()
    if(!is.null(inv)) {
        ## Found previously cached inverse data, no need to recompute
        message("getting cached data")
        return(inv)
    }
    # get original matrix data
    data <- x$get()
    # compute inverse matrix
    inv <- solve(data, ...)
    x$setinv(inv)
    # return inverse matrix
    inv
}

