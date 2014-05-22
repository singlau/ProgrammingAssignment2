## The following two functions calculate the inverse of a matrix.
## If the inverse matrix is in the cache, then the cache is
## return. If the inverse matrix is not in the cache, then solve()
## is used to calculate the inverse matrix.

## This function creates a special "matrix" object that can cache
## its inverse.

makeCacheMatrix <- function(x = matrix()) {
        
        ## variable im contains the inverse matrix
        im <- NULL
        
        ## set the value of the matrix and initalize im in
        ## cache
        set <- function(y) {
                x <<- y
                im <<- NULL
        }
        
        ## get the value of the matrix
        get <- function() x
        
        ## cache the inverse matrix
        setInvmatrix <- function(Invmatrix) im <<- Invmatrix
        
        ## get the value of the inverse matrix from cache
        getInvmatrix <- function() im
        
        ## return a list containing above functions
        list(set = set, get = get,
             setInvmatrix = setInvmatrix,
             getInvmatrix = getInvmatrix)
}

## This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has already
## been calculated (and the matrix has not changed), then the 
## cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        
        ## try to get the inverse matrix in cache
        im <- x$getInvmatrix()
        
        ## If im is not null, the inverse matrix is cached and
        ## the value is returned
        if(!is.null(im)) {
                message("getting cached data")
                return(im)
        }
        
        ## if im is null, retrieve the matrix from cache
        data <- x$get()
        
        ## use solve to find the inverse matrix
        im <- solve(data, ...)
        
        ## cache the inverse matrix
        x$setInvmatrix(im)
        im
}