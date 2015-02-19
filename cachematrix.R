## This pair of functions allow computation results to be cached so it can be used
## repeatedly without running CPU Cycles. It computes and caches the inverse of 
## a matrix.
##
## makeCacheMatrix - creates a Matrix Cace with Setter / Getter Methods for
## - the Matrix
## - and it's inverse
## cacheSolve 


## This function creates a special "matrix" object
## that can cache its inverse.
makeCacheMatrix <- function( mtx = matrix() ) {
    ## the call to this function sets the mtx Object
    ## the inverse is set to null immediately
    inverse <- NULL
    
    ## setter function for Matrix - reset the inverse to null
    set <- function( x ) {
        mtx <<- x;
        inverse <<- NULL;
    }
    
    ## getter function for Matrix
    get <- function() {
        return ( mtx )
    }
    
    ## setter function for the Inverse of the Matrix
    setInverse <- function( inv ) {
        inverse <<- inv
    }
    
    ## getter function for the Inverse of the Matrix
    getInverse <- function() {
        return ( inverse )
    }
    
    ## return the list of functions of the environment so one can
    ## query the matrix and the cached inverse computation
    return ( list (
        set = set, 
        get = get, 
        setInverse = setInverse , 
        getInverse = getInverse )
        )
}


## This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.
cacheSolve <- function(mtx, ...) {
    inverse <- mtx$getInverse()
    
    ## if present in cache, simply return it.
    if ( !is.null( inverse ) ) {
        message("Getting cached data...")
        return ( inverse )
    }
    
    ## computing the matrix inverse
    dataM <- mtx$get()    # get the matrix
    inverse <- solve( dataM )  # use built in function to solve
    
    ## save the inverse into the cache for future calls
    mtx$setInverse( inverse )
    return ( inverse )
}
