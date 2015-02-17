## Functions to cache matrix inversion
##
## Usage example:
## a <- makeCacheMatrix(<matrix>)   ## Makes a matrix inversion cacheable
## a$get()                          ## Gets original matrix
## a$set(<matrix>)                  ## Sets a new matrix
## b <- cacheSolve(a)               ## Solves matrix inversion using cache if it exists

## Create special matrix that caches matrix inversion (solve function)
makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    #set a new matrix and clear cache
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    #get current matrix 
    get <- function() x
    #set inversion cache
    setsolve <- function(solve) s <<- solve
    #get inersion cache
    getsolve <- function() s
    list(set = set, 
         get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## Get a cached matrix inversion if exists, otherwise cache it's
## inversions for next calls
cacheSolve <- function(x, ...) {
    #look for cached inversion
    s <- x$getsolve()
    if(!is.null(s)) {
        #cached inversion exists, return it
        message("getting cached data")
        return(s)
    }
    #get actual matrix and get it's inversion (solve)
    data <- x$get()
    s <- solve(data, ...)
    #cache it's inversion
    x$setsolve(s)
    #returns matrix inversion
    s
}
