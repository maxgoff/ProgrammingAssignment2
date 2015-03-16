## For Programming Assignment 2
## The makeCacheMatrix function presumes an invertible matrix as the argument.
## 

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL                                       # Initialize the inverted matrix to NULL
    set <- function(y) {                            #  y is the argument passwed to makeCacheMatirx
        x <<- y                                     #  set x for the fucntion environment to y
        m <<- NULL                                  #  set m for makeCacheMatrix environment to NULL
    }
    
    get <- function() x                             #  create function 'get' in makeCacheVetor parent
    
    setinv <- function(m_inv) m <<- m_inv           #  sets matrix inversion value in makeCacheMatrix frame    
    
    getinv <- function() m                          #  returns value of m from makeCacheMatrix frame
    list(set = set, get = get,                      #  list out value of the makeCacheMatrix frame
         setinv = setinv,
         getinv = getinv)

}


## Set the cache if not set -- otherwise retrieve from cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinv()
    if(!is.null(m)) {                               ## is the inverse matrix data cached?  If so, return it ...
        message("getting cached data")
        return(m)
    }
    data <- x$get()                                ## if not then get matrix data 
    m <- solve(data, ...)                          ## invert the matrix
    x$setinv(m)                                    ## and cache the data
    return(m)                                      ## return the inverse matrix
}
