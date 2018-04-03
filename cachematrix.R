## These two functions, makeCacheMatrix and cacheSolve, are 
## a pair of functions which will calculate the inverse of a matrix
## and save it to a cache which can be recalled.

## makeCacheMatrix creates a special "matrix", which is really a list
## containing functions to 
## (1) set the value of the matrix
## (2) get the value of the matrix
## (3) set the value of the inverse of the matrix
## (4) get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        
        ## m will hold the value for inverse of the matrix
        ## to begin, we will cleared it and set to null
        m <- NULL
        
        ## (1) 'set' is a function which is taking object 'y' into x,
        ## and clears m, like just above.
        ## these are <<- because 'set' is not in parent env of x and m
        set <- function(y) {
                ## x, parent env, takes y's value
                x <<- y
                ## m, parent env, is cleared
                ## this prevents an old m returning by mistake
                m <<- NULL 
        }
        ## (2) 'get' is a function which returns the matrix x
        get <- function() x
        ## (3) 'setsolve' function  puts the inverse into m; parent env
        setinverse <- function(inverse) m <<- inverse
        ## (4) 'getsolve' is a function, returns the value of inverted matrix.
        getinverse <- function() m
        ## Now setting makeCacheMatrix as a list,
        ## containing all of the functions just defined, 
        ## returns them to parent env.
        ## they are named, so can be called by 'x$...' in the next function
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve is a function that computes the inverse of the special 'matrix' above
## if the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        ## m takes the value of getsolve above
        m <- x$getinverse()
        ## if not null, then m is a valid inverted matrix, return it.
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        ## using the makeCacheMatrix functions, to invert the matrix.
        ## putting matrix x into data.
        data <- x$get()
        ## the solve function result is run on data and put into m
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
