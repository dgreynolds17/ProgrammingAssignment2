## The makeCacheMatrix and cacheSolve functions are two functions that
## combine to cache the inverse of a matrix data set 

## The makeCacheMatrix function creates a special matrix that will be 
## inversed

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL ##set the initial
    set <- function(y) { ## function that sets the matrix with data
        x <<- y
        m <<- NULL
    }
    get <- function() { ## function that gets the data matrix
        x
    }
    setinverse <- function(inverse) { ## function that sets the matrix inverse
        m <<- inverse
    }
    getinverse <- function() { ## function that gets the inverse matrix
        m
    }
    list (set = set, get = get, setinverse = setinverse,
          getinverse = getinverse) ##returns a list of the functions
}


## cacheSolve function creates the inverse of the special matrix defined above
## If the data matrix has not been changed and has already been inversed and 
## cached it will be immediately returned, otherwise the matrix will be inversed 

cacheSolve <- function(x, ...) {
    m <- x$getinverse() ## Find the matrix that is the inverse of 'x'
    if (!is.null(m)) { ## If there is a cached matrix, return the inverse
        message("getting cached inverse matrix")
        return(m)
    }
    data <- x$get() ## get the matrix from the special matrix above
    m <- solve(data) ## inverse the matrix 
    x$setinverse(m) ## set the inverse
    m ## return the inversed matrix
    }
