## Put comments here that give an overall description of what your
## functions do

## The makeCacheMatrix function receive a variable(matrix) and sets in memory.
## The function sets and gets the value of the matrix and the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
	m_inv <- NULL
    set <- function(y) {
        x <<- y
        m_inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m_inv <<- inverse
    getinverse <- function() m_inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## This function returns the inverse of the matrix.
## The function checks if the inverse has already computed and gets the results, if not it computes the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	m_inv <- x$getinverse()
    if(!is.null(m_inv)) {
        message("getting cached data.")
        return(m_inv)
    }
    data <- x$get()
    m_inv <- solve(data)
    x$setinverse(m_inv)
    m_inv
}
