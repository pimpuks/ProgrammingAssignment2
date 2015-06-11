## This R source file contains 2 functions makeCacheMatrix and cacheSolve. 
## It uses the concept of lexical scoping to create a "special" matrix that can
## cache its inverse value after the first calculation. 


## makeCacheMatrix returns a list which contains functions and value of matrix 
## and the inverse in its environment. The functions are set, get, setinverse 
## and getinverse

makeCacheMatrix <- function(x = matrix()) {
    # every time makeCacheMatrix is called, it creates a new environment and 
    # initialize i to null
    # i will be used to store inverse value when setinverse function is called
    # the returned list, and function set, get, setinverse, getinverse shares 
    # value of i and x
    i <- NULL
    
    # update the matrix value to new value and initialize i to null 
    # as the value of matrix is changed.
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    
    # get is a function that returns the actual matrix value (x)
    get <- function() x
    
    # setinverse is a function that stores the inverse in variable i
    # in parent level for caching purpose
    setinverse <- function(inverse) i <<- inverse
    
    # getinverse is a function that returns the cached inverse
    getinverse <- function() i
    
    # return a list contains all functions required for future use
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve first check if the inverse of x is available in cache
## if yes, it will return the cached inverse
## otherwise, it retrieve the value of the matrix (x$get()), 
## calls R function solve() and store the value in cache via setinverse() of x

cacheSolve <- function(x, ...) {
    # get inverse attribute of x 
    i <- x$getinverse()
    
    #check if getinverse() return null 
    #if null means the inverse has not been cache into i variable of the matrix
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    
    # retrieve the actual matrix value of x
    data <- x$get()
    # calculate the inverse of matrix data
    i <- solve(data, ...)
    # store the inverse to x for the next call
    x$setinverse(i)
    # return the inverse
    i
        ## Return a matrix that is the inverse of 'x'
}
