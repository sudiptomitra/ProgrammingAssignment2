## Put comments here that give an overall description of what your
## functions do
## > source("cachematrix.R")    load R program
## > a <- makeCacheMatrix()     create functions
## > a$set(matrix(1:4, 2, 2))   create matrix in working environment
## > cacheSolve(a)              1st run returns inverted matrix
##                              from working environment
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
##
## > cacheSolve(a)              2nd and subsequent runs
##                              returns inverted matrix from cache
## getting cached data          
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5


## Write a short comment describing this function
## This fucntion caches a value 
## makeCacheMatrix creates and returns a list of functions
## used by cacheSolve to get or set the inverted matrix in cache

makeCacheMatrix <- function(x = matrix()) {
        # stores the cached value
        # initialize to NULL
        cache <- NULL
        
        # create the matrix in the working environment
        set <- function(y) {
                x <<- y
                cache <<- NULL
        }

        # get the value of the matrix but not the inverse 
        get <- function() x
        # invert the matrix and store in cache
        setMatrix <- function(inverse) cache <<- inverse
        # get the inverted matrix from cache
        getInverse <- function() cache
        
        
        # return the created functions to the working environment
        list(set = set, get = get,
             setMatrix = setMatrix,
             getInverse = getInverse)
}

        


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## attempt to get the inverse of the matrix stored in cache
        cache <- x$getInverse()
        
        # return inverted matrix from cache if it exists
        # else create the matrix in working environment
        if (!is.null(cache)) {
                message("getting cached data")
                
                # display matrix in console
                return(cache)
        }
        
        # create matrix since it does not exist
        matrix <- x$get()
        
        ## Creating the inverse and assigning to cache
        cache <- solve(matrix, ...)
        
        # set inverted matrix in cache
        x$setMatrix(cache)
        return(cache)
}
