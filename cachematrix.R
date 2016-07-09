## Put comments here that give an overall description of what your
## functions do
##
## Ibrahim Jusoh, 10/7/2016
## Cousera Data Scientist, Module 2 - R Programing, Week 3, Assignment 2
##
## Matrix inversion is usually a costly computation and 
## there may be some benefit to caching the inverse of a matrix rather than computing it repeatedly 
## Below are two functions that are used to create a special object that stores a numeric matrix and caches its inverse.
## 

## Write a short comment describing this function
##
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        # set list element of function makeCacheMatrix
        list(set = set,get = get,setinverse = setinverse,getinverse = getinverse)

}


## Write a short comment describing this function
##
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.
## Note : For this assignment, assume that the matrix supplied is always invertible

cacheSolve <- function(x, ...) {

        ## Return a matrix that is the inverse of 'x'
        
        ## Check inverse value is exist in cache or not. 
	i <- x$getinverse()

        ## If value exist in cache (not NULL) then display from cache
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }

        ## If not in cache then get the original matrix value and calculate its inverse matrix.
        data <- x$get()
        i <- solve(data, ...)    
        ## Store the calculated inverse matrix to cache and display it.
        x$setinverse(i)
        i
}
