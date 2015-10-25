## R Programming - Programming Assignment 2
## In this programming assignment, the scoping rules of the R language and how they can be
## manipulated to preserve state inside of an R object is introduced. The operator <<- is used to assign 
## a value to an object in an environment that is different from the current environment. It is very
## useful when doing a costly computation like the matrix inversion in this assignment. Instead of
## recalculating, we will just cache the value with the help of <<- operator.

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## makeCacheMatrix creates a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
        # Initially set the inv to NULL
        inv <- NULL
        
        #function to set the value of matrix and clear the old inverse from the cache
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        # Get value of the matrix
        get <- function() x
        
        # Set the inverse
        setinverse <- function(inverse) inv <<- inverse
        
        # Get the inverse
        getinverse <- function() inv
        
        # Creates the list with the four functions above
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}



## cacheSolve: This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated and the matrix has 
## not changed, then cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        # Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        
        # Return the inverse if it is already calculated
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        
        # If the inverse is not calculated yet, calculate it, cache it and return it
        data <- x$get()             # get the value of matrix
        inv <- solve(data, ...)     # calculate the inverse
        x$setinverse(inv)           # cache the result
        inv                         # return the inverse
}


## Usage example:
## x <- matrix(1:4, nrow=2, ncol=2)
## m <- makeCacheMatrix(x)
## s <- cacheSolve(m)
