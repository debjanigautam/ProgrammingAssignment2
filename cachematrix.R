## Objective: Caching the inverse of matrix
## We have to create functions named "makeCacheMatrix" and "cacheSolve" which can work as follows:
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
## Author: Debjani Ghosh
## makeCacheMatrix creates a special "matrix" object, which is really a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
 inverseOfMatrix <- NULL
        setTheValueOfVector <- function(y){
                x <<- y
                inverseOfMatrix <- NULL
        }
        getTheValueOfVector <- function() x
        setTheValueOfInverse <- function(inverse) inverseOfMatrix <<- inverse
        getTheValueOfInverse <- function() inverseOfMatrix
        list(set=setTheValueOfVector, get=getTheValueOfVector, setInverse=setTheValueOfInverse, getInverse=getTheValueOfInverse)

}
## The following function calculates the inverse of the matrix created with the above function. 
## However, it first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data and sets the value of the mean in the cache via the setTheValueOfInverse function.


cacheSolve <- function(x, ...) {
       inverse <- x$getInverse()
        if(!is.null(inverse)){
                message("Cached data")
                return(inverse)
        }
        else{
                message("No Cache in the first run but inverse is:")
                data <- x$get()
                inverse <- solve(data)
                x$setInverse(inverse)
                return(inverse)
        }          
}
