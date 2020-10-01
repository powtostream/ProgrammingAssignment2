## These are two functions that are used to create a special object that stores
## a matrix and cache's its inverse.

## This function creates a special "matrix" object that checks if the given
## matrix is invertible and can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        if (det(x)!=0 && ncol(x)==nrow(x)){
                inv <- NULL
                set <- function(y){
                        if (det(y)!=0 && ncol(y)==nrow(y)){
                                x <<- y
                                inv <<- NULL
                        }
                        else {
                                message("The matrix is not invertible")
                        }
                }
                get <- function() x
                setinverse <- function(inverse) inv <<- inverse
                getinverse <- function() inv
                list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
        }
        else{
                message("The matrix is not invertible")
        }
}

## This function computes the inverse of the special "matrix" or takes it from
## cache if it was already calculated
## The argument of this function can only be one returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if (!is.null(inv)){
                message("getting cashed data")
                return(inv)
        }
        else{
                data <- x$get()
                inv <- solve(data)
                x$setinverse(inv)
                inv
        }
        ## Return a matrix that is the inverse of 'x'
}
