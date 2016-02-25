## The makeCacheMatrix function creates a special "matrix",
## which is really a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
inv <- NULL
set <- function(y){
            x <<-y
            inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse 
        getinv <- function() inv
        list(set=set, get=get, 
        setinv=setinv, 
        getinv=getinv)
}


## This function calculates the inverse of a matrix
## Because inverse calculation of a matrix can be time consuming
## So the function will first check if the inverse has been calculated before
## If so, the function will return the cached inverse without calculating again
## If not, the function will calculate the inverse of the matrix and store the result

cacheSolve <- function(x, ...) {
inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setinv(inv)
        inv
}
