## This pair of function will cache the inverse of a matrix.


## This function creates a special "matrix" object that can cahche its inverse.
## 1. set the matrix
## 2. get the matrix
## 3. set the inverse of the matrix
## 4. get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function (y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) s <<- solve
        getsolve <- function() s
        list(set=set, get=get, 
             setsolve=setsolve, 
             getsolve=getsolve)
}



## This function computes the inverse of the special "matrix" returned by makeCacheMatrix. 
## It will first check whether the inverse of the special "matrix" has been calculated. 
## If so, cacheSolve will retrieve the inverse from the cache. 
## Otherwise, it will calculate the inverse and set the value to the cache.

cacheSolve <- function(x, ...) {
        ## first check whether the inverse has been calculated already
        s<- x$getsolve()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }    
        ## If so, retrieve the inverse from the cache. 
        
        data<-x$get()
        s<-solve(data,...)
        x$setsolve(s)  
        ## If not,  calculate the inverse and set the value to the cache
        s                    
        ## Return a matrix that is the inverse of 'x'
}