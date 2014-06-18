## Matrix inversion is usually a costly computation and their may be some benefit to caching the inverse of a matrix rather than compute it repeatedly (there are also alternatives to matrix inversion that we will not discuss here). My assignment is to write a pair of functions that cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


#Call  the makeChcheMatrix() function and assign it's
#  return value ( a list of four functions) to a variable, v
#  v is now a list of four functions
v<-makeCacheMatrix()

#use v's set function to create a matrix 
x<-matrix(c(1,2,3,2,3,4,3,4,6),ncol=3,nrow=3)
v$set(x)

#use v's get function to retrieve the matrix created 
v$get()

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
        ## Return a matrix that is the inverse of 'x'
}

#pass the list v to the cacheSolve() function
#   the inverse of the matrix should be returned
cacheSolve(v)

#pass the list v to the cacheSolve() function a second time
#  the inverse of the matrix should be returned
#  also a message "getting cached data" indicating that the inverse
#  is not being calculated this time but is being retrieved from the cached
#  value
cacheSolve(v)

#use v's set function to create a new matrix y 
y<-matrix(c(1,3,5,2,3,4,7,6,9),ncol=3,nrow=3)
v$set(y)

#use v's get function to retrieve the matrix created 
v$get()



#pass the list v to the cacheSolve() function
#   the inverse of the matrix y should be returned
cacheSolve(v)

#pass the list v to the cacheSolve() function a second time
#  the inverse of the matrix should be returned
#  also a message "getting cached data" indicating that the inverse
#  is not being calculated this time but is being retrieved from the cached
#  value
cacheSolve(v)




