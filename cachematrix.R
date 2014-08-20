makeVector <- function(x = numeric()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmean <- function(mean) m <<- mean
        getmean <- function() m
        list(set = set, get = get,
             setmean = setmean,
             getmean = getmean)
}
cachemean <- function(x, ...) {
        m <- x$getmean()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- mean(data, ...)
        x$setmean(m)
        m
}
makeCacheMatrix<- function(x = matrix()) {
        ##in order to understand what this thing does, i will speak of functions' environments
        ##in terms of levels. So the "home environment" is level 1.
        ##Environment within the object, created by this function, for example,
        ##using x<-makeCacheMatrix(), is level 2.
        
        ##set() inputs the matrix  from lvl 1 to lvl 2.
        ##get() outputs the matrix from lvl 2 to lvl 1.
        ##setinv()  inputs the inverse from lvl 1 to lvl 2.
        ##getinv() outputs the inverse from lvl 2 to lvl 1.
        
        
        ##creates an object for computing inverse and storing it in cache
        inv <- NULL                        ##inv is the cache, in which inverse matrix is stored
        set <- function(y) {               ##writes down a new matrix, and cleares the inverse
                x <<- y
                inv <<- NULL               ##when the matrix is renewed, the inverse should be recomputed
        }
        get <- function() x                ##returns the matrix currently under disposal
        setinv <- function(inverse){
                inv <<- inverse            ##overwrites the inverse matrix cache
        }
        getinv <- function() inv           ##returns the inverse matrix currently in cache
        list(set = set, 
             get = get,
             setinv=setinv,
             getinv=getinv)
}
cacheSolve <- function(x, ...) {##X is the object created by makeCacheMatrix
        ## Return a matrix that is the inverse of 'X'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
