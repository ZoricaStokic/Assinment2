# Assinment2
makeVector <- function(x = numeric()) {# makeVector creates a special vector
    m <- NULL
    set <- function(y) {    #set the value of the vector
        x <<- y
        m <<- NULL
    }
    get <- function() x     #get the value of the vector
    setmean <- function(mean) m <<- mean   #set the value of the mean
    getmean <- function() m     #get the value of the mean
    list(set = set, get = get,
        setmean = setmean,
        getmean = getmean)
}
 #The following function calculates the mean of the special "vector" created with the above function. However, it first checks to see if the mean has already been calculated. If so, it gets the mean from the cache and skips the computation. Otherwise, it calculates the mean of the data and sets the value of the mean in the cache via the setmean function.
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

#Assignment: Caching the Inverse of a Matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL      # sets the value of m to NULL-provides a default if cacheSolve has not yet been used.
    set <- function(y) { #set the value of the matrix.
        x <<- y       #caches the inputted matrix so that cacheSolve can check whether it has changed.
        inv <<- NULL   # sets the value of inv.
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)    #creates a list to house the four functions.
}
cacheSolve <- function(x, ...) {      # Compare matrix to what was there before.
    inv <- x$getinverse()    # if an inverse has already been calculated this gets it.
    if(!is.null(inv)) {     # getting cached data-check to see if cacheSolve has been run before.
        message
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)   # compute the value of the inverse of the input matrix.
    x$setinverse(inv)    # run the setinverse function on the inverse to cache the inverse.
    inv               # return the inverse.
}
