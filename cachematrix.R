makeCacheMatrix <- function(x = matrix()){
    invMatrix <- NULL
    set <- function(y){
        x <<- y
        invMatrix <<- NULL
    }
    get <- function() x
    setInv <- function(inv) invMatrix <<- inv
    getInv <- function() invMatrix
    list(set = set, get = get, setInv = setInv, getInv = getInv)
}

cacheSolve <- function(x, ...){
    
    invMatrix <- x$getInv()
    if (!is.null(invMatrix)){
        message("getting cached data")
        return(invMatrix)
    }
    data <- x$get()
    invMatrix <- solve(data, ...)
    x$setInv(invMatrix)
    invMatrix
}