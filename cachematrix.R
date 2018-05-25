## Use Matrix as input then 1) set value, 2) get value, 3) set inverse, 4) get inverse
## Matrix can cache its own inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv_Matrix = NULL
        set = function(Z) {
                x <<- Z
                inv_Matrix <<- NULL
        }
        get = function() x
        setinv = function(inverse) inv_Matrix <<- inverse
        getinv = function() inv_Matrix
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## it computes the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv_Matrix = x$getinv()
        if(!is.null(inv_Matrix)) {
                message(" getting cached data ")
                return(inv_Matrix)
        }
        data = x$get()
        inv_Matrix = solve(data, ...)
        x$setinv(inv_Matrix)
        inv_Matrix
}