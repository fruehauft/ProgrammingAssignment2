#Creating empty m matrix that is going to store the inversion calculation once it's calculated for the first time.
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        #Both x and m are defined in a different environment, which is why we use "<<-".
        x <<- y
        m <<- NULL
    }
    #Setting the inverse of matrix x and getting the inverse of that matrix -matrix m.
    get <- function() x
    #Function that will store the inverse of x in matrix m.
    setinverse <- function(inverse) m <<- inverse
    #Function that will retrieve information stored in matrix m.
    getinverse <- function() m
    list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}

#Checking if the matrix m is populated with the inverse matrix x.
cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    #If matrix m is empty, then proceed to the next step. If it's NOT empty, then get data stored in matrix m -inverse of x.
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    #This step is only executed if matrix m is empty. If it's empty, then the inverse of matrix x is calculated using "solve(x)".
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
