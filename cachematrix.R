##These little functions have the role to store a matrix and its inverse
##and check whether the inverse has been calculated or not. 


## makeCacheMatrix: returns a list containing functions to set and get 
## a matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
    inverse_mtr <- NULL
    set <- function(y){
        x <<- y
        inverse_mtr = NULL
    }
    get <- function() x
    setInverse <- function(solve) reverse_mtr <<- solve
    getInverse <- function() reverse_mtr
    list(set = set, get = get, setInverse = setInverse, 
         getInverse = getInverse)
}


## cacheSolve: check whether the inverse matrix has been calculated. 
## If so, it gets the inverse from cache, otherwise the inverse will 
## be calculated and stored

cacheSolve <- function(x, ...) {
       inverse <- x$getInverse()
       if(is.null(inverse)){
           normal <- x$get()
           inverse <- solve(normal)
           x$setInverse(inverse)
       }
       x$getInverse()
}
