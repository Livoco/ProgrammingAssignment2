## 2018-08-14 by Livoco
## This pair of functions aim at caching the inverse of a matrix instead of
## computing it repeatedly.

## Examples
## a <- matrix(c(2, 0, 0, 2), ncol = 2)
## my_cm <- makeCacheMatrix(a)
## my_cm$get()  
## my_cm$getInv()
## inverse <- cacheSolve(my_cm) #solve the inverse for the first time
## my_cm$getInv()
## inverse <- cacheSolve(my_cm) #return a cached value after first time# 


## Attention:
## The use of object$set(inverse) is error prone.
## If `inverse` is assigned to NULL, the object behaves as a newly-made object,
## i.e. the inverse should be calculated again, even the matrix is not changed.
## If `inverse` is assigned to a non-NULL but wrong value, the object behaves 
## as nothing happens, even the content of the cached inverse matrix has been
## changed.
## Be sure to use object$set(inverse = cacheSolve(object))
## 
## To confirm the inverse is correct, you can do my_cm$get() %*% my_cm$getInv()
## An identity matrix which is of the same dimensions as 'x' is expected.

## This function creates a special "matrix" object what can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        ## `x` is an object of matrix type.
        
        ## Return a list of functions of the CacheMatrix object can use.
        ## 'set(y)':    set matrix `y` as the new value of a created 
        ##              CacheMatirx object;
        ## 'get()':     get the value of a created CacheMatirx object;
        ## 'setInv(inverse)':  set the value of the inverse of matrix x to be 
        ##                      `inverse`
        ## 'getInv():   get the`inverse` value of the inverse of matrix x
        ## ====================================================================
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function() {
                x
        }
        setInv <- function(inverse) {
                inv <<- inverse
        }
        getInv <- function() {
                inv
        }
        list(set = set,
             get = get,
             setInv = setInv,
             getInv = getInv)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInv()
        if(is.null(inv)) {
                inv <- solve(x$get())
                x$setInv(inv)
                return(inv)
        }
        else {
                message("getting cached data")
                return(x$getInv())
        }
}