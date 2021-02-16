## cache the inverse of the matrix

## creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x <<- y                   
        inv <<- NULL             
    }
    get <- function(){x}          
    setinv <- function(inverse){
        inv <<- inverse           
    }
    getinv <- function(){inv}    
    list(set = set,
         get = get,
         getinv = getinv,
         setinv = setinv)
}


## computes the inverse of the special "matrix" returned by makeCacheMatrix 
## above. If the inverse has already been calculated (and the matrix has not 
## changed), then the cacheSolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
    inv <- x$getinv()              
    if (!is.null(inv)){          
        message('the inverse has already been calculated')
        return(inv)
    }
    matrix <- x$get()
    inv <- solve(matrix)          
    x$setinv(inv)               
    inv                         
}
