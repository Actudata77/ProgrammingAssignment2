## This pair of function will allow us to create an object that is 
## capable of storing a given input of matrix data and caches its 
## inverse, which will be benficial because matrix inversion is 
## very costly computation wise.


## This first function is going to create a special "matrix" 
## object that is able of caching the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()){
    inv <- NULL
    set <- function(y){
      x <<- y
      inv <<- NULL
  }
    
    get <- function()x
    setInverse <- function(Inverse) inv <<- Inverse
      getInverse <- function() inv
      list(set = set, get = get, setInverse = setInverse,
      getInverse = getInverse)

}



## In turn, this second function is going to compute the inverse
## of the given matrix below stored under makeCacheMatrix, using 
## the fucntion cacheSolve. When testing this function if the 
## inverse of a matrix that is already computed is asked for again, 
## it will beretrieved from the cache, hence the message " Getting cached data".

cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
      if(!is.null(inv)){
        message("Getting cached data")
        return(inv)
    }
    
    matrice <- x$get()
  inv <- solve(matrice, ...)
  x$setInverse(inv)
  inv
       
}  
  

