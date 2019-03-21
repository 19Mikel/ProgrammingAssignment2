## The two functions in this program will compute and cache the inverse of a matrix.
## It also returns a matrix that is inversed.

## This function creates special "matrix" object that can cache it's inverse

makeCacheMatrix <- function(x = matrix()) {
	  inv = NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function()inv
  list(set=set,get=get,setinv=setinv,getinv=getinv)

}


## This function computes the inverse of the "matrix" returned by makeCacheMatrix function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("retrieve inversed cached data")
    return(inv)
  }
  data <- x$get()
  inv = solve(data)
  x$setInverse(inv)
  inv
}
