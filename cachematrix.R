## The file contain two functions which can cache the inverse of a matrix.
## 

## Function makeCacheMatrix can make a special matrix which contain the values of matrix and inverse of that matrix.
## Typical usage of the functionis as following example:
## 1. creat or get some matrix: a <- matrix(1:4, nrow = 2, ncol = 2)
## 2. Make a dummy special matrix: aMatrix <- makeCacheMatrix()
## 3. Initialized the special matrix: aMatrix$set(a)

makeCacheMatrix <- function(x = matrix()) {
  #Set the cached inverse varible "inv" to NULL
  inv <- NULL 
  # This function initilizes the "Special" Matrix
  set <- function(y){
    x <<- y
    inv <<- NULL 
  }
  # This function return the matrix itself
  get <- function() x
  # This function store the calculated inverse
  setinv <- function(inverse) inv <<- inverse
  # This function retriev the cached inverse
  getinv <- function() inv
  # Return this "special" matrix
  list(set = set, get = get, setinv = setinv, getinv = getinv)
  

}


## This function retrive the inverse of the special matrix x depending on 
## if there is value stored in the cached varible 

cacheSolve <- function(x, ...) {
  
  # Get the stored value from the special matrix.
  inv <- x$getinv()
  # The value is not NULL indicating that the value have already calculated. 
  if (!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  # The value is NULL, calculate it and store it in the special matrix
  data <- x$get()
  inv <- solve(data)
  x$setinv(inv)
  inv
        ## Return a matrix that is the inverse of 'x'
}
