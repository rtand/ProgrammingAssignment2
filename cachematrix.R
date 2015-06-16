## makeCacheMatrtx stores a matrix and the associated inverse
## $get returns the original matrix
## $set sets the matrix to the new input
## $setinverse sets the inverse matrix to the input, no validation is done
## $getinverse returns the contents of the cached inverse



makeCacheMatrix <- function(x = matrix()) 
{
	m <- NULL
  	set <- function(y) 
  	{
    	x <<- y
    	m <<- NULL
  	}
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


##cacheSolve attempts to find the inverse of a given matrix and 
##load it back to the makeCacheMatrix 
##A matrix must be square and nonsingular

cacheSolve <- function(x, ...) 
{
    ## Return a matrix that is the inverse of 'x'
	m <- x$getinverse()
  	if(!is.null(m)) 
  	{
    	message("getting cached data")
    	return(m)
  	}
  	data <- x$get()
  	m <- solve(data, ...)
  	x$setinverse(m)
  	m
}
