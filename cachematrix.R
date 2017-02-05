#makeCacheMatrix creates cached matrix object for inverse computation
#cacheSolve checks if inverse exists, if it does not then calculate

#this function creates a "matrix" object that can compute its inverse
makeCacheMatrix <- function(x = matrix()) {
  i<- NULL
  set<- function(y){
    x<<-y
    i<<- NULL
  }
  get<- function() x
  setinverse<- function(solve) i<<-solve
  getinverse<- function() i
  list(set = set, get=get, setinverse=setinverse,getinverse=getinverse)
}


#computes inverse of matrix , if inverse caluclated will retrieve inverse from cache
cacheSolve <- function(x, ...) {
  i<-x$getinverse()
  if(!is.null(i)){
      message("getting cached data")
      return(i)
  }
  data<- x$get()
  i<- solve(data,...)
  x$setinverse(i)
  i
}
