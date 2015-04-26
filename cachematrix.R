# this function will cache the value of the matrix inverse 
# next time you will need it, will be looked up in the cache rather than recomputed

# makeCacheMatrix creates a special matrix, a list of 4 functions, 2 sets and 2 gets

makeCacheMatrix <- function(x=matrix()) {
  j <- NULL
  set <- function(y){
    x <<-y
    j <<-NULL
  }
  get <- function () x
  setinv <- function(inv) j <<- inv
  getinv <- function() j
  list(set=set,get=get,setinv=setinv,getinv=getinv)
}

# this function calculates the inverse matrix of the "special" matrix and sets the inverse into cache 

cacheSolve <- function(x,...){
  j<-x$getinv()
  if(!is.null(j)){
    message("getting cached matrix inverse")
    return(j)
  }
  data <- x$get()
  j <- solve(data,...)
  x$setinv(j)
  j
}
