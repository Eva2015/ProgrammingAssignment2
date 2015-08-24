## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates a list containing a function to
## set and get value of the matrix
## set and get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inver<-NULL
  set<-function(y) {
    x<<-y
    inver<<-NULL
  }
  get<-function()x
  setinverse<-function(inverse)inver<<-inverse
  getinverse<-function()inver
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## This function returns the inverse of the matrix
## first check the inverse has been computed,
## then get the result and skip
## If not, it computes the inverse
## and set the value in the cache

cacheSolve <- function(x, ...) {
  inver<-x$getinverse()
  if(!is.null(inver)) {
    messge("getting cache data")
    return(inver)
  }
  data<-x$get()
  inver<-solve(data)
  x$setinverse(inver)
  inver
        ## Return a matrix that is the inverse of 'x'
}
