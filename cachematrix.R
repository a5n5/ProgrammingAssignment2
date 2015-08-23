## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL ##sets inv to NULL
  set<-function(y){
    x<<-y
    inv<<-NULL
  } ##set function . Will reset value of X to Y and inv to NULL
  get<-function() x
  setinverse<-function(inverse) inv<<-inverse ##takes value of inverse and assigns to setinv
  getinverse<-function()inv ## displays the value of inverrse
  list(set=set,get=get, setinverse=setinverse, getinverse=getinverse) ##makecachematrix is set as a list of 4 functions
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  inv<-x$getinverse()
  if(!is.null(inv)){
    message("getting cached data.")
    return(inv)
  }
  data<-x$get()
  inv<-solve(data)
  x$setinverse(inv)
  inv ## Return a matrix that is the inverse of 'x'
}

