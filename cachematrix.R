## Matrix inversion is usually a costly computation 
## and there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly
## makeCacheMatrix creates a list of function that:
# 1. set: set the value of the matrix
# 2. get: get the value of the matrix
# 3. setinverse: sets the value of the inverse of the matrix
# 4. getinverse: gets the value of the inverse of the matrix

##  This function creates a special "matrix" object that can cache its inverse.

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


##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  inv<-x$getinverse() ##gets the value of inverse in x
  if(!is.null(inv)){ 
    message("getting cached data.")
    return(inv)
  } #checks if not null then returns value
  data<-x$get() ##sets x to data
  inv<-solve(data) ##calculate inverse of data
  x$setinverse(inv) ## sets inverse to x$setinverse
  inv ## Return a matrix that is the inverse of 'x'
}

