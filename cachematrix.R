makeCacheMatrix <- function(x = matrix()) {
  m<-null
  set<-function(y){
    x<<-y
    m<<-null
  }
  get <- function()x
  getinverse<-function(mean) m<<-solve(x)
  getinverse<-function()m
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
       
  
   ## Return a matrix that is the inverse of 'x'
  m<-x$getinverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data<-x$get()
  m<-solve(data,...)
  x$getinverse(m)
  m
  
}

