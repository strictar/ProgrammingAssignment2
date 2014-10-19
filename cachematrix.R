?matrix
?solve

#
#makeCacheMatrix creates a matrix containing the values of a quadratic matrix with the created values and its inversions (with set and get)
#cachematrix generates the inversion of the CacheMatrix, but firts it checks 
# if there is no previous calculation of the asked matrix
#if it was not calculated it calculates and saves the inverse in the cache
#but if the inverse was already generated before it just returns the previous values (only if the matrix was not changed) 
#função setmatrix

makeCacheMatrix <- function (x=matrix ()){
  m <-NULL
  set<- function (y) {
    x<<-y
    m<<-NULL
  }
  get <- function ()x
  setmatrix <- function (solve) m<<-solve
  getmatrix <- function () m
  list(set=set, get=get, setmatrix=setmatrix, getmatrix=getmatrix)
}

cachematrix <- function (x=matrix) {
  m <- x&getmatrix ()
  if (!is.null (m)){
    message ("getting cached data")
    return (m)
  }
  data <- x$get ()
  m<- solve (matrix)
  x$setmatrix (m)
  m
}
