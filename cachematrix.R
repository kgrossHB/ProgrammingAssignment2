# The following functions are created to either return a cached
# inverse of a matrix or (if the inverse hasn't already been computed)
# to compute the inverse and cache it, so that it can be returned
# again later


##################################
# STEP 1: makeCacheMatrix-function
##################################

# create a function that creates a list of four functions, we can
# later use to either get or set the value of the inverse matrix

# because we run this main function (makeCacheMatrix) only once,
# we can draw on the environment created by running the function,
# without removing the objects inside of this environment
# --> this way, we can use the environment to cache values
# (if the function would run again, the environment would be newly
# created and therefor empty)

makeCacheMatrix <- function(x = matrix()) {
      
      # by running this function a new environment is created which
      # I will call the "make-environment"
      
      
      # create empty object "inv" inside the "make-environment", which
      # will later contain the inverse of the matrix
      inv <- NULL
      
      
      # by running a function inside our main function, again a new
      # environment is created; but because "<<-" is used instead of
      # "<-" the here defined objects will be stored in the
      # "make-environment" instead of the new environment
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      
      
      # create a function to get the object x (a matrix)
      get <- function() x
      
      
      # create a function that assigns the inverse to "inv"
      # to save the inverse inside of the "make-environment" the "<<-"
      # operator is used;
      # this way the inverse gets cached inside of the "make-environment",
      # which is stable because we don't run the main function again;
      # would it be saved inside of the environment of this new "setinv"-
      # function, the inverse would be removed and overwritten every time
      # we run the "setinv"-function
      setinv <- function(inverse) inv <<- inverse
      
      
      # create a function, that gets the value of "inv" (the inverse)
      getinv <- function() inv
      
      
      # return a list of four elements - the four created functions
      list(set = set, get = get,
           setinv = setinv,
           getinv = getinv)
      
}


#############################
# STEP 2: cacheSolve-function
#############################


# this function uses the functions created above, to either get the
# already cached inverse or calculate it and get it afterwards

cacheSolve <- function(x, ...) {
      
      # use the "getinv"-function (x$getinv) to assign the existing
      # (inside of the make-environment) value of "inv" to "inv"
      inv <- x$getinv()
      
      # if there now is a valid value assigned to inv, the inverse
      # already got computed and was cached (inside the
      # make-environment); return this cached inverse
      if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
      }
      
      # if there is no value assigned to "inv", get the value of the
      # matrix using the above created "get"-function and assign it to
      # the object "data"
      data <- x$get()
      
      # compute the inverse of the matrix now stored in "data" and
      # assign it to the object "inv"
      inv <- solve(data, ...)
      
      # before returning the inverse, set the inverse in the
      # make-environment to the just computed inverse using the
      # "setinv"-function; now the inverse is cached and you won't have
      # to compute it again
      x$setinv(inv)
      
      # return the inverse stored in "inv"
      inv
      
}
