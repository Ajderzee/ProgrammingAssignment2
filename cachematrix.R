#below is the function to create special matrix object

makeCacheMatrix<-function(x,r){
  
  #x is the matrix that you want to create
  #r is the number of rows which is also equal to number of columns (square matrix)
  
  mat<-matrix(x,nrow=r,ncol=r)
  inverse<-NULL
  
  fset<-function(y,r){      #function to edit a created matrix
    
    mat<<-matrix(y,nrow=r,ncol=r)
    inverse<<-NULL
    
  }
  
  fget<-function() {        #function to get the matrix
    
    return (mat)
    
  }
  
  fgetinverse<-function(){    #function to get the inverse of the matrix,if stored in cache
    
    return (inverse)
    
  }
  
  fsetinverse<-function(i){     #function to store the inverse in cache
    
    inverse<<-i
    
  }
  
  list(setmatrix=fset,getmatrix=fget,getinverse=fgetinverse,setinverse=fsetinverse)
  
}



#below is the function to get the inverse

cacheSolve<-function(x){
  
  #x is the passed object created using function makeCacheMatrix
  
  inverse<-x$getinverse()
  
  if(!is.null(inverse)){
    
    print("Inverse already calculated and cached")
    return(inverse)
  }
  
  
  inverse<-solve(x$getmatrix())
  x$setinverse(inverse)
  
  print("Inverse calculated now and stored in cache for future use")
  x$getinverse()
  
  
}