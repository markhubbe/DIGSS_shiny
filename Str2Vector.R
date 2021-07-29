string2vector<-function(string,numeric=TRUE){
  
  str_vector<-strsplit(string,"")[[1]]
  
  commas<-which(str_vector==",")
  
  result<-NULL
  
  if(numeric==TRUE){
    if(length(commas)==0){
      result[1]<-as.numeric(string)
    }else{
      index=1
      for(a in 1:length(commas)){
        colon=FALSE
        if(a==1){
          tmp_str<-substr(string,1,commas[a]-1)
          
          if(length(which(str_vector[1:(commas[a]-1)]==":")>0)){
            colon=TRUE
            colon_pos<-which(str_vector[1:(commas[a]-1)]==":")
          }
        }else{
          tmp_str<-substr(string,commas[a-1]+1,commas[a]-1)
          
          if(length(which(str_vector[(commas[a-1]+1):(commas[a]-1)]==":")>0)){
            colon=TRUE
            colon_pos<-which(str_vector[(commas[a-1]+1):(commas[a]-1)]==":")
          }
        }
        
        if(colon==TRUE){
          min_val<-as.numeric(substr(tmp_str,1,colon_pos-1))
          max_val<-as.numeric(substr(tmp_str,colon_pos+1,nchar(tmp_str)))
          for(b in min_val:max_val){
            result[index]<-b
            index<-index+1
          }
        }else{
          result[index]<-as.numeric(tmp_str)
          index<-index+1
        }
        
        
      }
      
      #the final bit, after the last comma
      colon=FALSE
      tmp_str<-substr(string,commas[a]+1,nchar(string))
      if(length(which(str_vector[(commas[a]+1):length(str_vector)]==":")>0)){
        colon=TRUE
        colon_pos<-which(str_vector[(commas[a]+1):length(str_vector)]==":")
      }
      
      if(colon==TRUE){
        min_val<-as.numeric(substr(tmp_str,1,colon_pos-1))
        max_val<-as.numeric(substr(tmp_str,colon_pos+1,nchar(tmp_str)))
        for(b in min_val:max_val){
          result[index]<-b
          index<-index+1
        }
      }else{
        result[index]<-as.numeric(tmp_str)
        index<-index+1
      }
      
      
    }
  }else{
    if(length(commas)==0){
      result[1]<-string
    }else{
      index=1
      for(a in 1:length(commas)){
        
        if(a==1){
          result[index]<-substr(string,1,commas[a]-1)
          
        }else{
          offset=0
          if(str_vector[commas[a-1]+1]==" "){
            offset=1
          }
          result[index]<-substr(string,commas[a-1]+1+offset,commas[a]-1)
        }
        index <- index+1
      }
      
      #last part
      offset=0
      if(str_vector[commas[a]+1]==" "){
        offset=1
      }
      result[index]<-substr(string,commas[a]+1+offset,nchar(string))
      
    }
  }
  
  return(result)
  
}