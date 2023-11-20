#!/usr/bin/env Rscript
library(coro)
library(reticulate)
library(jsonlite)

n = 10000

read_input <-generator(function(stdin){

  prev_key=NULL
  df <-data.frame()
  while(length(buffer <- readLines(stdin, n=n))>0) {
    # read the input data to some amount and convert it to a df
    values_list= lapply(buffer,function(x){strsplit(x,"\t",fixed=T)})
    buffered_df <- data.frame(matrix(unlist(values_list), nrow=length(values_list), byrow=TRUE))
    # get the unique keys in the df (sorted as the map sent them)
    keys = sort(unique(buffered_df[1])$X1)
    # set the last processed key if it is null
    if(is.null(prev_key)){prev_key<-keys[1]}
    # check the different cases
    if ((length(keys)>1) && (prev_key %in% keys)){
      # The df contains more than 1 key including previous key
      # get the remaining of the prev_key information and yeld all df
      prev_df <- buffered_df[buffered_df[1]==prev_key,]
      df<- rbind(df, prev_df)
      key = unique(df[1])[1]
      values = df[2]
      yield(list(key=key, values=values))
      # if we have other complete keys in the df, send them

      intermediate_keys = keys[-1]
      intermediate_keys = intermediate_keys[-length(intermediate_keys)]
      for(small_key in intermediate_keys){
        small_df <- buffered_df[buffered_df[1]==small_key,]
        key = unique(small_df[1])[1]
        values = small_df[2]
        yield(list(key=key, values=values))
      }

      # the last key might be incomplete and come in the next buffer
      prev_key <- keys[length(keys)]
      df <- buffered_df[buffered_df[1]==prev_key,]
    }else if ((length(keys)>1) && !(prev_key %in% keys)){
      # The df contains more than 1 key but not the previous key
      # yeld the previous information
      key = unique(df[1])[1]
      values = df[2]
      yield(list(key=key, values=values))
      # if we have other complete keys in the df, send them
      intermediate_keys = keys[-length(keys)]
      for(small_key in intermediate_keys){
        small_df <- buffered_df[buffered_df[1]==small_key,]
        key = unique(small_df[1])[1]
        values = small_df[2]
        yield(list(key=key, values=values))
      }

      # the last key might be incomplete and come in the next buffer
      prev_key <- keys[length(keys)]
      df <- buffered_df[buffered_df[1]==prev_key,]
    }else if ((length(keys)==1) && (prev_key == keys[1])){
      # the df only contains one key and is the previous one
      # just apped the info in the df
      df<- rbind(df, buffered_df)
    }else if ((length(keys)==1) && !(prev_key == keys[1])){
      # the df only contains one key but is not the previous one
      # yeld the previous information
      key = unique(df[1])[1]
      values = df[2]
      yield(list(key=key, values=values))
      # get the buffer and it might be incomplete and come in the next buffer
      prev_key = keys[1]
      df <- buffered_df

    }else{
      # This should never happen
      stop("Unhandled case")
    }
  }
  # we reached here after the loop, and the df might contain information of the last processed key
  if(!is.null(prev_key)){
    # if the prev_key is null, we never got data. if not, yield the remaining information
    key = unique(df[1])[1]
    values = df[2]
    yield(list(key=key, values=values))
  }
})


