#Custom functions for Case Study 2
#Last modified 11/26/17

#Wrapper function for gsub that maintains object classes
gsubkeep = function(pattern='',replacement=NA,x, ignore.case=FALSE, perl=FALSE,
                    fixed=FALSE, useBytes=FALSE) {
  if (is.null(dim(x)) && !is.null(class(x))) {
    colObj = x
    colClass = class(colObj)
    colObj = gsub(pattern=pattern,replacement=replacement,x=colObj,
                  ignore.case=ignore.case,perl=perl,fixed=fixed,
                  useBytes=useBytes)
    if (colClass == 'factor') {
      colObj = as.factor(colObj)
    } else { 
      colObj = as(colObj,colClass) 
    }
    x = colObj
  } else {
    for (i in 1:length(x)) {
      colObj = x[,i]
      colClass = class(colObj)
      colObj = gsub(pattern=pattern,replacement=replacement,x=colObj,
                    ignore.case=ignore.case,perl=perl,fixed=fixed,
                    useBytes=useBytes)
      if (colClass == 'factor') {
        colObj = as.factor(colObj)
      } else { 
        colObj = as(colObj,colClass) 
      }
      x[,i] = colObj
    }
  }
  return(x)
}

#Function that row-stacks the identified columns of a list of data frames
#Parameters: {x: list of data frames, elements: the particular data frames within x to stack, 
#             cols: the particular columns to stack, colname: names of the columns selected with cols parameter }
list_rowbind = function(x,elements=1:length(x),cols = columns(1:length(x[[1]])),
                        colname = colnames(x[[1]])[cols]) {
  result = data.frame()
  for (i in elements) {
    append_df = x[[i]][,cols]
    colnames(append_df) = colname
    result = bind_rows(result,append_df)
  }
  return(result)
}

#Function that creates a list of indices and corresponding strings which match keywords
#Paratermers: {x: a vector (to be coerced to character) to search, findtext: a vector of keywords to look for}
match_list = function(x,findtext='') {
  findtext = as.character(findtext)
  x = tolower(as.character(x))
  result = list(length(findtext))
  for (i in 1:length(findtext)) {
    result[i] = list(cbind('Idx'=grep(findtext[i],x),'Match'=x[grep(findtext[i],x)]))
  }
  return(result)
}