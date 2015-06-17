read_rrewll = function(fpath, distcol = "dist", surfdistcol = "surfdist", quiet = FALSE){
# read water level data from Matt's Matlab file
# input data is typically formatted as a Matlab structure with multiple 
# equal-length data columns and an optional note.
# This function outputs a dataframe with named columns and adds additional 
# elements (e.g. note) as attributes. 
  require(R.matlab)
  if(!quiet)
    message("Reading '", fpath, "'")  
  # read the file
  mobj = readMat(fpath)
  mobjn = names(mobj)
  if(!quiet)
    message("Found variables: ", paste(mobjn, collapse = ", "))
  ret = vector("list", length = length(mobjn))
  names(ret) = mobjn
  # loop through variables in file
  for(n in mobjn){
    if(!quiet)
      message("Extracting ", n, "...")
    obj = mobj[[n]][,1,1]
    it = sapply(obj, length)
    if(!quiet)
      message("...contains entries: ", paste(names(it), collapse = ", "))    
    # coerce to dataframe
    dlength = max(unique(it))
    dcols = names(it[it == dlength])
    if(!quiet)
      message("...data columns are: ", paste(dcols, collapse = ", "))      
    dlist = lapply(obj[dcols], as.vector)
    dobj = as.data.frame(dlist)
    # add attributes
    acols = names(obj[!(names(obj) %in% dcols)])
    if(!quiet)
      message("...attributes are: ", paste(acols, collapse = ", "))
    for(acol in acols)
      attr(dobj, acol) <- obj[[acol]]
    ret[[n]] = obj
  }
  attr(ret, "sourcefile") = fpath
  if(!quiet){
    message("Output list contains: ", paste("'", names(ret), "'", 
      sep = "", collapse = ", "))
    message("Attribute 'sourcefile' is ", '"', attr(ret, "sourcefile"), '"')
    message("DONE")
  }
  return(ret)  
}  

read_rrectd = function(fpath, quiet = FALSE){
# read CTD cast data from Matt's Matlab file
# input data is typically formatted as a Matlab structure with multiple 
# equal-length data columns and two columns 'surfdist' and 'surfelev' with
# surface information for each cast location. This function outputs a dataframe 
# with named columns and adds additional elements (e.g. note) as attributes. 
  require(R.matlab)
  if(!quiet)
  message("Reading '", fpath, "'")
  mobj = readMat(fpath)
  mobjn = names(mobj)
  if(!quiet)
    message("Found variables: ", paste(mobjn, collapse = ", "))
  ret = vector("list", length = length(mobjn))
  names(ret) = mobjn
  # loop through variables in file
  for(n in mobjn){
    if(!quiet)
      message("Extracting ", n, "...")
    obj = mobj[[n]][,1,1]
    it = sapply(obj, length)
    if(!quiet)
      message("...contains entries: ", paste(names(it), collapse = ", "))    
    # coerce to dataframe
    dlength = max(unique(it))
    dcols = names(it[it == dlength])
    if(!quiet)
      message("...data columns are: ", paste(dcols, collapse = ", "))      
    dlist = lapply(obj[dcols], as.vector)
    dobj = as.data.frame(dlist)
    # add location data
    llength = it[[surfcol]]
    lcols = names(it[it == llength])
    if(!quiet)
      message("...location columns are: ", paste(lcols, collapse = ", "))      
    lobj = lapply(obj[lcols], as.vector)
    dobj[lcols[lcols != surfcol]] = NA
    if(!quiet)
      message("...pairing: ", paste(distcol, surfdistcol, sep = " <=> "))      
    for(i in seq(length(lobj[[surfcol]]))){
      for(lcol in lcols[lcols != surfcol])
        dobj[which(dobj[[distcol]] == lobj[[surfcol]][i]), lcol] = lobj[[lcol]][i]      
    }
    if(all(names(it) %in% c(dcols, lcols)))
      message("...no additional attributes")
    else{
      acols = names(it)[!(names(it) %in% c(dcols, lcols))]
      if(!quiet)
        message("...attributes are: ", paste(acols, collapse = ", "))
      for(acol in acols)
        attr(dobj, acol) <- obj[[acol]]
    }
    ret[[n]] = dobj
  }
  attr(ret, "sourcefile") = fpath
  if(!quiet){
    message("Output list contains: ", paste("'", names(ret), "'", 
      sep = "", collapse = ", "))
    message("Attribute 'sourcefile' is ", '"', attr(ret, "sourcefile"), '"')
    message("DONE")
  }
  return(ret)  
}

