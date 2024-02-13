# two functions, "read.irradiance" is to read all the
# spectrum data of txt files in a folder and merge them into a metafile;
# "read.irradiance.smooth", except for above, increases function to smooth the
#spectrum, normalization according to integration time, and minus the substrate
# part (wavelengths greater than 800nm)


#' Read sediment irradiance raw data
#'
#' Read all the txt files in the folder and merge them into a data frame
#'
#' @param File The folder that you save your txt files
#'
#' @return A dataframe named merge.data
#' @export
read.irradiance <- function(File="")
{
  a <- list.files(path = paste("./", File, sep = ""))
  #show all the txt files in "File"
  a.sort <- gtools::mixedsort(a, decreasing = F)#sort files according to names
  dir <- paste("./",File,"/",a.sort,sep="") #build path variable dir
  n <- length(dir)  #number of files under folder "File"
  merge.data <- read.table(file = dir[1], fill = TRUE, skip = 14, sep = "")
  #import the first txt file
  for (i in 2:n){
    new.data <- read.table(file = dir[i], fill = TRUE, skip = 14, sep = "")
    New.data <- new.data[ ,-1]
    merge.data <- cbind(merge.data,New.data)
  } #build a circle to read all the txt files from the second one and merge them
  return(merge.data)
}



read.irradiance.smooth <- function(File="")
{
  a <- list.files(path = paste("./", File, sep = ""))
  #show all the txt files in "File"
  a.sort <- gtools::mixedsort(a, decreasing = F)#sort files according to names
  dir <- paste("./",File,"/",a.sort,sep="") #build path variable dir
  n <- length(dir)  #number of files under folder "File"
  merge.data <- read.table(file = dir[1], fill = TRUE, skip = 14, sep = "")
  #import the first txt file
  merge.data.spl <- smooth.spline(merge.data[[1]],merge.data[[-1]])
  #use smooth.spline to smooth the spectrum data
  df1 <- as.data.frame(merge.data.spl$x)
  df2 <- as.data.frame(merge.data.spl$y)
  #change list to data.frame
  integrationtime <- read.table(file = dir[1], fill = TRUE, skip = 6, nrows = 1,
                                sep   = "")
  #read the integration time automatically*
  merge.data <- cbind(df1, df2/integrationtime[[4]])
  #divide integration time, combine these two data.frame into one

  for (i in 2:n){
    new.data <- read.table(file = dir[i], fill = TRUE, skip = 14, sep = "")
    new.data.spl <- smooth.spline(new.data[[1]], new.data[[-1]])
    df1 <- as.data.frame(new.data.spl$x)
    df2 <- as.data.frame(new.data.spl$y)
    new.data <- cbind(df1, df2)
    New.data <- new.data[ ,-1]
    integrationtime <- read.table(file = dir[i], fill = TRUE, skip = 6,
                                  nrows = 1, sep = "")
    merge.data <- cbind(merge.data,New.data/integrationtime[[4]])
  }
  #build a circle to read all the txt files from the second one and merge them

  row <- which(merge.data[1]>800)
  #find the indices that wavelength greater than 800 nm*
  for (i in 2:(n+1)){
    merge.data[i] <- merge.data[i]-mean(merge.data[row[1]:row[length(row)],i])
  }
  #substrate the part of greater than 800 nm wavelength*
  return(merge.data)
}




