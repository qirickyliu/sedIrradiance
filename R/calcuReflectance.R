# 1 function
# calculate reflectance at every depth


calculate.reflectance <- function(Data)
{
  i=2
  reflectance <- Data[1]
  while (i<=(ncol(Data)-9)){
    depth <- data.frame(Data[i:(i+9)])
    Data$mean <- rowMeans(depth)
    #average the 10 scans at this depth
    meanvalue <- as.data.frame(Data$mean)
    reflectance <- cbind(reflectance, meanvalue)
    i=i+10
  }
  #use while loop to average the spectrum data
  for (i in 2:(ncol(reflectance)-1)){
    reflectance[i] <- reflectance[i]/reflectance[ncol(reflectance)]
    reflectance[i][which(reflectance[i]<0),] <- NA
    reflectance[i][which(reflectance[i]>1),] <- NA
    #only use reflectance values that between 0 and 1, ignore the others*
  }
  #divide the white reference
  reflectance <- reflectance[,-ncol(reflectance)]
  #abandon the white reference column
  matplot(reflectance[1], reflectance[,-1], type="l", xlab = "wavelength",
          ylab   = "reflectance")
  logreflectance <- data.frame(reflectance[1], log10(reflectance[,-1]))
  #logarithmic transformation*
  matplot(logreflectance[1], logreflectance[,-1], type="l",
          xlab = "wavelength", ylab   = "logreflectance")
  return(list(reflectance, logreflectance))
}



