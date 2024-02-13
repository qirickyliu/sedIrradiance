# 4 functions
# calculate.intecounts: average 10 scans at same depth and calculate integration
# calculate.irradiance: calculate intecounts at every depth and output profiles
# linear.fit: plot linear fit and output estimated light attenuation coefficient
# nonlinear.fit: plot nonlinear fit and output estimated light attenuation coefficient

calculate.intecounts <- function(Start, End, Data)
{
  depth <- data.frame(Data[Start:End])
  Data$mean <- apply(depth, 1, mean)
  #average the 10 scans at this depth
  f <- approxfun(x=Data[[1]], y=Data$mean)
  #define a function "f" to fit the average spectrum
  intecounts <- integrate(f, lower = 400, upper = 700)
  #integration 400-700nm wavelength as counts per second at this depth*
  return(intecounts)
}



calculate.irradiance <- function(imax, data)
{
  i=2
  irradiance <- c()
  while (i<=imax){
    intecounts <- calculate.intecounts(Start = i, End = i+9, Data = data)
    irradiance <- c(irradiance, intecounts$value)
    i=i+10
  }
  #use a while loop to calculate irradiance at different depth and generate "irradiance"
  return(irradiance)
  #use names function to assign the output variable name
}



linear.fit <- function(depth, irradiance)
{
  l <- which.min(diff(irradiance))
  #use diff function to locate where the exp function starts automatically rather than a while loop*
  depth_fit <- depth[l:length(depth)]
  #only use the part that change exponentially, abandon others
  irradiance_fit <- log(irradiance[l:length(depth)])
  #use the same part with depth, and do a logarithmic transformation
  fitlinear <- lm(irradiance_fit~depth_fit)
  summary(fitlinear)
  k <- -coef(fitlinear)[2]
  plot(depth, log(irradiance), main = paste("k=", round(k,5)*1e+06, "/m",
                                            sep =   ""))
  lines(0:depth_fit[length(depth_fit)],
        predict(fitlinear, newdata =
                  data.frame(depth_fit = 0:depth_fit[length(depth_fit)])))
}



nonlinear.fit <- function(depth, irradiance)
{
  l <- which.min(diff(irradiance))
  depth_fit <- depth[l:length(depth)]-depth[l]
  #only use the part that change exponentially, abandon others
  irradiance_fit <- irradiance[l:length(depth)]
  #use the same part with depth, don't do logarithmic transformation
  fitnonlinear <- nls(irradiance_fit~a*exp(-k*depth_fit),
                      start = c(a=max(irradiance_fit), k=0.003))
  summary(fitnonlinear)
  k <- coef(fitnonlinear)[2]
  plot(depth, irradiance, main = paste("k=", round(k,5)*1e+06, "/m",
                                       sep = ""))
  pars <- as.list(coef(fitnonlinear))
  with(pars, curve(a*exp(-k*(x-depth[l])), add=TRUE, lwd=2))
}




