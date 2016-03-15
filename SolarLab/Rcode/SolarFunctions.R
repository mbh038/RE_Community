# Solar Functions
################################################################################
# declination angle
deltaOdot<-function(t){
  # t is the time in day number 1-365
  #t<-t*365
  asin(sin(-0.4091)*cos((2*pi/365.24)*(t+10)+0.0334*sin((2*pi/365.24)*(t-2))))
}

#sunrise and sunset hour angle
h0<-function(phi,t){
  #t is day number
  acos(tan(phi)*tan(deltaOdot(t)))
}

#cosine of zenith angle
cosTheta<-function(phi,delta,h){
  # theta is the zenith angle
  sin(phi)*sin(delta)+cos(phi)*cos(delta)*cos(h)
}

# solar flux
solarFlux<-function(S0,phi,t){
  # S0 is solar constant
  # phi is latitude in radians
  # t is day of year
  
  # hour angle -pi to pi
  h<-pi*(2*(t-floor(t))-1)
  # declination angle
  delta<-deltaOdot(t)
  flux<-S0*cosTheta(phi,delta,h)
  # make it zero at nighttime (when cos theta is negative)
  flux<-pmax(flux,rep(0,length(t)))
  flux
}