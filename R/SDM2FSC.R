#' @title 2-Span Slope Deflection Method(Fixed-Support-Cantilever)
#'
#' @description This function computes the moments and the rotations using slope deflection method(SDM).
#'
#' @param LengthAB The length of span AB
#' @param LengthBC The length of span BC
#' @param Typeof1 The type of load(i.e., UDL/PL/TDLS/TDLE) on span AB
#' @param Typeof2 The type of load(i.e., UDL/PL/TDLS/TDLE) on span BC
#' @param Magnitude1 The magnitude of the load on span AB
#' @param Magnitude2 The magnitude of the load on span BC
#' @param Pointof1 The point at  which the load is applied on span AB
#' @param Pointof2 The point at  which the load is applied on span BC
#' @param EAB Young's Modulus(E) of span AB
#' @param EBC Young's Modulus(E) of span BC
#' @param IAB Inertia(second moment of area, I) of span AB
#' @param IBC Inertia(second moment of area, I) of span BC
#' @param CM Moment value
#' @param CMomentDirection Moment direction
#' @export
#' @details Given the span length, magnitude of load, type of load, point of action, Young's Modulus, and interia, the function computes the moments and the rotations using slope deflection method(SDM).
#'
#' @section Note
#' The possible values for Typeof1 and Typeof2 are UDL/PL/TDLS/TDLE.\cr
#' The possible value for CMomentDirection is "C" if clock-wise.\cr
#' UDL:Uniformly Distributed Load \cr
#' PL:Point Load\cr
#' TDLS:Triangular Distributed Load(the maximum value is at the start, see http://www.abzwater.com/sdm)\cr
#' TDLE:Triangular Distributed Load(the maximum value is at the end, see http://www.abzwater.com/sdm )\cr
#' @return  Moments and Rotations
#'
#' @author \strong{Sivarajah Mylevaganam}
#'
#' @examples
#' Single load on a span (http://www.abzwater.com/sdm)
#' SDM2FSC(3,2,"UDL","PL",20,40,1,1,1,1,1,1,10,"C")
#' SDM2FSC(4,8,"PL","PL",0,40,1,4,1,1,1,1,5,"C")
#' SDM2FSC(2,2,"PL","UDL",40,20,1,1,1,1,1,1,40,"C")
#'
#'
#' @keywords Slope
#' @keywords Deflection
#' @keywords Structural
#' @keywords Civil
#' @keywords Engineering
#' @keywords Indeterminate
#'
#' @references (1) Wikipedia contributors. (2018, February 19). Slope deflection method. In Wikipedia, The Free Encyclopedia. Retrieved 09:17, March 26, 2018, from https://en.wikipedia.org/w/index.php?title=Slope_deflection_method&oldid=826540985
#' \cr
#' \cr
#' (2) Norris, Charles Head; John Benson Wilbur; Senol Utku (1976). Elementary Structural Analysis (3rd ed.). McGraw-Hill. pp. 313–326. ISBN 0-07-047256-4.
#' \cr
#' \cr
#' (3) McCormac, Jack C.; Nelson, James K. Jr. (1997). Structural Analysis: A Classical and Matrix Approach (2nd ed.). Addison-Wesley. pp. 430–451. ISBN 0-673-99753-7.
#' \cr
#' \cr
#'
#' @seealso
#' \link{SDM2FS}
#' \link{SDM2FF}
#'
#'


SDM2FSC=function(LengthAB,
                LengthBC,
                Typeof1,
                Typeof2,
                Magnitude1,
                Magnitude2,
                Pointof1,
                Pointof2,
                EAB,EBC,IAB,IBC,
                CM,
                CMomentDirection
) {
  FEMAB=0
  FEMBA=0
  FEMBC=0
  FEMCB=0
  ThetaA=0
  ThetaB=0
  ThetaC=0

  if(Typeof1=="UDL")
  {
    FEMAB=-1*Magnitude1*(LengthAB^2)/12
    FEMBA=1*Magnitude1*(LengthAB^2)/12
  }else if(Typeof1=="PL"){
    FEMAB=-1*Magnitude1*Pointof1*((LengthAB-Pointof1)^2)/(LengthAB^2)
    FEMBA=1*Magnitude1*(LengthAB-Pointof1)*(Pointof1^2)/(LengthAB^2)
  }else if(Typeof1=="TDLS")
  {
    FEMAB=-1*Magnitude1*(LengthAB^2)/20
    FEMBA=1*Magnitude1*(LengthAB^2)/30
  }else if(Typeof1=="TDLE")
  {
    FEMAB=-1*Magnitude1*(LengthAB^2)/30
    FEMBA=1*Magnitude1*(LengthAB^2)/20
  }



  if(Typeof2=="UDL")
  {
    FEMBC=-1*Magnitude2*(LengthBC^2)/12
    FEMCB=1*Magnitude2*(LengthBC^2)/12
  }else if(Typeof2=="PL"){
    FEMBC=-1*Magnitude2*Pointof2*((LengthBC-Pointof2)^2)/(LengthBC^2)
    FEMCB=1*Magnitude2*(LengthBC-Pointof2)*(Pointof2^2)/(LengthBC^2)

  }else if(Typeof2=="TDLS")
  {
    FEMBC=-1*Magnitude2*(LengthBC^2)/20
    FEMCB=1*Magnitude2*(LengthBC^2)/30
  }else if(Typeof2=="TDLE")
  {
    FEMBC=-1*Magnitude2*(LengthBC^2)/30
    FEMCB=1*Magnitude2*(LengthBC^2)/20
  }

  CanAddition=0

  if(CMomentDirection=="C"){
    CanAddition=CM
  }else{
    CanAddition=-CM
  }

  matrix1=matrix(c(4*EAB*IAB/LengthAB+4*EBC*IBC/LengthBC,
                   2*EBC*IBC/LengthBC,
                   2*EBC*IBC/LengthBC,
                   4*EBC*IBC/LengthBC),
                 nrow=2, byrow=TRUE)
  matrix1
  matrix2=matrix(c(-FEMBA-FEMBC,-FEMCB+CanAddition), nrow=2, byrow=TRUE)
  matrix2
  result=solve(matrix1,matrix2)
  result

  ThetaB=result[1]
  ThetaC=result[2]


  MAB=2*EAB*IAB/LengthAB*(2*ThetaA+ThetaB)+FEMAB
  MBA=2*EAB*IAB/LengthAB*(2*ThetaB+ThetaA)+FEMBA
  MBC=2*EBC*IBC/LengthBC*(2*ThetaB+ThetaC)+FEMBC
  MCB=2*EBC*IBC/LengthBC*(2*ThetaC+ThetaB)+FEMCB


  print(c("Fixed End Moment AB",FEMAB))
  print(c("Fixed End Moment BA",FEMBA))
  print(c("Fixed End Moment BC",FEMBC))
  print(c("Fixed End Moment CB",FEMCB))
  print(c("Moment AB",MAB))
  print(c("Moment BA",MBA))
  print(c("Moment BC",MBC))
  print(c("Moment CB",MCB))
  print(c("Theta B",ThetaB))
  print(c("Theta C",ThetaC))
}

#Movement of Canti load not the point of interest
#C if clock-wise
