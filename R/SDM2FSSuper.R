#' @title 2-Span Slope Deflection Method(Fixed-Support) with Multiple Load
#'
#' @description This function computes the moments and the rotations using slope deflection method(SDM).
#'
#' @param InputMatrix The matrix with parameter values for SDM2FS().
#'
#' @export
#' @details Given the span length, magnitude of load, type of load, point of action, Young's Modulus, and interia, the function computes the moments and the rotations using SDM.
#'
#' @note
#' The possible values for Typeof1 and Typeof2 are UDL/PL/TDLS/TDLE.\cr
#' UDL:Uniformly Distributed Load \cr
#' PL:Point Load\cr
#' TDLS:Triangular Distributed Load(the maximum value is at the start, see http://www.abzwater.com/sdm)\cr
#' TDLE:Triangular Distributed Load(the maximum value is at the end, see http://www.abzwater.com/sdm )\cr
#'
#' @return  Moments and Rotations
#'
#' @author \strong{Sivarajah Mylevaganam}
#'
#' @examples
#' Multiple load on a span (http://www.abzwater.com/sdm/mload)
#' inputMatrix=matrix(c(3,2,"UDL","PL",20,40,1,1,1,1,1,1,3,2,"PL","PL",40,0,1,1,1,1,1,1), nrow=2, byrow=TRUE)
#' SDM2FSSuper(inputMatrix)
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
#' \link{SDM2FF}
#' \link{SDM2FS}
#' \link{SDM2FSC}
#'
#'
SDM2FSSuper=function(InputMatrix){
  FEMABG=0
  FEMBAG=0
  FEMBCG=0
  FEMCBG=0
  MABG=0
  MBAG=0
  MBCG=0
  MCBG=0
  ThetaBG=0
  ThetaCG=0


  for(i in 1:dim(InputMatrix)[1]){
    x=SDM2FS2(as.numeric(InputMatrix[i,1]),
             as.numeric(InputMatrix[i,2]),
             InputMatrix[i,3],
             InputMatrix[i,4],
             as.numeric(InputMatrix[i,5]),
             as.numeric(InputMatrix[i,6]),
             as.numeric(InputMatrix[i,7]),
             as.numeric(InputMatrix[i,8]),
             as.numeric(InputMatrix[i,9]),
             as.numeric(InputMatrix[i,10]),
             as.numeric(InputMatrix[i,11]),
             as.numeric(InputMatrix[i,12]))

    FEMABG=FEMABG+x[1]
    FEMBAG=FEMBAG+x[2]
    FEMBCG=FEMBCG+x[3]
    FEMCBG=FEMCBG+x[4]
    MABG=MABG+x[5]
    MBAG=MBAG+x[6]
    MBCG=MBCG+x[7]
    MCBG=MCBG+x[8]
    ThetaBG=ThetaBG+x[9]
    ThetaCG=ThetaCG+x[10]
  }


  print(c("Fixed End Moment AB",FEMABG))
  print(c("Fixed End Moment BA",FEMBAG))
  print(c("Fixed End Moment BC",FEMBCG))
  print(c("Fixed End Moment CB",FEMCBG))
  print(c("Moment AB",MABG))
  print(c("Moment BA",MBAG))
  print(c("Moment BC",MBCG))
  print(c("Moment CB",MCBG))
  print(c("Theta B",ThetaBG))
  print(c("Theta B",ThetaCG))

}


SDM2FS2=function(LengthAB,
                LengthBC,
                Typeof1,
                Typeof2,
                Magnitude1,
                Magnitude2,
                Pointof1,
                Pointof2,
                EAB,EBC,IAB,IBC
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


  matrix1=matrix(c(4*EAB*IAB/LengthAB+4*EBC*IBC/LengthBC,
                   2*EBC*IBC/LengthBC,
                   2*EBC*IBC/LengthBC,
                   4*EBC*IBC/LengthBC),
                 nrow=2, byrow=TRUE)
  matrix1
  matrix2=matrix(c(-FEMBA-FEMBC,-FEMCB), nrow=2, byrow=TRUE)
  matrix2
  result=solve(matrix1,matrix2)
  result

  ThetaB=result[1]
  ThetaC=result[2]


  MAB=2*EAB*IAB/LengthAB*(2*ThetaA+ThetaB)+FEMAB
  MBA=2*EAB*IAB/LengthAB*(2*ThetaB+ThetaA)+FEMBA
  MBC=2*EBC*IBC/LengthBC*(2*ThetaB+ThetaC)+FEMBC
  MCB=2*EBC*IBC/LengthBC*(2*ThetaC+ThetaB)+FEMCB

  allMatrix=c(FEMAB,FEMBA, FEMBC, FEMCB, MAB, MBA, MBC, MCB, ThetaB,ThetaC )
  return(allMatrix)

}




