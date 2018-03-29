# CEngineering
Tools/Functions for Civil and Water Resources Engineering


<b><h1>How to Install</h1></b>

devtools::install_github("sivarajahm/CEngineering", force=TRUE)

<b><h1>Video</h1></b>
<a href="http://www.abzwater.com/sdm/video">http://www.abzwater.com/sdm/video</a>

<b><h1>Usage</h1></b>
<b>SDM2FF()</b><br/>
CEngineering::SDM2FF(5,10,"PL","UDL",0,5,1,1,1,1,1,1)<br/>
<b>SDM2FS()</b><br/>
CEngineering::SDM2FS(3,2,"UDL","PL",20,40,1,1,1,1,1,1)<br/>
<b>SDM2FSC()</b><br/>
CEngineering::SDM2FSC(3,2,"UDL","PL",20,40,1,1,1,1,1,1,10,"C")

<b>SDM2FFSuper()</b><br/>
inputMatrix=matrix(c(3,2,"PL","PL",40,40,1,1,1,1,1,1,3,2,"PL","PL",40,0,2,1,1,1,1,1,3,2,"PL","UDL",0,5,1,1,1,1,1,1), nrow=3, byrow=TRUE)
<br/>
CEngineering::SDM2FFSuper(inputMatrix)






