######################################################################################################################
### CT SEM - an R/OpenMx based program for estimating oscillating and nonoscillating processes in continuous time  ###
### Voelkle & Oud (January, 2012) 																				   ###
### OpenMx version: 1.2.0; R version: 2.13.0												           			   ###
### Direct any questions to voelkle@mpib-berlin.mpg.de															   ###
######################################################################################################################

continuous_time <- function(data, Tpoints, n.latent, DRIFT, CINT, G, D, n.manifest, LAMBDA, PHI1, THETA, latentM1, manifestM)
{

measurements	<- 1:(Tpoints-1)
colnames(data)[1:(Tpoints*n.manifest)] 					<- c(paste("V",1:(Tpoints*n.manifest),sep=""))
colnames(data)[((Tpoints*n.manifest)+1):ncol(data)] 	<- paste("I",1:(Tpoints-1),sep="")
require(OpenMx)		
			
#------------------------------------------------------------------------------------------#
# define the generic RAM matrices (A, S, F, M; cf. McArdle, 2005; McArdle & McDonald, 1984)
#------------------------------------------------------------------------------------------#
			
### 1. A matrix
lambda.differ	<- FALSE			
theta.differ	<- FALSE

Avalues 	<- matrix(0,nrow=(n.manifest*Tpoints+n.latent*Tpoints), ncol=(n.manifest*Tpoints+n.latent*Tpoints))
				for (i in seq(n.latent,n.latent*(Tpoints-1),n.latent))
					{
					Avalues[(1+i):(i+n.latent), ((1+i)-n.latent):i] <- NA
					}
				for (j in 1:Tpoints)
					{
					Avalues[((1+n.latent*Tpoints-n.manifest)+j*n.manifest):(n.latent*Tpoints+j*n.manifest), (j*n.latent-n.latent+1):(j*n.latent)] <- LAMBDA
					}
Afree 		<- ((Avalues != 0) & (Avalues != 1) & is.na(Avalues) == F)  
DRIFTfree 	<- ((DRIFT != 0) & (DRIFT != 1))
Alabels 	<- matrix(,nrow=(n.manifest*Tpoints+n.latent*Tpoints), ncol=(n.manifest*Tpoints+n.latent*Tpoints))
				for (i in 1:(Tpoints-1))
				{
				temp <- matrix((paste("EXPd", i,  sep = "")), nrow=n.latent, ncol=n.latent, byrow=TRUE)	
				BETA_labels <- temp 
				for (k in 1:n.latent)
					{
					for(j in 1:n.latent)
						{
						BETA_labels [k,j] = paste(BETA_labels[k,j], "[",k,",",j,"]",sep="") 
						}
					}
				Alabels[(1+i*n.latent):(n.latent+i*n.latent), (i*n.latent-n.latent+1):(i*n.latent)] <- BETA_labels
				}
				templambda <- matrix((paste("lambda", 1:(n.manifest*n.latent), sep = "")), nrow=n.manifest, ncol=n.latent, byrow=FALSE)
				for (l in 1:Tpoints)
				{
				if(lambda.differ) LAMBDA_labels <- paste(templambda, "_", l, sep="")
				else LAMBDA_labels <- templambda 
				Alabels[((1+n.latent*Tpoints-n.manifest)+l*n.manifest):(n.latent*Tpoints+l*n.manifest), (l*n.latent-n.latent+1):(l*n.latent)] <- LAMBDA_labels
				} 

### 2. S matrix
Svalues 	<- matrix(0,nrow=(n.manifest*Tpoints+n.latent*Tpoints), ncol=(n.manifest*Tpoints+n.latent*Tpoints))
				for(i in 1:Tpoints)
				{
				Svalues[(i*n.latent-n.latent+1):(i*n.latent), (i*n.latent-n.latent+1):(i*n.latent)] <- NA
				}
				Svalues[1:n.latent, 1:n.latent] <- PHI1	# first time point
				for(j in 1:Tpoints)
				{
				Svalues[((n.latent*Tpoints+1)+j*n.manifest-n.manifest):((n.latent*Tpoints)+j*n.manifest), ((n.latent*Tpoints+1)+j*n.manifest-n.manifest):((n.latent*Tpoints)+j*n.manifest)] <- THETA
				}
Sfree 		<- ((Svalues != 0) & (Svalues != 1) & is.na(Svalues) == F)
Gfree 		<- ((G != 0) & (G != 1))
Slabels 	<- matrix(,nrow=(n.manifest*Tpoints+n.latent*Tpoints), ncol=(n.manifest*Tpoints+n.latent*Tpoints))
				for (i in 1:Tpoints)
				{
				temp <- matrix((paste("Qd", i-1,  sep = "")), nrow=n.latent, ncol=n.latent, byrow=TRUE)
				PSI_labels <- temp 
				Qd.ind <- matrix(1:n.latent**2,n.latent,n.latent)
					for (k in 1:n.latent)
					{
						for(j in 1:n.latent)
						{
						PSI_labels [k,j] = paste(PSI_labels[k,j], "[",Qd.ind[k,j],",",1,"]",sep="") 
						}
					}
				Slabels[(i*n.latent-n.latent+1):(i*n.latent), (i*n.latent-n.latent+1):(i*n.latent)] <- PSI_labels
				}
				tempphi <- matrix(paste("phi",1:n.latent,matrix(1:n.latent,n.latent,n.latent,byrow=T),sep=""),n.latent,n.latent)
				for(i in 1:n.latent)
				{
					for(j in 1:n.latent)
					{
					if(Svalues[i,j]==Svalues[j,i]) tempphi[i,j]=tempphi[j,i]
					}
				}
				Slabels[1:n.latent, 1:n.latent] <- tempphi
				temptheta <- matrix(paste("theta",1:n.manifest,matrix(1:n.manifest,n.manifest,n.manifest,byrow=T),sep=""),n.manifest,n.manifest)
					for (j in 1:Tpoints)
					{
					if(theta.differ) THETA_labels<- paste(temptheta, "_", j, sep="")
					else THETA_labels <- temptheta 
					Slabels[((n.latent*Tpoints+1)+j*n.manifest-n.manifest):((n.latent*Tpoints)+j*n.manifest), ((n.latent*Tpoints+1)+j*n.manifest-n.manifest):((n.latent*Tpoints)+j*n.manifest)] <- THETA_labels
					}

### 3. F matrix
Fvalues 	<- cbind(matrix(0,nrow=(n.manifest*Tpoints), ncol = n.latent*Tpoints), diag(1,(n.manifest*Tpoints)))
Fnamesy 	<- c(c(paste("F", 1:(n.latent*Tpoints), sep="")), c(paste("V", 1:(n.manifest*Tpoints), sep="")))
Fnamesx		<- c(paste("V", 1:(n.manifest*Tpoints), sep=""))

### 4. M matrix
Mvalues 	<- matrix(,nrow=(n.manifest*Tpoints+n.latent*Tpoints), ncol=1)
				for (i in 1:Tpoints)
				{
				Mvalues[(i*n.latent-n.latent+1):(i*n.latent), 1] <- NA
				}
				Mvalues[1:n.latent, 1] <- latentM1
				for (j in 1:Tpoints)
				{
				Mvalues[((n.latent*Tpoints+1)+j*n.manifest-n.manifest):((n.latent*Tpoints)+j*n.manifest), 1] <- manifestM
				}
Mfree 		<- ((Mvalues != 0) & (Mvalues != 1) & is.na(Mvalues)==F)
CINTfree 	<- ((CINT!= 0) & (CINT!= 1))
Mlabels 	<- matrix(,nrow=(n.manifest*Tpoints+n.latent*Tpoints), ncol=1)
				for (i in 1:(Tpoints-1))
				{
				M_labels <- matrix((paste("intd", i, sep = "")), nrow=n.latent, ncol=1, byrow=TRUE)
					for(j in 1:n.latent)
					{
					M_labels [j,1] = paste(M_labels[j,1], "[",j,",",1,"]",sep="") 
					}
				Mlabels[(1+i*n.latent):(n.latent+i*n.latent), 1] <- M_labels
				}
				Mlabels[1:n.latent, 1] <- matrix((paste("m", 1:n.latent, sep = "")), nrow=n.latent, ncol=1, byrow=TRUE)	
					for (j in 1:Tpoints)
					{
					Mlabels[((1+n.latent*Tpoints-n.manifest)+j*n.manifest):(n.latent*Tpoints+j*n.manifest), 1] <- NA
					}
CINTlabels 	<- paste("cint",1:n.latent,sep="")
					
### Labels
INTERVALlabels 	<- paste("data.I",1:(Tpoints-1),sep="")
DRIFTlabels 	<- matrix(,n.latent,n.latent)
					for (i in 1:n.latent)
					{
						for (j in 1:n.latent)
						{
						DRIFTlabels[i,j] = paste("F",i,j,sep="")
						}			
					}
Glabels 		<- matrix(,n.latent,n.latent)
					for (i in 1:n.latent)
					{
						for (j in i:n.latent)
						{
						Glabels[j,i] = paste("g",i,j,sep="")
						}
					}	

#-----------------------------------------------------------------------------------------------------------#
# Define OpenMx Algebras for the continuous time drift matrix (A), intercept (INT), and error covariance (Q)
#-----------------------------------------------------------------------------------------------------------#

### drift matrix (A)
EXP_Function 	<- function(number) 
					{
					shortAlgString 	<- paste("(solve(II-(1/2)%x%DRIFT%x%(DEF[,", number, "]/", D, ")) %*%(II+(1/2)%x%DRIFT%x%(DEF[,", number, "]/", D, ")))", sep="")  	
					algName 		<- paste("EXPd", number, sep="")																									
					shortAlgName 	<- paste(algName, "Step1", sep="")																									
					alg1 			<- eval(substitute(mxAlgebra(theExpression, name=shortAlgName), list(theExpression = parse(text=shortAlgString)[[1]]))) 			
					algString 		<- paste(rep(shortAlgName, D), collapse=" %*% ")  									
					alg2 			<- eval(substitute(mxAlgebra(theExpression, name=algName), list(theExpression = parse(text=algString)[[1]])))						
					return(list(alg1, alg2))	
					}
	
### intercept (INT)
int_Function 	<- function(number2) 
					{
					shortAlgString1 <- paste("solve(DRIFT)%*%((", sep="") 		
					shortAlgString2 <- paste("(solve(II-(1/2)%x%DRIFT%x%(DEF[,", number2, "]/", D, ")) %*%(II+(1/2)%x%DRIFT%x%(DEF[,", number2, "]/", D, ")))", sep="")  
					shortAlgString3 <- paste(")-II)%*%t(CINT)", sep="") 		
					algName 		<- paste("intd", number2, sep="")																									
					shortAlgName 	<- paste(algName, "Step1", sep="")																									
					alg1 			<- eval(substitute(mxAlgebra(theExpression, name=shortAlgName), list(theExpression = parse(text=shortAlgString2)[[1]]))) 			
					algStringmid	<- paste(rep(shortAlgName, D), collapse=" %*% ")
					algString		<- paste(shortAlgString1, algStringmid, shortAlgString3, sep="")
					alg2 			<- eval(substitute(mxAlgebra(theExpression, name=algName), list(theExpression = parse(text=algString)[[1]])))						
					return(list(alg1, alg2))	
					}
	
### error covariance (Q)
Qd_Function 	<- function(number3) 
					{
					shortAlgString1 <- paste("solve(DRIFTHATCH)%*%((", sep="") 			
					shortAlgString2 <- paste("(solve(II%x%II-(1/2)%x%DRIFTHATCH%x%(DEF[,", number3, "]/", D, ")) %*%(II%x%II+(1/2)%x%DRIFTHATCH%x%(DEF[,", number3, "]/", D, ")))", sep="")  
					shortAlgString3 <- paste(")-(II%x%II))%*%rvectorize(Q)", sep="") 	
					algName 		<- paste("Qd", number3, sep="")																									
					shortAlgName 	<- paste(algName, "Step1", sep="")																									
					alg1 			<- eval(substitute(mxAlgebra(theExpression, name=shortAlgName), list(theExpression = parse(text=shortAlgString2)[[1]]))) 			
					algStringmid	<- paste(rep(shortAlgName, D), collapse=" %*% ")																			
					algString		<- paste(shortAlgString1, algStringmid, shortAlgString3, sep="")
					alg2 			<- eval(substitute(mxAlgebra(theExpression, name=algName), list(theExpression = parse(text=algString)[[1]])))						
					return(list(alg1, alg2))	
					}

#----------------------#
# Define Model (OpenMx)
#----------------------#

model=	mxModel("CT_SEM", type="RAM",
			mxData(observed=data, type="raw"),
			mxMatrix(type="Full", labels=DRIFTlabels, values=DRIFT, byrow=TRUE, free=DRIFTfree, name="DRIFT"), 
			mxMatrix(type="Full", labels=CINTlabels, values=CINT, free=CINTfree, name="CINT"),
			mxMatrix(type="Lower", labels=Glabels, values=G, byrow=TRUE, free=Gfree, name="G"),
			mxMatrix(type="Iden", nrow=n.latent, ncol=n.latent, free=FALSE, name="II"),
			mxMatrix(type="Full", nrow=1, ncol=(Tpoints-1), free=FALSE, labels=INTERVALlabels, name="DEF"),			
		mxMatrix(
			values=Avalues,
	  		free=Afree,
			labels=Alabels,
			name="A"),
		mxMatrix(
			values=Svalues,
	  		free=Sfree,
			labels=Slabels,
			name="S"),
		mxMatrix(
			values=Fvalues,
			free=FALSE,
			dimnames=list(Fnamesx, Fnamesy),
			name="F"),
		mxMatrix( 
			free=t(Mfree),
			values=t(Mvalues),
			labels=t(Mlabels),
			dimnames=list(1, Fnamesy),
			name="M"),

### apply the three functions defined above to the different time intervals

	### drift matrix (A)
	mxAlgebra(vec2diag(eigenval(DRIFT)), name = "EVA"),
	mxAlgebra(eigenvec(DRIFT), name = "EVEC"),
	mxMatrix("Full", values=(matrix(1,n.latent,n.latent)-diag(n.latent)), name = "tempa"),
	EXP_algebras <- mapply(EXP_Function, measurements),		

	### intercept (INT)
	int_algebras <- mapply(int_Function, measurements),	

	### error covariance (Q)
	mxAlgebra(DRIFT%x%II + II%x%DRIFT, name = "DRIFTHATCH"),
	mxAlgebra(G%*%t(G), name = "Q"),
#	mxAlgebra(vec2diag(eigenval(DRIFTHATCH)), name = "EVAH"),
#	mxAlgebra(eigenvec(DRIFTHATCH), name = "EVECH"),
	mxMatrix("Full", values=(matrix(1,n.latent**2,n.latent**2)-diag(n.latent**2)), name = "tempb"),
	Qd_algebras <- mapply(Qd_Function,measurements),			

### fitting function
mxRAMObjective("A","S","F","M")
)

#----------------------------#
# Run model & output results 
#----------------------------#

modelf		<- mxModel(model,EXP_algebras,int_algebras,Qd_algebras,
                           mxComputeSequence(list(
                               mxComputeGradientDescent(engine="NPSOL"),
                               mxComputeStandardError(),
                               mxComputeReportDeriv())))
 mxRun(modelf, silent=TRUE)
}
