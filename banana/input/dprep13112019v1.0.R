# Discretization using the Holte's 1R method

unor <-
  function(a,binsize,out=c("symb","num"))
  {#discretizacion 1R. It has only one restriction that does not
    #allow a discretization in more than 20 intervals.
    #This can be fixed by considering the maximum number 
    # of intervals as a parameterof the function. 
    b=a[order(a[,1]),]
    n=dim(a)[1]
    cumcut=c(1,binsize)
    mayor=moda(b[cumcut[1]:cumcut[2],2])
    j=binsize
    while(j<n){j=j+1;if(b[j,2]!=mayor)break}
    j1=j-1
    #    print(j1)
    while(j1<n){j1=j1+1;if(b[j1,1]!=b[j1-1,1])break}
    cumcut[2]=j1-1
    #   print(cumcut)
    for(jj in 2:20)
    {maxclass= moda(b[(cumcut[jj]+1):(cumcut[jj]+binsize),2])[1]
    mayor=c(mayor,maxclass)
    ind=cumcut[jj]+binsize
    while(ind<n){ind=ind+1;if(b[ind,2]!=maxclass)break}
    tempo=ind-1
    while(tempo<n){tempo=tempo+1;if(b[tempo,1]!=b[tempo-1,1])break}
    cumcut=c(cumcut,tempo-1)
    if(tempo>n)break
    }
    #   print(cumcut)
    nm=length(mayor)
    #merging
    finalcut=cumcut[((1:(nm-1))[abs(diff(mayor))>=1])+1] 
    a1=as.vector(a[,1])
    cutpoints=b[finalcut,1]
    cutpoints=c(-Inf,cutpoints,Inf)
    if(out=="num")
      a1=cut(a1,cutpoints,labels=F)
    else{a1=cut(a1,cutpoints)}
    return(a1)
  }



disconeR = function (data,convar,binsize=15,out=c("symb","num")) 
  {#data=as.matrix(data)
    if(sum(is.na(data))> 0) 
      stop("This dataset has missing values, impute them before running this function.\n",call.=FALSE)
    p=dim(data)[2]
    n=dim(data)[1]
    data1=data
    data1[,p]=as.numeric(factor(data1[,p]))
    #print(data1)
    i=1
    for (i in convar)
    {    a=data1[,c(i,p)]
    
    #b=a[order(a[,1]),]
    data1[,i]=unor(a,binsize,out)
    data[,1]=data[,1]}
    as.data.frame(cbind(data1[,-p],data[,p]))
    dataoneR=as.data.frame(cbind(data1[,-p],data[,p]))
    return(dataoneR)
  }
moda <-
  structure(function (x, na.rm = TRUE) 
  {
    if (na.rm == TRUE) 
      m1 = rev(sort(table(x[])))
    else m1 = rev(sort(table(x, exclude = NULL)))
    moda = names(m1[m1 == m1[1]])
    if (is(x, "numeric")) 
      moda = as.numeric(moda)
    return(moda)
  }, source = c("function(x,na.rm=TRUE)", "{", "  ", "#Function that finds the mode of vector x", 
                "", "  if(na.rm==TRUE) m1=rev(sort(table(x[])))", "    else m1=rev(sort(table(x,exclude=NULL)))", 
                "  moda=names(m1[m1==m1[1]])", "  if (is(x,\"numeric\")) moda=as.numeric(moda)", 
                "  return(moda)", "}"))




# Discretization using the equal width method
discew = function (data, varcon, out=c("symb","num")) 
{
#Fixed by Edgar Acuna, September 2015
#It handles discretization into integer numbers and intervals 
    # if (sum(is.na(data))> 0) 
    #     stop("This dataset has missing values, impute them before running this function.\n",call.=FALSE)
    p <- dim(data)[2]
    f <- p - 1
    ft <- rep(0, f)
#    for (i in 1:length(varcon)) {
#        ft[varcon[i]] <- 1
#    }
ft[varcon]=1
if(out=="symb"){
    for (i in 1:f) {
        if (ft[i] > 0) {
            grupos <- nclass.scott(data[, i])
            a=min(data[,i])
            b=max(data[,i])
            w=(b-a)/grupos
            cutpoints=seq(a,b,w)
            cutpoints[1]=-Inf
            cutpoints[grupos+1]=Inf
            data[, i] <- as.vector(cut(data[, i], cutpoints))
            data[, i]= as.factor(data[, i])
        }
    }
}
# else{
#  for (i in 1:f) {
#         if (ft[i] > 0) {
#             grupos <- nclass.scott(data[, i])
#             data[, i] <- as.vector(cut(data[, i], grupos,labels=FALSE))
#         }
#     }
# }
dataew=data
return(dataew)
}


disc2 = function (x, k,out=c("symb","num")) 
{
  n = length(x)
  ciclo = ceiling(n/k)
  y = x
  if(out=="num")
  {for (i in 1:(k - 1)) {
    y[order(x)[((i - 1) * ciclo + 1):(i * ciclo)]] = i
  }
    y[order(x)[((k - 1) * ciclo + 1):n]] = k
    return(y)
  }
  else{
    cutpoints=(1:k-1)*ciclo
    cutpoints=cutpoints[-1]
    cutpoints=c(-Inf,.5*(y[order(x)[cutpoints]]+y[order(x)[cutpoints+1]]),Inf)
    y=cut(y,cutpoints)
    return(y)
  }
}

# Discretization using the method of equal frequencies
discef= function (data, varcon, k,out=c("symb","num")) 
{
  #Fixed by Esgar Acuna, September  2015
  #It handles numerical and symbolic discretization
  p <- dim(data)[2]
  n=dim(data)[1]
  f <- p - 1
  ft <- rep(0, f)
  #    for (i in 1:length(varcon)) {
  #       ft[varcon[i]] = 1
  #  }
  ft[varcon]=1
  for (i in 1:f) {
    a=table(data[,i])
    ciclo=ceiling(n/k)
    if(length(a[a>ciclo])>0){ft[i]=0}
    if (ft[i] > 0) {
      data[, i] <- disc2(as.vector(data[, i]), k,out)
    }
  }
  tempo=length(ft[ft>0])
  cat("\n","variables to be discretized:",which(ft==1),"\n") 
  data
}
# Discretization Using The Minimum Entropy Criterion 
# Discretization using the entropy with Minimum Description Length.
# discmentr =function (data, varcon, out=c("symb","num")) 
# {#Discretization using entropy with MDL as stopping criteria
#   #Fixed by Edgar Acuna, November 2015
#   n <- dim(data)[1]
#   p <- dim(data)[2]
#   data1 <- data
#   for (j in varcon) {
#     var <- varcon[j]
#     sal <- discretevar(data, var, n, p)
#     nparti <- sal[1]
#     points <- sal[-1]
#     if(out=="num"){
#       if(nparti>1)
#       {data1[, j] <- as.vector(cut(data[,j],nparti,labels=FALSE))}
#       else{data1[,j]=1}}
#     else{
#       if(nparti>1)
#       {limites=c(-Inf,points,Inf)
#       data1[, j] <- as.vector(cut(data[,j],limites))}
#       else{data1[,j]="All"}}
#   }
#   return(data1)
# }
# discretevar=function (data, var, n, p) 
#   {
#     idx <- sort(data[, var], index.return = TRUE)
#     x <- idx$x
#     cx <- data[idx$ix, p]
#     midpoint <- midpoints1(x)
#     pts=rep(0,n)
#     disc <- .C("discrete", as.double(x), as.integer(cx), as.double(midpoint), 
#                as.integer(n), puntos = as.double(pts), npart = integer(1),PACKAGE="dprep")
#     np <- disc$npart
#     cat("The number of partitions for var", var, "  is :", np, 
#         "\n")
#     cat("The cut points are: ")
#     points <- sort(disc$puntos[1:(disc$npart - 1)])
#     print(points)
#     y <- c(np, points)
# }
# library(graphics)
# midpoints1=function (x) {
#     n <- length(x)
#     mid=rep(0,n)
#     points <- .C(.Points, as.double(x), as.integer(n), mpoint = as.double(mid),PACKAGE="graphics")
#     points$mpoint
#   }
