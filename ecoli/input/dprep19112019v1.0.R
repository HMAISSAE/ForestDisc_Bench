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


mod_chi2modif= function (data, alp = 0.5) 
{
  dat <- data
  p <- ncol(dat) - 1
  alpha <- alp
  d <- 0.01
  LevO <- LevCon(dat)
  Disc <- chiM(dat, alpha = alpha)$Disc.data
  LevN <- LevCon(Disc)
  e <- 0.1
  while ((LevN - LevO) < e) {
    if (alpha <= 0.2) 
      break
    alpha <- alpha - d
    Disc <- chiM(Disc, alpha = alpha)$Disc.data
    LevN <- LevCon(Disc)
    LevO <- LevN
  }
  options(digits = 3)
  eps <- 1e-01
  sig <- array(alpha, p)
  d <- 10
  cutp = list()
  for (i in 1:p) {
    LevO <- LevCon(Disc)
    while (TRUE) {
      val <- value(i, Disc, alpha = sig[i])
      Disc <- val$disc
      LevN <- LevCon(Disc)
      if (LevN < LevO || sig[i] <= eps) 
        break
      LevO <- LevN
      sig[i] <- sig[i]/d
    }
    cutp[[i]] <- val$cuts
  }
  return(list(cutp = cutp, Disc.data = Disc))
}

discretize_rgr_modif=function (input, target, min_perc_bins = 0.1, max_n_bins = 5) 
{
  fpoints = c()
  max_depth = 20
  target = as.character(target)
  min_n = round(min_perc_bins * length(input))
  all_cuts = recursive_gr_cuts_aux(input, target, fpoints, 
                                   max_depth, min_n)
  max_n_bins = max_n_bins - 1
  fpoints_top = all_cuts[1:min(max_n_bins, length(all_cuts))]
  if (is.null(fpoints_top)) 
  {
    fpoints_top_ord = fpoints_top
  }else 
  {
    fpoints_top_ord = fpoints_top[order(fpoints_top)]
  }
  
  input_bin = Hmisc::cut2(input, cuts = c(fpoints_top_ord,max(input)))
  return(input_bin)
}

recursive_gr_cuts_aux=function (input, target, fpoints, max_depth, min_n) 
{
  points = unique(quantile(input, probs = seq(0.2, 0.8, by = 0.2)))
  if (length(points) == 1 | length(fpoints) >= max_depth) 
    return(fpoints)
  r = c()
  for (p in points) {
    gr = binary_gain_ratio(input, target, test_points = p)
    total_left = sum(input < p)
    total_right = sum(input >= p)
    gr = if (total_left < min_n | total_right < min_n) 
      0
    else gr
    r = c(r, gr)
  }
  position_max = which(max(r) == r)[1]
  max_point = points[position_max]
  input_left = input[input < max_point]
  input_right = input[input >= max_point]
  target_left = target[input < max_point]
  target_right = target[input >= max_point]
  if (length(input_left) > min_n & length(input_right) > min_n & length(fpoints) <= max_depth) {
    fpoints = c(fpoints, max_point)
    fpoints_left = recursive_gr_cuts_aux(input = input_left, 
                                         target = target_left, fpoints, max_depth, min_n)
    fpoints_right = recursive_gr_cuts_aux(input = input_right, 
                                          target = target_right, fpoints, max_depth, min_n)
    fpoints = unique(c(fpoints_right, fpoints_left))
  }
  return(fpoints)
}

binary_gain_ratio=function (input, target, test_points) 
{
  input_bin = cut2(input, cuts = test_points)
  if (length(levels(input_bin)) == 1) 
    return(0)
  gr = gain_ratio(input_bin, target)
  return(gr)
}

mdlp_modif=function (data) 
{
  p <- length(data[1, ]) - 1
  y <- data[, (p + 1)]
  xd <- data
  cutp <- list()
  i=3
  for (i in 1:p) {
    x <- data[, i]
    cuts1 <- cutPoints_m(x, y)
    cuts <- c(min(x), cuts1, max(x))
    cutp[[i]] <- cuts1
    if (length(cutp[[i]]) == 0 | length(unique(x)) < 2 ) 
      cutp[[i]] <- "All"
    if (length(unique(x)) > 1 ) 
      xd[, i] <- as.integer(cut(x, unique(cuts), include.lowest = TRUE))
  } 
  return(list(cutp = cutp, Disc.data = xd))
}


cutPoints_m=function (x, y) 
{
  od <- order(x)
  xo <- x[od]
  yo <- y[od]
  depth <- 1
  gr <- function(low, upp, depth = depth) {
    x <- xo[low:upp]
    y <- yo[low:upp]
    n <- length(y)
    ct <- cutIndex(x, y)
    if (is.null(ct)) 
      return(NULL)
    ci <- ct[1]
    entropy <- ct[2]
    ret <- mdlStop_m(ci, y, entropy)
    if (is.null(ret)) 
      return(NULL)
    return(c(ci, depth + 1))
  }
  part <- function(low = 1, upp = length(xo), cutPoints = NULL, 
                   depth = depth) {
    x <- xo[low:upp]
    y <- yo[low:upp]
    n <- length(x)
    if (n < 2) 
      return(cutPoints)
    cc <- gr(low, upp, depth = depth)
    ci <- cc[1]
    depth <- cc[2]
    if (is.null(ci)) 
      return(cutPoints)
    cutPoints <- c(cutPoints, low + ci - 1)
    cutPoints <- as.integer(sort(cutPoints))
    return(c(part(low, low + ci - 1, cutPoints, depth = depth), 
             part(low + ci, upp, cutPoints, depth = depth)))
  }
  res <- part(depth = depth)
  ci <- NULL
  cv <- numeric()
  if (!is.null(res)) {
    ci <- as.integer(res)
    cv <- (xo[ci] + xo[ci + 1])/2
  }
  res <- unique(cv)
  return(res)
}

mdlStop_m=function (ci, y, entropy) 
{
  n <- length(y)
  es <- ent(y)
  left <- 1:ci
  right <- (ci + 1):n
  gain <- es - entropy
  l0 <- levels(factor(y))
  l1 <- levels(factor(y[left]))
  l2 <- levels(factor(y[right]))
  k <- length(l0)
  k1 <- length(l1)
  k2 <- length(l2)
  delta <- mylog(3^k - 2) - (k * es - k1 * ent(y[left]) - 
                               k2 * ent(y[right]))
  cond <- mylog(n - 1)/n + delta/n
  # condition relaxation
  if (gain < cond - 0.01) 
    return(NULL)
  return(gain)
}

