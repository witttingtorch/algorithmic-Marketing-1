library(recommenderlab)
library(data.table)
Food <- read.csv(":.filepath/Food_Reviews.csv",stringsAsFactors=FALSE)
View(Food)
Food$Score <- as.factor(Food$Score)
str(Food)
sample.split = function( Y, SplitRatio = 2/3, group = NULL ) 
{ # Split data from vector Y into 2 bins in predefined ratio while preserving relative retios of
  # different labels in Y.
  # if (0<=SplitRatio<1) then "SplitRatio" fraction of points from Y will go to bin 1
  # if (1<=SplitRatio)   then "SplitRatio" number   of points from Y will go to bin 1
  # Returns logical vector of the same length as Y marking points to be added to bin 1
  nSamp  = length(Y)                  # number of sample labels
  nGroup = length(group)
  if (nGroup>0 && nGroup!=nSamp) 
    stop("Error in sample.split: Vectors 'Y' and 'group' have to have the same length")
  BinOne = logical(nSamp)             # boolean mask of samples in bin #1 set to false
  SplitRatio = abs(SplitRatio)        # make sure split ratio is positive
  if (SplitRatio>=nSamp) 
    stop("Error in sample.split: 'SplitRatio' parameter has to be i [0, 1] range or [1, length(Y)] range")
  U = unique(Y)                       # unique labels
  nU = length(U)                      # how many different labels?
  if (2*nU>nSamp | nU==1) {           # single label or most are different
    n = if(SplitRatio>=1) SplitRatio else SplitRatio*nSamp # how many samples will be in the first bin?
    rnd = runif(nSamp)                # get "nSamp" random numbers
    if (nGroup) split(rnd, group) <- lapply(split(rnd, group), mean)
    ord = order(rnd)                  # order them 
    BinOne[ord[1:n]] = TRUE   # and n idx samples will be remarked as true (bin 1)
  } else {                            # few different labels
    rat = if(SplitRatio>=1) SplitRatio/nSamp else SplitRatio
    for( iU in 1:nU) {                # for each label...
      idx = which(Y==U[iU])           # find samples that have it
      n   = round(length(idx)*rat)    # how many samples will be in the first bin?
      rnd = runif(length(idx))        # get random numbers
      if (nGroup) {
        grp = group[idx]              # get group labels of current subset of labels
        split(rnd, grp) <- lapply(split(rnd, grp), mean)
      }
      ord = order(rnd)                # order them       
      BinOne[idx[ord[1:n]]] = TRUE    # and n idx samples will be remarked as true (bin 1)
    }
  }
  if (SplitRatio>=1) {            # if user selected actual number not ratio
    n = sum(BinOne)-SplitRatio    # how many extra points do we have in bin 1?
    if (n>0)      BinOne[sample(which( BinOne), n)] = FALSE # move  n points from bin 1 to 2
    else if (n<0) BinOne[sample(which(!BinOne),-n)] = TRUE  # move -n points from bin 2 to 1
  }
  return( BinOne )
}

set.seed(90)
split<-sample.split(Food$Score,SplitRatio = 0.05)
Food <- subset(Food,split==TRUE)
Selected_col <-c("UserId","ProductId","Score")
Food_select<-Food[,Selected_col]
rownames(Food_select)<-NULL
Food_select$Score <- as.numeric(Food_select$Score)
Food_select <-unique(Food_select)
Rating_Matrix <-dcast(Food_select,UserId~ProductId,value.var ="Score")
User=Rating_Matrix[,1]
Products=colnames(Rating_Matrix)[2:ncol(Rating_Matrix)]
Rating_Matrix[,1]<-NULL
Rating_Matrix <-as.matrix(Rating_Matrix)
dimnames(Rating_Matrix)=list(User=User,Products=Products)
R_matrix <-as(Rating_Matrix, "realRatingMatrix")
hist(getRatings(R_matrix),breaks = 15,main="Distribution of ratings",xlab ="ratings",col = "Yellow")
head(as(R_matrix,"data.frame"))
identical(as(R_matrix,"matrix"),Rating_Matrix)
image(R_matrix,main="Rawratings")
userbased<-Recommender(R_matrix[1:1700],method="UBCF")
userbased
names(getModel(userbased))
set.seed(2016)
scheme <- evaluationScheme(R_matrix[1:1700], method="split", train = .9,k=1, given=1, goodRating=3)
scheme
algorithms <-list("random items" =list(name="RANDOM", param=NULL),"popular items" =list(name="POPULAR", param=NULL),"user-based CF" =list(name="UBCF", param=list(nn=50)),"item-based CF" =list(name="IBCF", param=list(k=50)))
results <-evaluate(scheme, algorithms, type ="topNList",n=c(1, 3, 5, 10, 15, 20))
plot(results, annotate=c(1,3), legend="bottomright")
Recom_UB <-pred
