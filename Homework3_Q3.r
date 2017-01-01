
# Getting Data ------------------------------------------------------------
require(zoo, quietly = T)
require(fImport, quietly = T)
require(tseries, quietly = T)
require(igraph, quietly = T)

# Energy Sector Data Collection
E1 <- coredata(get.hist.quote(instrument="PTR", start="2003-01-01", end="2008-01-01",
                              quote= c("Close"), provider="yahoo", drop=TRUE))

E2 <- coredata(get.hist.quote(instrument="XOM", start="2003-01-01", end="2008-01-01",
                              quote= c("Close"), provider="yahoo", drop=TRUE))

E3 <- coredata(get.hist.quote(instrument="CVX", start="2003-01-01", end="2008-01-01",
                              quote= c("Close"), provider="yahoo", drop=TRUE))
E4 <- coredata(get.hist.quote(instrument="TOT", start="2003-01-01", end="2008-01-01",
                              quote= c("Close"), provider="yahoo", drop=TRUE))
E5 <- coredata(get.hist.quote(instrument="SNP", start="2003-01-01", end="2008-01-01",
                              quote= c("Close"), provider="yahoo", drop=TRUE))
# Manufacturing Sector Data Collection
M1 <- coredata(get.hist.quote(instrument="TM", start="2003-01-01", end="2008-01-01",
                              quote= c("Close"), provider="yahoo", drop=TRUE))
length(M1)

M2 <- (get.hist.quote(instrument="HMC", start="2003-01-01", end="2008-01-01",
                      quote= c("Close"), provider="yahoo", drop=TRUE))
length(M2)

M3 <- coredata(get.hist.quote(instrument="F", start="2003-01-01", end="2008-01-01",
                              quote= c("Close"), provider="yahoo", drop=TRUE))
length(M3)
M4 <- coredata(get.hist.quote(instrument="GPC", start="2003-01-01", end="2008-01-01",
                              quote= c("Close"), provider="yahoo", drop=TRUE))
length(M4)
M5 <- coredata(get.hist.quote(instrument="JCI", start="2003-01-01", end="2008-01-01",
                              quote= c("Close"), provider="yahoo", drop=TRUE))
length(M5)

# Financial Sector Data Collection
F1 <- coredata(get.hist.quote(instrument="WFC", start="2003-01-01", end="2008-01-01",
                              quote= c("Close"), provider="yahoo", drop=TRUE))
length(F1)

F2 <- coredata(get.hist.quote(instrument="JPM", start="2003-01-01", end="2008-01-01",
                              quote= c("Close"), provider="yahoo", drop=TRUE))
length(F2)

F3 <- coredata(get.hist.quote(instrument="HSBC", start="2003-01-01", end="2008-01-01",
                              quote= c("Close"), provider="yahoo", drop=TRUE))
length(F3)
F4 <- coredata(get.hist.quote(instrument="BAC", start="2003-01-01", end="2008-01-01",
                              quote= c("Close"), provider="yahoo", drop=TRUE))
length(F4)
F5 <- coredata(get.hist.quote(instrument="C", start="2003-01-01", end="2008-01-01",
                              quote= c("Close"), provider="yahoo", drop=TRUE))
length(F5)

# Healthcare Sector Data Collection
H1 <- coredata(get.hist.quote(instrument="JNJ", start="2003-01-01", end="2008-01-01",
                              quote= c("Close"), provider="yahoo", drop=TRUE))
length(H1)

H2 <- coredata(get.hist.quote(instrument="PFE", start="2003-01-01", end="2008-01-01",
                              quote= c("Close"), provider="yahoo", drop=TRUE))
length(H2)

H3 <- coredata(get.hist.quote(instrument="NVS", start="2003-01-01", end="2008-01-01",
                              quote= c("Close"), provider="yahoo", drop=TRUE))
length(H3)
H4 <- coredata(get.hist.quote(instrument="MRK", start="2003-01-01", end="2008-01-01",
                              quote= c("Close"), provider="yahoo", drop=TRUE))
length(H4)
H5 <- coredata(get.hist.quote(instrument="UNH", start="2003-01-01", end="2008-01-01",
                              quote= c("Close"), provider="yahoo", drop=TRUE))
length(H5)

# Services Sector Data Collection
S1 <- coredata(get.hist.quote(instrument="LMT", start="2003-01-01", end="2008-01-01",
                              quote= c("Close"), provider="yahoo", drop=TRUE))
length(S1)

S2 <- coredata(get.hist.quote(instrument="ACN", start="2003-01-01", end="2008-01-01",
                              quote= c("Close"), provider="yahoo", drop=TRUE))
length(S2)

S3 <- coredata(get.hist.quote(instrument="CNI", start="2003-01-01", end="2008-01-01",
                              quote= c("Close"), provider="yahoo", drop=TRUE))
length(S3)
S4 <- coredata(get.hist.quote(instrument="FDX", start="2003-01-01", end="2008-01-01",
                              quote= c("Close"), provider="yahoo", drop=TRUE))
length(S4)
S5 <- coredata(get.hist.quote(instrument="NOC", start="2003-01-01", end="2008-01-01",
                              quote= c("Close"), provider="yahoo", drop=TRUE))
length(S5)

# Telecom  Sector Data Collection
T1 <- coredata(get.hist.quote(instrument="T", start="2003-01-01", end="2008-01-01",
                              quote= c("Close"), provider="yahoo", drop=TRUE))
length(T1)

T2 <- coredata(get.hist.quote(instrument="VZ", start="2003-01-01", end="2008-01-01",
                              quote= c("Close"), provider="yahoo", drop=TRUE))
length(T2)

T3 <- coredata(get.hist.quote(instrument="CHL", start="2003-01-01", end="2008-01-01",
                              quote= c("Close"), provider="yahoo", drop=TRUE))
length(T3)
T4 <- coredata(get.hist.quote(instrument="NTT", start="2003-01-01", end="2008-01-01",
                              quote= c("Close"), provider="yahoo", drop=TRUE))
length(T4)
T5 <- coredata(get.hist.quote(instrument="VOD", start="2003-01-01", end="2008-01-01",
                              quote= c("Close"), provider="yahoo", drop=TRUE))
length(T5)

# Retail Sector Data Collection
R1 <- coredata(get.hist.quote(instrument="WMT", start="2003-01-01", end="2008-01-01",
                              quote= c("Close"), provider="yahoo", drop=TRUE))
length(R1)

R2 <- coredata(get.hist.quote(instrument="AMZN", start="2003-01-01", end="2008-01-01",
                              quote= c("Close"), provider="yahoo", drop=TRUE))
length(R2)

R3 <- coredata(get.hist.quote(instrument="HD", start="2003-01-01", end="2008-01-01",
                              quote= c("Close"), provider="yahoo", drop=TRUE))
length(R3)
R4 <- coredata(get.hist.quote(instrument="MCD", start="2003-01-01", end="2008-01-01",
                              quote= c("Close"), provider="yahoo", drop=TRUE))
length(R4)
R5 <- coredata(get.hist.quote(instrument="WBA", start="2003-01-01", end="2008-01-01",
                              quote= c("Close"), provider="yahoo", drop=TRUE))
length(R5)

# Diversified Business Sector Data Collection
D1 <- coredata(get.hist.quote(instrument="BRK-A", start="2003-01-01", end="2008-01-01",
                              quote= c("Close"), provider="yahoo", drop=TRUE))
length(D1)

D2 <- coredata(get.hist.quote(instrument="BRK-B", start="2003-01-01", end="2008-01-01",
                              quote= c("Close"), provider="yahoo", drop=TRUE))
length(D2)

D3 <- coredata(get.hist.quote(instrument="GE", start="2003-01-01", end="2008-01-01",
                              quote= c("Close"), provider="yahoo", drop=TRUE))
length(D3)
D4 <- coredata(get.hist.quote(instrument="UTX", start="2003-01-01", end="2008-01-01",
                              quote= c("Close"), provider="yahoo", drop=TRUE))
length(D4)
D5 <- coredata(get.hist.quote(instrument="ABB", start="2003-01-01", end="2008-01-01",
                              quote= c("Close"), provider="yahoo", drop=TRUE))
length(D5)

# Media Sector Data Collection
Me1 <- coredata(get.hist.quote(instrument="PG", start="2003-01-01", end="2008-01-01",
                               quote= c("Close"), provider="yahoo", drop=TRUE))
length(Me1)

Me2 <- coredata(get.hist.quote(instrument="KO", start="2003-01-01", end="2008-01-01",
                               quote= c("Close"), provider="yahoo", drop=TRUE))
length(Me2)

Me3 <- coredata(get.hist.quote(instrument="CMCSA", start="2003-01-01", end="2008-01-01",
                               quote= c("Close"), provider="yahoo", drop=TRUE))
length(Me3)
Me4 <- coredata(get.hist.quote(instrument="DIS", start="2003-01-01", end="2008-01-01",
                               quote= c("Close"), provider="yahoo", drop=TRUE))
length(Me4)
Me5 <- coredata(get.hist.quote(instrument="UL", start="2003-01-01", end="2008-01-01",
                               quote= c("Close"), provider="yahoo", drop=TRUE))
length(Me5)

# Industrial Data Collection
I1 <- coredata(get.hist.quote(instrument="GE", start="2003-01-01", end="2008-01-01",
                              quote= c("Close"), provider="yahoo", drop=TRUE))
length(I1)

I2 <- coredata(get.hist.quote(instrument="HON", start="2003-01-01", end="2008-01-01",
                              quote= c("Close"), provider="yahoo", drop=TRUE))
length(I2)

I3 <- coredata(get.hist.quote(instrument="GD", start="2003-01-01", end="2008-01-01",
                              quote= c("Close"), provider="yahoo", drop=TRUE))
length(I3)
I4 <- coredata(get.hist.quote(instrument="MMM", start="2003-01-01", end="2008-01-01",
                              quote= c("Close"), provider="yahoo", drop=TRUE))
length(I4)
I5 <- coredata(get.hist.quote(instrument="UPS", start="2003-01-01", end="2008-01-01",
                              quote= c("Close"), provider="yahoo", drop=TRUE))
length(I5)


#Data Matrix
nodematrix <- matrix(c(E1,E2,E3,E4,E5,M1,M2,M3,M4,M5,F1,F2,F3,F4,F5,H1,H2,H3,H4,H5,S1,S2,S3,
                       S4,S5,T1,T2,T3,T4,T5,R1,R2,R3,R4,R5,D1,D2,D3,D4,D5,Me1,Me2,Me3,Me4,Me5,I1,I2,I3,I4,I5),nrow=length(E1))


x=cor(nodematrix) #correlation matrix



# Z Matrix and Significance Matrix ----------------------------------------
Z = log10((1+x)/(1-x))/2  # H function
m=choose(nrow(x),2)
alpha=0.05
thres=qnorm(1-(alpha/(2*m)))/sqrt(nrow(nodematrix)-1)

Zcap=abs(Z)>thres #signficance matrix



### NonParamteric Bootstrap


# Bootstrap ---------------------------------------------------------------


B <- 1000
n <- nrow(nodematrix)
a.nboot <- list(rep(NA, B))
for (b in 1:B)
{
  idx <- sample(1:n, replace = TRUE)
  x.boot <- nodematrix[idx,]  # ECDF
  cor.mat <- matrix(NA, nrow=nrow(Z), ncol = ncol(Z))
  for (i in 1:ncol(x)){
    for (j in 1:ncol(x)){
      cor.mat[i,j] <- cor(x.boot[,i], x.boot[,j])
    }
  }
  a.nboot[[b]] <- cor.mat   #Rcap* Bootstrapped samples of correlation matrix 
}


#Delta samples
max_matrix=rep(NA,1000)
for (i in 1:1000)
{
  max_matrix[i]=sqrt(nrow(nodematrix))*max(a.nboot[[i]]-x)   
  
}

ecdf(max_matrix)

#CI for bootstrap
t_alpha = quantile(ecdf(max_matrix),0.05)
lower_bound =  x- (t_alpha)/(sqrt(nrow(nodematrix)))
upper_bound =  x+ (t_alpha)/(sqrt(nrow(nodematrix)))


#plotting graph
i=which(Zcap==TRUE,arr.ind=TRUE)
df <-data.frame(a=i[,1],b=i[,2])
df.g <- simplify(graph.data.frame(d = df[df$a!=df$b, ], directed = FALSE))
k_1=layout_on_grid(df.g, dim = 2)

#color nodes
for (i in 1:50){
  x=V(df.g)$name==i
  if((i-1)%%5==0){
    k=i}
  V(df.g)$color[which(x==TRUE)] <- colors()[k+1]
}

plot(df.g,layout=k_1)


names = c("Energy Sector","Manufacturing Sector","Financial Sector","Healthcare Sector","Services Sector",
          "Telecom Sector","Retail Sector","Diversified Business Sector","Media Sector","Industrial Sector")

legend('topleft',names, 
       lty=1, col=c(colors()[1], colors()[6],colors()[11],colors()[16],colors()[21],colors()[26],colors()[31],colors()[36],colors()[41],colors()[46]), bty='n', cex=.75)


## Conclusion 

### Comparison of connections between sectors

```{r}
# 1. Energy sector
# Intracluster connections
nrow(df[df$a < 6 & df$b < 6, ])
# Intercluster connections
nrow(df[df$a < 6 & df$b > 6, ])


# 2. Manufacturing sector
# Intracluster connections
nrow(df[(df$a > 5 & df$a < 11) & (df$b > 5 & df$b < 11), ])
#Intercluster clustering
nrow(df[(df$a > 5 & df$a < 11) & (df$b < 6), ])+ nrow(df[(df$a > 5 & df$a < 11) & (df$b > 11), ])

# 3. Financial sector
# Intracluster connections
nrow(df[(df$a > 10 & df$a < 16) & (df$b > 10 & df$b < 16), ])
#Intercluster clustering
nrow(df[(df$a > 10 & df$a < 16) & (df$b < 11), ])+ nrow(df[(df$a > 10 & df$a < 16) & (df$b > 15), ])

# 4. Healthcare sector
# Intracluster connections
nrow(df[(df$a > 15 & df$a < 21) & (df$b > 15 & df$b < 21), ])
#Intercluster clustering
nrow(df[(df$a > 15 & df$a < 21) & (df$b < 16), ])+ nrow(df[(df$a > 15 & df$a < 21) & (df$b > 20), ])


# 5. Services sector
# Intracluster connections
nrow(df[(df$a > 20 & df$a < 26) & (df$b > 20 & df$b < 26), ])
#Intercluster clustering
nrow(df[(df$a > 20 & df$a < 26) & (df$b < 21), ])+ nrow(df[(df$a > 20 & df$a < 26) & (df$b > 25), ])


# 6. Telecom sector
# Intracluster connections
nrow(df[(df$a > 25 & df$a < 31) & (df$b > 25 & df$b < 31), ])
#Intercluster clustering
nrow(df[(df$a > 25 & df$a < 31) & (df$b < 26), ])+ nrow(df[(df$a > 25 & df$a < 31) & (df$b > 30), ])


# 7. Retail sector
# Intracluster connections
nrow(df[(df$a > 30 & df$a < 36) & (df$b > 30 & df$b < 36), ])
#Intercluster clustering
nrow(df[(df$a > 30 & df$a < 36) & (df$b < 31), ])+ nrow(df[(df$a > 30 & df$a < 36) & (df$b > 35), ])

# 8. Diversified sector
# Intracluster connections
nrow(df[(df$a > 35 & df$a < 41) & (df$b > 35 & df$b < 41), ])
#Intercluster clustering
nrow(df[(df$a > 35 & df$a < 41) & (df$b < 36), ])+ nrow(df[(df$a > 35 & df$a < 41) & (df$b > 40), ])

# 9. Media sector
# Intracluster connections
nrow(df[(df$a > 40 & df$a < 46) & (df$b > 40 & df$b < 46), ])
#Intercluster clustering
nrow(df[(df$a > 40 & df$a < 46) & (df$b < 41), ])+ nrow(df[(df$a > 40 & df$a < 46) & (df$b > 45), ])


# 10. Industrial sector
# Intracluster connections
nrow(df[(df$a > 45 & df$a < 51) & (df$b > 45 & df$b < 51), ])
#Intercluster clustering
nrow(df[(df$a > 45 & df$a < 51) & (df$b < 46), ])+ nrow(df[(df$a > 45 & df$a < 51) & (df$b > 50), ])


