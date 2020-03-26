#################################################
########## Exercise 3 (p.414, Chap.10) ##########
#################################################

x = cbind(c(1, 1, 0, 5, 6, 4), c(4, 3, 4, 1, 2, 0))
x

## a)

plot(x[,1], x[,2])

## b)

label = sample(2, nrow(x), replace=T)
label

## c)

centroid_1 = c(mean(x[label==1, 1]), mean(x[label==1, 2]))
centroid_2 = c(mean(x[label==2, 1]), mean(x[label==2, 2]))
centroid_1
centroid_2

## d)

euclidian_dis = function(a, b) {
  return(sqrt((a[1] - b[1])^2 + (a[2]-b[2])^2))
}
assign_labels = function(x, centroid_1, centroid_2) {
  labels = rep(, nrow(x))
  for (i in 1:nrow(x)) {
    if (euclidian_dis(x[i,], centroid_1) < euclidian_dis(x[i,], centroid_2)) {
      labels[i] = 1
    } else {
      labels[i] = 2
    }
  }
  return(labels)
}


#################################################
########## Exercise 8 (p.416, Chap.10) ##########
#################################################

install.packages("ISLR")
library(ISLR)

## a)

pr.out = prcomp(USArrests, center=T, scale=T)
pr.var = pr.out$sdev^2
pve = pr.var / sum(pr.var)
pve

#################################################
########## Exercise 9 (p.416, Chap.10) ##########
#################################################

## a)

hc_comp = hclust(dist(USArrests), method="complete")
plot(hc_comp)

## b)

cutree(hc_comp, 3)
table(cutree(hc_comp, 3))

## c)

dsc = scale(USArrests)
hc_s_complete = hclust(dist(dsc), method="complete")
plot(hc_s_complete)

## d)

cutree(hc_s_complete, 3)
table(cutree(hc_s_complete, 3))