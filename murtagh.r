
x = cluster064.5

iris_set <- function(set){
return(iris.values[set,])
}

number <- function(set){
ell = dim(set)[1]
return(ell)
}

euc_dist <- function(vec1,vec2){
difference = vec1-vec2
sqdist = difference[,1]*difference[,1] + difference[,2]*difference[,2] + difference[,3]*difference[,3] + difference[,4]*difference[,4]
return (sqrt(sqdist))
}

is_degenerate <- function(x,y,z) {
a = x-y
b = x-z
norma = a[1]*a[1] + a[2]*a[2] + a[3]*a[3] + a[4]*a[4]
normb = b[1]*b[1] + b[2]*b[2] + b[3]*b[3] + b[4]*b[4]
inner = a[1]*b[1] + a[2]*b[2] + a[3]*b[3] + a[4]*b[4]
redinner = inner/(sqrt(norma)*sqrt(norma))
return(abs(redinner)==1)
}

triangles <- function(dataset) {
triangcount = 0
ell = number(dataset)
iris = dataset
for (i in 1:ell) {
for (j in 1:i-1) {
for (k in 1:j-1) {
#if (!is_degenerate(a,b,c)){
ab = iris[i,1]*iris[i,1]+iris[i,2]*iris[i,2]+iris[i,3]*iris[i,3]+iris[i,4]*iris[i,4]
bc = iris[j,1]*iris[j,1]+iris[j,2]*iris[j,2]+iris[j,3]*iris[j,3]+iris[j,4]*iris[j,4]
cd = iris[k,1]*iris[k,1]+iris[k,2]*iris[k,2]+iris[k,3]*iris[k,3]+iris[k,4]*iris[k,4]
#}
x = ab
y = bc 
z = cd
alpha = (y*y+z*z-x*x)/(2*y*z)
beta = (x*x+z*z-y*y)/(2*x*z)
gamma = (x*x*+y*y-z*z)/(2*x*y)
if(length(alpha)>0 && length(beta)>0 && length(gamma)>0){
rad  = 0.999391
angle1 = acos(alpha)-acos(beta)
angle2 = acos(alpha)-acos(gamma)
angle3 = acos(beta)-acos(gamma)
if ((!is.na(angle1) && (angle1 >= rad)) | (!is.na(angle2) && (angle2 >= rad)) | (!is.na(angle3) && (angle3 >= rad))){
triangcount = triangcount+1
}
}
}
}
}
return(triangcount)
}

y = iris_set(x)
t = triangles(y)

n = number(y)

total = n*(n-1)*(n-3)/6

alpha = t/total

print(t)
print(n)

print(alpha)
