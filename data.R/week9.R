# WEEK 9

library(tidyverse)
#install.packages("MASS")    # run only once
library(MASS)

mu_x = 3
mu_y = 1
sigma_x = 4
sigma_y = 2
rho = -0.7   # correlation coefficient

# Construct mean vector and covariance matrix
mu = c(mu_x,mu_y)
sigma = cbind(c(sigma_x^2, sigma_x*sigma_y*rho),
              c(sigma_x*sigma_y*rho, sigma_y^2))

# Draw a random sample of size n
n = 20
values = mvrnorm(n, mu, sigma)

# Plot sample on a scatterplot
ggplot(NULL, aes(x=values[,1], y=values[,2])) +
  geom_point()





















# Part 3. PCA by matrix operations

# Calculate sample mean vector
M = colMeans(values)
M
# ... should be close-ish to mu (compare)
mu

# Calculate sample covariance matrix
C = cov(values)
C
# ... should be close-ish to sigma (compare)
sigma

# Diagonalise sample covariance matrix C
E = eigen(C)
D = diag(E$values)
P = E$vectors
Pinv = solve(P)

# ... check
P %*% D %*% Pinv
# ... should be exactly C
C

# Subtract the column mean from each column of sample,
# this is called "centering".
centred_values = cbind(values[,1]-M[1],
                       values[,2]-M[2])

# Transform the centred sample points by 
# applying Pinv.
# Thinking back to the flag, it was stored as two
# rows for x and y, but here we have two columns.
# Solution: take transpose, i.e., t().
transformed_values = t(Pinv %*% t(centred_values))

# Check det(Pinv)
# -- if positive then rotation
# -- if negative then reflection (think of as rotate and reflect)
det(Pinv)

# Plot original and transformed points
ggplot(NULL) +
  geom_point(aes(x=values[,1], y=values[,2]),
             color='red') +
  geom_point(aes(x=transformed_values[,1], y=transformed_values[,2]),
             color='green') +
  coord_fixed()

# Consider correlation of transformed points
# ... what do you notice?
cor(transformed_values[,1],transformed_values[,2])

# Consider covariance matrix of transformed points
cov(transformed_values)
# ... compare with D matrix (diagonal matrix containing eigenvalues)
# ... what do you notice?
D









# Part 4. PCA by R

# It is no surprise that R does everything for us
# in one easy command ...
pca_results = prcomp(values)

# Transformation matrix (compare with P)
pca_results$rotation
# ... some differences in positives/negatives

# Transformed points (compare with transformed_values)
pca_results$x

# Sample mean of original points (compare with M)
pca_results$center

# Plot original and transformed points
ggplot(NULL) +
  geom_point(aes(x=values[,1], y=values[,2]),
             color='red') +
  geom_point(aes(x=transformed_values[,1], y=transformed_values[,2]),
             color='green') +
  geom_point(aes(x=pca_results$x[,1], y=pca_results$x[,2]),
             color='blue') +
  coord_fixed()
