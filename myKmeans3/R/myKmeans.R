#'Input Check
#'
#'This function checks the inputs of the K-means algorithm.
#'
#'This function checks if users enter datasets and enter the matrix of cluster
#'centers or valid number of cluster centers.
#'
#'@param data a matrix, each row represents a sample.
#'@param centers a matrix, each row represents a cluster center.
#'@param k numeric, number of cluster centers.
#'
#'@seealso See \code{\link{myKmeans}} for more information about inputs.
#'
#'@examples
#'data <- iris[,3:4]
#'center <- data[c(1,3,4),]
#'
#'Kmeans_check(data,center,NA)
#'
#'Kmeans_check(data,NA,k=3)
#'
#'\dontrun{
#'Kmeans_check(data,NA,0)
#'}
Kmeans_check<-function(data,centers,k){
  if(sum(is.na(data))){
    stop("Please use valid datasets!")
  }
  if(sum(is.na(centers))){
    if(is.na(k)){
      stop("Please enter either matrix of cluster centers or number of clusters!")
    }
    else if(k<=0){
      stop("Please enter valid number of clusters!")
    }
  }
}

#'Euclidean Distance
#'
#'This function computes the euclidean distance of two sample points.
#'
#'@param a,b numeric vectors, the euclidean distance is computed as the square
#'  root of the summation of the square of the difference between each
#'  coordinate of \code{a} and \code{b}. \code{a} and \code{b} should have the
#'  same length.
#'
#'@return A numeric value representing the euclidean distance between point
#'  \code{a} and \code{b}.
#'
#'@examples
#'a <- c(1,7,4)
#'b <- c(2,0,5)
#'Dist(a,b)
Dist<-function(a,b){
  dist <- sqrt(sum((a-b)^2))
  return(dist)
}

#'Cluster Center Initiation
#'
#'This function randomly chooses \code{k} initial cluster centers of the K-means
#'algorithm.
#'
#'@param data a matrix, each row represents a sample.
#'@param k numeric, number of cluster centers.
#'
#'@return A matrix consisting of \code{k} randomly chosen initial cluster
#'  centers. Each row represents a cluster center.
#'
#'@seealso See \code{\link{myKmeans}} for more information about inputs.
#'
#'@examples
#'data <- iris[,3:4]
#'Center_init(data,3)
Center_init<-function(data,k){
  data_rows <- nrow(data)
  index_init <- sample.int(n = data_rows, size = k,replace = FALSE)
  return(data[index_init,])
}

#'Nearest Cluster Center
#'
#'This function finds the nearest cluster center of a sample.
#'
#'This function first computes the distance between a sample and each cluster
#'center, and finds the most close one to the sample, i.e. specify the cluster
#'affiliation of the sample.
#'
#'@param a a numeric vector which represents a sample.
#'@param cluster_centers a matrix, each row represents a cluster center.
#'@param k numeric, number of cluster centers.
#'
#'@return A numeric value. Row subscript of the nearest cluster center of sample
#'  \code{a} in cluster center matrix.
#'
#'@examples
#'data <- iris[,3:4]
#'cluster_centers <- data[c(1,3,4),]
#'a <- data[14,]
#'Nearest_cluster(a,cluster_centers,3)
Nearest_cluster<-function(a,cluster_centers,k){
  dist_cluster <- numeric(k)
  for(i in 1:k){
    dist_cluster[i] <- Dist(a,cluster_centers[i,])
  }
  return(which.min(dist_cluster))
}

#'Cluster Center Update
#'
#'This function updates the cluster centers after iteration.
#'
#'This function computes the new cluster centers by averaging the coordinates of
#'the samples affiliated to each cluster.
#'
#'@param data a matrix, each row represents a sample.
#'@param cluster_affiliation a vector, each element represents the cluster affiliation of each sample.
#'@param k numeric, number of cluster centers.
#'
#'@return A matrix. It has the same form and dimension as the cluster centers before iteration.
#'
#'@seealso See \code{\link{Nearest_cluster}} for more information about the
#'  computation of the cluster affiliation of a sample.
#'@examples
#'data <- iris[,3:4]
#'cluster_affiliation <- sample(c(1,2,3), nrow(data), replace = TRUE)
#'center_new <- cluster_center(data,cluster_affiliation,3)
cluster_center<-function(data,cluster_affiliation,k){
  cluster_cents <- matrix(0, nrow = k, ncol = ncol(data))
  for(i in 1:k){
    indi <- which(cluster_affiliation == i)
    cluster_cents[i,] <- apply(data[indi,],2,mean)
  }
  return(cluster_cents)
}

#'Center Change
#'
#'This function verifies if the cluster centers change after each iteration.
#'
#'@param center_former a matrix, cluster centers before iteration.
#'@param center_present a matrix, cluster centers after iteration.
#'@param tol numeric, tolerance value of center change.
#'
#'@return A bool. If the difference of the centers before iteration and the centers after iteration is smaller
#'than the tolerance number, the returned value is \code{TRUE}, otherwise \code{FALSE}.
#'
#'@examples
#'data <- iris[,3:4]
#'center_former <- data[c(1,3,4),]
#'center_present <- data[c(1,3,5),]
#'tol <- 1e-6
#'center_change = Center_change(center_former,center_present,tol)
#'center_change
Center_change<-function(center_former,center_present,tol){
  change <- TRUE
  change_value <- sum((center_present - center_former)^2)
  if(change_value < tol){
    change <- FALSE
  }
  return(change)
}

#'K-means Algorithm
#'
#'This function implements the K-means algorithm.
#'
#'The K-means algorithm was first proposed by James MacQueen (1967)
#'<Doi:10.1.1.308.8619>. Users can either specify the initial cluster centers
#'directly or just the number of cluster centers.
#'
#'@param data a matrix, each row represents a sample.
#'@param centers a matrix, each row represents a cluster center. Default is NA.
#'@param k numeric, number of cluster centers. Default is NA.
#'
#'@aliases kmeans
#'@return An object of class "\code{list}" which has two components. The first
#'  component is a vector of final cluster affiliations of samples. The second
#'  component is a matrix consisting of the final cluster centers.
#'
#'@seealso See \code{\link{Kmeans_check}} for more information about input check.
#'
#'@examples
#'data <- iris[,3:4]
#'centers <- data[c(1,3,4),]
#'mk1 <- myKmeans(data, centers)
#'
#'mk2 <- myKmeans(data, k = 3)
myKmeans<-function(data = NA, centers = NA, k = NA){
  Kmeans_check(data,centers,k)
  tol <- 1e-06
  data_row <- nrow(data)
  cluster_affiliation <- numeric(data_row)
  if(sum(is.na(centers))){
    center_former <- Center_init(data,k)
  }
  else{
    center_former <- centers
    k <- nrow(centers)
  }
  center_change <- TRUE
  while(center_change){
    for(i in 1:data_row){
      cluster_affiliation[i] <- Nearest_cluster(data[i,],center_former,k)
    }
    center_present <- cluster_center(data,cluster_affiliation,k)
    center_change = Center_change(center_former,center_present,tol)
    center_former = center_present
  }
  return(list(cluster_affiliation = cluster_affiliation, 
              cluster_centers = center_present))
}

#'Cluster Visualization
#'
#'This function visualizes the clustering results of the K-means algorithm.
#'
#'This function makes use of \code{ggplot2} and plots the clustering results of
#'the K-means algorithm. Users can also enters the original clusters for
#'comparison. It currently only supports two-dimensional visualization.
#'
#'@param data a matrix, each row represents a sample. It should be the same as
#'  what was used in the K-means algorithm.
#'@param var_names a (two-dimensional) vector. The names of the coordinates used
#'  in the K-means algorithm.
#'@param kmeans_result a list, output of the K-means algorithm.
#'@param cluster_origin a vector, the original cluster affiliations of samples (if accessible). Default is NA.
#'
#'@return A plot of the clustering result of the K-means algorithm and an
#'  (optional) plot of the original cluster affiliations of samples.
#'
#'@seealso See \code{\link{myKmeans}} for more information about the output of the
#'  K-means algorithm.
#'
#'@examples
#'data <- iris[,c(3:4)]
#'mk <- myKmeans(data, k = 3)
#'cv <- cluster_visual(data,colnames(iris)[3:4],mk)
#'cv
cluster_visual<-function(data,var_names,kmeans_result,cluster_origin = NA){
  data_plot <- data.frame(data,factor(kmeans_result$cluster_affiliation))
  colnames(data_plot) <- c(var_names,"Cluster")
  p <- ggplot2::ggplot(data = data_plot,
    ggplot2::aes(x = data_plot[,1],y = data_plot[,2], 
    color = Cluster)) + ggplot2::geom_point() + 
    ggplot2::labs(title = "Cluster Plot",x = var_names[1],y = var_names[2])
  
  result = list(plot = p)
  if(!sum(is.na(cluster_origin))){
    data_plot_origin <- data.frame(data,factor(cluster_origin))
    colnames(data_plot_origin) <- c(var_names,"Cluster")
    p_origin <- ggplot2::ggplot(data = data_plot_origin,
      ggplot2::aes(x = data_plot_origin[,1],y = data_plot_origin[,2], 
      color = Cluster)) + ggplot2::geom_point() + 
      ggplot2::labs(title = "Cluster Plot(Original)",
                    x = var_names[1],y = var_names[2])
    
    result = list(plot = p, plot_origin = p_origin)
  }
  return(result)
}

