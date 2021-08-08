#include <Rcpp.h>
using namespace Rcpp;

double Dist_C(NumericVector a,NumericVector b){
  int length = a.size();
  double dist = 0;
  for(int i = 0; i < length; i++){
    dist += pow(a[i]-b[i],2);
  }
  return sqrt(dist);
}

NumericMatrix Center_init_C(NumericMatrix data, int k){
  int data_rows = data.nrow();
  int data_cols = data.ncol();
  NumericVector index_init(k);
  int i = 0, j = 0;
  while(i < k){
    index_init[i] = rand()%data_rows;
    i++;
    for(j = 0; j < i; j++){
      if(index_init[j] == index_init[i]){
        i--;
        break;
      }
    }
  }
  NumericMatrix center_init(k,data_cols);
  for(i = 0; i < k; i++){
    center_init.row(i) = data.row(index_init(i));
  }
  return center_init;
}

int Nearest_cluster_C(NumericVector a, NumericMatrix cluster_centers, int k){
  NumericVector dist_cluster(k);
  int i = 0;
  for(i = 0; i < k; i++){
    dist_cluster[i] = Dist_C(a,cluster_centers.row(i));
  }
  int min_index = 0;
  for(i = 1; i < k; i++){
    if(dist_cluster[i] < dist_cluster[min_index]){
      min_index = i;
    }
  }
  return min_index;
}

NumericMatrix cluster_center_C(NumericMatrix data, NumericVector cluster_affilation, int k){
  NumericMatrix cluster_cents(k,data.ncol());
  int i = 0, j = 0, m = 0;
  NumericVector cluster_num(k);
  for(j = 0; j < k; j++){
    for(i = 0; i < data.nrow(); i++){
      if(cluster_affilation[i] == j){
        cluster_cents.row(j) = cluster_cents.row(j) + data.row(i);
        cluster_num[j]++;
      }
    }
    for(m = 0; m < data.ncol(); m++){
      cluster_cents(j,m) = cluster_cents(j,m)/cluster_num[j];
    }
  }
  return cluster_cents;
}

bool Center_change_C(NumericMatrix center_former, NumericMatrix center_present, double tol){
  bool change = 1;
  double change_value = 0;
  for(int i = 0; i < center_former.nrow(); i++){
    change_value += sum(pow(center_present.row(i) - center_former.row(i),2));
  }
  if(change_value < tol){
    change = 0;
  }
  return change;
}

//' K-means Algorithm Using C++
//' 
//' This function implements the K-means algorithm using C++.
//' 
//' 
//' @param data a matrix, each row represents a sample.
//' @param centers a matrix, each row represents a cluster center. Default is NA.
//' @param k numeric, number of cluster centers. Default is NA.
//' 
//' @export
//[[Rcpp::export]]
List myKmeans_C(NumericMatrix data, NumericMatrix centers = NumericMatrix(2,2), int k = 0){
  double tol = 1e-6;
  int data_row = data.nrow(), i;
  NumericVector cluster_affilation(data_row);
  NumericMatrix center_former;
  if(k != 0){
    center_former = Center_init_C(data,k);
  }
  else{
    center_former = centers;
    k = center_former.nrow();
  }
  NumericMatrix center_present = center_former;
  bool center_change = 1;
  while(center_change){
    for(i = 0; i < data_row; i++){
      cluster_affilation[i] = Nearest_cluster_C(data.row(i),center_former,k);
    }
    center_present = cluster_center_C(data,cluster_affilation,k);
    center_change = Center_change_C(center_former,center_present,tol);
    center_former = center_present;
  }
  return List::create(Named("cluster_affilation") = cluster_affilation + 1,
                      Named("cluster_centers") = center_present);
}