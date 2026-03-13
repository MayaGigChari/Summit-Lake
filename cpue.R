#Resampling and preserving unsampled area structure.
row_resamp <- function(row) {
  non_na_values <- row[!is.na(row)]
  resampled_values <- sample(non_na_values, length(non_na_values), replace = TRUE)
  row[!is.na(row)] <- resampled_values
  return(row)
}

cpue_iteration<- function(mat,CPUE_bar) {
  resampled_rows<-mat[sample(1:nrow(mat), size = nrow(mat), replace = TRUE),]
  resampled_mat<- t(apply(resampled_rows,1, row_resamp))
  n= nrow(resampled_mat) #terms in the Wu correction
  n_norm<- sqrt(n/(n-1))
  mi= rowSums(!is.na(resampled_mat))
  mi_norm<-sqrt(mi/(mi-1))
  mibar = mean(mi)
  wi= mi/mibar
  ci = rowSums(resampled_mat, na.rm = TRUE)
  cibar = ci/mi #average catch per set in a transect.
  resampled_mat_sweep <- sweep(resampled_mat, 1, cibar, "-")
  mat_new = CPUE_bar+n_norm*(wi*cibar-CPUE_bar)+wi*mi_norm*resampled_mat_sweep
  CPUE_new = 1/n * (sum(mat_new / mi, na.rm = TRUE))
  CPUE_output<- CPUE_new
}
#Run the simulation, retrieve statistics
cpue_boot<- function(mat, raw_data_only = FALSE) {
  mi_true = rowSums(!is.na(mat))
  mbar_true = mean(mi_true)
  total_catch = sum(mat, na.rm = TRUE)
  CPUE_bar = (1/nrow(mat))*sum(rowSums(mat, na.rm = TRUE)/mbar_true)
  CPUE_output<- map_dbl(1:1000, ~cpue_iteration(mat, CPUE_bar))
  hist(CPUE_output, breaks = 20)
  mean<- mean(CPUE_output)
  sd<- sd(CPUE_output)
  if(raw_data_only){
    return(CPUE_output)
  }
  return(list("mean" = mean, "sd" = sd))
}

#how did this work before? 
pop_mat<- function(matricized_fish, transect_matrix, t_numbers)
{
  
  stop = FALSE
  i = 1
  for(j in 1:t_numbers)
  {
    #print(paste("transect", j))
    m = matricized_fish$transect_number[i]
    k = m
    #now k becomes 11
    p = 1
    while(k == m)
    {
      transect_matrix[j,p] = matricized_fish$count[i]
      p = p+1
      i = i +1
      k = matricized_fish$transect_number[i]
      if(i > length(matricized_fish$count))
      {
        print("true")
        stop = TRUE
        break
      }
      
    }
    if(stop){break}
  }
  return(transect_matrix)
}

