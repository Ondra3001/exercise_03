IndexOfMin <- function(in_list, first_idx, last_idx){
  searched_id <- first_idx
  for (i in ((first_idx+1):last_idx)){
    if (in_list[i] < in_list[searched_id]){
        searched_id <- i
    }
  }
  
  return (searched_id)
}

#IndexOfMin(search_list, 1, 5)

SelectionSort <- function(in_list, n){
  for (i in (1:(n-1))){
    j <- IndexOfMin(in_list, i, n)
    first_swap <- in_list[i]
    in_list[i] <- in_list[j]
    in_list[j] <- first_swap
    
  }
  return (in_list)
}
search_list <- c(4,1,56,3,84,6,9)
#SelectionSort(search_list,7)

RecursiveSelectionSort <- function(in_list, first_idx, last_idx){
  if (first_idx < last_idx){
    idx <- IndexOfMin(in_list, first_idx, last_idx)
    swap <- in_list[first_idx]
    in_list[first_idx] <- in_list[idx]
    in_list[idx] <- swap
    
    in_list <- RecursiveSelectionSort(in_list, first_idx + 1, last_idx)
  }
  return (in_list)
    
  
}

search_list <- c(4,1,56,3,84,6,9)
print(RecursiveSelectionSort(search_list, 1, 7))


