
# update contact matrix

updatematcontact <- function(data){
if (!is.null(data$Hplanning)){
  contacts <- data$Hplanning

  matContact <- lapply(data$ward_names, function(W) {
    W <- contacts$professionals %>% endsWith(., paste0("_", W)) %>% contacts[., ]
    W %<>% .[, data$ward_names] %>% colSums
    W %<>% divide_by(sum(.)) %>% multiply_by((100))
    W
  }) %>% do.call(rbind, .)

  rownames(matContact) <- colnames(matContact)
  return(matContact)
}
  }
