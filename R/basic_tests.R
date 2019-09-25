if(FALSE) {

library(tidytensor)


runif(1:(4*5*1*7*5)) %>%
  array(dim = c(4, 5, 1, 7, 5)) %>%
  tt() %>%
  set_ranknames(one, two, three, four, five) %>%
  set_dimnames_for_rank(one, .dots = letters[1:4]) %>%
  set_dimnames_for_rank(two, .dots = letters[1:5]) %>%
  set_dimnames_for_rank(four, .dots = letters[1:7]) %>%
  set_dimnames_for_rank(five, .dots = rev(letters[1:5])) %>%
  print(show_names = F, bottom = "3d", max_per_level = 2)



} # end if(FALSE)
