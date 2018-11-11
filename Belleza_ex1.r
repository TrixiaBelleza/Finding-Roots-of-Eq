find_root <- function(a,b,c) {
  
  innerRoot = (b^2)-(4*a*c)
  if(a == 0) {
    return(NA)
  }
  else {
    if(innerRoot < 0) {
      root_1 = (-b + sqrt(as.complex(innerRoot)))/(2*a)
      root_2 = (-b - sqrt(as.complex(innerRoot)))/(2*a)
    }
    else {
      root_1 = (-b + sqrt(innerRoot))/(2*a)
      root_2 = (-b - sqrt(innerRoot))/(2*a)
    }
    
    roots <- c(root_1,root_2)
    
  }
  
  return(roots)
}

print(find_root(1,0,9))

print(find_root(1,0,-9))


print(find_root(0,0,9))