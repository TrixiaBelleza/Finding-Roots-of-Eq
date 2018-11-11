SecantMethod <- function(func, x0, x1, macheps, maxIter) {
  #since func = cos(x) we have to make it look like cos(x0) and cos(x1)
  #sub to capture the first 5 characters as a group ((.{5})) 
  # followed by one or more characters in another capture group ((.*)) 
  #and then replace with the backreference of first group (\\1) followed by a <new string you want to place> followed by second backreference (\\2).
  
  func0 = sub("(.{5})(.*)", "\\10\\2", func) #add a zero to x, for example cos(x) becomes cos(x0)
  y0 = eval(parse(text=func0))
  
  func1 = sub("(.{5})(.*)", "\\11\\2", func) #add a zero to x, for example cos(x) becomes cos(x0)
  y1 = eval(parse(text=func1))
  
  count = 0
  errApprox = 999999
  while(errApprox >= macheps & count < maxIter) {
    x = x1 - ((x1 - x0) * y1 / (y1 - y0))
    y = eval(parse(text=func))
    if(y == 0) { #if nag zero yung f(x) ibig sabihin, yung x na yung root mo.
      return (x)
    }
    if(count != 0) { #if hindi siya 1st iteration
      #((x_curr - x_prev) / x_curr) * 100%
      #x_prev = x1
      errApprox = abs((x-x1) / x) * 100
    }
    x0 = x1
    y0 = y1
    x1 = x
    y1 = y
    count = count + 1
  }
  return(list(root = x, num_iter = count, ea = errApprox))
}

func = "cos(x)"
sec = SecantMethod(func, 0, 2, 0.00005, 5)
print(sec$root)



