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
      print(paste("Iter ", count,sep=""))
      print(paste("x0 = ", x0, " | x1 = ", x1, " | f(x0) = ", y0, " | f(x1) = ", y1, " | x = ", x, " | f(x) = ", y, " | errorApprox = ", errApprox, sep=""))
    }
    else { 
      print(paste("Iter ", count,sep=""))
      print(paste("x0 = ", x0, " | x1 = ", x1, " | f(x0) = ", y0, " | f(x1) = ", y1, " | x = ", x, " | f(x) = ", y, " | errorApprox = NA", sep=""))
    }
    x0 = x1
    y0 = y1
    x1 = x
    y1 = y
    count = count + 1
   
  }
  return(list(root = x, num_iter = count, ea = errApprox))
}

compute_h_and_d <- function(y1, y0, x1, x0) {
  h = x1 - x0
  d = (y1 - y0) / h
  return(list(h=h, d=d))
}

find_discriminant <- function(a,b,c) {
  
  innerDiscriminant = (b^2)-(4*a*c)
  if(a == 0) {
    return(NA)
  }
  else {
    if(innerDiscriminant < 0) {
      discriminant_1 = (b + sqrt(as.complex(innerDiscriminant)))
      discriminant_2 = (b - sqrt(as.complex(innerDiscriminant)))
    }
    else {
      discriminant_1 = (b + sqrt(innerDiscriminant))
      discriminant_2 = (b - sqrt(innerDiscriminant))
    }
    if(abs(discriminant_1) > abs(discriminant_2)) {
      return(discriminant_1)
    }
    else {
      return(discriminant_2)
    }
    discriminants <- c(discriminant_1,discriminant_2)
  }
  
  return(discriminants)
}

MullerMethod <- function(func, x0, x1, x2, macheps, maxIter) {
  errApprox = 99999
  count = 0
  while(errApprox >= macheps & count < maxIter) {
    func0 = sub("(.{5})(.*)", "\\10\\2", func) #add a zero to x, for example cos(x) becomes cos(x0)
    y0 = eval(parse(text=func0))
    
    func1 = sub("(.{5})(.*)", "\\11\\2", func) #add a one to x, for example cos(x) becomes cos(x1)
    y1 = eval(parse(text=func1))
    
    func2 = sub("(.{5})(.*)", "\\12\\2", func) #add a two to x, for example cos(x) becomes cos(x2)
    y2 = eval(parse(text=func2))
    
    #get h0 and d0
    get_h_and_d = compute_h_and_d(y1, y0, x1, x0)
    h0 = get_h_and_d$h
    d0 = get_h_and_d$d
    
    #get h1 and d1
    get_h_and_d = compute_h_and_d(y2, y1, x2, x1)
    h1 = get_h_and_d$h
    d1 = get_h_and_d$d
    
    #get A, B, C
    A = (d1 - d0) / (h1 + h0)
    B = (A*h1) + d1 
    C = y2

    x3 = x2 - (2*C / find_discriminant(A,B,C))
    
    func3 = sub("(.{5})(.*)", "\\13\\2", func) #add a two to x, for example cos(x) becomes cos(x2)
    y3 = eval(parse(text=func3))
    errApprox = (abs(x3 - x2) / x3) * 100
    print(errApprox)
    x0 = x1
    x1 = x2
    x2 = x3
    count = count+1
  }
  return(list(x0 = x0, x1 = x1, x2 = x2, x3 = x3, num_iterations = count, ea = errApprox))
}

func = "cos(x)"
#sec = SecantMethod(func, 0, 2, 0.00005, 5)
#print(paste("root: ", sec$root, sep=""))
MullerMethod(func,0,2,4,0.00001,7)


