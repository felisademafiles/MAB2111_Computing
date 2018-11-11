#1.Define an R function that removes NA values from a vector.
Remove.NA <- function (n){
  Removed.NA <- subset(n, is.na(n)==FALSE)
  print(Removed.NA)  
}


#2. Define an R function that computes the factorial of given an integer argument. The output should be a vector of length 1.
fact <- function(x){
i=1
product=1
  if(x==0 | x==1){
    product=1
  }else{
    while (i <= x){
        product = i*product
        i=i+1
      }
  }
  print(product)
}



#5. Define an R function that accepts a Date (POSIXct) as argument and outputs the day of the week as characters. Use modulo operator.
day <- function(d){
  if(is(d,"POSIXct")==TRUE){
    seccount <- unclass(d)
    mod <- seccount%%604800
        if(mod < 86400){
          print("Thursday")
        }else if(mod >= 86400 & mod < 172800){
          print("Friday")
        }else if(mod >= 172800 & mod < 259200){
          print("Saturday")
        }else if(mod >= 259200 & mod < 345600){
          print("Sunday")
        }else if(mod >= 345600 & mod <= 432000){
          print("Monday")
        }else if(mod >= 432000 & mod < 518400){
          print("Tuesday")
        }else{
          print("Wednesday")
        }
  }else{
    print("The value is not in POSIXct format.")
  }
}



#6. Create a function to compute for your net pay at work.
net_pay <- function(monthly_gross,days_worked=22,months_pay=13){
  
  annual_taxable_income=monthly_gross*months_pay
  monthly_net = 1 
  
  #for tax deductions
  if(annual_taxable_income<=250000){
    monthly_net = monthly_gross
  }else if(annual_taxable_income>250000 & annual_taxable_income<=400000){
    monthly_net = monthly_gross-(((annual_taxable_income-250000)*0.2)/12)
  }else if(annual_taxable_income>400000 & annual_taxable_income<=800000){
    monthly_net = monthly_gross-((((annual_taxable_income-400000)*0.25)+30000)/12)
  }else if(annual_taxable_income>800000 & annual_taxable_income<=2000000){
    monthly_net = monthly_gross-((((annual_taxable_income-800000)*0.3)+130000)/12)
  }else if(annual_taxable_income>2000000 & annual_taxable_income<=8000000){
    monthly_net = monthly_gross-((((annual_taxable_income-2000000)*0.32)+490000)/12)
  }else{
    monthly_net = monthly_gross-((((annual_taxable_income-2410000)*0.35)+490000)/12)
  }
  
  #for deductions due to absences
  if(days_worked<22){
    monthly_net=monthly_net-((monthly_net/22)*(22-days_worked))
  }
  
  #philhealth employee contribution
  monthly_phil_base=monthly_gross-(monthly_gross%%1000)
  
  
  if(monthly_gross<9000){
    monthly_net=monthly_net-100
  }
  else if(monthly_gross>=9000 & monthly_gross<35000){
    phil_cont=(monthly_phil_base*0.025)/2
    monthly_net=monthly_net- phil_cont
  } else{
    monthly_net=monthly_net-437.50
  }
  
  #SSS Employee contribution
  if(monthly_gross<1000){
    monthly_sss_cont = 0
  }else if(monthly_gross>=15750){
    monthly_sss_cont = 581.3
  }else{
    mod_monthly_gross=monthly_gross%%1000
    if(mod_monthly_gross<250){
      monthly_sss_base=monthly_gross-mod_monthly_gross
    }else if (mod_monthly_gross>=250 & mod_monthly_gross<750){
      monthly_sss_base=(monthly_gross-mod_monthly_gross)+500
    }else{
      monthly_sss_base=(monthly_gross-mod_monthly_gross)+1000
    }
    monthly_sss_cont = monthly_sss_base*0.0363
  }
  
  monthly_net=monthly_net-monthly_sss_cont 

  print(monthly_net)
  
}



#9. Create a function isPrime(n) that accepts an integer and outputs a Boolean value (TRUE or FALSE) depending whether the integer is a prime number or not.
is.prime <- function(p){
prime = TRUE
len = p/2
  if(p==2){
    print(prime)
  }
  else if(p < 2 & p >=0){
    print("0 and 1 are neither prime nor composite")
  }else if(p<0){
    print("Enter a positive number.")
  }else{
     for(i in 2:len){
       if(i <= len){
         if(p%%i == 0){
            prime = FALSE
            i=i+1
            break
         }else{
           prime = TRUE
         }
       } 
     }
      print(prime)
  }
}













#DETERMINANT
m <- matrix(c(1,2,3, 4,5,6, 7,8,9), nrow=3, ncol=3)
class(m)

Determ <- function (m){
  if(class(m)!="matrix"){
    print("not a matrix")
  }else {
    if(nrow(m)!=ncol(m)){
      print("not a square matrix")
    }else{
      
    }
    
  }
  
}
Determ(n)


