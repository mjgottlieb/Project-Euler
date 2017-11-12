# Project-Euler
Project Euler problems solved using R & Python

**********
Problem #1:
Find the sum of all the multiples of 3 or 5 below 1000. 
*Solved using R

Prob1 <- c()

a=0
b=0
n=1000
while (a <= n)
{if (a%%3 == 0)
{
Prob1 = c(Prob1,a)
}
if (b%%5 == 0)
{if (b%%3 != 0)
{Prob1 <- c(Prob1, b)}}
a = a+1
b = b+1}

sum(Prob1)


**********
Problem #2:
By considering the terms in the Fibonacci sequence whose values do not exceed four million, find the sum of the even-valued terms
*Solved using R

Arr <- c(1,2)
pos = 3
n=Arr[pos-2]+Arr[pos-1]

while (n <= 4000000)
{Arr <- c(Arr,n)
pos = pos+1
n=Arr[pos-2]+Arr[pos-1]}

evenpos= 1
evenvec <- c()
while (evenpos <= length(Arr))
{
a = Arr[evenpos]
  if (a%%2 == 0)
  {
  evenvec <- c(evenvec,Arr[evenpos])
  }
evenpos = evenpos + 1
}

sum(evenvec)


**********
Problem #3:
What is the largest prime factor of the number 600851475143 ?
#Solived using R

#Create isPrime function
isPrime <- function(x)
{
a=2
store <- c()
while (a<=sqrt(x))
{
if (x%%a == 0)
	{
	store <- c(store,a)
	break
	}
a = a+1
}
(if ((sum(store)>0)==TRUE)
{0}
else
{1})
}

#define variables
factors <- c()
primes <- c()
n = 600851475143

#Sampling the largest factors of n
div <- c(4601:5200)
b=1
mod <- c()
while (b<=length(div))
{
mod <- c(mod,n%%div[b])
b=b+1
}
mod

(n/2)-sqrt(n)

#isPrime(5000004401)
isPrime(n/1471)
#identiry factors (largest to smallest), test isPrime
test = floor(n/2)
#while (length(primes)==0)
while (test>(n/2-10000000000))
{
if (n%%test==0)
	{
	factors <- c(factors,test)
	}
test = test - 1
}
factors
#if (isPrime(factors[length(factors)] == 1))
#	{
#	primes <- c(primes,test)
#	} 
test = test-1
}

c=2
facts <- c()
while (c<=sqrt(n))
{
if (n%%c == 0)
	{
	facts <- c(facts, n/c)
	}
c=c+1
}
facts
isPrime(facts[1])

smallfacts <-c(n/facts)
isPrime(smallfacts[7])
smallfacts[4]

*******
Problem #4: Find the largest palindrome made from the product of two 3-digit numbers.
*Solved using R

#Create vector of products, sort high to low
a1 <- c(rev(100:999))
b = 999
m <- c()
while (b>99)
{
m <- c(m,b*a1)
b = b-1
}
m <- sort(m, decreasing = TRUE)

#Create functions to split the products, reverse the latter segment
mirrorDos <- function (c)
{
end=floor(c*.1)
first=c-(end*10)
rev=(first*10)+end
rev
}
#
mirrorTres <- function(d)
{
end=floor((d)*.01)
mid=floor((d-(end*100))*.1)
first=d-(end*100)-(mid*10)
rev=(first*100)+(mid*10)+end
rev
}
#
isPal6 <- function(n)
{
l=nchar(n, type = "chars", allowNA = FALSE, keepNA = NA)
firstT <- substr(n,1,3)
lastT <- substr(n,4,6)
first3 <- as.numeric(firstT)
last3 <- as.numeric(lastT)
if (mirrorTres(last3) == first3)
{1}
else {0}
}
#
isPal5 <- function(n)
{
l=nchar(n, type = "chars", allowNA = FALSE, keepNA = NA)
firstD <- substr(n,1,2)
lastD <- substr(n,4,5)
first2 <- as.numeric(firstD)
last2 <- as.numeric(lastD)
if (mirrorDos(last2) == first2)
{1}
else {0}
}

#Run Test

x=1
isPal = 0
while (isPal <1)
{
isPal = isPal6(m[x])
x= x+1
}
m[x-1]


********
