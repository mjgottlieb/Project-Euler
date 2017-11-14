# Project-Euler
Project Euler problems solved using R & Python

**********
**Problem #1:** <br />
Find the sum of all the multiples of 3 or 5 below 1000. 


**Solution (Using R):**

Prob1 <- c()

a=0 <br />
b=0 <br />
n=1000 <br />
while (a <= n) <br />
{if (a%%3 == 0) <br />
{ <br />
Prob1 = c(Prob1,a) <br />
} <br />
if (b%%5 == 0) <br />
{if (b%%3 != 0) <br />
{Prob1 <- c(Prob1, b)}} <br />
a = a+1 <br />
b = b+1} <br />

sum(Prob1)

**Answer: 233,168**

**********
**Problem #2:** <br />
By considering the terms in the Fibonacci sequence whose values do not exceed four million, find the sum of the even-valued terms

**Solution (Using R):**

Arr <- c(1,2) <br />
pos = 3 <br />
n=Arr[pos-2]+Arr[pos-1]

while (n <= 4000000) <br />
{Arr <- c(Arr,n) <br />
pos = pos+1 <br />
n=Arr[pos-2]+Arr[pos-1]}

evenpos= 1 <br />
evenvec <- c() <br />
while (evenpos <= length(Arr)) <br />
{ <br />
a = Arr[evenpos] <br />
  if (a%%2 == 0) <br />
  { <br />
  evenvec <- c(evenvec,Arr[evenpos]) <br />
  } <br />
evenpos = evenpos + 1 <br />
}

sum(evenvec)

**Answer: 4,613,732**

**Answer: 6,857**

**********
**Problem #3:**
What is the largest prime factor of the number 600851475143 ?

**Solution (Using R):**

#Create isPrime function <br />
isPrime <- function(x) <br />
{ <br />
a=2 <br />
store <- c() <br />
while (a<=sqrt(x)) <br />
{ <br />
if (x%%a == 0) <br />
	{ <br />
	store <- c(store,a) <br />
	break <br />
	} <br />
a = a+1 <br />
} <br />
(if ((sum(store)>0)==TRUE) <br />
{0} <br />
else <br />
{1}) <br />
}

#define variables <br />
factors <- c() <br />
primes <- c() <br />
n = 600851475143

#Sampling the largest factors of n <br />
div <- c(4601:5200) <br />
b=1 <br />
mod <- c() <br />
while (b<=length(div)) <br />
{ <br />
mod <- c(mod,n%%div[b]) <br />
b=b+1 <br />
} <br />
mod

(n/2)-sqrt(n)

#identiry factors (largest to smallest), test isPrime <br />
test = floor(n/2) <br />
#while (length(primes)==0) <br />
while (test>(n/2-10000000000)) <br />
{ <br />
if (n%%test==0) <br />
	{ <br />
	factors <- c(factors,test) <br />
	} <br />
test = test - 1 <br />
} <br />
factors <br />
if (isPrime(factors[length(factors)] == 1)) <br />
	{ <br />
	primes <- c(primes,test) <br />
	}  <br />
test = test-1 <br />
}

c=2 <br />
facts <- c() <br />
while (c<=sqrt(n)) <br />
{ <br />
if (n%%c == 0) <br />
	{ <br />
	facts <- c(facts, n/c) <br />
	} <br />
c=c+1 <br />
} <br />
facts <br />
isPrime(facts[1]) <br />

smallfacts <-c(n/facts) <br />
isPrime(smallfacts[7]) <br />
smallfacts[4]


*******
**Problem #4:** <br /> 
Find the largest palindrome made from the product of two 3-digit numbers.

**Solution (using R):**

#Create vector of products, sort high to low <br />
a1 <- c(rev(100:999)) <br />
b = 999 <br />
m <- c() <br />
while (b>99) <br />
{ <br />
m <- c(m,b*a1) <br />
b = b-1 <br />
} <br />
m <- sort(m, decreasing = TRUE)

#Create functions to split the products, reverse the latter segment <br />
mirrorDos <- function (c) <br />
{ <br />
end=floor(c*.1) <br />
first=c-(end*10) <br />
rev=(first*10)+end <br />
rev <br />
} <br />

mirrorTres <- function(d) <br />
{ <br />
end=floor((d)*.01) <br />
mid=floor((d-(end*100))*.1) <br />
first=d-(end*100)-(mid*10) <br />
rev=(first*100)+(mid*10)+end <br />
rev <br />
} <br />

isPal6 <- function(n) <br />
{ <br />
l=nchar(n, type = "chars", allowNA = FALSE, keepNA = NA) <br />
firstT <- substr(n,1,3) <br />
lastT <- substr(n,4,6) <br />
first3 <- as.numeric(firstT) <br />
last3 <- as.numeric(lastT) <br />
if (mirrorTres(last3) == first3) <br />
{1} <br />
else {0} <br />
}

isPal5 <- function(n) <br />
{ <br />
l=nchar(n, type = "chars", allowNA = FALSE, keepNA = NA) <br />
firstD <- substr(n,1,2) <br />
lastD <- substr(n,4,5) <br />
first2 <- as.numeric(firstD) <br />
last2 <- as.numeric(lastD) <br />
if (mirrorDos(last2) == first2) <br />
{1} <br />
else {0} <br />
}

#Run Test

x=1 <br />
isPal = 0 <br />
while (isPal <1) <br />
{ <br />
isPal = isPal6(m[x]) <br />
x= x+1 <br />
} <br />
m[x-1]

**Answer: 906,609**

********
**Problem #5:** <br /> 
What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?

**Solution (Using R):**

div <- c(1:20) <br />
mod <- c(1) <br />
n = 2520


while (sum(mod)>0) <br />
{ <br />
mod <- c(n%%div) <br />
n=n+20 <br />
} <br />
n

**Answer: 232,792,560**

********
**Problem #6:** <br /> 
The sum of the squares of the first ten natural numbers is 1^2 + 2^2 + ... + 10^2 = 385 <br />
The square of the sum of the first ten natural numbers is (1 + 2 + ... + 10)^2 = 552 = 3025 <br />
Hence the difference between the sum of the squares of the first ten natural numbers and the square of the sum is 3025 − 385 = 2640.

Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum.

**Solution (Using R):**

n <- c(1:100)

((sum(n))^2)-sum(n^2)

Answer: 25,164,150

********
Problem #7: <br />
By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13.

What is the 10 001st prime number?

**Solution (Using R):**

#isPrime function <br />
isPrime <- function(x) <br />
{ <br />
a=2 <br />
store <- c() <br />
while (a<(x/2)) <br />
{ <br />
if (x%%a == 0) <br />
	{ <br />
	store <- c(store,a) <br />
	break <br />
	} <br />
a = a+1 <br />
} <br />
(if ((sum(store)>0)==TRUE) <br />
{0} <br />
else <br />
{1}) <br />
} <br />
)

count = 0 <br />
x=3 <br />
while (count < 10001) <br />
{ <br />
if (isPrime(x) == 1) <br />
	{count = count + 1} <br />
x=x+1 <br />
} <br />
x-1 <br />
isPrime(x-1) <br />
count <br />

**Answer: 104,743**

********
**Problem #10:** <br />
The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.

Find the sum of all the primes below two million.

**Solution (Using R):**

#Utilitzes "isPrime()" function from problem #7 <br />
n=2 <br />
store <- c() <br />
while (n<2000000) <br />
{if (isPrime(n)==1) <br />
{store <- c(store,n)} <br />
n=n+1} <br />
store <br />
sum(store)

**Answer: 142,913,828,922**

********
**Problem #11:** <br /> 
In the 20×20 grid below, four numbers along a diagonal line have been marked in bold.

08 02 22 97 38 15 00 40 00 75 04 05 07 78 52 12 50 77 91 08 <br />
49 49 99 40 17 81 18 57 60 87 17 40 98 43 69 48 04 56 62 00 <br />
81 49 31 73 55 79 14 29 93 71 40 67 53 88 30 03 49 13 36 65 <br />
52 70 95 23 04 60 11 42 69 24 68 56 01 32 56 71 37 02 36 91 <br />
22 31 16 71 51 67 63 89 41 92 36 54 22 40 40 28 66 33 13 80 <br />
24 47 32 60 99 03 45 02 44 75 33 53 78 36 84 20 35 17 12 50 <br />
32 98 81 28 64 23 67 10 **26** 38 40 67 59 54 70 66 18 38 64 70 <br />
67 26 20 68 02 62 12 20 95 **63** 94 39 63 08 40 91 66 49 94 21 <br />
24 55 58 05 66 73 99 26 97 17 **78** 78 96 83 14 88 34 89 63 72 <br />
21 36 23 09 75 00 76 44 20 45 35 **14** 00 61 33 97 34 31 33 95 <br />
78 17 53 28 22 75 31 67 15 94 03 80 04 62 16 14 09 53 56 92 <br />
16 39 05 42 96 35 31 47 55 58 88 24 00 17 54 24 36 29 85 57 <br />
86 56 00 48 35 71 89 07 05 44 44 37 44 60 21 58 51 54 17 58 <br />
19 80 81 68 05 94 47 69 28 73 92 13 86 52 17 77 04 89 55 40 <br />
04 52 08 83 97 35 99 16 07 97 57 32 16 26 26 79 33 27 98 66 <br />
88 36 68 87 57 62 20 72 03 46 33 67 46 55 12 32 63 93 53 69 <br />
04 42 16 73 38 25 39 11 24 94 72 18 08 46 29 32 40 62 76 36 <br />
20 69 36 41 72 30 23 88 34 62 99 69 82 67 59 85 74 04 36 16 <br />
20 73 35 29 78 31 90 01 74 31 49 71 48 86 81 16 23 57 05 54 <br />
01 70 54 71 83 51 54 69 16 92 33 48 61 43 52 01 89 19 67 48 <br />

The product of these numbers is 26 × 63 × 78 × 14 = 1788696.

What is the greatest product of four adjacent numbers in the same direction (up, down, left, right, or diagonally) in the 20×20 grid?

**Solution (Using R):**

a = matrix (c(08,02,22,97,38,15,00,40,00,75,04,05,07,78,52,12,50,77,91,08, <br /> 49,49,99,40,17,81,18,57,60,87,17,40,98,43,69,48,04,56,62,00, <br /> 
81,49,31,73,55,79,14,29,93,71,40,67,53,88,30,03,49,13,36,65, <br /> 
52,70,95,23,04,60,11,42,69,24,68,56,01,32,56,71,37,02,36,91, <br />
22,31,16,71,51,67,63,89,41,92,36,54,22,40,40,28,66,33,13,80, <br />
24,47,32,60,99,03,45,02,44,75,33,53,78,36,84,20,35,17,12,50, <br />
32,98,81,28,64,23,67,10,26,38,40,67,59,54,70,66,18,38,64,70, <br />
67,26,20,68,02,62,12,20,95,63,94,39,63,08,40,91,66,49,94,21, <br />
24,55,58,05,66,73,99,26,97,17,78,78,96,83,14,88,34,89,63,72, <br />
21,36,23,09,75,00,76,44,20,45,35,14,00,61,33,97,34,31,33,95, <br />
78,17,53,28,22,75,31,67,15,94,03,80,04,62,16,14,09,53,56,92, <br /> 
16,39,05,42,96,35,31,47,55,58,88,24,00,17,54,24,36,29,85,57, <br />
86,56,00,48,35,71,89,07,05,44,44,37,44,60,21,58,51,54,17,58, <br />
19,80,81,68,05,94,47,69,28,73,92,13,86,52,17,77,04,89,55,40, <br />
04,52,08,83,97,35,99,16,07,97,57,32,16,26,16,79,33,27,98,66, <br />
88,36,68,87,57,62,20,72,03,46,33,67,46,55,12,32,63,93,53,69, <br />
04,42,16,73,38,25,39,11,24,94,72,18,08,46,29,32,40,62,76,36, <br />
20,69,36,41,72,30,23,88,34,62,99,69,82,67,59,85,74,04,36,16, <br />
20,73,35,29,78,31,90,01,74,31,49,71,48,86,81,16,23,57,05,54, <br />
01,70,54,71,83,51,54,69,16,92,33,48,61,43,52,01,89,19,67,48),nrow=20,ncol=20) <br />
grid = t(a)

#rows max <br />
c=1 <br />
r=1 <br />
rmax = 0 <br />
while (r<=20) <br />
{ <br />
while (c<=17) <br />
{ <br />
test = grid[r,c]*grid[r,c+1]*grid[r,c+2]*grid[r,c+3] <br /> 
if (test > rmax) {rmax = test} <br />
c=c+1 <br />
} <br />
r=r+1 <br />
c=1 <br />
} <br />

#columns max <br />
c=1 <br />
r=1 <br />
cmax = 0 <br />
while (c<=20) <br />
{ <br />
while (r<=17) <br />
{ <br />
test = grid[r,c]*grid[r+1,c]*grid[r+2,c]*grid[r+3,c] <br /> 
if (test > cmax) {cmax = test} <br />
r=r+1 <br />
} <br />
c=c+1 <br />
r=1 <br />
} <br />


#diagonal down max <br />
c=1 <br />
r=1 <br />
ddmax = 0 <br />
while (r<=17) <br />
{ <br />
while (c<=17) <br />
{ <br />
test = grid[r,c]*grid[r+1,c+1]*grid[r+2,c+2]*grid[r+2,c+3]  <br />
if (test > ddmax) {ddmax = test} <br />
c=c+1 <br />
} <br />
r=r+1 <br />
c=1 <br />
} <br />


#diagonal up max <br />
c=1 <br />
r=4 <br />
dumax = 0 <br />
while (r<=20) <br />
{ <br />
while (c<=17) <br />
{ <br />
test = grid[r,c]*grid[r-1,c+1]*grid[r-2,c+2]*grid[r-3,c+3]  <br />
if (test > dumax) {dumax = test} <br />
c=c+1 <br />
} <br />
r=r+1 <br />
c=1 <br />
} <br />


allthree <- c(rmax, cmax, ddmax, dumax) <br />
max(allthree) <br />

**Answer: 70,600,674**

********
**Problem #12:** <br />
The sequence of triangle numbers is generated by adding the natural numbers. So the 7th triangle number would be 1 + 2 + 3 + 4 + 5 + 6 + 7 = 28. The first ten terms would be:

1, 3, 6, 10, 15, 21, 28, 36, 45, 55, ...

Let us list the factors of the first seven triangle numbers:

 1: 1 <br />
 3: 1,3 <br />
 6: 1,2,3,6 <br />
10: 1,2,5,10 <br />
15: 1,3,5,15 <br />
21: 1,3,7,21 <br />
28: 1,2,4,7,14,28 <br />
We can see that 28 is the first triangle number to have over five divisors.

What is the value of the first triangle number to have over five hundred divisors?

**Solution (Using R):**

trinum = c(1) <br />
n=2 <br />
while (n<2000000) <br />
{trinum <- c(trinum,trinum[n-1]+n) <br />
n=n+1} <br />

count = 0 <br />
check = 1 <br />
x=1 <br />
while (x<=length(trinum)) <br />
{while (check <= sqrt(trinum[x])) <br />
{if (trinum[x]%%check == 0) <br />
{count = count+1} <br />
if (count>250)  <br />
{break <br />
x} <br />
check = check +1} <br />
x=x+1}

trinum[n-1]

**Answer: **

********
**Problem #19:** <br /> 
You are given the following information, but you may prefer to do some research for yourself.

1 Jan 1900 was a Monday. <br />
Thirty days has September, <br />
April, June and November. <br />
All the rest have thirty-one, <br />
Saving February alone, <br />
Which has twenty-eight, rain or shine. <br />
And on leap years, twenty-nine. <br />
A leap year occurs on any year evenly divisible by 4, but not on a century unless it is divisible by 400. <br />
How many Sundays fell on the first of the month during the twentieth century (1 Jan 1901 to 31 Dec 2000)?

**Solution (Using R):

m1 <- c(1:31) <br />
m2 <- c(1:28) <br />
m2a <- c(1:29) <br />
m3 <- c(1:31) <br />
m4 <- c(1:30) <br />
m5 <- c(1:31) <br />
m6 <- c(1:30) <br />
m7 <- c(1:31) <br />
m8 <- c(1:31) <br />
m9 <- c(1:30) <br />
m10 <- c(1:31) <br />
m11 <- c(1:30) <br />
m12 <- c(1:31)

yearReg <- c(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12) <br />
yearLeap <- c(m1,m2a,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12) <br />
days <- c(1:7) <br />
dow <- c(1:7)

dates <- c(yearReg) <br />
yr = 1901 <br />
while (yr <= 2000) <br />
{if (yr%%4 == 0) <br />
  {dates <- c(dates,yearLeap)} <br />
 else {dates <- c(dates,yearReg)} <br />
yr = yr+1 <br />
} <br />
length(dates) <br />
while (length(days) < length(dates)) <br />
{days <- c(days,dow)} <br />
length(days) <br />
dates[366] <br />

n=366 <br />
sun1=0 <br />
while (n<=length(days)) <br />
{if (days[n] == 7) <br />
   {if (dates[n] == 1) <br />
    {sun1 = sun1 + 1}} <br />
n = n+1 <br />
} <br />
sun1

**Answer: 171** 
