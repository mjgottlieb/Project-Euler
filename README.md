# Project-Euler
Project Euler problems solved using R & Python

**********
Problem #1: <br />
Find the sum of all the multiples of 3 or 5 below 1000. 


Answer: <br />
*Solved using R

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


**********
Problem #2: <br />
By considering the terms in the Fibonacci sequence whose values do not exceed four million, find the sum of the even-valued terms

Answer: <br />
*Solved using R

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


**********
Problem #3:
What is the largest prime factor of the number 600851475143 ?

Answer:
Solived using R

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
if (isPrime(factors[length(factors)] == 1))
	{
	primes <- c(primes,test)
	} 
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
Problem #5: What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?

Answer:
*Solved using R

div <- c(1:20)
mod <- c(1)
n = 2520


while (sum(mod)>0)
{
mod <- c(n%%div)
n=n+20
}
n


********
Problem #6: The sum of the squares of the first ten natural numbers is,

12 + 22 + ... + 102 = 385
The square of the sum of the first ten natural numbers is,

(1 + 2 + ... + 10)2 = 552 = 3025
Hence the difference between the sum of the squares of the first ten natural numbers and the square of the sum is 3025 − 385 = 2640.

Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum.

Answer:
*Solved using R

n <- c(1:100)

((sum(n))^2)-sum(n^2)



********
Problem #7: By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13.

What is the 10 001st prime number?

Answer:

#isPrime function
isPrime <- function(x)
{
a=2
store <- c()
while (a<(x/2))
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
)

count = 0
x=3
while (count < 10001)
{
if (isPrime(x) == 1)
	{count = count + 1}
x=x+1
}
x-1
isPrime(x-1)
count



********
Problem #10: The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.

Find the sum of all the primes below two million.

Answer:
*Solved using R

#Utilitzes "isPrime()" function from problem #3
n=2
store <- c()
while (n<2000000)
{if (isPrime(n)==1)
{store <- c(store,n)}
n=n+1}
store
sum(store)



********
Problem #11: In the 20×20 grid below, four numbers along a diagonal line have been marked in red.

08 02 22 97 38 15 00 40 00 75 04 05 07 78 52 12 50 77 91 08
49 49 99 40 17 81 18 57 60 87 17 40 98 43 69 48 04 56 62 00
81 49 31 73 55 79 14 29 93 71 40 67 53 88 30 03 49 13 36 65
52 70 95 23 04 60 11 42 69 24 68 56 01 32 56 71 37 02 36 91
22 31 16 71 51 67 63 89 41 92 36 54 22 40 40 28 66 33 13 80
24 47 32 60 99 03 45 02 44 75 33 53 78 36 84 20 35 17 12 50
32 98 81 28 64 23 67 10 26 38 40 67 59 54 70 66 18 38 64 70
67 26 20 68 02 62 12 20 95 63 94 39 63 08 40 91 66 49 94 21
24 55 58 05 66 73 99 26 97 17 78 78 96 83 14 88 34 89 63 72
21 36 23 09 75 00 76 44 20 45 35 14 00 61 33 97 34 31 33 95
78 17 53 28 22 75 31 67 15 94 03 80 04 62 16 14 09 53 56 92
16 39 05 42 96 35 31 47 55 58 88 24 00 17 54 24 36 29 85 57
86 56 00 48 35 71 89 07 05 44 44 37 44 60 21 58 51 54 17 58
19 80 81 68 05 94 47 69 28 73 92 13 86 52 17 77 04 89 55 40
04 52 08 83 97 35 99 16 07 97 57 32 16 26 26 79 33 27 98 66
88 36 68 87 57 62 20 72 03 46 33 67 46 55 12 32 63 93 53 69
04 42 16 73 38 25 39 11 24 94 72 18 08 46 29 32 40 62 76 36
20 69 36 41 72 30 23 88 34 62 99 69 82 67 59 85 74 04 36 16
20 73 35 29 78 31 90 01 74 31 49 71 48 86 81 16 23 57 05 54
01 70 54 71 83 51 54 69 16 92 33 48 61 43 52 01 89 19 67 48

The product of these numbers is 26 × 63 × 78 × 14 = 1788696.

What is the greatest product of four adjacent numbers in the same direction (up, down, left, right, or diagonally) in the 20×20 grid?

Answer:
*Solved using R

a = matrix (c(08,02,22,97,38,15,00,40,00,75,04,05,07,78,52,12,50,77,91,08,49,49,99,40,17,81,18,57,60,87,17,40,98,43,69,48,04,56,62,00,81,49,31,73,55,79,14,29,93,71,40,67,53,88,30,03,49,13,36,65,52,70,95,23,04,60,11,42,69,24,68,56,01,32,56,71,37,02,36,91, 22,31,16,71,51,67,63,89,41,92,36,54,22,40,40,28,66,33,13,80, 24,47,32,60,99,03,45,02,44,75,33,53,78,36,84,20,35,17,12,50, 32, 98, 81, 28, 64, 23, 67, 10, 26, 38, 40, 67, 59, 54, 70, 66, 18, 38, 64, 70, 67, 26, 20, 68, 02, 62, 12, 20, 95, 63, 94, 39, 63, 08, 40, 91, 66, 49, 94, 21, 24, 55, 58, 05, 66, 73, 99, 26, 97, 17, 78, 78, 96, 83, 14, 88, 34, 89, 63, 72, 21, 36, 23, 09, 75, 00, 76, 44, 20, 45, 35, 14, 00, 61, 33, 97, 34, 31, 33, 95, 78, 17, 53, 28, 22, 75, 31, 67, 15, 94, 03, 80, 04, 62, 16, 14, 09, 53, 56, 92, 16, 39, 05, 42, 96, 35, 31, 47, 55, 58, 88, 24, 00, 17, 54, 24, 36, 29, 85, 57, 86, 56, 00, 48, 35, 71, 89, 07, 05, 44, 44, 37, 44, 60, 21, 58, 51, 54, 17, 58, 19, 80, 81, 68, 05, 94, 47, 69, 28, 73, 92, 13, 86, 52, 17, 77, 04, 89, 55, 40, 04, 52, 08, 83, 97, 35, 99, 16, 07, 97, 57, 32, 16, 26,26,79, 33, 27, 98, 66, 88, 36, 68, 87, 57, 62, 20, 72, 03, 46, 33, 67, 46, 55, 12, 32, 63, 93, 53, 69, 04, 42, 16, 73, 38, 25, 39, 11, 24, 94, 72, 18, 08, 46, 29, 32, 40, 62, 76, 36, 20, 69, 36, 41, 72, 30, 23, 88, 34, 62, 99, 69, 82, 67, 59, 85, 74, 04, 36, 16, 20, 73, 35, 29, 78, 31, 90, 01, 74, 31, 49, 71, 48, 86, 81, 16, 23, 57, 05, 54, 01, 70, 54, 71, 83, 51, 54, 69, 16, 92, 33, 48, 61, 43, 52, 01, 89, 19, 67, 48),nrow=20,ncol=20)
grid = t(a)
grid[3,4]

#rows max
c=1
r=1
rmax = 0
while (r<=20)
{
while (c<=17)
{
test = grid[r,c]*grid[r,c+1]*grid[r,c+2]*grid[r,c+3] 
if (test > rmax) {rmax = test}
c=c+1
}
r=r+1
c=1
}
rmax

#columns max
c=1
r=1
cmax = 0
while (c<=20)
{
while (r<=17)
{
test = grid[r,c]*grid[r+1,c]*grid[r+2,c]*grid[r+3,c] 
if (test > cmax) {cmax = test}
r=r+1
}
c=c+1
r=1
}
cmax

#diagonal down max
c=1
r=1
ddmax = 0
while (r<=17)
{
while (c<=17)
{
test = grid[r,c]*grid[r+1,c+1]*grid[r+2,c+2]*grid[r+2,c+3] 
if (test > ddmax) {ddmax = test}
c=c+1
}
r=r+1
c=1
}
ddmax

#diagonal up max
c=1
r=4
dumax = 0
while (r<=20)
{
while (c<=17)
{
test = grid[r,c]*grid[r-1,c+1]*grid[r-2,c+2]*grid[r-3,c+3] 
if (test > dumax) {dumax = test}
c=c+1
}
r=r+1
c=1
}
dumax

allthree <- c(rmax, cmax, ddmax, dumax)
max(allthree)



********
Problem #12: The sequence of triangle numbers is generated by adding the natural numbers. So the 7th triangle number would be 1 + 2 + 3 + 4 + 5 + 6 + 7 = 28. The first ten terms would be:

1, 3, 6, 10, 15, 21, 28, 36, 45, 55, ...

Let us list the factors of the first seven triangle numbers:

 1: 1
 3: 1,3
 6: 1,2,3,6
10: 1,2,5,10
15: 1,3,5,15
21: 1,3,7,21
28: 1,2,4,7,14,28
We can see that 28 is the first triangle number to have over five divisors.

What is the value of the first triangle number to have over five hundred divisors?

Answer:
*Solved using R

trinum = c(1)
n=2
while (n<2000000)
{trinum <- c(trinum,trinum[n-1]+n)
n=n+1}
trinum


count = 0
check = 1
x=1
while (x<=length(trinum))
{while (check <= sqrt(trinum[x]))
{if (trinum[x]%%check == 0)
{count = count+1}
if (count>250) 
{break
x}
check = check +1}
x=x+1}

trinum[n-1]



********
Problem #19: You are given the following information, but you may prefer to do some research for yourself.

1 Jan 1900 was a Monday.
Thirty days has September,
April, June and November.
All the rest have thirty-one,
Saving February alone,
Which has twenty-eight, rain or shine.
And on leap years, twenty-nine.
A leap year occurs on any year evenly divisible by 4, but not on a century unless it is divisible by 400.
How many Sundays fell on the first of the month during the twentieth century (1 Jan 1901 to 31 Dec 2000)?

Answer:
*Solved using R

m1 <- c(1:31)
m2 <- c(1:28)
m2l <- c(1:29)
m3 <- c(1:31)
m4 <- c(1:30)
m5 <- c(1:31)
m6 <- c(1:30)
m7 <- c(1:31)
m8 <- c(1:31)
m9 <- c(1:30)
m10 <- c(1:31)
m11 <- c(1:30)
m12 <- c(1:31)

yearReg <- c(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12)
yearLeap <- c(m1,m2l,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12)
days <- c(1:7)
dow <- c(1:7)

dates <- c(yearReg)
yr = 1901
while (yr <= 2000)
{if (yr%%4 == 0)
  {dates <- c(dates,yearLeap)}
 else {dates <- c(dates,yearReg)}
yr = yr+1
}
length(dates)
while (length(days) < length(dates))
{days <- c(days,dow)}
length(days)
dates[366]
#datesArray
#days

#test <- c(datesArray * days)
#length(test)

n=366
sun1=0
while (n<=length(days))
{if (days[n] == 7)
   {if (dates[n] == 1)
    {sun1 = sun1 + 1}}
n = n+1
}
sun1
