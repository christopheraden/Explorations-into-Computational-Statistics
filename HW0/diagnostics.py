#1. Write a program that prints the numbers from 1 to 100.
#But for multiples of three print "Fizz" instead of the
#number and for the multiples of five print "Buzz".
#For numbers which are multiples of both three and
#five print "FizzBuzz".


def fizzbuzz(n):
    if isinstance(n, (int, long)) is False:
        print "n is not of type integer"
        return 1
            
    for num in range(1, n+1):
        if num % 3 == 0:
            if num % 5 == 0:
                print "FizzBuzz"
            else:
                print "Fizz"
        elif num % 5 == 0:
            print "Buzz"
        else:
            print num
    return 0
#fizzbuzz(100)

#2. Write a program that generates 1000 uniform random numbers
#between 0 and ![equation](http://latex.codecogs.com/gif.latex?2%5Cpi)
#(call this ![equation](http://latex.codecogs.com/gif.latex?x)),
#and 1000 uniform random
#numbers between 0 and 1 (call this ![equation](http://latex.codecogs.com/gif.latex?y)).
#You will then have
#10,000 pairs of random numbers.
#Transform ![equation](http://latex.codecogs.com/gif.latex?%28x%2Cy%29) to
# ![equation](http://latex.codecogs.com/gif.latex?%28u%2Cv%29) where:
#![equation](http://latex.codecogs.com/gif.latex?u%3Dy*%5Ccos%28x%29%2C), and,
#![equation](http://latex.codecogs.com/gif.latex?v%3Dy*%5Csin%28x%29%2C)
#<!--
#$$
#u = y * \cos(x) , \qquad
#v = y * \sin(x)
#$$
#-->
#Make a 2D scatterplot of the 10,000 (u,v) pairs.
#What is the distribution of:  ![equation](http://latex.codecogs.com/gif.latex?r%3D%5Csqrt%28u%5E2%2Bv%5E2%29)?

import numpy as np
import matplotlib.pyplot as pp
from math import pi


def circle_maker(n):
    if isinstance(n, (int, long)) is False:
        print "n is not of type integer"
        return 1

    x = np.random.uniform(0, 2*pi, n)
    y = np.random.uniform(0, 1, n)

    u = np.kron(y, np.cos(x))
    v = np.kron(y, np.sin(x))
    pp.plot(u,v)
    pp.axis([-1,1,-1,1])
    pp.show()
    return 0
#circle_maker(100)

#3. Consider the following snippet:
#
#"Hello, my name is Bob. I am a statistician. I like statistics very much."
#
#a. Write a program to spit out every character in the snippet
#to a separate file (i.e., file `out_01.txt` would contain the character `H`,
#file `out_02.txt` would contain `e` etc.). Note that the `,` and spaces
#should also get their own files.
#
#b. Write a program to combine all files back together into a single file
#that contains the original sentence. **Take care to respect whitespace
#and punctuation!**

def filer(string):
     #Coerce input to string, regardless of input type.
    string = str(string)
    #Determine number of digits.
    digits = len(str(len(string)))
    i = 1
    for letter in string:
        filename = "out_" + str(i).zfill(digits) + ".txt"
        f = open(filename, 'w')
        f.write(letter)
        f.close()
        i += 1
    return 0
#filer('Hello, my name is Bob. I am a statistician. I like statistics very much.')

def defiler(f_dir='./', f_prefix='out_', f_suffix='txt', out_name='out.txt'):
    string = ''
    import glob
    for file in glob.glob(f_dir+f_prefix+'*.'+f_suffix):
        f = open(file)
        string += f.read()
        f.close()
    f = open(out_name, 'w')
    f.write(string)
    f.close()
    return 0
#defiler()

import numpy as np
import matplotlib.pyplot as plt

def AR1(n, rho):
    eps = np.random.normal(size=n)
    y = np.empty(n)
    y[0] = 0
    for i in xrange(1, n-1):
        y[i] = rho * y[i-1] + eps[i]
    return y
y = AR1(1000, .5)
plt.plot(xrange(1000), y)
plt.title("Plot of AR(1) Time Series with Rho=0.5")
plt.show()

Reps = [np.array(AR1(1000,.5)) for i in xrange(200)]
Reps_matrix = np.asmatrix(Reps).transpose()

Yi_means = Reps_matrix.mean(axis=1).ravel().tolist()[0]
plt.plot(xrange(1000), Yi_means)
plt.title("Plot of  mean(Y_i), i=1,...,1000, from an AR(1) with Rho=0.5, 200 reps")
plt.show()

Yi_vars = Reps_matrix.var(axis=1).ravel().tolist()[0]
plt.plot(xrange(1000), Yi_vars)
plt.title("Plot of  var(Y_i), i=1,...,1000, from an AR(1) with Rho=0.5, 200 reps")
plt.show()

Rep_means = Reps_matrix.mean(axis=0).ravel().tolist()[0]
plt.plot(xrange(200), Rep_means)
plt.title("Plot of the grand mean of the whole time series, from an AR(1) with Rho=0.5, 200 reps")
plt.show()

Rep_vars = Reps_matrix.var(axis=0).ravel().tolist()[0]
plt.plot(xrange(200), Rep_vars)
plt.title("Plot of the grand variance of the whole time series from an AR(1) with Rho=0.5, 200 reps")
plt.show()

