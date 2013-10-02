#FizzBuzz
x = 1:100
y = as.character(x)
y [ x %% 3 == 0] = "Fizz"
y [ x %% 5 == 0] = "Buzz"
y [ x %% 15 == 0] = "FizzBuzz"

#Generates x,y
n=1000
x = 2*pi*runif(n)
y = runif(n)
u = kronecker(cos(x),y)
v = kronecker(sin(x),y)
jpeg(filename="UV_Plot.jpg",width=1000, height=1000)
plot(u, v)
dev.off()

r = sqrt(u^2 + v^2)

#Write a program to spit out every character in 
#the snippet to a separate file
string = "Hello, my name is Bob. I am a statistician. I like statistics very much."
letters = unlist(strsplit(string, ""))
max.digit = length(unlist(strsplit(as.character(length(letters)), "")))
for(i in 1:length(letters)) { write(x=letters[i], file=paste("out_", formatC(i, digits=max.digit, wid=max.digit, flag="0"), ".txt", sep="")) }
files = list.files(pattern="out_[0-9]+\\.txt")

#Write a program to combine all files back together into a single file 
import = sapply(files, function(f) {unlist(scan(f, what=character(), blank.lines.skip=FALSE))})
import[ import == ""] = " "
write(paste(import, collapse=""), file="out_final.txt")