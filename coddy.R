is_weekend <- c(TRUE,  FALSE, FALSE, FALSE, FALSE, FALSE, TRUE)
class(is_weekend)

weather <- factor(c("Sunny", "Rainy", "Cloudy", "Sunny", "Rainy"))
class(weather)

weekend_true <- is_weekend==TRUE
sum(weekend_true)

levels(weather)

mixed_list <- list(42, "R programming", TRUE, c(1, 2, 3))
print(mixed_list[[4]][2])

new_list <- c(mixed_list, list(c("apple", "banana", "cherry")))
print(new_list)
length(new_list[[5]])





# ****************************************************************************************************
#data frames
books <- data.frame(
  title = c("The Great Gatsby", "To Kill a Mockingbird", "1984", "Pride and Prejudice"),
  author = c("F. Scott Fitzgerald", "Harper Lee", "George Orwell", "Jane Austen"),
  year = c(1925, 1960, 1949, 1813),
  pages = c(180, 281, 328, 432)
)

books$title
class((books$title))
books[1,2]
class(books[1,2])
books["title"]
class(books["title"])
summary(books)
str(books)
length(books)
nrow(books)
books$author[books$title=="1984"]
books$pages
average(sum(books$pages))
sum(books$pages)/4
books$title[which.min(books$year)]
books$language <- "english"
nrow(books)
ncol(books)




# ****************************************************************************************************
class(c(1,2,3))
b1 <- class(c(1,2,3))=="numeric"
b2 <- length(c(1,2))==2
b3 <- class(b1)!=class(b2)
b4 <- b1 & b2 & !b3
b4









# ****************************************************************************************************
fruits <- c("apple", "banana", "cherry")
for (fruit in fruits) {
  print(paste("I like", fruit))
}
fruits[1]
length(fruits)


for (i in 1:5) {
  print(i^2)
}

for (i in seq(2, 10, by = 2)) {
  print(i)
}

for (i in 1:3) {
  for (j in 1:3) {
    print(paste("i =", i, "j =", j))
  }
}




# ****************************************************************************************************
numbers <- seq(1, 5, by=1)
sum_even_squares <- 0
count_odd_squares <- 0

for(i in numbers) {
  squared <- i^2 
    if (squared %% 2 == 0) {
      sum_even_squares <- sum_even_squares + squared
    } else {
      count_odd_squares <- count_odd_squares + 1
    }
}

cat("Sum of even squares:", sum_even_squares, "\n")
cat("Count of odd squares:", count_odd_squares, "\n")



# ********************************************************************
factor_var <- factor(c("small", "medium", "large"))

is_weekend <- factor(c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE))
print(is_weekend)
weather <- factor(c("Sunny", "Rainy", "Cloudy", "Sunny", "Rainy"))
print(weather)
levels(weather)

sum(is_weekend=="TRUE")


# ********************************************************************
count <- 1
while (count <= 5) {
  print(count)
  count <- count + 1
}

i <- 1
while (TRUE) {
  if (i > 6) {
    break
  }
  print(i)
  i <- i + 1
}




# ********************************************************************
n <- 5
factorial <- 1
i <- 1

while (i<=n) {
  factorial <- factorial * i
  i <- i+1
}
print (factorial)




# ********************************************************************
n <- 10
sum_int <- 0
i <- 1

while(i <= n) {
  sum_int <- sum_int + i
  i <- i+1
}
print (sum_int)



# ********************************************************************
# Write a while loop in R that:
# Starts at 1
# Goes up to n
# Counts how many numbers are divisible by 3
# Prints the count

n <- 36
divisible <- 0
i <- 1

while (i <= n) {
  if (i%%3==0) {
    divisible <- divisible + 1
  }
    i <- i+1
}
print (divisible)



# ********************************************************************
# Write a while loop that:
# goes from 1 to n
# counts how many numbers are divisible by both 2 and 3
# prints the count

n <- 40
divisible <- 0
i <- 1

while (i <= n) {
  if (i%%3 == 0 && i%%2 == 0) {
    divisible <- divisible + 1
  } 
  i <- i+1
} 
print (divisible)