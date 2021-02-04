(**
 ** 31. Determine whether a given integer number is prime. (medium)
 **)
val is_prime: int -> bool


(**
 ** 32. Determine the greatest common divisor of two positive integer
 ** numbers. (medium)
 **
 ** Use Euclid's algorithm.
 **)
val gcd: int -> int -> int


(**
 ** 33. Determine whether two positive integer numbers are coprime. (easy)
 **
 ** Two numbers are coprime if their greatest common divisor equals 1.
 **)
val coprime: int -> int -> bool


(**
 ** 34. Calculate Euler's totient function φ(m). (medium)
 **
 ** Euler's so-called totient function φ(m) is defined as the number of positive integers r (1 ≤ r < m) that are coprime to m. We let φ(1) = 1.
 ** 
 ** Find out what the value of φ(m) is if m is a prime number. Euler's totient function plays an important role in one of the most widely used public key cryptography methods (RSA). In this exercise you should use the most primitive method to calculate this function (there are smarter ways that we shall discuss later).
 **
 **)
val phi: int -> int


(**
 ** 35. Determine the prime factors of a given positive integer. (medium)
 **
 ** Construct a flat list containing the prime factors in ascending order.
 **)
val factors: int -> int list

(**
 ** 36. Determine the prime factors of a given positive integer (2). (medium)
 **
 ** Construct a list containing the prime factors and their multiplicity.
 ** Hint: The problem is similar to problem Run-length encoding of a list (direct solution).
 **)
val factors2: int -> (int * int) list


(**
 ** 37. Calculate Euler's totient function φ(m) (improved). (medium)
 **
 ** See problem "Calculate Euler's totient function φ(m)" for the definition of Euler's totient function. If the list of the prime factors of a number m is known in the form of the previous problem then the function phi(m) can be efficiently calculated as follows: Let [(p1, m1); (p2, m2); (p3, m3); ...] be the list of prime factors (and their multiplicities) of a given number m. Then φ(m) can be calculated with the following formula:
 **
 ** φ(m) = (p1 - 1) × p1m1 - 1 × (p2 - 1) × p2m2 - 1 × (p3 - 1) × p3m3 - 1 × ⋯
 **)
val phi_improved: int -> int

(**
 ** 38. Compare the two methods of calculating Euler's totient function. (easy)
 **
 ** Use the solutions of problems "Calculate Euler's totient function φ(m)" and "Calculate Euler's totient function φ(m) (improved)" to compare the algorithms. Take the number of logical inferences as a measure for efficiency. Try to calculate φ(10090) as an example.
 **)
val timeit: (int -> int) -> int -> float


(**
 ** 39. A list of prime numbers. (easy)
 **
 ** Given a range of integers by its lower and upper limit, construct a list of all prime numbers in that range.
 **)
val all_primes: int -> int -> int list


(**
 ** 40. Goldbach's conjecture. (medium)
 **
 ** Goldbach's conjecture says that every positive even number greater than 2 is the sum of two prime numbers. Example: 28 = 5 + 23. It is one of the most famous facts in number theory that has not been proved to be correct in the general case. It has been numerically confirmed up to very large numbers. Write a function to find the two prime numbers that sum up to a given even integer.
 **)
val goldbach: int -> (int * int)


(**
 ** 41. A list of Goldbach compositions. (medium)
 **
 ** Given a range of integers by its lower and upper limit, print a list of all even numbers and their Goldbach composition.
 **
 ** In most cases, if an even number is written as the sum of two prime numbers, one of them is very small. Very rarely, the primes are both bigger than say 50. Try to find out how many such cases there are in the range 2..3000.
 **)
val goldbach_list: int -> int -> (int * (int * int)) list
