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
