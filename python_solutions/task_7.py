import math

def is_prime(n):

    if n <= 1:
        return False
    for i in range(2, int(math.sqrt(n)) + 1):
        if n % i == 0:
            return False
    return True

def find_nth_prime(n):
    if n <= 0:
        raise ValueError("N must be natural number")

    prime_count = 0
    current_number = 1
    while prime_count < n:
        current_number += 1
        if is_prime(current_number):
            prime_count += 1
    return current_number

n = 10001
result = find_nth_prime(n)
print(f'{n}th prime number is {result}')