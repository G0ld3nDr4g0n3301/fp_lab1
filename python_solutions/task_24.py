import math


def solve_permutation(target_index, digits):
    n = len(digits)
    available_digits = list(digits) 
    permutation = []
    
    current_index = target_index

    for i in range(1, n + 1):
        remaining_count = n - i
        if remaining_count == 0:
            factorial = 1
        else:
            factorial = math.factorial(remaining_count)

        digit_index = current_index // factorial

        chosen_digit = available_digits[digit_index]
        permutation.append(chosen_digit)

        available_digits.pop(digit_index)

        current_index %= factorial

        if current_index == 0 and available_digits:
            permutation.extend(available_digits)
            break

    return "".join(permutation)

N = 1000000
initial_digits = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']
result = solve_permutation(N - 1, initial_digits)

print(f"The {N}th lexicographic permutation is: {result}")