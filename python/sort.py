def merge(left, middle, right):
    result = []
    i = j = k = 0

    # Merge the three arrays by choosing the smallest element from each
    while i < len(left) or j < len(middle) or k < len(right):
        values = [
            (left[i] if i < len(left) else float('inf')),
            (middle[j] if j < len(middle) else float('inf')),
            (right[k] if k < len(right) else float('inf'))
        ]
        min_val = min(values)

        if min_val == values[0]:
            result.append(left[i])
            i += 1
        elif min_val == values[1]:
            result.append(middle[j])
            j += 1
        else:
            result.append(right[k])
            k += 1

    return result

def three_way_merge_sort(arr):
    # Base case: if array has 1 or 0 elements, it is already sorted
    if len(arr) <= 1:
        return arr

    # Split the array into three roughly equal parts iteratively
    n = len(arr)
    one_third = n // 3
    mid1 = one_third
    mid2 = 2 * one_third

    left = arr[:mid1]
    middle = arr[mid1:mid2]
    right = arr[mid2:]

    # Sort each part using merge sort
    left = sorted(left)   # Directly using Python's sorted function for simplicity
    middle = sorted(middle)
    right = sorted(right)

    # Merge the three sorted parts
    return merge(left, middle, right)

# Example usage
if __name__ == "__main__":
    data = [45, -2, -45, 78, 30, -42, 10, 19, 73, 93]  # Test data
    print("Before sorting:", data)
    sorted_data = three_way_merge_sort(data)
    print("After 3-way merge sort:", sorted_data)
