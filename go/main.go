package main

import (
	"fmt"
	"math"
	"math/rand"
	"runtime"
	"sync"
	"testing"
)

// Merge three sorted arrays
func mergeThreeSortedArrays(left, middle, right []int) []int {
	result := make([]int, 0, len(left)+len(middle)+len(right))
	i, j, k := 0, 0, 0

	for i < len(left) || j < len(middle) || k < len(right) {
		smallest := math.MaxInt
		if i < len(left) && left[i] < smallest {
			smallest = left[i]
		}
		if j < len(middle) && middle[j] < smallest {
			smallest = middle[j]
		}
		if k < len(right) && right[k] < smallest {
			smallest = right[k]
		}

		if i < len(left) && smallest == left[i] {
			result = append(result, left[i])
			i++
		} else if j < len(middle) && smallest == middle[j] {
			result = append(result, middle[j])
			j++
		} else if k < len(right) && smallest == right[k] {
			result = append(result, right[k])
			k++
		}
	}

	return result
}

// 3-way Merge Sort
func threeWayMergeSort(arr []int) []int {
	if len(arr) <= 1 {
		return arr
	}

	oneThird := int(math.Ceil(float64(len(arr)) / float64(3)))
	twoThird := 2 * oneThird

	left := arr[:oneThird]
	middle := arr[oneThird:twoThird]
	right := arr[twoThird:]

	left = threeWayMergeSort(left)
	middle = threeWayMergeSort(middle)
	right = threeWayMergeSort(right)

	return mergeThreeSortedArrays(left, middle, right)
}

// 3-way Merge Sort (Concurrent)
func threeWayMergeSortConc(arr []int) []int {
	if len(arr) <= 1 {
		return arr
	}

	if len(arr) <= 2048 {
		return threeWayMergeSort(arr)
	}

	oneThird := int(math.Ceil(float64(len(arr)) / float64(3)))
	twoThird := 2 * oneThird

	left := arr[:oneThird]
	middle := arr[oneThird:twoThird]
	right := arr[twoThird:]

	var wg sync.WaitGroup

	wg.Add(1)
	go func() {
		left = threeWayMergeSortConc(left)
		wg.Done()
	}()

	wg.Add(1)
	go func() {
		middle = threeWayMergeSortConc(middle)
		wg.Done()
	}()

	wg.Add(1)
	go func() {
		right = threeWayMergeSortConc(right)
		wg.Done()
	}()

	wg.Wait()

	return mergeThreeSortedArrays(left, middle, right)
}

func benchmarkThreeWayMergeSort() {
	sizes := []int{1000, 10000, 100000, 1000000}
	for _, size := range sizes {
		arr := make([]int, size)
		for i := 0; i < size; i++ {
			arr[i] = rand.Intn(1000) // Populate array with random values
		}
		res := testing.Benchmark(func(b *testing.B) {
			threeWayMergeSort(arr)
		})
		fmt.Printf("\n = Three Way Merge Sort - Size: %d - Total Elapsed Time: %d µs - Total Used Memory: %d bytes", size, res.T.Microseconds(), res.MemBytes)
	}
}

func benchmarkConcurrentThreeWayMergeSort() {
	sizes := []int{1000, 10000, 100000, 1000000}
	for _, size := range sizes {
		arr := make([]int, size)
		for i := 0; i < size; i++ {
			arr[i] = rand.Intn(1000) // Populate array with random values
		}
		res := testing.Benchmark(func(b *testing.B) {
			threeWayMergeSortConc(arr)
		})
		fmt.Printf("\n = Concurrent Three Way Merge Sort - Size: %d - Total Elapsed Time: %d µs - Total Used Memory: %d bytes", size, res.T.Microseconds(), res.MemBytes)
	}
}

func main() {
	fmt.Println("Number of CPUs:", runtime.NumCPU())
	fmt.Println("GOMAXPROCS:", runtime.GOMAXPROCS(0))

	benchmarkThreeWayMergeSort()
	benchmarkConcurrentThreeWayMergeSort()

	fmt.Println("\n\tOK")
}
