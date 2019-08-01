import sorting.Sorting
import sorting.Sorting.SortingMethod

object Main extends App {
	
	Sorting.TestSort(SortingMethod.Quick, 100000, Some(10))
	Sorting.TestSort(SortingMethod.Quick3Way, 100000, Some(10))
	//Sorting.TestSort(SortingMethod.Merge, 10000000)
	//Sorting.TestSort(SortingMethod.Shell, 10000000)


}

