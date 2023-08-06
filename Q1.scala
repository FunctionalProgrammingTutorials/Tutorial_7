object Q1{
    def main (args: Array[String]): Unit = {
        var evenList: List[Int] = filterEvenNumbers(List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10));
        println(evenList);
    }

    val  filterEvenNumbers = (numbers :List[Int])=> {
        numbers.filter(num => num % 2 == 0);
    }

}