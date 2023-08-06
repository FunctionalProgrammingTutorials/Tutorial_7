object Q3{
    def main(args: Array[String]): Unit = {
        var primeList: List[Int] = filterPrime(List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)).filter(_ != 1)
        println(primeList)
    }

    val filterPrime = (numbers: List[Int]) => {
        numbers.map(num => {
            var flag = 0;
            for(i <- 2 to num/2){
                if(num == 1 || num % i == 0){
                    flag = 1;
                }   
            }
            if(flag == 0){
                num;
            }
            else{
                0;
            }
        }).filter(_ != 0)
    }

}