object Q3{
    def main(args: Array[String]): Unit = {
        var primeList: List[Int] = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10).filter(_ > 1);
        println(filterPrime(primeList))
    }

    def gcd(a:Int, b:Int):Int = b match{
        case x if (b == 0) => a;
        case x if (b > a) => gcd(b,a);
        case _ => gcd(b,a%b); 
    } 

    def prime(p:Int, n:Int = 2): Boolean = n match{
        case x if(p == n) => true;
        case x if(gcd(p, n) > 1) => false;
        case _=> prime(p, n+1);
    }

    val filterPrime = (numbers: List[Int]) => {
        numbers.filter(num => prime(num));
    }

}