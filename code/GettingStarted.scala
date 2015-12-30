object MyModule {
    def abs(n: Int): Int =
        if (n < 0) -n
        else n

    private def formatAbs(x: Int) = {
        val msg = "The absolute value of %d is %d"
        msg.format(x, abs(x))
    }

    def fib(n: Int): Int = {
        @annotation.tailrec
        def loop(n: Int, n1: Int, n2: Int): Int =
            if (n == 0) n1
            else loop(now + 1, n2, n1 + n2)
        loop(n,0,1)
    }

    def factorial(n: Int): Int = {
        @annotation.tailrec
        def go(n: Int, acc: Int): Int = 
            if (n <= 0) acc
            else go(n-1, acc*n)
        go(n, 1)
    }
    
    def formatResult(name: String, n: Int, f: Int => Int) = {
        val msg = "The %s of %d is %d"
        msg.format(name, n, f(n))
    }

    def main(args: Array[String]): Unit = 
        println(formatResult("absolute value", -42, abs))
        println(formatResult("factorial", 7, factorial))
}