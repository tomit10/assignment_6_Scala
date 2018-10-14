// Advanced Part 3 about a really dumb investment strategy
//==========================================================

object CW6c {


//two test portfolios

val blchip_portfolio = List("GOOG", "AAPL", "MSFT", "IBM", "FB", "AMZN", "BIDU")
val rstate_portfolio = List("PLD", "PSA", "AMT", "AIV", "AVB", "BXP","CCI", 
                            "DLR", "EQIX", "EQR", "ESS", "EXR", "FRT", "GGP", "HCP") 


// (1.a) The function below takes a stock symbol and a year as arguments.
//       It should read the corresponding CSV-file and read the January 
//       data from the given year. The data should be collected in a list of
//       strings for each line in the CSV-file.

import io.Source
import scala.util._

//def get_january_data(symbol: String, year: Int) : List[String] = ...
def get_january_data(symbol: String, year: Int) : List[String] ={
    val eF=(Source.fromFile(symbol+".csv").getLines).toList
    val yString= year.toString
    //val secondL1=for(n<-eF; if(n._1.startsWith(yString))) yield n
    val h= eF.filter(x=>x.startsWith(yString))
    h
}
// (1.b) From the output of the get_january_data function, the next function 
//       should extract the first line (if it exists) and the corresponding
//       first trading price in that year as Option[Double]. If no line is 
//       generated by get_january_data then the result is None


//def get_first_price(symbol: String, year: Int) : Option[Double] = ...
def get_first_price(symbol: String, year: Int) : Option[Double] ={

    val tempJ=get_january_data(symbol,year)
    if(tempJ.isEmpty)
    {
        None
    }
    else
    {
        //val e=tempJ.sortBy(_._1)
        val t=tempJ.head
        val t1=t.split(",")
        val t2=(t1(1)).toDouble
        Option(t2)
    }
}


// (1.c) Complete the function below that obtains all first prices
//       for the stock symbols from a portfolio (list of strings) and 
//       for the given range of years. The inner lists are for the
//       stock symbols and the outer list for the years.


//def get_prices(portfolio: List[String], years: Range) : List[List[Option[Double]]] = ...
def get_prices(portfolio: List[String], years: Range) : List[List[Option[Double]]] ={
val yL=years.toList
val t=for(y<-yL) yield {
     for(p<-portfolio) yield get_first_price(p,y)
  }
t
}



// (2) The first function below calculates the change factor (delta) between
//     a price in year n and a price in year n + 1. The second function calculates
//     all change factors for all prices (from a portfolio). The input to this
//     function are the nested lists created by get_prices above.

//def get_delta(price_old: Option[Double], price_new: Option[Double]) : Option[Double] = ...
def get_delta(price_old: Option[Double], price_new: Option[Double]) : Option[Double] ={
    if(price_old==None&&price_new==None)
    {
        None
    }
    else
    {
        val delta=((price_new.get-price_old.get)/price_old.get)
        Option(delta)
    }
}

//def get_deltas(data: List[List[Option[Double]]]) :  List[List[Option[Double]]] = ...
def get_deltas(data: List[List[Option[Double]]]) :  List[List[Option[Double]]] ={
for(t1<-(0 to (data.size)-2).toList) yield
for(v2<-(0 to (data(t1).size)-1).toList) yield get_delta(data(t1)(v2), data(t1+1)(v2))
}



// (3) Write a function that given change factors, a starting balance and a year
//     calculates the yearly yield, i.e. new balance, according to our dump investment 
//     strategy. Another function calculates given the same data calculates the
//     compound yield up to a given year. Finally a function combines all 
//     calculations by taking a portfolio, a range of years and a start balance
//     as arguments.

//def yearly_yield(data: List[List[Option[Double]]], balance: Long, year: Int) : Long = ... 

//def compound_yield(data: List[List[Option[Double]]], balance: Long, year: Int) : Long = ... 

//def investment(portfolio: List[String], years: Range, start_balance: Long) : Long = ...


//test cases for the two portfolios given above

//investment(rstate_portfolio, 1978 to 2017, 100)
//investment(blchip_portfolio, 1978 to 2017, 100)

}
