import java.util.Date
import java.text.SimpleDateFormat
import java.text.DateFormat
import java.text.ParseException

import scala.collection.mutable.Map

object app extends App {

  case class Record(numberPlate: String, dateTime: Date, enter: Boolean, kilometers: Int)

  println("Welcome to the Scala worksheet")
  processInputFile(radixSort)("pruebas/1.txt")
  println("---------------------------------")
  processInputFile(quickSort)("pruebas/1.txt")
  
  val sortMe = "bla" :: "foo" :: "bar" :: Nil;
  val sortTwo = List("bla123", "foo145", "bar123", "145bla", "143bla", "10", "11")

  
  

  def quickSort(numberPlate: List[String]): List[String] = numberPlate match {
    case Nil => { Nil }
    case x :: Nil => x :: Nil
    case x :: xs => {
      val (smaller, bigger) = xs.partition(y => y < x)
      quickSort(smaller) ::: x :: quickSort(bigger)
    }
  }

  def radixSort(stringToSort: List[String]): List[String] = {
    def recRadixSort(charsToSort: List[Array[Char]], pos: Int, maxLength: Int): List[Array[Char]] = {
      val conversionTable = collection.immutable.HashMap(
        '0' -> 0, '1' -> 1, '2' -> 2, '3' -> 3, '4' -> 4, '5' -> 5, '6' -> 6,
        '7' -> 7, '8' -> 8, '9' -> 9, 'A' -> 10, 'B' -> 11, 'C' -> 12, 'D' -> 13,
        'E' -> 14, 'F' -> 15, 'G' -> 16, 'H' -> 17, 'I' -> 18, 'J' -> 19, 'K' -> 20,
        'L' -> 21, 'M' -> 22, 'N' -> 23, 'O' -> 24, 'P' -> 25, 'Q' -> 26, 'R' -> 27,
        'S' -> 28, 'T' -> 29, 'U' -> 30, 'V' -> 31, 'W' -> 32, 'X' -> 33, 'Y' -> 34, 'Z' -> 35)
        
      def innerBucketSort(charsToSort: List[Array[Char]], bucket: Array[List[Array[Char]]]): Array[List[Array[Char]]] = charsToSort match {
        case Nil => bucket
        case x :: xs =>
            val index = if ( x.length > pos ) conversionTable.getOrElse(x(pos).toUpper, 0)
                        else (0)
           if ( bucket(index) == null) bucket(index) = Nil
           bucket(index) = x :: bucket(index)
           innerBucketSort(xs, bucket)
      }
      if (pos < maxLength) {
        val bucket = innerBucketSort(charsToSort, new Array[List[Array[Char]]](36))
        val sortedList = bucket.filter(l => l != null ).flatten.toList
        recRadixSort(sortedList.reverse, pos + 1, maxLength)
      } else {
        charsToSort.reverse
      }
    }
    
    val charsToSort = stringToSort.map(s => s.toCharArray.reverse);
    val maxLength: Int = charsToSort.foldLeft(0)((i, s) => i max s.length)
    val reverseSorted = recRadixSort(charsToSort, 0, maxLength)
    reverseSorted.map(ca => String.valueOf(ca.reverse));
  }

  def processInputFile(sortFunction: List[String] => List[String])(fileName: String) {
    def parseFares(fare: String): Array[Int] = fare.split(" ").map(s => s.toInt)
    def parseRecords(lines: Iterator[String], n: Integer) = {
        val fares = parseFares(lines.next)
        def parseRecord(trips: List[String]) {
          if ( lines.hasNext ) {
        	  val nextTrip = lines.next.trim()
        	  if (nextTrip.length == 0) { calcFares(sortFunction)(fares, trips) }
        	  else parseRecord(nextTrip :: trips)
          } else {calcFares(sortFunction)(fares, trips)}
        }
        for ( i <- 0 until n) {
          parseRecord(Nil);
        }
    }

    val lines = scala.io.Source.fromFile(fileName).getLines
    val noOfTestCases = lines.next.toInt
    if (noOfTestCases > 0) {
      lines.next
      parseRecords(lines, noOfTestCases);
    }
  }

  def calcFares(sortFunction: List[String] => List[String])(fareTable: Array[Int], trips: List[String]) = {
    def calcFares(trips: List[Record], totalFares: Map[String, Double]): Map[String, Double] = trips match {
      case x :: y :: xs =>
        // tarifa entry.
        val dateTimeOfTrip = if (x.dateTime.compareTo(y.dateTime) < 0) x.dateTime else y.dateTime
        val fareForTrip = fareTable(dateTimeOfTrip.getHours) / 100.0 *
          // kilometers
          Math.abs(x.kilometers - y.kilometers) +
          // mas unoscala xor
          1
        // mas dos si no existe en el map
        val fareSoFare: Double = totalFares.getOrElse(x.numberPlate, 2);
        totalFares(x.numberPlate) = fareSoFare + fareForTrip;
        calcFares(xs, totalFares);
      case nil => totalFares;

    }
    def parseTrip(trip: String): Record = {
      //numberplate datetime enter/exit kilometros
      val tripSplit = trip.split(" ")
      val df = new SimpleDateFormat("MM:dd:HH:mm")
      //         numberplate   Date                    enter|exit               kilometros
      new Record(tripSplit(0), df.parse(tripSplit(1)), tripSplit(2) == "enter", tripSplit(3).toInt)

    }
    def getPairs(records: List[Record], pairs: List[Record]): List[Record] = records match {
      case x :: y :: xs => if (x.numberPlate == y.numberPlate && (x.enter) ^ (y.enter)) {
        getPairs(xs, x :: y :: pairs)
      } else { getPairs(y :: xs, pairs) }
      case nil => pairs
    }
    val parsedSortedTripsPairs = getPairs(sortFunction(trips).map(parseTrip), Nil)
    //horas * lookup ingreso
    parsedSortedTripsPairs.foreach(println)
    calcFares(parsedSortedTripsPairs, Map()).foreach(println)
  }

  //quickSort(sortMe).foreach(println)
  //quickSort(sortTwo).foreach(println)
  //println("-------------------")
  //radixSort(sortMe).foreach(c => println(c.toString()))
  //radixSort(sortTwo).foreach(println)
  //radixSort("1:2:3" :: "10103" :: "10204" :: "133" :: "223" :: "233" :: "213" :: "1334" :: "1333" :: "134" :: Nil).foreach(println);
  radixSort("ABCD123 01:01:06:01 enter 17" :: "765DEF 01:01:07:00 exit 9500" :: "ABCD123 01:01:08:03 exit 95" :: "765DEF 01:01:05:59 enter 17" :: Nil).foreach(println)

}