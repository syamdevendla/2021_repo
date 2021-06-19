
import scala.collection.mutable._


//* All prices tags will be in Pounds, 60p  => 0.6 pound
//* Input any string items
//* Assuming only small case items are allowed. So, converting all strings to lowercase.
//* only small case strings are allowed to display Items
//* User can add any item but other than sale items all are ignored.
//* An exception thrown for an empty list
//* Exception thrown:  Empty string for Item display and negative or zero prices display 

class shoppingCart() extends Exception{
  
   def getTotal(list: List[String]): Double ={
     if(list.isEmpty){
       throw new Exception("NO_ITEMS_SELECTED")
     }
    var itemsCount: HashMap[String,Long] = HashMap() 
    list.foreach{x =>
      val item = x.toLowerCase()
      if(itemAndValueMap.contains(item)){
        if(itemsCount.contains(item)){
          itemsCount(item) = itemsCount(item) + 1
        }
        else{
           itemsCount.put(item, 1)
        }
      }
    }
    return(__computeTotal(itemsCount))
  }
  private def __computeTotal(itemsCount: HashMap[String,Long]): Double ={
     var total: Double = 0
      itemsCount.foreach{
        case (key, value) => 
          total = total + (value * itemAndValueMap(key))
      }
     return total
   }
  
  //Add a new item in display(item name and price)
  def setItemForDisplay(item: (String,Double)) = {
    
    if(item._1.trim().isEmpty){
      throw new Exception("INVALID_ITEM")
    }
    if(item._2 <= 0){
      throw new Exception("INVALID_PRICE")
    } 
    itemAndValueMap.put(item._1.trim().toLowerCase(), item._2)
  }
  private var itemAndValueMap: HashMap[String,Double] = HashMap()   
}
//-> All prices tags will be in Pounds, 60p=> 0.6 pound
//0. Input any string items
//1. Assuming only small case items are allowed. So, converting all strings to lowercase.
//2. only small case strings are allowed to display Items
//3. User can add any item but other than sale items all are ignored.

object CheckOutUtil extends App {
  
  try{
    val chk = new shoppingCart()
    //setItemForDisplay
  }
  catch{
    case e: Throwable  => println(e)
  }

}