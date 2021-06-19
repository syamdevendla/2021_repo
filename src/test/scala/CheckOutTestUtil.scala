import collection.mutable.Stack
import org.scalatest._
import flatspec._
import matchers._

import org.scalatest.BeforeAndAfter

  //1. EmptyList shouuld return exception
  //2. GetTotal for only displayed items and discard other items
  //3. Enter garbage chars in list and check for output
  //4. Enter only apples/Oranges and check price
  //6. Enter mix of orange and apple and check price
  //7. Enter lot varieties in list along with orange and apple
  //8. java.lang.Exception: NO_ITEMS_SELECTED 



class checkOutTestUtil extends AnyFlatSpec with should.Matchers with BeforeAndAfter {
  var sCartObj: shoppingCart = _

  before {
    sCartObj = new shoppingCart
    sCartObj.setItemForDisplay("aPPle",0.6)
    sCartObj.setItemForDisplay("orAnge",0.25)
  }
  "A Checkout System" should "throw INVALID_PRICE if item price is <= 0" in {
         a [Exception] should be thrownBy {
          sCartObj.setItemForDisplay("apple",-1)
    }  
  }
  it should "throw INVALID_ITEM if item name is empty" in {
         a [Exception] should be thrownBy {
          sCartObj.setItemForDisplay(" ",1)
    }  
  }
  it should "throw NO_ITEMS_SELECTED if checkout list is empty" in {
         a [Exception] should be thrownBy {
          sCartObj.getTotal(List())
    }  
  }
  it should "use orange price as 0.25 and apple price 0.6" in { 
    val list = List("apPle","apPLe","Orange","aPplE")
    sCartObj.getTotal(list) should be (2.05)
  }
  it should "use orange price as 0.25, apple price 0.6 and discard other items" in {
    val list = List("apPle","grapes","Orange","PaPplE")
    sCartObj.getTotal(list) should be (0.85)
  }
  it should "use orange price as 0.25" in {
    val list= List("Orange","Orange","Orange","Orange","Orange","Orange","Orange","Orange","Orange","Orange")
    sCartObj.getTotal(list) should be (2.5)
  }
  it should "use list with no apple,orange and include garbage text for one of the item" in {
    val list = List("banana","grapes","strawberry","t@#$HYP")
    sCartObj.getTotal(list) should be (0)
  }
  
  after {
    //clean after test
  } 
}






