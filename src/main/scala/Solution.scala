/**
 * Created by mikelsanvicente on 20/01/16.
 */
object Solution {

  val pattern = "^([a-zA-Z0-9]+)\\, ?([a-zA-Z0-9]+)$".r

  def getMaxDuplicatedVendors(input: Array[String]) : Array[String] = {
    val items = parseAndCleanErrorsAndDuplicated(input)

    val duplicatedPerVendor = duplicatedByVendor(items, getDuplicatedItems(items))

    getMaxDuplicatedVendors(duplicatedPerVendor)
  }

  def parseAndCleanErrorsAndDuplicated(items:Array[String]) : Stream[(String,String)] = {
    items.toStream.flatMap( item =>
      item match {
        case pattern(vendorId, itemId) => Some((vendorId.trim, itemId.trim))
        case _ => None
      }
    ).distinct
  }

  def getDuplicatedItems(items: Stream[(String,String)]): List[String] = {
    items.
      groupBy(item=> item._2).
      mapValues(items => items.size ).
      filter( _._2 > 1 ).keys.toList
  }

  def duplicatedByVendor(items: Stream[(String,String)], duplicatedItems:List[String]): Map[String,Int] = {
    items.
      groupBy[String](item=> item._1).
      mapValues[Stream[String]](items =>  items.map(item => item._2)).
      mapValues[Int](items => items.intersect(duplicatedItems).size )
  }

  def getMaxDuplicatedVendors(duplicatedPerVendor: Map[String,Int]) = {
    val maxDuplicated = duplicatedPerVendor.maxBy(_._2)._2

    duplicatedPerVendor.filter( _._2 == maxDuplicated).map(_._1).toArray
  }
}
