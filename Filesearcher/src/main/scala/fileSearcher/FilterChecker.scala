package fileSearcher

import java.io.File
import scala.util.control.NonFatal

class FilterChecker(filter: String) { //valueobject maark is private omdat val keyword niet wordt gebruikt

  val filterAsRegex = filter.r

  def matches(content : String) =
    filterAsRegex findFirstMatchIn content match {
      case Some(_) => true
      case None => false
    }

  def findMatchedFiles(iOObject : List[IOObject]) =
    for(iOObject <- iOObject
        if(iOObject.isInstanceOf[FileObject])//for comprehension = looping thrue list extract each file object from the list
        if(matches(iOObject.name))) // check if the name mathces the given filter
      yield iOObject // recreate the list vai yielding of each matching file

  def findMatchedContentCount(file: File) = {

    def getFilterMatchCount(content: String) =
      (filterAsRegex findAllIn content).length

    import scala.io.Source
    try {
      val fileSource = Source.fromFile(file)
      try {
        fileSource.getLines().foldLeft(0)(
          (accumulator, line)=> accumulator + getFilterMatchCount(line))
      }
      catch {
        case NonFatal(_) => 0
      }
      finally
        fileSource.close()
    }
    catch {
      case NonFatal(_) => 0
    }

  }

}

//FilterChecker is a companion object
object FilterChecker { // Scala object is a way to create a singleton or static class that can have methods called directly on it without using the new keyword
  def apply(filter: String): FilterChecker = new FilterChecker(filter) //elk object kan deze methode oproepen
}

// infix notation: scala laat die toe om elke methode die één parameter bevat te laten oproepen zonder een dot of brackets

