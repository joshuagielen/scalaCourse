package fileSearcher

object Runner extends App {
  def convertToBool(boolString : String) =
    List("true", "t") exists(trueVal => trueVal == boolString.toLowerCase)

  val matcher = args match {
    case Array(filter) => new Matcher(filter)
    case Array(filter, rootPath) => new Matcher(filter, rootPath)
    case Array(filter, rootPath, checkSubFolder) => new Matcher(filter, rootPath, convertToBool(checkSubFolder))
    case Array(filter, rootPath, checkSubFolder, contentFilter, _*) => new Matcher(filter, rootPath, convertToBool(checkSubFolder), Some(contentFilter))
  }

  val results = matcher.execute()

  println(s"Found ${results.length} matches:")
  args match {
    case Array(_,_,_,_,outputFilePath,_*) => {
      SearchResultWriter.writeToFile(outputFilePath, results)
      println(s"Results written to $outputFilePath")
    }
    case _ => SearchResultWriter.writeToConsole(results)
  }

}
