package fileSearcher

import java.io.File

import org.scalatest.FlatSpec

class MatcherTests extends FlatSpec{

  val testFilesRootPath = "/home/jozua/Documents/projects/Filesearcher/"

  "Matcher that is passed a file matching the filter" should "return a list with that file name" in {
    val matcher = new Matcher("fake", "fakePath") //fake path wordt opgegeven

    val results = matcher.execute()

    assert(results == List((s"${testFilesRootPath}testfiles", None)))
  }

  "Matcher using a directory containing one file matching the filter" should "return a list with that file name" in {
    val matcher = new Matcher("txt", new File("./testfiles").getCanonicalPath()) // pass matcher a filter to look for files containing txt

    val results = matcher.execute()

    assert(results == List((s"${testFilesRootPath}readme.txt", None)))
  }

  "Matcher that is not passed a root file location" should "use the current location" in {
    val matcher = new Matcher("filter")
    assert(matcher.rootLocation == new File(".").getCanonicalPath())
  }

  "Matcher with sub folder checking matching a root location with two subtree files matching" should "return a list with those file names" in {
    val searchSubDirectories = true
    val matcher = new Matcher("txt", new File("./testfiles").getCanonicalPath(), searchSubDirectories)

    val results = matcher.execute()

    assert(results == List((s"${testFilesRootPath}randomfolder/notes.txt", None), (s"${testFilesRootPath}readme.txt", None)))
  }

  "Matcher given a path that has one file that matches file filter and content filter" should "return a list with that file name" in {
    val matcher = new Matcher("data", new File(".").getCanonicalPath(), true, Some("pluralsight"))

    val matchedFiles = matcher.execute()

    assert(matchedFiles == List((s"${testFilesRootPath}pluralsight.data", Some(3))))
  }

  "Matcher given a path that has no file that matches file filter and content filter" should "return a empty list" in {
    val matcher = new Matcher("txt", new File(".").getCanonicalPath(), true, Some("pluralsight"))

    val matchedFiles = matcher.execute()

    assert(matchedFiles == List())
  }

}
