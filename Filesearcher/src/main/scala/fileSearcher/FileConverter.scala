package fileSearcher

import java.io.File
import javax.tools.FileObject

object FileConverter {
  def convertToIOOject(file: File) =
    if(file.isDirectory()) DirectoryObject(file) // check if directory --> put in Directory Object
    else FileObject(file) // if file --> put in FileObject
}

//In deze methode wordt er gekeken of het bestand een folder is
