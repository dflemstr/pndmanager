package se.dflemstr.pndmanager.util

import _root_.scala.xml._
import _root_.scala.actors._
import _root_.scala.actors.Actor._
import _root_.java.io._
import _root_.net.liftweb.util._
import _root_.net.liftweb.http._
import _root_.se.dflemstr.pndmanager.model._

/**
 * Clones the whole repository to the specified path. This won't clone
 * the existing repository; just create new packages, delete old ones, and update
 * the repository metadata
 */
case class RepositoryCloner(path: String) extends Actor {
  val destDirectory = new File(path)
  require(destDirectory.isDirectory, "The repository clone path must be a directory!")
  require(destDirectory.canWrite, "The repository clone path must be writable!")

  val packagesDirectory = destDirectory.listFiles.find(_.getName == Package.digestAccessNode) match {
    case Some(file) => file
    case None =>
      val newDir = new File(destDirectory.getCanonicalPath + File.separator + Package.digestAccessNode)
      if(newDir.mkdir)
        newDir
      else
        error("Unable to access the packages directory for the repo clone and were unable to create it")
  }

  def updateRepoMetadata() = {
    //TODO: actually implement this
  }

  def act = {
    loop {
      react {
        case NewPackageNotification(pkg) =>
          val newPackageFile = new File(packagesDirectory.getCanonicalPath + File.separator + pkg.fileName)
          if(!newPackageFile.exists && newPackageFile.createNewFile()) { //File must not exist and must be created
            val output = new FileOutputStream(newPackageFile)
            try {
              output.write(pkg.pndFile.is)
            } catch {
              case ex => Log.warn("There was an error writing a package file: " + ex.getMessage)
            } finally {
              output.close()
            }
            updateRepoMetadata()
          } else Log.warn("Cannot create file (it might already exist): " + newPackageFile.getCanonicalPath)
        case PackageRemovalNotification(pkg) =>
          if(new File(packagesDirectory.getCanonicalPath + File.separator + pkg.fileName).delete())
            updateRepoMetadata()
          else
            Log.warn("Unable to delete the file " + pkg.fileName + " from the repo clone.")
      }
    }
  }
}
