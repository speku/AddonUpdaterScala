import java.io.File
import scala.io.Source._
import java.net.URL
import sys.process._
import net.lingala.zip4j.exception.ZipException;
import net.lingala.zip4j.core.ZipFile;
import scala.actors._
import Actor._
import scala.util.{Try, Success, Failure}

object Logic extends App {
  
  def getPath = {
    val regex = "(.*World of Warcraft).*".r
    val e = sys.env.get("WorldOfWarcraft")
    e match {
      case Some(p) => p match {
        case regex(g) if (new File(g).exists) => g
        case _ => sys.exit
      }
      case None => sys.exit
    }
  }
  
  def getAddons(p: String): Set[String] = {
    val reg = """## X-Curse-Project-ID: (.*)\W*""".r.unanchored
    lazy val reg2 = """## Title: (.*)\W*""".r.unanchored
    
    def getId(f: File): Option[String] = {
      Try(fromFile(f.toString + "/" + f.getName + ".toc").mkString) match {
        case Success(p) => p match {
          case reg(x) => Some(x)
          case reg2(x) => Some(x.replaceAll("\\s", "-"))
          case _ => None
        }
        case _ => None
      }
    }
    
    def mapFile(f: File) = {
      getId(f) match {
        case Some(id) => Set(id, f.getName)
        case None => Set(f.getName)
      }
    }
    new File(p + "/Interface/AddOns").listFiles.map(mapFile).flatten.toSet
  }
  
  def getURLdownloadUnzip(xs: Set[String], path: String) = {
   val base = "http://wow.curseforge.com/addons/%s/files/"
   lazy val base2 = "http://wow.curseforge.com/search/?search=%s"
   val regex1 = "<td class=\"col-file\"><a href=\".*?/files/(.*?)\">".r.unanchored
   val regex2 = "user-action user-action-download.*?href=\"(.*?)\">Download".r.unanchored
   val regex3 = ".*/([^/]*zip)".r.unanchored
   lazy val regex4 = "(?s)<tr class=\"odd row-joined-to-next\">.*?addons/(.+?)/\"".r.unanchored
   
   def getName(url: String) = {
     url match {
       case regex3(g) => g
     }
   }
   
   def download(url: String) {
     val y: File = new File(path + "/Interface/AddonUpdater/" + getName(url))
     (new URL(url) #> y).!
     new ZipFile(y).extractAll(path + "/Interface/AddOns")
   }
   
   def getURL(a: String, alrdy: Boolean = false) {
     val url = base.format(a)
      try {fromURL(url).mkString match {
        case regex1(e) => {fromURL(url + e).mkString match {
          case regex2(g) if (isValid(g)) => download(g)
          case _ =>}}}
      } catch {
        case e: Exception if (!alrdy) => {fromURL(base2.format(a.replaceAll("\\s", "+"))).mkString match {
          case regex4(g) if (isSimilar(a, g) >= 0.5) => getURL(g, true)
          case _ =>}}
      }
   }
    
    def isValid(url: String) = {
     val u = url.toLowerCase
     (u.contains("curse") || u.contains("wowace") || u.contains("wowinterface")) && (u.contains(".zip") || u.contains(".rar"))
    }
      
    def isSimilar(s1: String, s2: String): Double = {
      def strToPairs(s: String, acc: List[String]): List[String] = {
        if (s.size < 2) acc
        else strToPairs(s.drop(1),
          if (s.take(2).contains(" ")) acc else acc ::: List(s.take(2)))
      }
      val lst1 = strToPairs(s1.trim.replaceAll("\\W", "").toUpperCase, List())
      val lst2 = strToPairs(s2.trim.replaceAll("\\W", "").toUpperCase, List())
      (2.0 * lst2.intersect(lst1).size) / (lst1.size + lst2.size)
    }
    
    def prepare() {
      val f = new File(path + "/Interface/AddonUpdater")
      if (f.exists) {
        f.listFiles foreach (_.delete)
      } else {
        f.mkdir
      }
    }
    
    prepare
    xs foreach (f => actor {self ! getURL(f)})
  }
  
  val path = getPath
  getURLdownloadUnzip(getAddons(path), path)
}