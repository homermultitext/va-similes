import scala.io._
import edu.holycross.shot.cite._
import java.io.PrintWriter

// Models for linking urls:
//
//https://homermultitext.github.io/facsimiles/venetus-a/223v/#17.2
//
//http://www.homermultitext.org/iipsrv?OBJ=IIP,1.0&FIF=/project/homer/pyramidal/deepzoom/hmt/vaimg/2017a/VA012RN_0013.tif&RGN=0.2856,0.2751,0.1094,0.04860&wID=5000&CVT=JPEG




// Get these with CEX library from archival release.
def collectData(fName : String = "simdataonly.cex") : Vector[String] = {
  Source.fromFile(fName).getLines.toVector
}


// given a vector of CEX lines, create a vector of markdown lines
def mdLines(cex: Vector[String] = collectData()) : Vector[String] = {
  val iipbase = "http://www.homermultitext.org/iipsrv?OBJ=IIP,1.0&FIF=/project/homer/pyramidal/deepzoom/hmt/vaimg/2017a/"
  val baseZoomUrl = "http://www.homermultitext.org/ict2/?urn="
  val facsBaseUrl = "https://homermultitext.github.io/facsimiles/venetus-a/"
  val markdown = for (ln <- cex.filter(_.nonEmpty)) yield {
    // record ID + 5 properties.  Grab the 5 properties for display:
    val cols = ln.split("#").toVector
    val simileNum = cols(1)
    val psgRef = CtsUrn(cols(2)).passageComponent
    val note = cols(3)
    val imgOpt = Cite2Urn.nullable(cols(4))
    val pgOpt = Cite2Urn.nullable(cols(5))


    // Page info may be lacking.  Format appropriate
    // display string depending on whether or not they are null URNs
    // for facimile editions links to page and to text passage
    val pg = pgOpt match {
      case None => "No page reference"
      case u : Option[Cite2Urn] => {
        val pgId = u.get.objectComponent
        val pgLink = facsBaseUrl + pgId + "/"
        s"[${pgId}]($pgLink)"
      }
    }
    val psg = pgOpt match {
      case None => psgRef
      case u : Option[Cite2Urn] => {
        val pgId = u.get.objectComponent
        val psgLink = facsBaseUrl + pgId + "/#" + psgRef
        s"[${psgRef}]($psgLink)"
      }
    }

    // Image info may be lacking  Format appropriate display string.
    imgOpt match {
      case None => s"| ${simileNum} | no image | *Iliad* ${psg} |   ${pg} | ${note} |"
      case uOpt : Option[Cite2Urn] => {
        val urn =  uOpt.get
        val img = urn.dropExtensions.objectComponent
        val roi = urn.objectExtension
        val imgUrl = iipbase + img + ".tif&RGN=" + roi + "&WID=100&CVT=JPEG"
        val imgLink =  " ![](" + imgUrl + ")"
         s"| ${simileNum} | ${imgLink} | *Iliad* ${psg} |  ${pg} | ${note} |"
      }
    }
  }
  markdown
}







def printMd(mdVector: Vector[String] = mdLines(),fName: String = "similes-proof.md") : Unit = {
  val hdr = "| Simile number | Image | Comments on |  Appears on  | Note   |\n| :------------- | :------------- |:------------- |:------------- |:------------- |\n"

  new PrintWriter(fName){write(hdr + mdVector.filter(_.nonEmpty).mkString("\n")); close;}
  println("\nNew file is in " + fName)
}


println("\n\nWrite new file with display of simile numbers:\n")
println("\tprintMd(MARKDOWNLINES, FILENAME)")
