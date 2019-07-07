import scala.io._


// Get these with CEX library from archival release.
val lines = Source.fromFile("simdataonly.cex").getLines.toVector



// MOdels for linking urls:

//https://homermultitext.github.io/facsimiles/venetus-a/223v/#17.2
//
//http://www.homermultitext.org/iipsrv?OBJ=IIP,1.0&FIF=/project/homer/pyramidal/deepzoom/hmt/vaimg/2017a/VA012RN_0013.tif&RGN=0.2856,0.2751,0.1094,0.04860&wID=5000&CVT=JPEG


import edu.holycross.shot.cite._
val hdr = "| Simile number ß| Image | Comments on |  Appears on  | Note   |\n| :------------- | :------------- |:------------- |:------------- |:------------- |\n"

val iipbase = "http://www.homermultitext.org/iipsrv?OBJ=IIP,1.0&FIF=/project/homer/pyramidal/deepzoom/hmt/vaimg/2017a/"
val baseZoomUrl = "http://www.homermultitext.org/ict2/?urn="
val facsBaseUrl = "https://homermultitext.github.io/facsimiles/venetus-a/"

val mdLines = for (ln <- lines.filter(_.nonEmpty)) yield {
  val cols = ln.split("#").toVector
  val simileNum = cols(1)
  val psgRef = CtsUrn(cols(2)).passageComponent


//223v/#17.2

  val note = cols(3)
  val urnOpt = Cite2Urn.nullable(cols(4))
  val pgOpt = Cite2Urn.nullable(cols(5))

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

  urnOpt match {
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

import java.io.PrintWriter

new PrintWriter("similes-proof.md"){write(hdr + mdLines.filter(_.nonEmpty).mkString("\n")); close;}
