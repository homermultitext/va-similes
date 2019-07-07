import scala.io._

val lines = Source.fromFile("simdataonly.cex").getLines.toVector

//urn:cite2:hmt:similemarkers.v1:1#1#urn:cts:greekLit:tlg0012.tlg001.msA:2.87#bee simile#urn:cite2:hmt:vaimg.2017a:VA026RN_0027@0.81061164,0.23298284,0.01768607,0.01328168#urn:cite2:hmt:msA.v1:26r


// http://www.homermultitext.org/iipsrv?OBJ=IIP,1.0&FIF=/project/homer/pyramidal/deepzoom/hmt/vaimg/2017a/VA012RN_0013.tif&RGN=0.2856,0.2751,0.1094,0.04860&wID=5000&CVT=JPEG

import edu.holycross.shot.cite._
val hdr = "| Simile number     | Image | Comments on |  Appears on  | Note   |\n| :------------- | :------------- |:------------- |:------------- |:------------- |\n"

val iipbase = "http://www.homermultitext.org/iipsrv?OBJ=IIP,1.0&FIF=/project/homer/pyramidal/deepzoom/hmt/vaimg/2017a/"
val baseZoomUrl = "http://www.homermultitext.org/ict2/?urn="
//http://www.homermultitext.org/iipsrv?OBJ=IIP,1.0&FIF=/project/homer/pyramidal/deepzoom/hmt/vaimg/2017a/VA012RN_0013.tif&RGN=0.2856,0.2751,0.1094,0.04860&wID=5000&CVT=JPEG

//urn#number#passage#label#imageroi#folio


val mdLines = for (ln <- lines.filter(_.nonEmpty)) yield {
  val cols = ln.split("#").toVector
  val simileNum = cols(1)
  val psg = CtsUrn(cols(2)).passageComponent
  val note = cols(3)
  val urnOpt = Cite2Urn.nullable(cols(4))
  val pgOpt = Cite2Urn.nullable(cols(5))
  val pg = pgOpt match {
    case None => "No page reference"
    case u : Option[Cite2Urn] => u.get.objectComponent
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
