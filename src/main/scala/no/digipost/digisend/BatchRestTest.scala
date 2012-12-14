package no.digipost.digisend

import org.apache.commons.io.IOUtils
import org.joda.time.DateTime

import XmlTypes._

object BatchRestTest extends App with BatchClient with RestClient {
  val senderId = "179074"
  val filename1 = "ikke-gyldig-for-print-mange-sider.pdf"
  val filename2 = "rema.pdf"
  val fileContent1 = IOUtils.toByteArray(getClass.getResourceAsStream("/" + filename1))
  val fileContent2 = IOUtils.toByteArray(getClass.getResourceAsStream("/" + filename2))

  val xml = masseutsendelse(
    jobbinstillinger(senderId, "Jubajubajobb"),
    Seq (
      brev("pdf1", filename1, "Brev til deg", smsVarsling(Seq(new DateTime(2012, 12, 24, 10, 0)), Seq(2, 4))),
      brev("pdf2", filename2, "Annet brev til deg", smsVarsling(Seq(new DateTime(2012, 12, 24, 10, 0)), Seq(2, 4)))
    ),
    Seq(
      forsendelse("pdf1", NavnOgAdresse(FulltNavnFornavnForst("Frode Nerbråten"), Adresse("Hovsving 28", "2069", "Jessheim")), null),
      forsendelse("pdf2", Digipostadresse("wika.czarnowski.maciej.von#70UA"), null)
    ))
  println(xml)
  val zipArchive = zip(xml, Map(filename1 -> fileContent1, filename2 -> fileContent2))
//  FileUtils.writeByteArrayToFile(new File("/Users/frode/digipost/home/ftp/test_4/jail/masseutsendelse/masseutsendelse.zip"), zipArchive)

//  sftpUpload(senderId, "sending.zip", zipArchive)
  sendRestApi(senderId, xmlToMessages(xml))
}
