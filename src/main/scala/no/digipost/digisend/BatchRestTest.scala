package no.digipost.digisend

import org.joda.time.DateTime

import XmlTypes._
import Util._
import xml.PrettyPrinter
import util.Random

object BatchRestTest extends App with BatchClient with RestClient {
  val senderId = "6"
  val filename1 = "gyldig-for-print.pdf"
  val filename2 = "brev.pdf"
  val fileContent1 = fileAsBytes(filename1)
  val fileContent2 = fileAsBytes(filename2)

  val xml = Masseutsendelse(
    Jobbinstillinger(senderId, Random.nextString(4), "Frodes jobb " + Random.nextInt(), autogodkjenn = true),
    Seq (
      Brev("pdf1", filename1, "Brev til deg", Smsvarsling(Seq(new DateTime(2012, 12, 24, 10, 0)), Seq(2, 4))),
      Brev("pdf2", filename2, "Annet brev til deg", null)
    ),
    Seq(
      Forsendelse("pdf1", NavnOgAdresse(FulltNavnFornavnForst("Frode Nerbråten"), Adresse("Testveien", "1002", "Oslo")), null),
      Forsendelse("pdf2", Digipostadresse("frode.nerbråten#1234"), null),
      Forsendelse("pdf2", Digipostadresse("frode.nerbraaten#1234"))
    ))

  //debug
  println(new PrettyPrinter(150, 2).format(xml))

  //lag massutsendelse
  val zipArchive = zip(xml, Map(filename1 -> fileContent1, filename2 -> fileContent2))

  sftpUpload(senderId, "sending.zip", zipArchive, passphrase = "test")

  val c = client(senderId.toInt, "certificate.p12", logging = true, host = "https://qa.api.digipost.no")
  sendRestApi(c)(xmlToMessages(xml))
}
